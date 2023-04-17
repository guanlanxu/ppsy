#' Run equating analysis and evaluate stableness of the anchor set.
#'
#' @param admin String. Administration.
#' @param testcode String.
#' @param mode E or P
#' @param RSnum String. RS1-3.
#' @param mock T or F.
#' @param purpose normal or maintain.
#' @param direction normal or backward.
#' @param anchor_var variable used for anchor. "PREEQUATING" or "LINKING_STATUS".
#' @param dropuin UIN that dropped in calibration.
#' @param dropseq IRTLOC that dropped in calibration.
#' @param dnu Do-NOT-USE items in UIN.
#' @param dropuin_anchor Item UIN that to be dropped from anchor set.
#' @param drop_passid PassageID that to be dropped from anchor set.
#' @param forcein_uin Item UIN that is forced to be in anchor set.
#' @param forcein_passid PassageID that is forced to be in anchor set.
#' @param iter number of iteration cycle.
#' @param get_plot T or F. Whether to get plot.
#' @return anch_file: Parameter file about anchor items.
#' @return passageDrift: Parameter file for passage drift analysis.
#' @return drifted: List of items that are drifted.
#' @return BP_cover: Blueprint coverage of the anchor set.
#' @return irt_file: Scaled and unscaled parameter file.
#' @return internalirt: Item parameter file for internal use.
#' @return m1m2: Stocking_Lord constants from STUIRT.
#' @return base_vs_freeirt_summary: Summary statistics about base and unscaled parameters.
#' @return plot_reivew: plots.


eqSTUIRT_4drift <- function(admin,testcode,  mode, status, RSnum,mock,
                            purpose = "normal",direction = "normal",anchor_var="PREEQUATING",
                            dropuin ="",dropseq ="",dnu = "", dropuin_anchor ="",
                            drop_passid = "", forcein_uin = "", forcein_passid = "",iter="",
                            get_plot = T ){

  library(dplyr)
  library(haven)
  library(data.table)
  library(foreign)
  library(stringr)
  library(ppsy)
  library(gridExtra)
  library(grid)

  mypath = get_path(admin = admin, testcode = testcode, mode = mode, mock = mock,status = status, RSnum = RSnum,iter = iter,
                    purpose = purpose, pre_post = "post")
  test = mypath$test

  ######------------------------------------
  ##  A. Load item parameters
  ######---------------------------------------
  bank_par_org <- read_sas(mypath$bank_par_path)
  bank_par_org = as.data.frame(bank_par_org)
  colnames(bank_par_org) = toupper(colnames(bank_par_org))
  colnames(bank_par_org)[which( colnames(bank_par_org) == "ITEM_TOTAL_CORRELATION")] = "BASE_PTBIS"
  colnames(bank_par_org)[which( colnames(bank_par_org) == "P_VALUE")] = "BASE_PVALUE"
  colnames(bank_par_org)[which( colnames(bank_par_org) %in% paste0("IRT_",c("A","B","C",paste0("STEP",c(1:6)))))] =  gsub(x = colnames(bank_par_org)[which( colnames(bank_par_org) %in% paste0("IRT_",c("A","B","C",paste0("STEP",c(1:6)))))], pattern = "IRT", replacement = "BASE")

  cal_par <- read.csv(mypath$irtwq1_path_csv)
  colnames(cal_par) = toupper(colnames(cal_par))
  cal_par$PVALUE = cal_par$AVERAGE_ITEM_SCORE/cal_par$MAX_POINTS_OBS
  cal_par_uin = cal_par$UIN

  # A1. identify linking items
  if (toupper(purpose) == 'MAINTAIN') {
    # this is for pre-eq check and bank maintainness.
    # Use all OP items in the bank_year as anchor
    bank_par_org = bank_par_org[which(bank_par_org$ITEM_STATUS == "OP"),]
    bank_par_org$LINKING = ifelse(bank_par_org$SPOILED_ITEM == "" &
                                  ! bank_par_org$UIN %in% c(dropuin, dnu) &
                                  ! bank_par_org$IDMSEQ %in% dropseq &
                                  !bank_par_org$UIN %like% "_D" &
                                  bank_par_org$PARAMETER_YEAR == "SPRING 2022", "L","") # double check this rule!!!!


  } else if (toupper(purpose) != 'MAINTAIN' | is.na(purpose)) {

    if (toupper(status) == 'OP') {
      # bank par is in RA folder and free par is freeirt
      bank_par_org = bank_par_org[which(bank_par_org$ITEM_STATUS == "OP"),]
      bank_par_org$LINKING = ifelse( bank_par_org$SPOILED_ITEM == "" &
                                     ! bank_par_org$UIN %in% c(dropuin, dnu) &  ! bank_par_org$IDMSEQ %in% dropseq &  # remove items excluded from calibration
                                     ! bank_par_org$UIN %like% "_D" &  # remove trait items
                                     substr(bank_par_org$FORM_ID, 14, 14) == 1 &     # remove core 2 in anchor &
                                     bank_par_org[, which(toupper(colnames(bank_par_org)) == toupper(anchor_var))] == "Y"
                                      ,"L", "")
   # remove passages identified to drop
      bank_par_org$LINKING = ifelse((bank_par_org$LINKING== "L")  & (!is.na(bank_par_org$PASSAGE_NUMBER) & bank_par_org$PASSAGE_NUMBER != "" & bank_par_org$PASSAGE_NUMBER %in% drop_passid),"", bank_par_org$LINKING)
   # add forcein items
      bank_par_org$LINKING = ifelse((bank_par_org$LINKING== "") &
                                     ((!is.na(bank_par_org$UIN) & bank_par_org$UIN!="" & bank_par_org$UIN %in% forcein_uin) |
                                      (!is.na(bank_par_org$PASSAGE_NUMBER) & bank_par_org$PASSAGE_NUMBER !="" &   bank_par_org$PASSAGE_NUMBER %in% forcein_passid))
                                    , "L", bank_par_org$LINKING)
   #   bank_par_org$LINKING = ifelse((bank_par_org$UIN %in% dropuin_anchor), "", bank_par_org$LINKING)  # remove C-DIF items and other drop items

      if (toupper(direction) == "BACKWARD") {
        bank_par_org_tmp = bank_par_org
        cal_par_tmp = cal_par
        bank_par_org = cal_par_tmp
        cal_par = bank_par_org_tmp

        #rename
        colnames(bank_par_org)[which( colnames(bank_par_org) %in% paste0("UN_IRT_",c("A","B","C",paste0("STEP",c(1:6)))))] = paste0("BASE_",c("A","B","C",paste0("STEP",c(1:6))))
        colnames(bank_par_org)[which( colnames(bank_par_org) %in% paste0("UN_IRT_",c("A","B","C",paste0("STEP",c(1:6))),"_SE"))] = paste0("BASE_",c("A","B","C",paste0("STEP",c(1:6))),"_SE")
        colnames(bank_par_org)[which( colnames(bank_par_org) == "ITEM_TOTAL_CORRELATION")] = "BASE_PTBIS"
        colnames(bank_par_org)[which( colnames(bank_par_org) == "PVALUE")] = "BASE_PVALUE"

        colnames(cal_par)[which( colnames(cal_par) %in% paste0("BASE_",c("A","B","C",paste0("STEP",c(1:6)))))] = paste0("UN_IRT_",c("A","B","C",paste0("STEP",c(1:6))))
        colnames(cal_par)[which( colnames(cal_par) == "BASE_PTBIS")] = "ITEM_TOTAL_CORRELATION"
        colnames(cal_par)[which( colnames(cal_par) == "BASE_PVALUE")] = "PVALUE"


      }


    } else if (toupper(status) == "FT") {
      # use final OP as linking.
      bank_par_org = read.csv(paste0(mypath$path,"ANALYSES/EQ/OP/",RSnum,"/Final_Par/",toupper(testcode),"/", toupper(admin), "_irt_", toupper(testcode), "_",toupper(mode),"_OP_GX.csv"))
      bank_par_org = as.data.frame(bank_par_org)
      colnames(bank_par_org) = toupper(colnames(bank_par_org))
      bank_par_org$LINKING = ifelse(bank_par_org$STATUS == "OP", "L","")
      colnames(bank_par_org)[which( colnames(bank_par_org) %in% paste0("IRT_",c("A","B","C",paste0("STEP",c(1:6)))))] = paste0("BASE_",c("A","B","C",paste0("STEP",c(1:6))))

      cal_par <- read.csv(mypath$freeirt_path_csv)
      colnames(cal_par) = toupper(colnames(cal_par))

    }

  }


  # A2. compare p-value for passage drift (only for OP)
  if (toupper(status)=="OP"){
    passageDrift = left_join(cal_par, bank_par_org, by = c("UIN"="UIN","MODE"="MODE","MAX_POINTS"="MAX_POINTS","ITEM_STATUS"="ITEM_STATUS"))
    passageDrift$DIFF_A = passageDrift$UN_IRT_A - passageDrift$BASE_A
    passageDrift$DIFF_B = passageDrift$UN_IRT_B - passageDrift$BASE_B
    passageDrift$DIFF_C = passageDrift$UN_IRT_C - passageDrift$BASE_C
    passageDrift$DIFF_PVALUE = passageDrift$PVALUE - passageDrift$BASE_PVALUE
    passageDrift$DIFF_PTBIS = passageDrift$ITEM_TOTAL_CORRELATION - passageDrift$BASE_PTBIS
    passageDrift$DIFF_POS = passageDrift$SEQUENCE - passageDrift$BANK_SEQUENCE

    count1 = passageDrift[which(passageDrift$OBJECTIVE_1 != "" & passageDrift$ITEM_STATUS == "OP" &
                         (substr(passageDrift$FORM_ID,14,14)==1 | (substr(passageDrift$FORM_ID,14,14)==2) & passageDrift$LINKING == "L")
    ),] %>%
      group_by(OBJECTIVE_1) %>%
      summarise(allOP_N=n()) %>%
      mutate(allOP_pct = prop.table(allOP_N)*100)

    count2 = passageDrift[which(passageDrift$LINKING == "L"),] %>%
      group_by(OBJECTIVE_1) %>%
      summarise(link_N=n()) %>%
      mutate(link_pct = prop.table(link_N)*100)


    all.par =  passageDrift[which(passageDrift$LINKING == "L" &
                                  !passageDrift$UIN %in% c("",dropuin_anchor) & # drop C-DIF items
                                  ! is.na(passageDrift$UIN)),
                            c("UIN",paste0("UN_IRT_",c("A","B","C",paste0("STEP",c(1:6)))),"SCORE_CAT","MODEL","MAX_POINTS",
                                                                 paste0("BASE_",c("A","B","C",paste0("STEP",c(1:6)))),"OBJECTIVE_1")]

    count3 = all.par %>%
      group_by(OBJECTIVE_1) %>%
      summarise(final_N=n()) %>%
      mutate(final_pct = prop.table(final_N)*100)

    count4 = passageDrift[which(passageDrift$LINKING == "L" & passageDrift$UIN %in% c("",dropuin_anchor)),] %>%
      group_by(OBJECTIVE_1) %>%
      summarise(items_dropped = n())

    # A3. evaluate blueprint coverage
    BP_cover = left_join(left_join(left_join(count1,count2,by = "OBJECTIVE_1"),count3, by = "OBJECTIVE_1"),count4, by = "OBJECTIVE_1")
    BP_cover$diff_OP_pct = BP_cover$allOP_pct - BP_cover$final_pct
    BP_cover[(nrow(BP_cover)+1),2:ncol(BP_cover)] = as.list(colSums(BP_cover[,2:ncol(BP_cover)],na.rm = T))

    # A4. review and compare free and base statistics
    base_vs_un_par = psych::describe(data.frame(diff_a = all.par$UN_IRT_A - all.par$BASE_A,
                                                diff_b = all.par$UN_IRT_B - all.par$BASE_B,
                                                diff_c = all.par$UN_IRT_C - all.par$BASE_C))

    base_vs_un_ctt = psych::describe(data.frame(un_pval = passageDrift[which(passageDrift$LINKING == "L"),]$PVALUE,
                                                base_pval = passageDrift[which(passageDrift$LINKING == "L"),]$BASE_PVALUE,
                                                un_ptbis= passageDrift[which(passageDrift$LINKING == "L"),]$ITEM_TOTAL_CORRELATION,
                                                base_ptbis= passageDrift[which(passageDrift$LINKING == "L"),]$BASE_PTBIS))

    nOP = nrow(passageDrift[which(substr(passageDrift$FORM_ID,14,14) ==1),])  # dbl check in Spring23!!!

    write.csv(passageDrift,paste0(mypath$drift_path,toupper(admin),"_passDrift_",toupper(testcode),"_",toupper(status),"_", toupper(mode),"_GX.csv"),row.names = F, na = "")

    } else {
    passageDrift = inner_join(cal_par[,c("UIN",paste0("UN_IRT_",c("A","B","C",paste0("STEP",c(1:6)))),"SCORE_CAT","MODEL","MAX_POINTS","ITEM_STATUS")],
                                      bank_par_org[,c("UIN",paste0("BASE_",c("A","B","C",paste0("STEP",c(1:6)))))],by = "UIN")
    all.par = passageDrift[which(passageDrift$ITEM_STATUS == "OP" & !passageDrift$UIN %in% c("",dropuin_anchor) & ! is.na(all.par$UIN)),]

    base_vs_un_par = NA
    base_vs_un_ctt = NA
    BP_cover = NA
    nOP = nrow(passageDrift[which(passageDrift$ITEM_STATUS == "OP"),])

    }

  ######------------------------------------
  ##  B. Anchor Stability: get m1m2 and D2 evaluation for OP run
  ######---------------------------------------

  # B1. Extract pars for stuirt
  ne_par <- all.par[which(!is.na(all.par$UN_IRT_A) ), c("SCORE_CAT","UN_IRT_A","UN_IRT_B","UN_IRT_C",
                                                       "UN_IRT_STEP1","UN_IRT_STEP2","UN_IRT_STEP3",
                                                       "UN_IRT_STEP4","UN_IRT_STEP5","UN_IRT_STEP6","UIN","MODEL")]
  ol_par <- all.par[which(!is.na(all.par$BASE_A)), c("SCORE_CAT","BASE_A","BASE_B","BASE_C",
                                                     "BASE_STEP1","BASE_STEP2","BASE_STEP3",
                                                     "BASE_STEP4","BASE_STEP5","BASE_STEP6","UIN","MODEL")]

  # B2. get m1m2
  if (toupper(status) == "OP"){
    anchor_d2 = anchor_d2check(ne_par = ne_par, ol_par = ol_par,
                               mypath$stuirt_con_path, mypath$stuirt_out_path,
                               equate = T, get_plot = T)

    anch_prms = left_join(anchor_d2$anch_prms, passageDrift[,c("UIN","IRTLOC")], by = "UIN")
    anch_prms$TESTCODE = toupper(testcode)
    anch_prms_print = anch_prms[,c("IRTLOC","TESTCODE","UIN","MAX_POINTS","chng_A","chng_B","WRMSD","sortDiff","WRMSD_FLAG")]

    total_rows_per_page = 30
    start_row = 1

    if(total_rows_per_page > nrow(anch_prms_print)){
      end_row = nrow(anch_prms_print)
    }else {
      end_row = total_rows_per_page
    }

    pdf(mypath$drift_plot_path)

    for(i in 1:ceiling(nrow(anch_prms_print)/total_rows_per_page)){
      grid.newpage()
      grid.table(anch_prms_print[start_row:end_row, ], rows=NULL,theme =ttheme_default(base_size = 7))
      start_row = end_row + 1
      if((total_rows_per_page + end_row) < nrow(anch_prms_print)){
        end_row = total_rows_per_page + end_row
      }else {
        end_row = nrow(anch_prms_print)
      }
    }

    invisible(lapply(anchor_d2$ICC_plot, print))
    dev.off()

    # get delete-one m1m2 (only for OP and forward)
    if (toupper(direction) != 'BACKWARD' | is.na(direction)){
      rm1_m1m2 = data.frame(matrix(NA, nrow = nrow(ne_par),ncol = ncol(anchor_d2$m1m2)))
      colnames(rm1_m1m2) = colnames(anchor_d2$m1m2)

        for (a in 1:nrow(ne_par)){
        iter_m1m2 = anchor_d2check(ne_par = ne_par[-a,], ol_par = ol_par[-a,],
                       paste0(mypath$drift_path,"STUIRT/",toupper(admin),"_drift4TCC_",toupper(testcode),"_item",a,"del_GX",iter,".txt"),
                       paste0(mypath$drift_path,"STUIRT/",toupper(admin),"_drift4TCC_",toupper(testcode),"_item",a,"del_GX",iter,".out"),
                       equate = F, get_plot = F)
        rm1_m1m2[a,]= iter_m1m2$m1m2
      }
      rm1_m1m2$TESTCODE = toupper(testcode)
      rm1_m1m2$UIN = ne_par$UIN

   }

  } else {
    anchor_d2 = anchor_d2check(ne_par = ne_par, ol_par = ol_par,
                               mypath$stuirt_con_path, mypath$stuirt_out_path,
                               equate = F, get_plot = F)
  }

  write.csv(anchor_d2$m1m2,mypath$m1m2,row.names = F,na = "")



  ######------------------------------------
  ##  C. Equate non-anchor items based on m1m2
  ######---------------------------------------

  m1 = anchor_d2$m1m2$m1
  m2 = anchor_d2$m1m2$m2

  if (toupper(status) == "OP" & toupper(direction) == "BACKWARD"){
    # since backward, the current par is base, so EQ pars = free pars

    final.par = bank_par_org
    final.par$m1 = m1
    final.par$m2 = m2

    final.par$EQ_A = final.par$BASE_A
    final.par$EQ_B = final.par$BASE_B
    final.par$EQ_C = final.par$BASE_C
    final.par$EQ_STEP1 = final.par$BASE_STEP1
    final.par$EQ_STEP2 = final.par$BASE_STEP2
    final.par$EQ_STEP3 = final.par$BASE_STEP3
    final.par$EQ_STEP4 = final.par$BASE_STEP4
    final.par$EQ_STEP5 = final.par$BASE_STEP5
    final.par$EQ_STEP6 = final.par$BASE_STEP6

    final.par$EQ_A_SE = final.par$BASE_A_SE
    final.par$EQ_B_SE = final.par$BASE_B_SE
    final.par$EQ_C_SE = final.par$BASE_C_SE
    final.par$EQ_STEP1_SE = final.par$BASE_STEP1_SE
    final.par$EQ_STEP2_SE = final.par$BASE_STEP2_SE
    final.par$EQ_STEP3_SE = final.par$BASE_STEP3_SE
    final.par$EQ_STEP4_SE = final.par$BASE_STEP4_SE
    final.par$EQ_STEP5_SE = final.par$BASE_STEP5_SE
    final.par$EQ_STEP6_SE = final.par$BASE_STEP6_SE

    final.par = dplyr::left_join(final.par,cal_par[,c("UIN",paste0("UN_IRT_",c("A","B","C")),paste0("UN_IRT_STEP",c(1:6)))], by = c("UIN" = "UIN"))

    final.par$SCALEBACK_A = final.par$UN_IRT_A / m1
    final.par$SCALEBACK_B = m1 * final.par$UN_IRT_B + m2
    final.par$SCALEBACK_C = final.par$UN_IRT_C
    final.par$SCALEBACK_STEP1 = final.par$UN_IRT_STEP1 * m1
    final.par$SCALEBACK_STEP2 = final.par$UN_IRT_STEP2 * m1
    final.par$SCALEBACK_STEP3 = final.par$UN_IRT_STEP3 * m1
    final.par$SCALEBACK_STEP4 = final.par$UN_IRT_STEP4 * m1
    final.par$SCALEBACK_STEP5 = final.par$UN_IRT_STEP5 * m1
    final.par$SCALEBACK_STEP6 = final.par$UN_IRT_STEP6 * m1

  } else {

    final.par = cal_par
    # merge objectives
    final.par = left_join(final.par,bank_par_org[,c("UIN","OBJECTIVE_1","OBJECTIVE_3")],by = "UIN")
    final.par$m1 = m1
    final.par$m2 = m2

    final.par$EQ_A = final.par$UN_IRT_A/m1
    final.par$EQ_B = final.par$UN_IRT_B * m1 + m2
    final.par$EQ_C = final.par$UN_IRT_C
    final.par$EQ_STEP1 = final.par$UN_IRT_STEP1* m1
    final.par$EQ_STEP2 = final.par$UN_IRT_STEP2*m1
    final.par$EQ_STEP3 = final.par$UN_IRT_STEP3*m1
    final.par$EQ_STEP4 = final.par$UN_IRT_STEP4*m1
    final.par$EQ_STEP5 = final.par$UN_IRT_STEP5*m1
    final.par$EQ_STEP6 = final.par$UN_IRT_STEP6*m1

    final.par$EQ_A_SE = final.par$UN_IRT_A_SE/m1
    final.par$EQ_B_SE = final.par$UN_IRT_B_SE * m1
    final.par$EQ_C_SE = final.par$UN_IRT_C_SE
    final.par$EQ_STEP1_SE = final.par$UN_IRT_STEP1_SE*m1
    final.par$EQ_STEP2_SE = final.par$UN_IRT_STEP2_SE*m1
    final.par$EQ_STEP3_SE = final.par$UN_IRT_STEP3_SE*m1
    final.par$EQ_STEP4_SE = final.par$UN_IRT_STEP4_SE*m1
    final.par$EQ_STEP5_SE = final.par$UN_IRT_STEP5_SE*m1
    final.par$EQ_STEP6_SE = final.par$UN_IRT_STEP6_SE*m1

    final.par = dplyr::left_join(final.par,bank_par_org[,c("UIN",paste0("BASE_",c("A","B","C")),paste0("BASE_STEP",c(1:6)))], by = c("UIN" = "UIN"))

  }


  ######------------------------------------
  ##  D. Anchor Stability:  TCC evaluation (only for OP and forward)
  ######---------------------------------------
   if (toupper(status) == "OP" & (toupper(direction) != 'BACKWARD' | is.na(direction))){

  # D1. compute TCC based on equated pars

    # load testmap
     bank_irt = read_sas(mypath$bank_par_path)
     bank_irt = as.data.frame(bank_irt)
     colnames(bank_irt) = toupper(colnames(bank_irt))
     op_items = bank_irt[which(bank_irt$ITEM_STATUS == "OP" &
                      substr(bank_irt$FORM_ID,14,14)==1
                    ),]$UIN
     parms_4TCC = final.par[which(final.par$UIN %in% op_items),c("MODEL",paste0("EQ_",c("A","B","C")),
                                                                 paste0("EQ_STEP", 1:5))]
     SCcut = readxl::read_xlsx(paste0("C:/TDOE/Spring22/", toupper(test),"/equating_scaling_constants_", toupper(test),".xlsx"))
     SCcut = as.data.frame(SCcut)
     thHOSS = SCcut[which(toupper(SCcut$Subject) == toupper(testcode)),]$New_HOSS_theta
     thLOSS = SCcut[which(toupper(SCcut$Subject) == toupper(testcode)),]$New_LOSS_theta
     theta.vec = seq(thLOSS,thHOSS,by = 0.1)
     tcc = apply(parms_4TCC, 1, function(x) irt_fun(x[1], theta.vec = theta.vec,
                                                    D = 1.702, a = x[2],b = x[3],c = x[4],steps = x[5:9])$p)
     tcc = rowSums(tcc)

  # D2. compute TCC for remove-one iterations
     tcc_plot = list(NA)

     for (i in 1:nrow(ne_par)){

      # equate the test
       parms_4TCC_iter = final.par[which(final.par$UIN %in% op_items),c("MODEL",paste0("UN_IRT_",c("A","B","C")),
                                                     paste0("UN_IRT_STEP", 1:5))]
       parms_4TCC_iter$EQ_A = parms_4TCC_iter$UN_IRT_A/rm1_m1m2[i,]$m1
       parms_4TCC_iter$EQ_B = parms_4TCC_iter$UN_IRT_B * rm1_m1m2[i,]$m1 + rm1_m1m2[i,]$m2
       parms_4TCC_iter$EQ_C = parms_4TCC_iter$UN_IRT_C
       parms_4TCC_iter$EQ_STEP1 = parms_4TCC_iter$UN_IRT_STEP1* rm1_m1m2[i,]$m1
       parms_4TCC_iter$EQ_STEP2 = parms_4TCC_iter$UN_IRT_STEP2*rm1_m1m2[i,]$m1
       parms_4TCC_iter$EQ_STEP3 = parms_4TCC_iter$UN_IRT_STEP3*rm1_m1m2[i,]$m1
       parms_4TCC_iter$EQ_STEP4 = parms_4TCC_iter$UN_IRT_STEP4*rm1_m1m2[i,]$m1
       parms_4TCC_iter$EQ_STEP5 = parms_4TCC_iter$UN_IRT_STEP5*rm1_m1m2[i,]$m1

       parms_4TCC_iter = parms_4TCC_iter[,c("MODEL",paste0("EQ_",c("A","B","C")),
                                            paste0("EQ_STEP", 1:5))]
       # compute TCC
       tcc_iter = apply(parms_4TCC_iter, 1, function(x) irt_fun(x[1], theta.vec = theta.vec,
                                                      D = 1.702, a = x[2],b = x[3],c = x[4],steps = x[5:9])$p)
       tcc_iter = rowSums(tcc_iter)

       # plot
       tcc_dat = data.frame(theta = rep(theta.vec,2), TCC = c(tcc,tcc_iter),
                            group = c(rep("Full Anchor", length(theta.vec)),
                                      rep(paste("Remove", rm1_m1m2[i,]$UIN), length(theta.vec))))
       tcc_dat$group = as.factor(tcc_dat$group)
       tcc_plot[[i]] = ggplot(tcc_dat, aes(theta,TCC,color = group, shape = group))+
         geom_line() +
         geom_point()

  }
   # D3. save plots

     total_rows_per_page = 30
     start_row = 1

     if(total_rows_per_page > nrow(rm1_m1m2)){
       end_row = nrow(rm1_m1m2)
     }else {
       end_row = total_rows_per_page
     }

     pdf(mypath$drift_tcc_path)

     for(i in 1:ceiling(nrow(rm1_m1m2)/total_rows_per_page)){
       grid.newpage()
       grid.table(rm1_m1m2[start_row:end_row, ], rows=NULL,theme =ttheme_default(base_size = 7))
       start_row = end_row + 1
       if((total_rows_per_page + end_row) < nrow(rm1_m1m2)){
         end_row = total_rows_per_page + end_row
       }else {
         end_row = nrow(rm1_m1m2)
       }
     }

     invisible(lapply(tcc_plot, print))
     dev.off()

   }

  ######------------------------------------
  ##  E. create IRT file
  ######---------------------------------------

  ## IRT flags: (only for equated pars)
  final.par$EXCLUDED_FROM_IRT_ANALYSIS[final.par$EXCLUDED_FROM_IRT_ANALYSIS ==""] = NA
  final.par$FLAG_IRT = ifelse(
    !is.na(final.par$EXCLUDED_FROM_IRT_ANALYSIS) | final.par$EXCLUDED_FROM_IRT_ANALYSIS != "", "Y",
    ifelse(
    # final.par$UN_IRT_A < 0.2 |
      final.par$EQ_A < 0.2, "Y", ifelse(
        # final.par$un_IRT_B > 4 | final.par$un_IRT_B < -4 |
          final.par$EQ_B > 4 | final.par$EQ_B < -4, "Y", ifelse(
            #(( !is.na(final.par$UN_IRT_C)) &  final.par$un_IRT_C > 0.4) |
            (( !is.na(final.par$EQ_C)) & final.par$EQ_C > 0.4), "Y", ''))))

  # Change IRT_C of 2PL as 0, this will facilitate to use 3pl function on to the 2pl item.
  final.par$EQ_C = ifelse(final.par$MODEL=="2PL",0,final.par$EQ_C)
  final.par$UN_IRT_C = ifelse(final.par$MODEL=="2PL",0,final.par$UN_IRT_C)
  final.par$EQ_C_SE = ifelse(final.par$MODEL=="2PL",NA,final.par$EQ_C_SE)
  colnames(final.par) = toupper(colnames(final.par))

  if (toupper(direction) == "BACKWARD"){
    up_case_col_name = toupper(c("IRTLOC","ADMINCODE","Testcode","Mode","UIN","ITEM_STATUS","Max_points","Score_cat","Max_points_obs","Score_cat_obs",
                                 "N_IRT","AVERAGE_ITEM_SCORE","BASE_PTBIS","FLAG_IRT","flag_scr_dist",
                                 "SX2","SX2_flag","ZQ1","zq1_flag","G2","g2_flag",
                                 "EXCLUDED_FROM_IRT_ANALYSIS",
                                 "EQ_A","EQ_A_SE","EQ_B","EQ_B_SE","EQ_C","EQ_C_SE","EQ_Step1","EQ_Step1_SE",
                                 "EQ_Step2","EQ_Step2_SE","EQ_Step3","EQ_Step3_SE","EQ_Step4","EQ_Step4_SE",
                                 "EQ_Step5","EQ_Step5_SE","EQ_Step6","EQ_Step6_SE",
                                 "BASE_A","BASE_B"  ,  "BASE_C", "BASE_Step1", "BASE_Step2",  "BASE_Step3", "BASE_Step4", "BASE_Step5", "BASE_Step6","m1","m2",
                                 "BASE_A_SE","BASE_B_SE","BASE_C_SE", "BASE_Step1_SE", "BASE_Step2_SE","BASE_Step3_SE", "BASE_Step4_SE", "BASE_Step5_SE", "BASE_Step6_SE"))

  } else {

    up_case_col_name = toupper(c("IRTLOC","ADMINCODE","Testcode","Mode","UIN","ITEM_STATUS","Max_points","Score_cat","Max_points_obs","Score_cat_obs",
                                 "N_IRT","AVERAGE_ITEM_SCORE","ITEM_TOTAL_CORRELATION","FLAG_IRT","flag_scr_dist",
                                 "SX2","SX2_flag","ZQ1","zq1_flag","G2","g2_flag",
                                 "EXCLUDED_FROM_IRT_ANALYSIS",
                                 "EQ_A","EQ_A_SE","EQ_B","EQ_B_SE","EQ_C","EQ_C_SE","EQ_Step1","EQ_Step1_SE",
                                 "EQ_Step2","EQ_Step2_SE","EQ_Step3","EQ_Step3_SE","EQ_Step4","EQ_Step4_SE",
                                 "EQ_Step5","EQ_Step5_SE","EQ_Step6","EQ_Step6_SE",
                                 "UN_IRT_A","UN_IRT_B"  ,  "UN_IRT_C", "UN_IRT_Step1", "UN_IRT_Step2",  "UN_IRT_Step3", "UN_IRT_Step4", "UN_IRT_Step5", "UN_IRT_Step6","m1","m2",
                                 "UN_IRT_A_SE","UN_IRT_B_SE","UN_IRT_C_SE", "UN_IRT_Step1_SE", "UN_IRT_Step2_SE","UN_IRT_Step3_SE", "UN_IRT_Step4_SE", "UN_IRT_Step5_SE", "UN_IRT_Step6_SE"))

  }
  irt_file_col_name = c("IRTLOC","Admincode","Testcode","Mode","UIN","STATUS","Max_points","Score_cat","Max_points_obs","Score_cat_obs",
                        "N_IRT","AVERAGE_ITEM_SCORE","ITEM_TOTAL_CORRELATION","FLAG_IRT","flag_scr_dist","IRT_FIT_SX2",
                        "IRT_FIT_FLAG_SX2","IRT_FIT_ZQ1","IRT_FIT_FLAG_ZQ1","IRT_FIT_G2",
                        "IRT_FIT_FLAG_G2","EXCLUDED_FROM_IRT_ANALYSIS",
                        "IRT_A","IRT_A_SE","IRT_B","IRT_B_SE","IRT_C","IRT_C_SE","IRT_Step1","IRT_Step1_SE",
                        "IRT_Step2","IRT_Step2_SE","IRT_Step3","IRT_Step3_SE","IRT_Step4","IRT_Step4_SE",
                        "IRT_Step5","IRT_Step5_SE","IRT_Step6","IRT_Step6_SE",
                        "un_IRT_A","un_IRT_B"  ,  "un_IRT_C", "un_IRT_Step1", "un_IRT_Step2",  "un_IRT_Step3", "un_IRT_Step4", "un_IRT_Step5", "un_IRT_Step6","M1","M2",
                        "un_IRT_A_SE","un_IRT_B_SE","un_IRT_C_SE", "un_IRT_Step1_SE", "un_IRT_Step2_SE","un_IRT_Step3_SE", "un_IRT_Step4_SE", "un_IRT_Step5_SE", "un_IRT_Step6_SE")

  if (! "ZQ1" %in% colnames(final.par)){
    up_case_col_name = up_case_col_name[-which(up_case_col_name %in% c("ZQ1","ZQ1_FLAG","G2","G2_FLAG"))]
    irt_file_col_name = irt_file_col_name[-which(irt_file_col_name %in% c("IRT_FIT_ZQ1","IRT_FIT_FLAG_ZQ1","IRT_FIT_G2","IRT_FIT_FLAG_G2"))]
  }


  irt_file = final.par[,up_case_col_name]
  colnames(irt_file) = irt_file_col_name

  irt_file[,c(paste0("IRT_",c("A","B","C")),paste0("IRT_Step",c(1:6)))] = round_sas(irt_file[,c(paste0("IRT_",c("A","B","C")),paste0("IRT_Step",c(1:6)))] ,5)
  irt_file[,c(paste0("IRT_",c("A","B","C"),"_SE"),paste0("IRT_Step",c(1:6),"_SE"))] = round_sas(irt_file[,c(paste0("IRT_",c("A","B","C"),"_SE"),paste0("IRT_Step",c(1:6),"_SE"))] ,4)

  # merge in d2 and wrmsd info
  final.par = left_join(final.par,anch_prms[,c("UIN",setdiff(colnames(anch_prms),colnames(final.par)))], by = "UIN")

  write.csv(final.par,paste0(mypath$eq_path,toupper(admin),"_internalirt_",toupper(testcode),"_",toupper(mode),"_", toupper(status),"_GX.csv"), row.names = F, na='')
  write.csv(irt_file[which(toupper(irt_file$STATUS)==toupper(status)),],  # only for OP/FT items.
            mypath$final_irt_path_csv, row.names = F, na='')
  write.foreign(irt_file[which(toupper(irt_file$STATUS)==toupper(status)),],
                mypath$final_irt_path_txt,
                mypath$final_irt_path_sas, package="SAS")

  # save d2
d2_file = right_join(passageDrift[,c("UIN","IRTLOC","MAX_POINTS","ITEM_STATUS","PASSAGE_NUMBER","PARAMETER_YEAR","N_TOTAL","BANK_SEQUENCE",
                paste0("BASE_",c("A","B","C","STEP1","STEP2","STEP3","STEP4","STEP5")),
                "BASE_PVALUE","BASE_PTBIS","TESTCODE.x","OBJECTIVE_1","ITEM_TOTAL_CORRELATION","SX2_FLAG","ZQ1_FLAG","FITRATE","G2_FLAG","PVALUE","DIFF_PVALUE","DIFF_POS","DIFF_PTBIS")],
anch_prms[,c("UIN",paste0("EQ_",c("A","B","C","STEP1","STEP2","STEP3","STEP4","STEP5")),
                          "d2","WRMSD", "cv","WRMSD_FLAG", "sortDiff" ,  "chng_A"  ,   "chng_B" )], by = "UIN")
d2_file$chng_C = d2_file$EQ_C - d2_file$BASE_C
d2_file$sq_diff = d2_file$WRMSD^2

d2_file = d2_file[,c("MAX_POINTS","UIN","IRTLOC",paste0("EQ_",c("A","B","C","STEP1","STEP2","STEP3","STEP4","STEP5")),
                     "ITEM_STATUS","PASSAGE_NUMBER","PARAMETER_YEAR","N_TOTAL","BANK_SEQUENCE",
                     paste0("BASE_",c("A","B","C","STEP1","STEP2","STEP3","STEP4","STEP5")),
                     "BASE_PVALUE","BASE_PTBIS","sq_diff","cv","WRMSD","WRMSD_FLAG","TESTCODE.x","chng_B","chng_A","chng_C","sortDiff","OBJECTIVE_1",
                     "ITEM_TOTAL_CORRELATION", "SX2_FLAG","ZQ1_FLAG","FITRATE","G2_FLAG",
                     "PVALUE","DIFF_PVALUE","DIFF_POS","DIFF_PTBIS")]
colnames(d2_file) = c("MAX_POINTS","UIN","IRT_LOC",paste0("IRT_",c("A","B","C","STEP1","STEP2","STEP3","STEP4","STEP5")),
                      "ITEM_STATUS","PASSAGEID","PARAMETER_YEAR","N_TOTAL","POSITION",
                      paste0("BASE_",c("A","B","C","STEP1","STEP2","STEP3","STEP4","STEP5")),
                      "BASE_PVALUE","BASE_PTBIS","SQ_DIFF","CV","WRMSD","WRMSD_FLAG","TESTCODE","chng_B","chng_A","chng_C","sortDiff","OBJECTIVE_1",
                      "ITEM_TOTAL_CORRELATION", "IRT_FIT_FLAG_SX2","IRT_FIT_FLAG_ZQ1","IRT_FIT_G2C_CAT","IRT_FIT_FLAG_G2",
                      "PVALUE","DIFF_PVALUE","DIFF_POS","DIFF_ITC")

d2_file$FINAL_DROP = 0

write.csv(d2_file,mypath$d2_path_csv, na = "", row.names = F)
write.foreign(d2_file,
              mypath$d2_path_txt,
              mypath$d2_path_sas, package="SAS")

  # additional plot
  anch_prms_print$WRMSD_FLAG = as.factor(anch_prms$WRMSD_FLAG)
  all.par = left_join(all.par,anch_prms_print,by =c("UIN","MAX_POINTS"))

  plot_reivew = list(
    a_plot = ggplot(all.par, aes(UN_IRT_A, BASE_A)) +
      geom_point()+
      ggtitle("Plot of Free vs. Base: IRT A")+
      geom_abline(slope = 1, intercept = 0),

    b_plot = ggplot(all.par, aes(UN_IRT_B, BASE_B)) +
      geom_point()+
      ggtitle("Plot of Free vs. Base: IRT B")+
      geom_abline(slope = 1, intercept = 0),

    c_plot = ggplot(all.par, aes(UN_IRT_C, BASE_C)) +
      geom_point()+
      ggtitle("Plot of Free vs. Base: IRT C")+
      geom_abline(slope = 1, intercept = 0),

    wrmsd_flag_plot = ggplot(all.par, aes(x = WRMSD, y = chng_B, shape = WRMSD_FLAG,color = WRMSD_FLAG)) +
                      geom_point()+
                      facet_wrap(~ OBJECTIVE_1)+
                      ggtitle("Items Flagged by WRMSD: Remove item with largest drift and rerun",
                              subtitle = paste("nOP = ",nOP,
                              ", nAnch=", nrow(all.par),
                              ", nDropped=", length(dropuin_anchor[!is.na(dropuin_anchor) & dropuin_anchor!=""]),
                              ", minAnch=", 0.3* nOP))+
      theme(legend.position="bottom")

  )


  # summary report
  pdf(mypath$drift_log_path)
  Table1 <- tableGrob(round_sas(rbind(base_vs_un_par, base_vs_un_ctt)[,c(2:5,8:9,13)],3), theme =ttheme_default(base_size = 7))
  Table2 = tableGrob(BP_cover, theme =ttheme_default(base_size = 7))
  Table3 = tableGrob(anchor_d2$m1m2, theme =ttheme_default(base_size = 7))
  grid.arrange(Table1,Table2,Table3,ncol = 1, nrow = 3)

  total_rows_per_page = 30
  start_row = 1
  if(total_rows_per_page > nrow(all.par)){
    end_row = nrow(all.par)
  }else {
    end_row = total_rows_per_page
  }
  for(i in 1:ceiling(nrow(all.par)/total_rows_per_page)){
    grid.newpage()
    grid.table(all.par[start_row:end_row, c("IRTLOC","UIN",paste0("UN_IRT_",c("A","B","C")),
                                           paste0("BASE_",c("A","B","C")),
                                           "WRMSD","WRMSD_FLAG")], rows=NULL,theme =ttheme_default(base_size = 7))
    start_row = end_row + 1
    if((total_rows_per_page + end_row) < nrow(all.par)){
      end_row = total_rows_per_page + end_row
    }else {
      end_row = nrow(all.par)
    }
  }




  total_rows_per_page = 30
  start_row = 1

  if(total_rows_per_page > nrow(passageDrift)){
    end_row = nrow(passageDrift)
  }else {
    end_row = total_rows_per_page
  }


  for(i in 1:ceiling(nrow(passageDrift)/total_rows_per_page)){
    grid.newpage()
    grid.table(passageDrift[start_row:end_row, c("IRTLOC","UIN","PASSAGE_NUMBER","OBJECTIVE_1","LINKING",
                                                 "DIFF_A","DIFF_B","DIFF_PVALUE",
                                                 "DIFF_PTBIS", "DIFF_POS")], rows=NULL,theme =ttheme_default(base_size = 5))
    start_row = end_row + 1
    if((total_rows_per_page + end_row) < nrow(all.par)){
      end_row = total_rows_per_page + end_row
    }else {
      end_row = nrow(all.par)
    }
  }
  invisible(lapply(plot_reivew, print))
  dev.off()

  return(list(anch_file = all.par,
              passageDrift = passageDrift,
              drifted = anch_prms_print[which(anch_prms_print$WRMSD_FLAG == 1),],
              BP_cover = BP_cover,
              irt_file = irt_file,
              internalirt = final.par,
              m1m2 = anchor_d2$m1m2,
              base_vs_freeirt_summary = rbind(base_vs_un_par, base_vs_un_ctt),
              plot_reivew = plot_reivew))

}
#
# # eqSTUIRT("en1","eoc",'tneocspr22', 'e',"op",c("TE01E0156","TE01M0260","TN0025452"))
#
# eqSTUIRT("en1","eoc",'tneocspr22', 'e',"op",c("TE01E0156","TE01M0260","TN0025452","TN0003282"))# last uin is due to wrmsd
# eqSTUIRT("en2","eoc",'tneocspr22', 'e',"op","TE01E0356")
# eqSTUIRT("al1","eoc",'tneocspr22', 'e',"op",c("TN176157","TN314325"))
# eqSTUIRT("ge1","eoc",'tneocspr22', 'e',"op",c("TN0031231","TN0066551"))
# eqSTUIRT("bi1","eoc",'tneocspr22', 'e',"op","TB03M2173")
# eqSTUIRT("uh1","eoc",'tneocspr22', 'e',"op",c("TU01S0076","TU04M2744", "TU04S2516",  "TU04S2560","TU04S2601"))
# eqSTUIRT("im1","eoc",'tneocspr22', 'e',"op",c("TN448198", "TN714309"))
# eqSTUIRT("im2","eoc",'tneocspr22', 'e',"op",c("TN0031844", "TN0069573", "TN745872"))
# eqSTUIRT("im3","eoc",'tneocspr22', 'e',"op",c("T2A03M325", "TN0065789", "TN0069451", "TN0075352"))
# eqSTUIRT("al2","eoc",'tneocspr22', 'e',"op",c('TN0084884','TN0011191','TN0069451',"TN0032581")) # last uin is due to wrmsd
#
#
#
#
# eqSTUIRT("el2","ach",'tnachspr22', 'p',"op",c("TN0002036", "TN0065304", "TN128194",
#                                               "TN128830","TN0082533","TN0091993_P")) #wrmsd
# #  "TN0091993_P"  Erin removed this item too.
# eqSTUIRT("el3","ach",'tnachspr22', 'p',"op",c("TN0070136","TR01E0413","TR01E0416"))
# eqSTUIRT("el4","ach",'tnachspr22', 'p',"op",c("TN0034066", "TR01C0313"
#                                               ,
#                                               "TN0034106","TN0034070"
# )) #wrmsd
# eqSTUIRT("el5","ach",'tnachspr22', 'p',"op","TN0069147")#wrmsd
# eqSTUIRT("el6","ach",'tnachspr22', 'p',"op",c("TN0072046","TN532951", "TR01E0568","TR01E0569","TR01E0574")) #wrmsd
# eqSTUIRT("el7","ach",'tnachspr22', 'p',"op",c("TR01F0006","TR01S0669",
#                                               "TR01F0246","TN0069726") ) #wrmsd
#
# eqSTUIRT("el8","ach",'tnachspr22', 'p',"op","TN0081836")
# eqSTUIRT("ma2","ach",'tnachspr22', 'p',"op",c("TN0001941","TN0001936"))
# eqSTUIRT("ma3","ach",'tnachspr22', 'p',"op","")
# eqSTUIRT("ma4","ach",'tnachspr22', 'p',"op","")
# eqSTUIRT("ma5","ach",'tnachspr22', 'p',"op","TN214472") #wrmsd
# eqSTUIRT("ma6","ach",'tnachspr22', 'p',"op",c("T6M03S608",
#                                               "TN0004480","TN358534")) #wrmsd
# eqSTUIRT("ma7","ach",'tnachspr22', 'p',"op",c("TN0024816",
#                                               "TN045954"))
# eqSTUIRT("ma8","ach",'tnachspr22', 'p',"op","T8M03S152")
#
# eqSTUIRT("sc3","ach",'tnachspr22', 'p',"op","TS03M4812")
# eqSTUIRT("sc4","ach",'tnachspr22', 'p',"op","")
# eqSTUIRT("sc5","ach",'tnachspr22', 'p',"op","")
# eqSTUIRT("sc6","ach",'tnachspr22', 'p',"op","TS04S6967_255A")
# eqSTUIRT("sc7","ach",'tnachspr22', 'p',"op","TS03M5747")
# eqSTUIRT("sc8","ach",'tnachspr22', 'p',"op","")
# eqSTUIRT("ss6","ach",'tnachspr22', 'p',"op","TH04S6480")
# eqSTUIRT("ss7","ach",'tnachspr22', 'p',"op","")
# eqSTUIRT("ss8","ach",'tnachspr22', 'p',"op","")




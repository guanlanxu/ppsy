
eqSTUIRT_4drift <- function(admin,testcode,  mode, status, RSnum,mock,
                            purpose = "normal",direction = "normal",anchor_var="PREEQUATING",
                            dropuin ="",dropseq ="",dnu = "", dropuin_anchor ="",
                            get_plot = T ){

  library(dplyr)
  library(haven)
  library(data.table)
  library(foreign)
  library(stringr)
  library(ppsy)

  mypath = get_path(admin = admin, testcode = testcode, mode = mode, mock = mock,status = status, RSnum = RSnum,iter = "", purpose = purpose, pre_post = "post")

  test = mypath$test

  # load item parameters
  bank_par_org <- read_sas(mypath$bank_par_path)
  bank_par_org = as.data.frame(bank_par_org)
  colnames(bank_par_org) = toupper(colnames(bank_par_org))
  colnames(bank_par_org)[which( colnames(bank_par_org) == "ITEM_TOTAL_CORRELATION")] = "BASE_PTBIS"
  colnames(bank_par_org)[which( colnames(bank_par_org) == "P_VALUE")] = "BASE_PVALUE"
  colnames(bank_par_org)[which( colnames(bank_par_org) %in% paste0("IRT_",c("A","B","C",paste0("STEP",c(1:6)))))] = paste0("BASE_",c("A","B","C",paste0("STEP",c(1:6))))

  cal_par <- read.csv(mypath$freeirt_path_csv)
  colnames(cal_par) = toupper(colnames(cal_par))
  cal_par$PVALUE = cal_par$AVERAGE_ITEM_SCORE/cal_par$MAX_POINTS_OBS

  if (toupper(purpose) == 'MAINTAIN') {
    # this is for pre-eq check and bank maintainness.
    # Use all OP items in the bank_year as anchor
    bank_par_org$LINKING = ifelse(bank_par_org$ITEM_STATUS == "OP" &
                                  bank_par_org$SPOILED_ITEM == "" &
                                  ! bank_par_org$UIN %in% c(dropuin, dnu) &
                                  ! bank_par_org$IDMSEQ %in% dropseq &
                                  !bank_par_org$UIN %like% "_D" &
                                  bank_par_org$PARAMETER_YEAR == "SPRING 2022", "L","")


  } else if (toupper(purpose) != 'MAINTAIN' | is.na(purpose)) {

    if (toupper(status) == 'OP') {
      # bank par is in RA folder and free par is freeirt
      bank_par_org$LINKING = ifelse((bank_par_org$ITEM_STATUS == "OP" &
                                     bank_par_org$SPOILED_ITEM == "" &
                                     ! bank_par_org$UIN %in% c(dropuin, dnu) &
                                     ! bank_par_org$IDMSEQ %in% dropseq &
                                     ! bank_par_org$UIN %like% "_D" &
                                       #  substr(bank_par_org$FORM_ID, 14, 14) != 1 &     # remove core 2 in anchor &
                                     bank_par_org[, which(toupper(colnames(bank_par_org)) == toupper(anchor_var))] == "Y")  &
                                     ! bank_par_org$PASSAGE_NUMBER %in% drop_passid |
                                     (bank_par_org$UIN %in% forcein_uin | bank_par_org$PASSAGE_NUMBER %in% forcein_passid), "L", "")

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
      bank_par_org = read.csv(paste0(mypath$path,"ANALYSES/EQ/OP/",RSnum,"/Calibration/",toupper(testcode),"/", toupper(admin), "_freeirt_", toupper(testcode), "_",toupper(mode),"_OP_GX.csv"))
      bank_par_org = as.data.frame(bank_par_org)
      colnames(bank_par_org) = toupper(colnames(bank_par_org))
      bank_par_org$LINKING = ifelse(bank_par_org$ITEM_STATUS == "OP", "L","")
      colnames(bank_par_org)[which( colnames(bank_par_org) %in% paste0("UN_IRT_",c("A","B","C",paste0("STEP",c(1:6)))))] = paste0("BASE_",c("A","B","C",paste0("STEP",c(1:6))))


      cal_par <- read.csv(mypath$freeirt_path_csv)
      colnames(cal_par) = toupper(colnames(cal_par))

    }

  }

  # compare p-value for passage drift (only for OP)
  if (toupper(status)=="OP"){
    passageDrift = left_join(cal_par, bank_par_org, by = c("UIN"="UIN","MODE"="MODE","MAX_POINTS"="MAX_POINTS","ITEM_STATUS"="ITEM_STATUS"))
    passageDrift$DIFF_A = passageDrift$UN_IRT_A - passageDrift$BASE_A
    passageDrift$DIFF_B = passageDrift$UN_IRT_B - passageDrift$BASE_B
    passageDrift$DIFF_C = passageDrift$UN_IRT_C - passageDrift$BASE_C
    passageDrift$DIFF_PVALUE = passageDrift$PVALUE - passageDrift$BASE_PVALUE
    passageDrift$DIFF_PTBIS = passageDrift$ITEM_TOTAL_CORRELATION - passageDrift$BASE_PTBIS
    passageDrift$DIFF_POS = passageDrift$SEQUENCE - passageDrift$BANK_SEQUENCE

    all.par =  passageDrift[which(passageDrift$LINKING == "L" & !passageDrift$UIN %in% c("",dropuin_anchor) & ! is.na(passageDrift$UIN)),
                            c("UIN",paste0("UN_IRT_",c("A","B","C",paste0("STEP",c(1:6)))),"SCORE_CAT","MODEL","MAX_POINTS",
                                                                 paste0("BASE_",c("A","B","C",paste0("STEP",c(1:6)))))]
  } else {
    passageDrift = inner_join(cal_par[,c("UIN",paste0("UN_IRT_",c("A","B","C",paste0("STEP",c(1:6)))),"SCORE_CAT","MODEL","MAX_POINTS")],
               bank_par_org[,c("UIN",paste0("BASE_",c("A","B","C",paste0("STEP",c(1:6)))))],by = "UIN")
    all.par = passageDrift[which(passageDrift$ITEM_STATUS == "OP" & !passageDrift$UIN %in% c("",dropuin_anchor) & ! is.na(all.par$UIN)),]

  }


  all.par$model2 <- ifelse(all.par$SCORE_CAT == "2", "L3", "PC")
  all.par$type <- ifelse(all.par$model2 == "PC", "LC", "")
  all.par$DW = "DW"
  all.par$D = 1.702
  all.par$ID = seq.int(nrow(all.par))

  # handling C parameters
  all.par$BASE_A = as.numeric(all.par$BASE_A)
  all.par$BASE_B = as.numeric(all.par$BASE_B)
  all.par$BASE_C = as.numeric(all.par$BASE_C)

  all.par$UN_IRT_A = as.numeric(all.par$UN_IRT_A)
  all.par$UN_IRT_B = as.numeric(all.par$UN_IRT_B)
  all.par$UN_IRT_C = as.numeric(all.par$UN_IRT_C)


  all.par[which(is.na(all.par$BASE_C)), "BASE_C"] <- 0
  all.par[which(is.na(all.par$UN_IRT_C)), "UN_IRT_C"] <- 0

  # subset & reorder
  ne_par <-
    all.par[which(!is.na(all.par$UN_IRT_A)), c(
      "ID",
      "model2",
      "SCORE_CAT",
      "DW",
      "D",
      "UN_IRT_A",
      "type",
      "UN_IRT_B",
      "UN_IRT_C",
      "UN_IRT_STEP1",
      "UN_IRT_STEP2",
      "UN_IRT_STEP3",
      "UN_IRT_STEP4",
      "UN_IRT_STEP5",
      "UN_IRT_STEP6"
    )]
  ol_par <-
    all.par[which(!is.na(all.par$BASE_A)), c(
      "ID",
      "model2",
      "SCORE_CAT",
      "DW",
      "D",
      "BASE_A",
      "type",
      "BASE_B",
      "BASE_C",
      "BASE_STEP1",
      "BASE_STEP2",
      "BASE_STEP3",
      "BASE_STEP4",
      "BASE_STEP5",
      "BASE_STEP6"
    )]


  # Comute starting values
  ab = ppsy::get_ab(ol_par, ne_par)

  # handling NAs
  #  ol_par$IRT_A = round2(ol_par$IRT_A,5)
  ol_par$BASE_B = round_sas(ol_par$BASE_B, 5)
  ol_par$BASE_C = round_sas(ol_par$BASE_C, 5)
  ol_par$BASE_STEP1 = round_sas(ol_par$BASE_STEP1, 5)
  ol_par$BASE_STEP2 = round_sas(ol_par$BASE_STEP2, 5)
  ol_par$BASE_STEP3 = round_sas(ol_par$BASE_STEP3, 5)
  ol_par$BASE_STEP4 = round_sas(ol_par$BASE_STEP4, 5)
  ol_par$BASE_STEP5 = round_sas(ol_par$BASE_STEP5, 5)

  #ne_par$UN_IRT_A = round_sas(ne_par$UN_IRT_A, 5)
  ne_par$UN_IRT_B = round_sas(ne_par$UN_IRT_B, 5)
  ne_par$UN_IRT_C = round_sas(ne_par$UN_IRT_C, 5)
  ne_par$UN_IRT_STEP1 = round_sas(ne_par$UN_IRT_STEP1, 5)
  ne_par$UN_IRT_STEP2 = round_sas(ne_par$UN_IRT_STEP2, 5)
  ne_par$UN_IRT_STEP3 = round_sas(ne_par$UN_IRT_STEP3, 5)
  ne_par$UN_IRT_STEP4 = round_sas(ne_par$UN_IRT_STEP4, 5)
  ne_par$UN_IRT_STEP5 = round_sas(ne_par$UN_IRT_STEP5, 5)

  ol_par <- sapply(ol_par, as.character)
  ne_par <- sapply(ne_par, as.character)
  ol_par[is.na(ol_par)] <- ""
  ne_par[is.na(ne_par)] <- ""


  # write script

  out.path = mypath$stuirt_con_path
  out2.path <-mypath$stuirt_out_path

  unlink(out.path)


  sink(out.path, append = T)
  cat(c("NE ", nrow(ne_par), "\n"))
  for (i in 1:nrow(ne_par)) {
    cat(as.character(ne_par[i, ]), "\n")
  }
  cat(c("OL ", nrow(ne_par), "\n"))
  for (i in 1:nrow(ol_par)) {
    cat(as.character(ol_par[i, ]), "\n")
  }
  cat(
    paste0(
      "CI ",
      nrow(ne_par),
      " AO
OP KO SL
ND 41 PN 0 1 4
OD 41 PN 0 1 4
FS DO DO
ST ",
      ab$A,
      " ",
      ab$B,
      "\n",
      "SY BI BI
OP
BY"
    )
  )
  sink()


  # return.code <- shell("start ANALYSES/SPECIAL_STUDIES/SampleDown/STUIRT/STUIRT.exe - SYSIN out.path out2.path")
  system(
    "C:/Users/UXU99GU/Downloads/stuirt_for_pc_console/STUIRT_wc(v1.0).exe",
    intern = F,
    input = c(out.path, out2.path)
  )

  # extract Constants
  stu <- readLines(out2.path)
  line <- stu[which(grepl("^\\s*Stocking-Lord {1}", stu))]
  line2 <- unlist(strsplit(line, split = " "))
  line3 = line2[line2 != ""]
  line4 = which(grepl("Iter# TermC", stu))
  line5 = stu[(line4+2)]
  line6  = unlist(strsplit(line5, split = " "))

  A = as.numeric(line3[2])
  B = as.numeric(line3[3])

  m1m2 = data.frame(
    test = test,
    testcode = testcode,
    A = A,
    B = B,
    interation = line6[22],
    TermCode = line6[27]

  )

  write.csv(m1m2,
            paste0(mypath$eq_path,toupper(admin),"_m1m2_",toupper(testcode),"_",toupper(mode),"_", toupper(status),"_GX.csv"),
            row.names = F)




  ######------------------------------------
  ##  Equating based on A slope and B intercept
  ######---------------------------------------




  if (toupper(status) == "OP" & toupper(direction) == "BACKWARD"){
    # since backward, the current par is base, so EQ pars = free pars

    final.par = bank_par_org
    final.par$A = A
    final.par$B = B

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

  } else {

    final.par = cal_par
    final.par$A = A
    final.par$B = B

    final.par$EQ_A = final.par$UN_IRT_A/A
    final.par$EQ_B = final.par$UN_IRT_B * A + B
    final.par$EQ_C = final.par$UN_IRT_C
    final.par$EQ_STEP1 = final.par$UN_IRT_STEP1*A
    final.par$EQ_STEP2 = final.par$UN_IRT_STEP2*A
    final.par$EQ_STEP3 = final.par$UN_IRT_STEP3*A
    final.par$EQ_STEP4 = final.par$UN_IRT_STEP4*A
    final.par$EQ_STEP5 = final.par$UN_IRT_STEP5*A
    final.par$EQ_STEP6 = final.par$UN_IRT_STEP6*A


    final.par$EQ_A_SE = final.par$UN_IRT_A_SE/A
    final.par$EQ_B_SE = final.par$UN_IRT_B_SE * A
    final.par$EQ_C_SE = final.par$UN_IRT_C_SE
    final.par$EQ_STEP1_SE = final.par$UN_IRT_STEP1_SE*A
    final.par$EQ_STEP2_SE = final.par$UN_IRT_STEP2_SE*A
    final.par$EQ_STEP3_SE = final.par$UN_IRT_STEP3_SE*A
    final.par$EQ_STEP4_SE = final.par$UN_IRT_STEP4_SE*A
    final.par$EQ_STEP5_SE = final.par$UN_IRT_STEP5_SE*A
    final.par$EQ_STEP6_SE = final.par$UN_IRT_STEP6_SE*A

    final.par = dplyr::left_join(final.par,bank_par_org[,c("UIN",paste0("BASE_",c("A","B","C")),paste0("BASE_STEP",c(1:6)))], by = c("UIN" = "UIN"))
  }


  ######------------------------------------
  ##  Compute D2 for anchor/linking items
  ######---------------------------------------

  if ((toupper(status) == "OP"
      & toupper(direction) != "BACKWARD") | toupper(status) == "FT") {

    mydat = final.par[which(final.par$UIN %in% all.par$UIN & !is.na(final.par$BASE_A)), c("MODEL",
                          paste0("EQ_",c("A", "B", "C", "STEP1", "STEP2", "STEP3", "STEP4", "STEP5")),
                          c(paste0("BASE_", c("A", "B", "C")), paste0("BASE_STEP", c(1:5))),"IRTLOC","TESTCODE","UIN","MAX_POINTS",
                          "A","B")]
    mydat$A_diff = mydat$EQ_A - mydat$BASE_A
    mydat$B_diff = mydat$EQ_B - mydat$BASE_B
    mydat$C_diff = mydat$EQ_C - mydat$BASE_C



  } else if (toupper(status) == "OP"
             & toupper(direction) == "BACKWARD") {

    final.par$SCALEBACK_A = final.par$UN_IRT_A / A
    final.par$SCALEBACK_B = A * final.par$UN_IRT_B + B
    final.par$SCALEBACK_C = final.par$UN_IRT_C
    final.par$SCALEBACK_STEP1 = final.par$UN_IRT_STEP1 * A
    final.par$SCALEBACK_STEP2 = final.par$UN_IRT_STEP2 * A
    final.par$SCALEBACK_STEP3 = final.par$UN_IRT_STEP3 * A
    final.par$SCALEBACK_STEP4 = final.par$UN_IRT_STEP4 * A
    final.par$SCALEBACK_STEP5 = final.par$UN_IRT_STEP5 * A
    final.par$SCALEBACK_STEP6 = final.par$UN_IRT_STEP6 * A

    mydat = final.par[which(final.par$UIN %in% all.par$UIN & !is.na(final.par$SCALEBACK_A)), c("MODEL",
                          paste0("EQ_",c("A", "B", "C", "STEP1", "STEP2", "STEP3", "STEP4", "STEP5")),
                          c(paste0("SCALEBACK_", c("A", "B", "C")), paste0("SCALEBACK_STEP", c(1:5))),"IRTLOC","TESTCODE","UIN","MAX_POINTS",
                          "A","B")]

    mydat$A_diff = mydat$EQ_A - mydat$SCALEBACK_A
    mydat$B_diff = mydat$EQ_B - mydat$SCALEBACK_B
    mydat$C_diff = mydat$EQ_C - mydat$SCALEBACK_C

  }


  theta.vec = seq(-4,4,0.2)

  myd2 = apply(mydat, 1,
        function(x)
          ppsy::get_d2(x[1], theta.vec = theta.vec,
                       1.702,
                       x[2], x[3], x[4], x[5:9],
                       x[10], x[11], x[12], x[13:17], uin = x[20],get_plot = T))

  mydat$d2 = unlist(sapply(myd2, function(x) x[1]))
  mydat$WRMSD = unlist(sapply(myd2, function(x) x[2]))

  mydat$cv = ifelse(mydat$MAX_POINTS==1, 0.1,
                               ifelse(mydat$MAX_POINTS==2,0.15,
                                      ifelse(mydat$MAX_POINTS==3,0.225,
                                             ifelse(mydat$MAX_POINTS==4,0.3,
                                                    ifelse(mydat$MAX_POINTS ==5, 0.375,NA)))))
  mydat$WRMSD_FLAG = ifelse(mydat$WRMSD > mydat$cv,1,0)
  mydat$sortDiff = (mydat$WRMSD-mydat$cv)/mydat$MAX_POINTS
  mydat = mydat[c("IRTLOC","TESTCODE","UIN","MAX_POINTS","A_diff","B_diff","WRMSD","sortDiff","WRMSD_FLAG")]

  myplot = (sapply(myd2, function(x) x[3]))

  library(gridExtra)
  pdf(mypath$drift_plot_path)
  grid.table(mydat,rows=NULL,theme =ttheme_default(base_size = 7))
  invisible(lapply(myplot, print))
  dev.off()



  final.par$D2 = myd2$d2
  final.par$WRMSD = myd2$wrmsd

  if (toupper(purpose) != "MAINTAIN"){
    final.par[which(is.na(final.par$ANCHOR)),]$D2 = NA
    final.par[which(is.na(final.par$ANCHOR)),]$WRMSD = NA
  }

  final.par$WRMSD_CUT = ifelse(final.par$MAX_POINTS==1, 0.1,
                               ifelse(final.par$MAX_POINTS==2,0.15,
                                      ifelse(final.par$MAX_POINTS==3,0.225,
                                             ifelse(final.par$MAX_POINTS==4,0.3,
                                                    ifelse(final.par$MAX_POINTS ==5, 0.375,NA)))))
  final.par$WRMSD_FLAG = ifelse(final.par$WRMSD > final.par$WRMSD_CUT,"Y","N")


  ## IRT flags: (only for equated pars)
  final.par$EXCLUDED_FROM_IRT_ANALYSIS[final.par$EXCLUDED_FROM_IRT_ANALYSIS ==""] = NA
  final.par$FLAG_IRT = ifelse(
    !is.na(final.par$EXCLUDED_FROM_IRT_ANALYSIS),
    "Y",
    ifelse(
    #  final.par$UN_IRT_A < 0.2 |
      final.par$EQ_A < 0.2,
      "Y",
      ifelse(
      #  final.par$un_IRT_B > 4 |
      #    final.par$un_IRT_B < -4 |
          final.par$EQ_B > 4 | final.par$EQ_B < -4,
        "Y",
        ifelse(
        #  (( !is.na(final.par$UN_IRT_C)) &  final.par$un_IRT_C > 0.4) |
          ((
            !is.na(final.par$EQ_C)
          ) & final.par$EQ_C > 0.4), "Y", '')
      )
    )
  )

  # Change IRT_C of 2PL as 0, this will facilitate to use 3pl function on to the 2pl item.
  final.par$EQ_C = ifelse(final.par$MODEL=="2PL",0,final.par$EQ_C)
  final.par$UN_IRT_C = ifelse(final.par$MODEL=="2PL",0,final.par$UN_IRT_C)
  final.par$EQ_C_SE = ifelse(final.par$MODEL=="2PL",NA,final.par$EQ_C_SE)

  up_case_col_name = toupper(c("IRTLOC","ADMINCODE","Testcode","Mode","UIN","STATUS","Max_points","Score_cat","Max_points_obs","Score_cat_obs",
                               "N_IRT","AVERAGE_ITEM_SCORE","ITEM_TOTAL_CORRELATION","FLAG_IRT","flag_scr_dist","SX2","SX2_flag","ZQ1","zq1_flag","G2","g2_flag","EXCLUDED_FROM_IRT_ANALYSIS",
                               "EQ_A","EQ_A_SE","EQ_B","EQ_B_SE","EQ_C","EQ_C_SE","EQ_Step1","EQ_Step1_SE",
                               "EQ_Step2","EQ_Step2_SE","EQ_Step3","EQ_Step3_SE","EQ_Step4","EQ_Step4_SE",
                               "EQ_Step5","EQ_Step5_SE","EQ_Step6","EQ_Step6_SE",
                               "un_IRT_A","un_IRT_B"  ,  "un_IRT_C", "un_IRT_Step1", "un_IRT_Step2",  "un_IRT_Step3", "un_IRT_Step4", "un_IRT_Step5", "un_IRT_Step6","A","B",
                               "un_IRT_A_SE","un_IRT_B_SE","un_IRT_C_SE", "un_IRT_Step1_SE", "un_IRT_Step2_SE","un_IRT_Step3_SE", "un_IRT_Step4_SE", "un_IRT_Step5_SE", "un_IRT_Step6_SE"
  ))

  irt.file = final.par[,up_case_col_name]
  colnames(irt.file) = c("IRTLOC","Admincode","Testcode","Mode","UIN","STATUS","Max_points","Score_cat","Max_points_obs","Score_cat_obs",
                         "N_IRT","AVERAGE_ITEM_SCORE","ITEM_TOTAL_CORRELATION","FLAG_IRT","flag_scr_dist","IRT_FIT_SX2",
                         "IRT_FIT_FLAG_SX2","IRT_FIT_ZQ1","IRT_FIT_FLAG_ZQ1","IRT_FIT_G2","IRT_FIT_FLAG_G2","EXCLUDED_FROM_IRT_ANALYSIS",
                         "IRT_A","IRT_A_SE","IRT_B","IRT_B_SE","IRT_C","IRT_C_SE","IRT_Step1","IRT_Step1_SE",
                         "IRT_Step2","IRT_Step2_SE","IRT_Step3","IRT_Step3_SE","IRT_Step4","IRT_Step4_SE",
                         "IRT_Step5","IRT_Step5_SE","IRT_Step6","IRT_Step6_SE",
                         "un_IRT_A","un_IRT_B"  ,  "un_IRT_C", "un_IRT_Step1", "un_IRT_Step2",  "un_IRT_Step3", "un_IRT_Step4", "un_IRT_Step5", "un_IRT_Step6","M1","M2",
                         "un_IRT_A_SE","un_IRT_B_SE","un_IRT_C_SE", "un_IRT_Step1_SE", "un_IRT_Step2_SE","un_IRT_Step3_SE", "un_IRT_Step4_SE", "un_IRT_Step5_SE", "un_IRT_Step6_SE"
  )

  irt.file[,c(paste0("IRT_",c("A","B","C")),paste0("IRT_Step",c(1:6)))] = round_sas(irt.file[,c(paste0("IRT_",c("A","B","C")),paste0("IRT_Step",c(1:6)))] ,5)
  irt.file[,c(paste0("IRT_",c("A","B","C"),"_SE"),paste0("IRT_Step",c(1:6),"_SE"))] = round_sas(irt.file[,c(paste0("IRT_",c("A","B","C"),"_SE"),paste0("IRT_Step",c(1:6),"_SE"))] ,4)

  write.csv(final.par,paste0(path,eq_path,toupper(admin),"_internalirt_",toupper(testcode),"_",toupper(mode),"_", toupper(status),"_GX.csv"), row.names = F, na='')
  write.csv(irt.file[which(toupper(irt.file$STATUS)==toupper(status)),],  # only for OP/FT items.
            paste0(path,eq_path,toupper(admin),"_irt_",toupper(testcode),"_",toupper(mode),"_", toupper(status),"_GX.csv"), row.names = F, na='')



  library(foreign)
  write.foreign(irt.file[which(toupper(irt.file$STATUS)==toupper(status)),],
                paste0(path,eq_path,toupper(admin),"_irt_",toupper(testcode),"_",toupper(mode),"_", toupper(status),"_GX.txt"),
                paste0(path,eq_path,toupper(admin),"_irt_",toupper(testcode),"_",toupper(mode),"_", toupper(status),"_GX.sas"), package="SAS")
  return(list(drifted = final.par[which(final.par$WRMSD_FLAG == "Y"),c("UIN","WRMSD","WRMSD_CUT")],
              equated_irt_file = irt.file,
              myirt = final.par))

}


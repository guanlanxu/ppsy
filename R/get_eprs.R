#' Generate score tables.
#'
#' @param admin String. Administration.
#' @param testcode String.
#' @param mode E or P
#' @param status OP or FT.
#' @param battery 1 or 0.
#' @param RSnum String. RS1-3.
#' @param mock T or F.
#' @param pre_post PRE or POST.
#' @param what Types of score table. E.g., "pl3b","semb","ssb","pl4b","ssb","irtb","aberrant".
#' @param irt_add A table contains parameters to add to bank file.

#' @return Score table that specified in the `what` argument.


get_eprs <- function(admin, testcode, mode,status = "OP",battery, RSnum,mock,pre_post = "post",

                     what = c("pl3b","semb","ssb","pl4b","ssb","irtb","aberrant"),
                     irt_add = ""){

  library(haven)
  library("readxl")
  library("writexl")
  library(sas7bdat)
  library(dplyr)
  library(ppsy)


  mypath = get_path(admin = admin, testcode = testcode, mode = mode,status = status, RSnum = RSnum, mock = mock, pre_post = "post")

  # Load correct ptm
  if(toupper(mypath$test)  == "ACH" & toupper(testcode) %in% c("EL2INF", "EL2LIT", "EL2I", "EL2L")){
    ptm <- read_sas(paste0(mypath$path,"DATA/TEST_MAPS/PTM/",tolower(admin),"_ptm_",tolower("EL2"),"_",tolower(mode),"_gx.sas7bdat"))

  } else if(toupper(mypath$test)  == "ACH" & toupper(testcode) %in% c("BREL2INF", "BREL2LIT","BREL2I", "BREL2L")){
    ptm <- read_sas(paste0(mypath$path,"DATA/TEST_MAPS/PTM/",tolower(admin),"_ptm_",tolower("BREL2"),"_",tolower(mode),"_gx.sas7bdat"))

  } else{
    ptm <- read_sas(mypath$ptm_path)
  }
  ptm = as.data.frame(ptm)

  # hack br forms
  if (substr( toupper(testcode) ,1,2) == "BR" ){
    testcode2 = substr(toupper(testcode),3,nchar(toupper(testcode)))
  } else {
      testcode2 = toupper(testcode)
  }

  # setup for battery
  if (tolower(battery) == 1){
    # form = unique(testmap$Form_Group_Code)
    form = sort(unique(substr(ptm$Form_ID,1,14)))
    testnumber = unique(substr(unique(ptm$TestNumber),1,nchar(unique(ptm$TestNumber))-2))
  } else if (tolower(battery) == 0) {
    form = unique(ptm$Form_ID)
    testnumber = unique(ptm$TestNumber)
  }

  # load rsss
  RSSS <- read.csv(mypath$rsss_path_csv)

  pubformat = unique(ptm$Publish_Format[which(ptm$Publish_Format!="")])

  mylist = as.list(what)
  names(mylist) = tolower(what)
  ############################################################
  # PL4b
  if ("PL4B" %in% toupper(names(mylist))){
    SCcuts <- read_xlsx(paste0("C:/TDOE/Spring22/",toupper(mypath$test),"/equating_scaling_constants_", toupper(mypath$test),".xlsx"))
    SCcuts = as.data.frame(SCcuts)
    SSmin = SCcuts[which(toupper(SCcuts$Subject) == testcode2),"LOSS_SS"]
    SScut2 = SCcuts[which(toupper(SCcuts$Subject) == testcode2),"PL2_SScut"]
    SScut3 = SCcuts[which(toupper(SCcuts$Subject) == testcode2),"PL3_SScut"]
    SScut4 = SCcuts[which(toupper(SCcuts$Subject) == testcode2),"PL4_SScut"]
    SSmax = SCcuts[which(toupper(SCcuts$Subject) == testcode2),"HOSS_SS"]

    objective = testnumber
    nlevel = seq(1,length(unlist(c(SSmin,SScut2,SScut3,SScut4))))
    pl4b_tab = as.data.frame(expand.grid(form,testnumber,pubformat,objective,nlevel))
    pl4b_tab = pl4b_tab[order(pl4b_tab$Var1),]
    pl4b_tab$LB = unlist(c(SSmin,SScut2,SScut3,SScut4))
    pl4b_tab$UB = unlist(c(as.numeric(SScut2)-1, as.numeric(SScut3)-1, SScut4-1,as.numeric(SSmax)))
    pl4b_tab = as.data.frame(pl4b_tab)

    if (testcode2 %in% c("EL2INF","EL2I")){pl4b_tab$Var4 = 'Informational'}
    if (testcode2 %in% c("EL2LIT","EL2L")){pl4b_tab$Var4 = 'Literary'}

    mylist$pl4b = pl4b_tab
  }



############################################
  if ("SEMB" %in% toupper(names(mylist))){
  # semb
#  RSSS <- read.csv(mypath$rsss_path_csv)
  RSSS$rounded = round_sas(RSSS$Scale_Score_CSEM,0)
  RSSS_grouped <- RSSS%>%
    group_by(rounded, group = cumsum(c(1, diff(rounded) != 0))) %>%
    summarise(LB = min(Scale_Score), UB = max(Scale_Score)) %>%
    arrange(group)
  SEM = RSSS_grouped$rounded
  nSEM = length(SEM)
  # recalculate upper bound
  UUB = unlist(RSSS_grouped[2:nSEM,3]-1)
  RSSS_grouped$UUB = unlist(c( unlist(RSSS_grouped[2:nSEM,3]-1),RSSS_grouped[nrow(RSSS_grouped),ncol(RSSS_grouped)]))
  objective = testnumber
  tab = expand.grid(form,SEM)
  tab = tab[order(tab$Var1),]
  semb_tab = data.frame(form = tab$Var1,
                       testnum = rep(testnumber,nSEM*length(form)),
                       pubformat = rep(pubformat,nSEM*length(form)),
                       objective = rep(objective,nSEM*length(form)),
                       SEM = tab$Var2,
                       LB = rep(RSSS_grouped$LB,length(form)),
                       UB = rep(RSSS_grouped$UUB,length(form)))
  if (testcode2 %in% c("EL2INF","EL2I")){semb_tab$Var4 = 'Informational'}
  if (testcode2 %in% c("EL2LIT","EL2L")){semb_tab$Var4 = 'Literary'}
   mylist$semb = semb_tab
  }
  ############################################
  if ("SSB" %in% toupper(names(mylist))){
  # ssb
  SS = unique(RSSS$Scale_Score)
  nSS = length(SS)
  RS = unique(RSSS$Raw_Score)
  objective = testnumber
  tab = expand.grid(form,SS)
  tab = tab[order(tab$Var1),]
  ssb_tab = data.frame(form =tab$Var1,
                       testnum = rep(testnumber,nSS*length(form)),
                       pubformat = rep(pubformat,nSS*length(form)),
                       objective = rep(objective,nSS*length(form)),
                       SS = tab$Var2 )
  RSSS_sm2 = RSSS[,c("Scale_Score","Raw_Score")]
  RS_max = aggregate(Raw_Score~Scale_Score  , data = RSSS_sm2, max)
  RS_min = aggregate(Raw_Score~Scale_Score  , data = RSSS_sm2, min)
  RS_minmax= merge(RS_min,RS_max, by = "Scale_Score")
  RS_minmax=RS_minmax[order(RS_minmax[,1]),]
  RS_minmax$UB = c((RS_minmax[2:nrow(RS_minmax),2]-1),RS_minmax[nrow(RS_minmax),ncol(RS_minmax)])
  ssb_tab$LB = rep(RS_minmax$Raw_Score.x, length(form))
  ssb_tab$UB = rep(RS_minmax$UB,length(form))

  if (testcode2 %in% c("EL2INF","EL2I")){ssb_tab$Var4 = 'Informational'}
  if (testcode2 %in% c("EL2LIT","EL2L")){ssb_tab$Var4 = 'Literary'}
  mylist$ssb = ssb_tab
  }

  #################################
  # not for ALT
  if (toupper(mypath$test) %in% c('EOC', 'ACH')){

    if ("PL3B" %in% toupper(names(mylist))){
    # PL3b
    rsss_rc <- read.csv(mypath$rsss_rc_path_csv)
    rsss_rc$Max_fromTM = as.numeric(rsss_rc$Max_fromTM)
    rsss_rc$cut1 = as.numeric(rsss_rc$cut1)
    rsss_rc$cut2 = as.numeric(rsss_rc$cut2)
    objective = unique(rsss_rc$Objective)
    nobj = length(objective)
    ncuts = length(grep("cut",colnames(rsss_rc),value = T))
    PL = seq(1,(ncuts+1))

    tab= expand.grid(form,objective,PL)
    tab = tab[order(tab[,1]),]
    tab$LowerBound = rep(c(rep(0,nobj), c(rsss_rc$cut1),c(rsss_rc$cut2)),length(form))
    tab$UpperBound = rep(c(rsss_rc$cut1-1,rsss_rc$cut2-1,rsss_rc$Max_fromTM),length(form))
    tab2 = tab[order(tab[,1],tab[,2]),]
    nr = nrow(tab)
    pl3b_tab = data.frame(Form = tab2$Var1,
                          # Form_ID =
                          testnum = rep(testnumber,nr),
                          pubformat=rep(pubformat,nr),
                          objective = tab2$Var2,
                          PL = tab2$Var3,
                          LB = tab2$LowerBound,
                          UB=tab2$UpperBound)
    if (testcode2 %in% c("EL2INF","EL2I")){pl3b_tab$Var4 = 'Informational'}
    if (testcode2 %in% c("EL2LIT","EL2L")){pl3b_tab$Var4 = 'Literary'}
    mylist$pl3b = pl3b_tab
    }

    if ("IRTB" %in% toupper(names(mylist))){
    # IRTB
    objective = testnumber
    irtb_tab = as.data.frame(expand.grid(form,testnumber,pubformat,objective,round_sas(RSSS$IRT_Theta,4)))
    irtb_tab = irtb_tab[order(irtb_tab[,1]),]
    irtb_tab$LB = rep(RSSS$Raw_Score, length(form))
    irtb_tab$UB = rep(RSSS$Raw_Score, length(form))
    mylist$irtb = irtb_tab
    }

    if ("ABERRANT" %in% toupper(names(mylist))){
    #  # ABERRANT
    ptm = ptm[which(ptm$Spoiled_Item !="O"),]
    ptm[which(ptm$Trait_UIN!=""),"UIN"] = ptm[which(ptm$Trait_UIN!=""),"Trait_UIN"]
    ptm$Question_No = ifelse(ptm$Trait_UIN !="", ptm[which(ptm$UIN == strsplit(ptm$UIN, "#")[[1]][1]),]$Question_No, ptm$Question_No)

    if (pre_post == "pre"){
      IRTpar_all = read_sas(mypath$bank_par_path)
      } else if (pre_post == "post"){
      IRTpar_all = read.csv(mypath$final_irt_path_csv)
      }
    library(data.table)
    library(rqdatatable)
    # add pars for missing for ELA mock
    if (mock == T){

      if (all(is.na(irt_add))){irt_add = F} else if (all(irt_add == "")){irt_add = F} else if (! is.data.frame(irt_add)) {
        print("irt_add is not a data.frame")
      } else if ( any(! c("UIN","IDMSEQ") %in% toupper(colnames(irt_add)))){
        print("UIN or IDMSEQ is not supplied in the irt_add table")
      } else {
        irt_add = as.data.frame(irt_add)
        colnames(irt_add) = toupper(colnames(irt_add))
        if (all(irt_add$IRT_STEP1 %in% c(".","0") | is.na(irt_add$IRT_STEP1))){
          colnames(irt_add)[which(colnames(irt_add) %like% "IRT_STEP")] = paste0("IRT_STEP",0:5)
        }
        irt_added = natural_join(IRTpar_all,irt_add, by = 'UIN', jointype = "FULL")
        irt_added = irt_added[order(irt_added$IDMSEQ),]
        IRTpar_all = irt_added[,colnames(IRTpar_all)]
        IRTpar_all[IRTpar_all=="."] = NA
        # to_modify = irt_file[which(irt_file$UIN %in% irt_add$UIN),]
        # to_modify = to_modify[,unique(c(setdiff(colnames(irt_file),colnames(irt_add)), "UIN","IDMSEQ"))]
        # to_modify = dplyr::left_join(to_modify,irt_add, by = c("UIN"="UIN","IDMSEQ"="IDMSEQ"))
        # to_modify = to_modify[,colnames(irt_file)]
        # irt_file = as.data.frame(rbind(to_modify,irt_file[which(! irt_file$UIN %in% irt_add$UIN),]))
      }

    }

    IRTpar_all = as.data.frame(IRTpar_all)
    colnames(IRTpar_all) = toupper(colnames(IRTpar_all))
    colnames(IRTpar_all)[which(colnames(IRTpar_all) == "ITEM_STATUS")] = "STATUS"
    colnames(IRTpar_all)[which(colnames(IRTpar_all) == "N_TOTAL")] = "N_IRT"

    IRTpar_all <- IRTpar_all[which(IRTpar_all$STATUS=="OP" & ! is.na(IRTpar_all$IRT_A)),]
    IRTpar_all$MODEL = ifelse(IRTpar_all$MAX_POINTS >1, "GPC",
                              ifelse(IRTpar_all$MAX_POINTS ==1 & (IRTpar_all$IRT_C ==0 | is.na(IRTpar_all$IRT_C)), "2PL", "3PL")
    )

    IRTpar_all$step1_temp = ifelse(is.na(IRTpar_all$IRT_STEP1), NA,0)
    IRTpar_all$step1_temp_se = ifelse(is.na(IRTpar_all$IRT_STEP1), NA,0)

    IRTpar_all$UIN = ifelse(IRTpar_all$UIN %like% "_D", sapply(IRTpar_all$UIN, function(x) d2trait(x, testcode)),
                            IRTpar_all$UIN)
    IRTpar = left_join(IRTpar_all, unique(ptm[,c("UIN","Question_No")]), by = c("UIN"="UIN"))
 #   aberrant_tab = data.frame(matrix(NA, ncol = 29))
    # colnames(res) = c("assetuin","formid", "admin","seqno","item_status","mode","testcode","max_points","n_total","irt_flag","irt_model",
    #                   "irt_a","irt_a_se","irt_b","irt_b_se","irt_c","irt_c_se","irt_step1","irt_step1_se","irt_step2","irt_step2_se","irt_step3",
    #                   "irt_step3_se","irt_step4","irt_step4_se","irt_step5","irt_step5_se","irt_step6","irt_step6_se")
    #
    admin2 = ifelse(substr(tolower(admin),6,6) == "s", paste0("Spring 20", substr(admin, nchar(admin)-1,nchar(admin))),
                    ifelse(substr(tolower(admin),6,6) == "f", paste0("Fall 20", substr(admin, nchar(admin)-1,nchar(admin))),
                                                              NA))
    tab = as.data.frame(expand.grid(IRTpar$UIN,form,admin2,testnumber))

    aberrant_tab = data.frame(
      assetuin = tab$Var1,
      formid = tab$Var2,
      admin = tab$Var3,
      seqno = IRTpar$SEQUENCE,
      item_status = IRTpar$STATUS,
      mode = toupper(pubformat),
      testcode = tab$Var4,
      max_points = IRTpar$MAX_POINTS,
      n_total = IRTpar$N_IRT,
      irt_flag = IRTpar$FLAG_PSY,
      irt_model = IRTpar$MODEL,
      irt_a = IRTpar$IRT_A,
      irt_a_se = "",
      irt_b = IRTpar$IRT_B,
      irt_b_se = "",
      irt_c = IRTpar$IRT_C,
      irt_c_se = "",
      irt_step1 = IRTpar$step1_temp,
      irt_step1_se = IRTpar$step1_temp_se,
      irt_step2 = IRTpar$IRT_STEP1,
      irt_step2_se = "",
      irt_step3 = IRTpar$IRT_STEP2,
      irt_step3_se = "",
      irt_step4 = IRTpar$IRT_STEP3,
      irt_step4_se ="",
      irt_step5 = IRTpar$IRT_STEP4,
      irt_step5_se = "",
      irt_step6 = IRTpar$IRT_STEP5,
      irt_step6_se = ""
    )
     aberrant_tab$irt_c = ifelse(toupper(aberrant_tab$irt_model == "2PL"), 0, aberrant_tab$irt_c)
    aberrant_tab$irt_c = ifelse(toupper(aberrant_tab$irt_model == "GPC"), NA, aberrant_tab$irt_c)

     mylist$aberrant = aberrant_tab
    }
    #
    # for ( i in 1: nrow(tab)){
    #   if (toupper(testcode) == "EL3" & tab$assetuin[i] %like% "_D1") {
    #     uin1 = strsplit(tab$assetuin[i],"_")[[1]][1]
    #     tab[i,"assetuin"] = paste0(uin1,"#SCORE_TRAIT_FOD")
    #   } else if (toupper(testcode) == "EL3" & tab$assetuin[i] %like% "_D2") {
    #     uin1 = strsplit(tab$assetuin[i],"_")[[1]][1]
    #     tab[i,"assetuin"] = paste0(uin1,"#SCORE_TRAIT_LC")
    #   } else if (tab$assetuin[i] %like% "_D1") {
    #     uin1 = strsplit(tab$assetuin[i],"_")[[1]][1]
    #     tab[i,"assetuin"] = paste0(uin1,"#SCORE_TRAIT_FO")
    #   } else if (tab$assetuin[i] %like% "_D2") {
    #     uin1 = strsplit(tab$assetuin[i],"_")[[1]][1]
    #     tab[i,"assetuin"] = paste0(uin1,"#SCORE_TRAIT_DEV")
    #   } else if (tab$assetuin[i] %like% "_D3") {
    #     uin1 = strsplit(tab$assetuin[i],"_")[[1]][1]
    #     tab[i,"assetuin"] = paste0(uin1,"#SCORE_TRAIT_LANG")
    #   }else if (tab$assetuin[i] %like% "_D4") {
    #     uin1 = strsplit(tab$assetuin[i],"_")[[1]][1]
    #     tab[i,"assetuin"] = paste0(uin1,"#SCORE_TRAIT_CONV")
    #   }
    # }
    #

 #   res = rbind(res, tab)
#    res = res[-1,]



  } else if (toupper(mypath$test)== "ALT" ){
    print ('Note: PL3B, IRTB, ABERRANT are not for ALT.')
  }

   return(mylist)
}


# get_eprs(admin = "tnaltspr23", testcode = "bi1", mode = "p",battery = 0, RSnum = "RS2",mock = T
#          #, what = "pl4b"
#          )







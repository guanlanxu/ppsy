#' Generate raw-to-scale score file.
#'
#' @param admin String. Administration.
#' @param testcode String.
#' @param mode E or P
#' @param status OP or FT.
#' @param RSnum String. RS1-3.
#' @param mock T or F.
#' @param pre_post PRE or POST.
#' @param irt_add A table contains parameters to add to bank file.

#' @return rsss: Raw to Scale Score file.
#' @return pl3b: Raw to Scale Score file for reporting categories.
#' @import haven readxl foreign dplyr data.table rqdatatable
#' @export



get_rsss <- function(admin, testcode, mode,status = "OP",RSnum,mock,pre_post, irt_add = "" ){

  library(haven)
  library(readxl)
  library(foreign)
  library(ppsy)
  library(dplyr)
  library(data.table)
  library(rqdatatable)
  mypath = get_path(admin = admin,testcode = testcode, mode = mode,
                    status = status, mock = mock, RSnum = RSnum,pre_post = pre_post)
  test = mypath$test
#  setwd(mypath$path)

  if (mock == T | mock == 1) {
    RSnum2 = paste0(RSnum,"/X_Mock")
  } else {
    RSnum2 = RSnum
  }

  ##########################
  ### A. load irt pars
  ##########################

  if (toupper(pre_post) == "PRE"){
    # use bank pars
    # for br testcode, use regular pars if br pars doesn't exist
    if (substr(toupper(testcode),1,2) != "BR"){
      testcode2 = substr(testcode,1,3)
    } else if (substr(toupper(testcode),1,2) == "BR"){
      if (is.na(dir(paste0(mypath$path,"DATA/ITEM_PARAMETERS/"),full.names = T,ignore.case = T,
                    pattern = paste0(tolower(admin),"_irt_",tolower(substr(testcode,1,5)),"_",tolower(mode)))[1])) {
        testcode2 =  substr(testcode,3,5)
      } else {
        testcode2 =  substr(testcode,1,5)
      }
    }

    irt_file =read_sas(dir(paste0(mypath$path,"DATA/ITEM_PARAMETERS/"),full.names = T,ignore.case = T,
                           pattern = paste0(tolower(admin),"_irt_",tolower(testcode2),"_",tolower(mode)))[1])
    irt_file = as.data.frame(irt_file)
    colnames(irt_file) = toupper(colnames(irt_file))
    irt_file = irt_file[which(irt_file$SPOILED_ITEM != "O"),]

  } else if (toupper(pre_post) == "POST"){

    if (substr(toupper(testcode),1,2) != "BR"){
      testcode2 = substr(testcode,1,3)
    } else if (substr(toupper(testcode),1,2) == "BR") {testcode2 = substr(testcode,3,5)}
    irt_file =read.csv(dir(paste0(mypath$path,"ANALYSES/EQ/OP/",toupper(RSnum2),"/Final_Par/"),full.names = T,ignore.case = T,
                           pattern = paste0(tolower(admin),"_irt_",tolower(testcode2),"_",tolower(mode)))[1])
    bank_irt =read_sas(dir(paste0(mypath$path,"DATA/ITEM_PARAMETERS/"),full.names = T,
                           pattern = paste0(tolower(admin),"_irt_",tolower(testcode2),"_",tolower(mode)))[1])

    irt_file = as.data.frame(irt_file)
    bank_irt = as.data.frame(bank_irt)
    colnames(irt_file) = toupper(colnames(irt_file))
    colnames(bank_irt) = toupper(colnames(bank_irt))
    irt_file = left_join(irt_file,bank_irt[,c("UIN","OBJECTIVE_1","OBJECTIVE_3")], by = "UIN")
    irt_file = irt_file[which(is.na(irt_file$EXCLUDED_FROM_IRT_ANALYSIS) | irt_file$EXCLUDED_FROM_IRT_ANALYSIS == ""),]
 }


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
        irt_added = natural_join(irt_file,irt_add, by = 'UIN', jointype = "FULL")
        irt_file = irt_added[,colnames(irt_file)]
        irt_file[irt_file=="."] = NA
        irt_file = irt_file[order(irt_file$IDMSEQ),]
        # to_modify = irt_file[which(irt_file$UIN %in% irt_add$UIN),]
        # to_modify = to_modify[,unique(c(setdiff(colnames(irt_file),colnames(irt_add)), "UIN","IDMSEQ"))]
        # to_modify = dplyr::left_join(to_modify,irt_add, by = c("UIN"="UIN","IDMSEQ"="IDMSEQ"))
        # to_modify = to_modify[,colnames(irt_file)]
        # irt_file = as.data.frame(rbind(to_modify,irt_file[which(! irt_file$UIN %in% irt_add$UIN),]))
        }

  }


  # for el2 lit and info
  if (toupper(testcode) %in% c("EL2INF","BREL2INF")){
    irt_file = irt_file[which(irt_file$OBJECTIVE_3 %in% c(1,3,5,8)),]
  } else if (toupper(testcode) %in% c("EL2LIT","BREL2LIT")){
    irt_file = irt_file[which(irt_file$OBJECTIVE_3 %in% c(2,4,6)),]
  }

  colnames(irt_file)[which(colnames(irt_file) %in% c("ITEM_STATUS"))] = "STATUS"
  IRTpar_all <- irt_file[which(irt_file$STATUS!="FT" & ! is.na(irt_file$IRT_A)),]


  ##########################
  ### B. filter pars
  ##########################


  if (toupper(test) == "ALT"){
    IRTpar_all$IRT_A = 1
    IRTpar_all$IRT_C = 0
    IRTpar_all$D = 1
    if (! "IRT_STEP4" %in% colnames(IRTpar_all)){IRTpar_all$IRT_STEP4 = NA}
    if (! "IRT_STEP5" %in% colnames(IRTpar_all)){IRTpar_all$IRT_STEP5 = NA}
    IRTpar = IRTpar_all[,c("D","MAX_POINTS",paste0("IRT_",c("A","B","C","STEP1","STEP2","STEP3","STEP4","STEP5")))]
    IRTpar$MODEL = ifelse(IRTpar$MAX_POINTS ==1, "Rasch", "GPC")
  } else {
    IRTpar_all$D = 1.702
    IRTpar = IRTpar_all[,c("D","MAX_POINTS",paste0("IRT_",c("A","B","C","STEP1","STEP2","STEP3","STEP4","STEP5")))]
    IRTpar$MODEL = ifelse(IRTpar$MAX_POINTS >=2, "GPC",
                          ifelse(IRTpar$MAX_POINTS == 1 & (IRTpar$IRT_C == 0 | is.na(IRTpar$IRT_C)), "2PL",
                                 ifelse(IRTpar$MAX_POINTS == 1 & !is.na(IRTpar$IRT_C) & IRTpar$IRT_C != 0, "3PL" ,NA)))
  }
  tot.score = sum(IRTpar$MAX_POINTS)

 # IRTpar_all$equated_b = IRTpar_all$UN_IRT_B
  # ptm = read_sas(paste0("DATA/TEST_MAPS/PTM/", tolower(admin),"_ptm_", tolower(testcode),"_", tolower(mode),"_gx.sas7bdat"))
  # ptm[which(ptm$Trait_UIN!=""),"UIN"] = ptm[which(ptm$Trait_UIN!=""),"Trait_UIN"]

  #   for ( i in 1: nrow(IRTpar_all)){
  #     if (IRTpar_all$UIN[i] %like% "#SCORE_TRAIT_FOD") {
  #       uin1 = strsplit(IRTpar_all$UIN[i],"#")[[1]][1]
  #       IRTpar_all[i,"UIN"] = paste0(uin1,"_D1")
  #     } else if (IRTpar_all$UIN[i] %like% "#SCORE_TRAIT_LC") {
  #       uin1 = strsplit(IRTpar_all$UIN[i],"#")[[1]][1]
  #       IRTpar_all[i,"UIN"] = paste0(uin1,"_D2")
  #     } else if (IRTpar_all$UIN[i] %like% "#SCORE_TRAIT_FO") {
  #       uin1 = strsplit(IRTpar_all$UIN[i],"#")[[1]][1]
  #       IRTpar_all[i,"UIN"] = paste0(uin1,"_D1")
  #     } else if (IRTpar_all$UIN[i] %like% "#SCORE_TRAIT_DEV") {
  #       uin1 = strsplit(IRTpar_all$UIN[i],"#")[[1]][1]
  #       IRTpar_all[i,"UIN"] = paste0(uin1,"_D2")
  #     } else if (IRTpar_all$UIN[i] %like% "#SCORE_TRAIT_LANG") {
  #       uin1 = strsplit(IRTpar_all$UIN[i],"#")[[1]][1]
  #       IRTpar_all[i,"UIN"] = paste0(uin1,"_D3")
  #     }else if (IRTpar_all$UIN[i] %like% "#SCORE_TRAIT_CONV") {
  #       uin1 = strsplit(IRTpar_all$UIN[i],"#")[[1]][1]
  #       IRTpar_all[i,"UIN"] = paste0(uin1,"_D4")
  #     }
  # }

  # forms = unique(ptm$Form_ID)
  # forms = forms[which( (substr(forms,14,14)==1 & substr(forms,19,20)=="01") | substr(forms,14,14)==2)]

  #  score.tab = list(NA)
  #  for ( f in 1:length(forms)){
  #  form = forms[f]
  #  form_UIN = ptm[which(ptm$Form_ID == form),]$UIN


  ##########################
  ### C. Load cuts and scaling constants
  ##########################

  SCcuts <- read_xlsx(paste0("C:/TDOE/Spring22/", toupper(test),"/equating_scaling_constants_", toupper(test),".xlsx"))
  SCcuts = as.data.frame(SCcuts)
  colnames(SCcuts) = toupper(colnames(SCcuts))
  if (substr(toupper(testcode),1,2) == "BR"){
    testcode_cuts = substr(toupper(testcode),3,nchar(testcode))
    } else {testcode_cuts = testcode}
  if (toupper(test) == "ALT"){
    A <- SCcuts[which(toupper(SCcuts$SUBJECT) ==toupper(testcode_cuts)),"A"]
  } else {
    A <- SCcuts[which(toupper(SCcuts$SUBJECT) ==toupper(testcode_cuts)),"NEW_A"]
    }
  B <- SCcuts[which(toupper(SCcuts$SUBJECT) ==toupper(testcode_cuts)),"NEW_B"]
  Loss = SCcuts[which(toupper(SCcuts$SUBJECT) ==toupper(testcode_cuts)),"LOSS_SS"]
  Hoss = SCcuts[which(toupper(SCcuts$SUBJECT) ==toupper(testcode_cuts)),"HOSS_SS"]
  thLoss = SCcuts[which(toupper(SCcuts$SUBJECT) ==toupper(testcode_cuts)),"NEW_LOSS_THETA"]
  thHoss = SCcuts[which(toupper(SCcuts$SUBJECT) ==toupper(testcode_cuts)),"NEW_HOSS_THETA"]



  ##########################
  ### D. Compute TCC
  ##########################
  theta.vec = seq(thLoss,thHoss,by=0.0001)

  p = rowSums(apply(IRTpar,1,function(x) irt_fun(model = x[11],theta.vec = theta.vec, D=x[1],
                                     a = x[3], b =x[4] , c= x[5], steps = x[6:10])$p))
  info = rowSums(apply(IRTpar,1,function(x) irt_fun(model = x[11],theta.vec = theta.vec, D=x[1],
                                                    a = x[3], b =x[4] , c= x[5], steps = x[6:10])$info))
  p.longtab <- data.frame(theta.vec,p,csem = sqrt(1/info))

  p.short <- data.frame(Raw_Score = seq(0,tot.score) ,
                        unadjusted_theta = p.longtab[sapply(c(0:tot.score), function(x) which.min(abs(p-x))),"theta.vec"],
                        unadjusted_csem =  p.longtab[sapply(c(0:tot.score), function(x) which.min(abs(p-x))),"csem"])


  p.short$unadjusted_ss = A*p.short$unadjusted_theta+B
  cuts = SCcuts[which(toupper(SCcuts$SUBJECT) ==toupper(testcode_cuts)),which(colnames(SCcuts) %like% "SSCUT")]
  names(cuts) = paste0("cut",(2:(length(cuts)+1)))
  for (c in 1:length(cuts)){
    p.short[,names(cuts)[c]] = cuts[c]
  }

  ##########################
  ### E. Apply Adjustment
  ##########################
  # E1. adjust theta

  p.short$IRT_Theta = p.short$unadjusted_theta

  # max.thloss = max(p.short[which(abs(p.short$unadjusted_theta - thLoss) < 0.0001),"Raw_Score"])
  # min.thhoss = min(p.short[which(abs(p.short$unadjusted_theta - thHoss)< 0.0001),"Raw_Score"])

  # rule: Based on the specification that we adjust the theta scale  when the
  # rounded scale score is less than the loss of 200...

  # beginning:
  if (any(round_sas(p.short$unadjusted_ss,0) <= Loss)){
    if (max(p.short[which(round_sas(p.short$unadjusted_ss,0) <= Loss),"Raw_Score"])>0) {
      maxloss = max(p.short[which(round_sas(p.short$unadjusted_ss,0) <= Loss),"Raw_Score"])
      for (r in (maxloss+1):1){
        p.short[r,"IRT_Theta"] = p.short[r+1,"IRT_Theta"]-0.2
      }
    }
  }

  # ending:
  if (any(round_sas(p.short$unadjusted_ss,0) >= Hoss)){
    if ((min(p.short[which(round_sas(p.short$unadjusted_ss,0) >= Hoss),"Raw_Score"])<tot.score)) {
      minhoss = min(p.short[which(round_sas(p.short$unadjusted_ss,0) >= Hoss),"Raw_Score"])
      for (r in (minhoss+1):(tot.score+1)){
        p.short[r,"IRT_Theta"] = p.short[r-1,"IRT_Theta"]+0.2
      }
    }

  }


  # for ALT, there is condition that when HOSS is not hit, still
  # multiple raw score would get same CSEM, due to the data issue.

  if (any(p.short$unadjusted_ss < p.short[,names(cuts)[1]] & duplicated(p.short$IRT_Theta))) {
    max_dup = max(p.short[which(p.short$IRT_Theta == max(p.short[which(p.short$unadjusted_ss < p.short[,names(cuts)[1]] & duplicated(p.short$IRT_Theta)),"IRT_Theta"])),"Raw_Score"])
    for (r in (max_dup+1):1){
      p.short[r,"IRT_Theta"] = p.short[r+1,"IRT_Theta"]-0.2
    }
  }

  if (any(p.short$unadjusted_ss > p.short[,names(cuts)[length(cuts)]] & duplicated(p.short$IRT_Theta))) {
    min_dup = min(p.short[which(p.short$IRT_Theta == min(p.short[which(p.short$unadjusted_ss > p.short[,names(cuts)[length(cuts)]] & duplicated(p.short$IRT_Theta)),"IRT_Theta"])),"Raw_Score"])
    for (r in (min_dup+1):(tot.score+1)){
      p.short[r,"IRT_Theta"] = p.short[r-1,"IRT_Theta"]+0.2
    }
  }

  p.short$Unrounded_Scale_Score = A*p.short$IRT_Theta+B
  p.short$Scale_Score = round_sas(p.short$Unrounded_Scale_Score,0)
  p.short[1,"Scale_Score"] = Loss
  p.short[nrow(p.short),"Scale_Score"] = Hoss
  p.short[which(p.short$Scale_Score<Loss),"Scale_Score"] = Loss
  p.short[which(p.short$Scale_Score>Hoss),"Scale_Score"] = Hoss
  p.short$unadjusted_SScsem = p.short$unadjusted_csem*A

  # Special rule for SS and SC.
  if (toupper(testcode) %in% c("UH1","BI1","BRUH1","BRBI1",paste0("SC",3:8),paste0("SS",6:8))){
    unadjusted_ss = A*p.longtab$theta.vec+B
    SS_cut = round_sas(p.longtab[sapply(as.numeric(cuts),
                                     function(x) which.min(abs(unadjusted_ss-x))),c("p")],0)
    theta_cut = round_sas(sapply(as.numeric(cuts),
                              function(x)  (x-B)/A),4)
    p.short[SS_cut+1,]$unadjusted_theta = theta_cut
    for (c in 1: length(SS_cut)){
      p.short[SS_cut[c]+1,]$unadjusted_csem = p.longtab[which(abs(p.longtab$theta.vec -theta_cut[c]) < 0.00005),"csem"]
      p.short[SS_cut[c]+1,]$unadjusted_SScsem = p.longtab[which(abs(p.longtab$theta.vec - theta_cut[c]) < 0.00005),"csem"]*A
    }

    p.short[SS_cut+1,]$unadjusted_ss =  A*theta_cut+B
    p.short[SS_cut+1,]$IRT_Theta = theta_cut
    p.short[SS_cut+1,]$Unrounded_Scale_Score = A*theta_cut+B
    p.short[SS_cut+1,]$Scale_Score = as.numeric(cuts)
  }

  # adjust CSEM
  p.short$adjusted_SScsem = p.short$unadjusted_SScsem

  if (any(p.short$unadjusted_theta< -3.5)) {
    left.unreasonable.csem <- max(which(p.short$unadjusted_theta < -3.5))
    p.short[1:left.unreasonable.csem,"adjusted_SScsem"] = p.short[left.unreasonable.csem+1,"adjusted_SScsem"]+1.5*(abs(p.short[left.unreasonable.csem+1,"adjusted_SScsem"]-p.short[left.unreasonable.csem+2,"adjusted_SScsem"]))
  } else {left.unreasonable.csem <- NA}
  if (any(p.short$unadjusted_theta> 3.5)) {
    right.unreasonable.csem <- min(which(p.short$unadjusted_theta > 3.5))
    p.short[right.unreasonable.csem:nrow(p.short),"adjusted_SScsem"] = p.short[right.unreasonable.csem-1,"adjusted_SScsem"]+1.5*(abs(p.short[right.unreasonable.csem-1,"adjusted_SScsem"]-p.short[right.unreasonable.csem-2,"adjusted_SScsem"]))
  } else {right.unreasonable.csem <- NA}



  # Truncate the tail to 30
  p.short[which(p.short$adjusted_SScsem > 30 ),"adjusted_SScsem"] =30
  # if (any(p.short$unadjusted_ss < p.short[,names(cuts)[1]] & p.short$adjusted_SScsem > 30)) {
  #   p.short[which(p.short$unadjusted_ss < p.short[,names(cuts)[1]] & p.short$adjusted_SScsem > 30),"adjusted_SScsem"] =30
  # }
  # if (any(p.short$unadjusted_ss > p.short[,names(cuts)[length(cuts)]] & p.short$adjusted_SScsem > 30)) {
  #   p.short[which(p.short$unadjusted_ss > p.short[,names(cuts)[length(cuts)]] & p.short$adjusted_SScsem > 30),"adjusted_SScsem"] =30
  # }

  p.short$Scale_Score_CSEM = round_sas(p.short$adjusted_SScsem,0)

  if (toupper(test) == "ALT"){
    p.short[which(p.short$Scale_Score < as.numeric(cuts[1])),"PL"]  = 1
    p.short[which(p.short$Scale_Score >= as.numeric(cuts[1]) & p.short$Scale_Score < as.numeric(cuts[2])),"PL"]  = 2
    p.short[which(p.short$Scale_Score >= as.numeric(cuts[2] )),"PL"]  = 3
  } else {
    p.short[which(p.short$Scale_Score < as.numeric(cuts[1])),"PL"]  = 1
    p.short[which(p.short$Scale_Score >= as.numeric(cuts[1]) & p.short$Scale_Score < as.numeric(cuts[2])),"PL"]  = 2
    p.short[which(p.short$Scale_Score >= as.numeric(cuts[2]) & p.short$Scale_Score < as.numeric(cuts[3])),"PL"]  = 3
    p.short[which(p.short$Scale_Score >= as.numeric(cuts[3])),"PL"]  = 4
  }


  # Last round check: scale score csem
  p.short[which(p.short$Scale_Score == Loss),"Scale_Score_CSEM"] = p.short[max(which(p.short$Scale_Score == Loss)),"Scale_Score_CSEM"]
  p.short[which(p.short$Scale_Score == Hoss),"Scale_Score_CSEM"] = p.short[min(which(p.short$Scale_Score == Hoss)),"Scale_Score_CSEM"]

  p.short$Theta_csem = p.short$unadjusted_csem
  score.tab = p.short[,c("unadjusted_theta","unadjusted_ss",names(cuts),"PL","Raw_Score","Scale_Score","Scale_Score_CSEM",
                         "IRT_Theta","Theta_csem","Unrounded_Scale_Score")]

  score.tab$testcode =toupper(testcode)
  score.tab$mode =toupper(mode)
  score.tab$admin = toupper(admin)
  #  core_form = form,
  score.tab$orig_SScsem = p.short$adjusted_SScsem

  write.csv(score.tab,mypath$rsss_path_csv,row.names = F)
  write.foreign(score.tab, mypath$rsss_path_txt,mypath$rsss_path_sas, package="SAS")





  #######################
  ### pl3b table (only for ACH and EOC)
  ########################

  # pl3b is not needed for info and lit
  if (toupper(test) %in% c("ACH","EOC") & (! toupper(testcode) %in% c("EL2INF","EL2LIT","BREL2INF","BREL2LIT"))){

      ptm = read_sas(mypath$ptm_path)
      ptm = as.data.frame(ptm)
      colnames(ptm) = toupper(colnames(ptm))
      ptm=ptm[which(ptm$SPOILED_ITEM!="O"),]
      ptm$UIN = ifelse(ptm$TRAIT_UIN!="", sapply(ptm$TRAIT_UIN, trait2d), ptm$UIN)

      rc.info = unique(ptm[,c("OBJECTIVE_1","OBJECTIVE_3")])
      rc.info = as.data.frame(rc.info)

      # adjust rc info for science
      if (toupper(testcode) %in% c("BRBI1","BI1")) {
        rc.info$OBJECTIVE_3 = substr(rc.info$OBJECTIVE_3,3,3)
        rc.info = rc.info[which(rc.info$OBJECTIVE_3 %in% 1:4),]
      } else if (toupper(testcode) %in% c(paste0("SC",c(3:8)),paste0("BRSC",c(3:8)))) {
        rc.info = rc.info[which(rc.info$OBJECTIVE_3 %in% c("PS","LS","ESS")),]
        rc.info$OBJECTIVE_3 = ifelse(rc.info$OBJECTIVE_3 == "PS",1, ifelse(rc.info$OBJECTIVE_3 == "LS", 2, ifelse(rc.info$OBJECTIVE_3 == "ESS", 3, NA)))
      } else {rc.info$OBJECTIVE_3 = substr(rc.info$OBJECTIVE_3,1,1)}

      rc.info = rc.info[which(! is.na(rc.info$OBJECTIVE_1) & rc.info$OBJECTIVE_1 !=""),]
      rc.info$OBJECTIVE_3 = as.numeric(rc.info$OBJECTIVE_3)
      rc.info = rc.info[order(rc.info$OBJECTIVE_3),]
      colnames(rc.info) = c("repCat", "Strand")

      rc.info[,c("testcode","CUT1","CUT2","MAX","th_cut1", "th_cut2")] = NA
      rc.info$testcode = testcode

      # dbl check this code#################
      # if (toupper(testcode) %in% c("BREL2")){
      #   cb = ptm
      #   cb = ptm[,c("UIN","OBJECTIVE_1")]
      #   cb = unique(cb)
      #
      # } else {
      #   cb = IRTpar_all
      #   cb = cb[,c("UIN","OBJECTIVE_1")]
      # }
      #######################################

      rc.info[,c("th_cut1", "th_cut2")] = SCcuts[which(toupper(SCcuts$SUBJECT) == toupper(testcode_cuts)),c("NEW_PL3_THETACUT","NEW_PL4_THETACUT")]
      K = max(as.numeric(rc.info$Strand),na.rm=T)
      rc.info = rc.info[which(rc.info$Strand %in% seq(1,K)),]

      ## Since no Core2 or Breach form, forms = 1
      forms = 1
      rc.tab <- data.frame(matrix(NA, nrow = length(forms)*K, ncol = 11))
      colnames(rc.tab) = c(#"core_form",
        "testcode","objN","Objective","Max_fromTM","cut1","cut2","full_testcode","mode","origCut2","origCut1","Incremental")


      IRTpar = IRTpar_all

      for (k in 1:K){
        RC = IRTpar[which( IRTpar$OBJECTIVE_1 ==rc.info$repCat[k] & ! is.na(IRTpar$IRT_A)),]
        objective = RC$OBJECTIVE_1[1]
        tot.score = sum(RC$MAX_POINTS)
        RC$MODEL = ifelse(RC$MAX_POINTS >=2, "GPC",ifelse(RC$MAX_POINTS == 1 & (RC$IRT_C == 0| is.na(RC$IRT_C)), "2PL",
                                                          ifelse(RC$MAX_POINTS == 1 & RC$IRT_C !=0, "3PL", NA)))
        RC = RC[,c("MODEL",paste0("IRT_",c("A","B","C","STEP1","STEP2","STEP3","STEP4","STEP5")))]

        theta.vec = as.numeric(rc.info[k,c("th_cut1", "th_cut2")])
        p = rowSums(apply(RC,1,function(x) irt_fun(x[1],theta.vec,1.702,x[2],x[3],x[4],x[5:9])$p))
        rscut = ceiling(p)

        while (length(unique(as.numeric(rscut))) < length(rscut)){
          rscut[which(duplicated(as.numeric(rscut)))-1] = rscut[which(duplicated(as.numeric(rscut)))]-1
        }

        rc.tab[k,] = unlist(c(#substr(form,1,18),
          toupper(testcode), k, objective, tot.score,rscut,
          ptm$TESTNUMBER[1],toupper(mode), NA,NA,NA))
      }

        # add to EL2

        if (toupper(testcode) %in% c("EL2","BREL2")){

          rc.info_add = data.frame(repCat = c("Reading","Listening", "Writing"),
                                   Strand = c(9:11),
                                   testcode = testcode,
                                   CUT1 = NA,
                                   CUT2 = NA,
                                   MAX = NA,
                                   th_cut1 = rc.info$th_cut1[1],
                                   th_cut2 = rc.info$th_cut2[1],
                                   strand1 = unique(ptm[which(ptm$OBJECTIVE_3 %in% c(1,3,5)),]$OBJECTIVE_1),
                                   strand2 =unique(ptm[which(ptm$OBJECTIVE_3 %in% c(2,4,6)),]$OBJECTIVE_1))
          K = nrow(rc.info_add)

          rc.tab_add <- data.frame(matrix(NA, nrow = length(forms)*K, ncol = 11))
          colnames(rc.tab_add) = c("testcode","objN","Objective","Max_fromTM","cut1","cut2","full_testcode","mode","origCut2","origCut1","Incremental")

          for (k in 1:K){
            RC = IRTpar[which( (toupper(IRTpar$OBJECTIVE_1) ==rc.info_add$strand1[k] | IRTpar$OBJECTIVE_1 ==rc.info_add$strand2[k]) & ! is.na(IRTpar$IRT_A)),]
            objective = rc.info_add$Strand[k]
            tot.score = sum(RC$MAX_POINTS)
            RC$MODEL = ifelse(RC$MAX_POINTS >=2, "GPC",ifelse(RC$MAX_POINTS == 1 & (RC$IRT_C == 0| is.na(RC$IRT_C)), "2PL",
                                                              ifelse(RC$MAX_POINTS == 1 & RC$IRT_C !=0, "3PL", NA)))
            RC = RC[,c("MODEL",paste0("IRT_",c("A","B","C","STEP1","STEP2","STEP3","STEP4","STEP5")))]

            theta.vec = as.numeric(rc.info[k,c("th_cut1", "th_cut2")])
            p = rowSums(apply(RC,1,function(x) irt_fun(x[1],theta.vec,1.702,x[2],x[3],x[4],x[5:9])$p))
            rscut = ceiling(p)

            while (length(unique(as.numeric(rscut))) < length(rscut)){
              rscut[which(duplicated(as.numeric(rscut)))-1] = rscut[which(duplicated(as.numeric(rscut)))]-1
            }

            rc.tab_add[k,] = unlist(c(#substr(form,1,18),
              toupper(testcode), rc.info_add$Strand[k], rc.info_add$repCat[k], tot.score,rscut,
              ptm$TESTNUMBER[1],toupper(mode), NA,NA,NA))

          }
          rc.tab = rbind(rc.tab,rc.tab_add)
        }

      write.csv(rc.tab, mypath$rsss_rc_path_csv,row.names = F,na="")
      write.foreign(rc.tab, mypath$rsss_rc_path_txt,mypath$rsss_rc_path_sas, package="SAS")

  }  else {
    rc.tab = NA
  }


  return(list(rsss = score.tab, pl3b = rc.tab))
  }




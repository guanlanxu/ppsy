#' Run IRTpro calibration.
#'
#' @param admin String.
#' @param testcode String.
#' @param mode P or E.
#' @param status OP or FT.
#' @param RSnum RS1-3
#' @param mock T or F.
#' @param dnu Do-NOT-USE item in UIN.
#' @param dropuin Items that to drop in Calibration
#' @param dropseq IRTLOC that to drop in Calibration
#' @param flagDist Items that have flag and to drop in Calibration
#' @return freeirt: Freely calibrated IRT parameters
#' @return convergence: Convergence message from IRTpro.
#' @return idm_cal: IDM that is calibrated in IRTpro.
#' @import haven readr data.table reshape2 tidyverse rvest dplyr stringr xml2 foreign
#' @export

irtpro_fun <- function (admin, testcode, mode, status,RSnum,mock,
                        dnu ="",dropuin ="",dropseq ="",flagDist = "") {

  # load libraries
  library(haven)
  library(readr)
  library(data.table)
  library(ppsy)
  library(reshape2)
  library(tidyverse)
  library(rvest)
  library(dplyr)
  library(stringr)
  library(xml2)
  library(foreign)

  mypath = get_path(admin = admin, testcode = testcode, mode = mode, mock = mock,status = status, RSnum = RSnum)

  # set calibration folder for testcode

  if (mock == T | mock == 1) {
    dir.create(file.path(paste0(mypath$path, "ANALYSES/EQ/", toupper(status), "/", toupper(RSnum), "/X_Mock/Calibration"),toupper(testcode)), showWarnings =F)
  } else {
    dir.create(file.path(paste0(mypath$path, "ANALYSES/EQ/", toupper(status), "/", toupper(RSnum), "/Calibration"),toupper(testcode)), showWarnings =F)
  }

  test = mypath$test
  convergence_path <- mypath$convergence_path
  con_file <- mypath$con_path
  par_path <- mypath$irtpro_par_path

  # load bank par
  bank_par <- read_sas(mypath$bank_par_path)
  bank_par = as.data.frame(bank_par)
  colnames(bank_par) = toupper(colnames(bank_par))
  bank_par = bank_par[order(bank_par$IDMSEQ),]
  bank_par$IDMID = paste0("score_",bank_par$IDMSEQ)

  # load original IDM for CTT
  idm_original = read.csv(mypath$idm_original_path)
  irt_par_4ctt = bank_par[which(bank_par$ITEM_STATUS %in% unique(c("OP", "FTReport",toupper(status))) &
                                  (bank_par$SPOILED_ITEM == "" | is.na(bank_par$SPOILED_ITEM))),]
  idm_4ctt = idm_original[,irt_par_4ctt$IDMID]
  myctt = get_ctt(irt_par_4ctt, idm_4ctt)

  # load idm for calibration
  idm = read.csv(mypath$idm_path)
  idm_score = idm[,which(colnames(idm)%like% "score_")]
  irt_par = bank_par[which(bank_par$ITEM_STATUS %in% unique(c("OP", "FTReport",toupper(status)))),]
  irt_par$EXCLUDED = ifelse((irt_par$SPOILED_ITEM != "")| (irt_par$UIN %in% c("", dnu,dropuin,flagDist))| (irt_par$IDMSEQ %in% dropseq) ,'Y',"")
  irt_par = left_join(irt_par[,which(! colnames(irt_par) %in% c("ITEM_TOTAL_CORRELATION", "N_TOTAL"))], myctt, by = c("IDMSEQ" = "IDMSEQ","UIN" = "UIN"))

  irt_par_cal = irt_par[which(irt_par$EXCLUDED != "Y"),]
  idm_cal = idm[,c(irt_par_cal$IDMID)]

  # compute N_IRT
  irt_par_cal$N_IRT = apply(idm_cal, 2, function (x) {sum(length(which(x != -1)))})

  # add id vars to idm_cal
  idm_cal = cbind(idm[,1:2],idm_cal)

  colnames(irt_par_cal) = toupper(colnames(irt_par_cal))
  colnames(irt_par_4ctt) = toupper(colnames(irt_par_4ctt))
  colnames(irt_par) = toupper(colnames(irt_par))

 # Calibration prep

 if ("MAX_SCORE_POINTS" %in% colnames(irt_par_cal)) {
    colnames(irt_par_cal)[which(colnames(irt_par_cal) == "MAX_SCORE_POINTS")] = "MAX_POINTS"
  }
  irt_par_cal$MODEL <- ifelse(irt_par_cal$MAX_POINTS>1,"GPC",
                          ifelse(irt_par_cal$MAX_POINTS ==1 & irt_par_cal$ITEM_TYPE == "MC"  & str_detect(irt_par_cal$KEY,",") == TRUE, "2PL",
                                 ifelse(irt_par_cal$MAX_POINTS ==1 & irt_par_cal$ITEM_TYPE == "MC" , "3PL",
                                        ifelse(irt_par_cal$MAX_POINTS ==1 & irt_par_cal$ITEM_TYPE == "MS" , "2PL",
                                               ifelse(irt_par_cal$MAX_POINTS ==1 & irt_par_cal$ITEM_TYPE == "XI" , "2PL",NA)))))

  # write control file

  # remove previous run when rerun
  unlink(con_file)

  sink(con_file,append=T)
  cat("Data: \n")
  cat(paste0("     File=",paste0(substr(mypath$idm_path, 1,nchar(mypath$idm_path)-3),"ssig;  \n")))
  cat("\nAnalysis:
     Name=-;
     Mode=Calibration; \n")
  cat("\nComments: \n")
  cat("\nEstimation:
     Method = BAEM;
     E-Step = 500, 1e-005;
     SE = S-EM;
     M-Step = 50, 1e-005;
     Quadrature = 41, 4;
     SEM = 0.001;
     SS = 1e-005; \n")
  cat("\nSave:
     PRM, INF, FAC, IRT, DBG \n")
  cat("\nSCORING:
     Pattern = MAP;
     Score Persons;
     Mean = 0;
     SD = 1; \n")
  cat("\nMiscellaneous:
     Decimal = 4;
     Processors = 1;
     Print CTLD, GOF, Loadings, P-Nums, Diagnostic;
     Min Exp = 1; \n")
  cat("\nGroups : \n")
  cat("\nGroup :
     Dimension = 1; \n")

  # add groups
  cat("     ",paste0("Items =", colnames(idm_cal)[3],", \n"))
  cat("     ",paste0(colnames(idm_cal)[4:(ncol(idm_cal)-1)],", \n     "))
  cat(paste0(colnames(idm_cal)[ncol(idm_cal)],"; \n"))

  # add codes

  for (i in 1:nrow(irt_par_cal)){

    max = irt_par_cal$MAX_POINTS[i]
    code = c(NA)
    for (j in 0:max){
      code[j+1] = paste0(j,"(",j,")")
    }
    code.all <- paste(code,collapse=",")
    cat("     ",paste0("Codes(",colnames(idm_cal)[i+2],") = ", code.all, "; \n"))
  }

  # add MODEL
  for (i in 1:nrow(irt_par_cal)){

    if (irt_par_cal$MODEL[i] == "GPC") {
      cat("     ",paste0("Model(",colnames(idm_cal)[i+2],") = GP Credit; GAMMAMATRIX(",colnames(idm_cal)[i+2],") = TREND;\n"))
    } else {
      cat("     ",paste0("Model(",colnames(idm_cal)[i+2],") = ", irt_par_cal$MODEL[i], ";\n"))
    }
  }
  cat("     Mean = 0.0;\n")
  cat("     Covariance = 1.0;\n")

  # Constraints
  cat("\nConstraints:\n")

  # priors
  cat("\nPriors:\n")

  for (i in 1:nrow(irt_par_cal)){

    if (irt_par_cal$MODEL[i] == "3PL") {
      cat("     ",paste0("(",colnames(idm_cal)[i+2],", Slope[0]) = Lognormal, 0, 1;\n"))
      cat("     ",paste0("(",colnames(idm_cal)[i+2],", Guessing[0]) = Beta, 6, 16;\n"))
    }
  }

  sink()


  # convert to ssig format
  setwd("C:/Program Files/IRTPRO 6.0/")
  return.code <- shell(paste0("ASCII2SSIG64.exe -  ",mypath$idm_path," /header"))
  #shell(paste0('C:/"Program Files"/"IRTPRO 6.0"/ASCII2SSIG64.exe -  ',path,idm_path," /header")) # adding double quote also works here but not safe.
  return.code <- shell(paste0("IRTPROx64.exe -RUN  ", con_file))

  # Extract results

  output = out_irtpro(mypath$out_path)
  myout = output$output

  # merge results
  myout$IDMSEQ = as.numeric(myout$IDMSEQ)
  freeirt = left_join(irt_par,myout,by = c("IDMSEQ" = "IDMSEQ","MAX_POINTS" = "max_points"))
  freeirt = left_join(freeirt,irt_par_cal[,c("UIN","N_IRT")],
                      by = c("UIN" = "UIN"))
  freeirt$admincode = toupper(admin)
  freeirt$testcode = toupper(testcode)
  freeirt$flag_irt = NA
  colnames(freeirt)[which(colnames(freeirt) == "IDMSEQ")] = "IRTLOC"
  colnames(freeirt)[which(colnames(freeirt) == "EXCLUDED")] = "EXCLUDED_FROM_IRT_ANALYSIS"

  # reorder
  freeirt <- freeirt[,c("IRTLOC","admincode","testcode","MODE","UIN","ITEM_STATUS","MAX_POINTS","SCORE_CAT","MAX_POINTS_OBS","SCORE_CAT_OBS",
                      "AVERAGE_ITEM_SCORE","N_IRT","ITEM_TOTAL_CORRELATION","flag_irt","FLAG_SCR_DIST","EXCLUDED_FROM_IRT_ANALYSIS",
                      "un_irt_A","un_irt_B" , "un_irt_C","un_irt_Step1","un_irt_Step2","un_irt_Step3","un_irt_Step4",
                      "un_irt_Step5","un_irt_Step6","un_irt_A_se","un_irt_B_se","un_irt_C_se","un_irt_Step1_se",
                      "un_irt_Step2_se","un_irt_Step3_se","un_irt_Step4_se","un_irt_Step5_se","un_irt_Step6_se","SX2" ,"SX2_P","SX2_flag","model")]
  colnames(freeirt) = toupper(colnames(freeirt))

  write.csv(freeirt,mypath$freeirt_path_csv,row.names = F,na = '')

  # write_sas function is not reliable. Wait for fix.
  # write_sas(data = output,
  #           path = paste0(path, cal_path,toupper(testcode),"/",admin,"_","freeirt_",toupper(testcode),"_",toupper(mode),"_", toupper(status),"_GX.sas7bdat"))

  #use this instead
  write.foreign(freeirt, mypath$freeirt_path_txt,
                mypath$freeirt_path_sas, package="SAS")

  return(list(freeirt = freeirt, convergence = output$convergence,idm_cal = idm_cal))
}



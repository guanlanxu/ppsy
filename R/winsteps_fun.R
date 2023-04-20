#' Run Winsteps calibration.
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
#' @return convergence: Convergence message from Winsteps.
#' @return to_consider: Items that consider to drop.
#' @import haven readr data.table reshape2 tidyverse foreign
#' @export


winsteps_fun <- function(admin,testcode,mode,status,RSnum,mock,
                         dnu = "",dropuin = "",dropseq = "",flagDist = ""
                         ){

  # load libraries
  library(haven)
  library(readr)
  library(data.table)
  library(ppsy)
  library(reshape2)
  library(tidyverse)
  library(foreign)



  mypath = get_path(admin = admin,testcode = testcode, mode = mode, status = status, mock = mock, RSnum = RSnum)


  test = mypath$test

  setwd(mypath$path)

  if(strsplit(mypath$bank_par_path, "[.]")[[1]][2] == "csv"){
    bank_par = read_csv(mypath$bank_par_path)
  } else if (strsplit(mypath$bank_par_path, "[.]")[[1]][2] == "sas7bdat"){
    bank_par = read_sas(mypath$bank_par_path)
  }
  bank_par = as.data.frame(bank_par)
  colnames(bank_par) = toupper(colnames(bank_par))
  bank_par = bank_par[order(bank_par$IDMSEQ),]
  if (any(c("ITEM_STATUS","SPOILED_ITEM","MAX_POINTS") %in% colnames(bank_par))){
    irt_par = bank_par[which(bank_par$ITEM_STATUS %in% unique(c("OP", "FTReport",toupper(status)))),]  # make irt_par match status
    irt_par$EXCLUDED = ifelse((irt_par$SPOILED_ITEM == "O")| (irt_par$UIN %in% c(dnu,dropuin,flagDist))| (irt_par$IDMSEQ %in% dropseq) ,'Y',"")
    irt_par_4ctt = bank_par[which(bank_par$ITEM_STATUS %in% unique(c("OP", "FTReport",toupper(status)))& bank_par$SPOILED_ITEM != "O"),]
  } else {
    print("Warning: some variables are not in the bank_par file (check: ITEM_STATUS, SPOILED_ITEM, MAX_POINTS!!!)")
  }

  irt_par_cal = irt_par[which(irt_par$EXCLUDED != "Y"),]  # only contain calibated info

  # Create directories
  dir.create(file.path(paste0("ANALYSES/"), "EQ"), showWarnings = FALSE)
  dir.create(file.path(paste0("ANALYSES/EQ/"), "OP"), showWarnings = FALSE)
  dir.create(file.path(paste0("ANALYSES/EQ/OP/"), RSnum), showWarnings = FALSE)

  cal_path = mypath$cal_path
  idm_path = mypath$idm_path

  con_file = mypath$con_path


  # initialize output location
  out_file = mypath$winsteps_out_path


  # read in and manage IDM
  idm = read.delim(idm_path,header = F)

  idm = data.frame(studentUUID = substr(idm$V1,1,36),
                   formID = substr(idm$V1, 40, 59),
                   scores = substr(idm$V1, 65,120))   # note: check if the last item was counted in.
  idm_score = data.frame(do.call("rbind",  strsplit(idm[,"scores"], split = "", fixed = T)))
  idm_score = apply(idm_score,2, as.numeric)
  irt_par = irt_par[order(irt_par$IDMSEQ),]
  idm_score = idm_score[,irt_par$IDMSEQ]
  colnames(idm_score) = paste0("score_",irt_par$IDMSEQ)
  idm = cbind(idm, idm_score)

  IDMseq_cal = irt_par_cal$IDMSEQ   # sequence of OP items and FTReport items
  item_delete = (1:ncol(idm_score)) [which( ! (1:ncol(idm_score)) %in% IDMseq_cal)]  # items not in the calibrated map
  idm_cal = idm[,paste0("score_",IDMseq_cal)]



  # remove previous run when rerun
  unlink(con_file)
  # write control file
  sink(file = con_file, append = T)
  cat("&INST\n")
  cat(paste0("TITLE=", admin, "_", testcode, "_", mode, "_", status, "_CON\n"))
  cat("NAME1=1\n")
  cat("NAMELEN=37\n")
  cat("ITEM1=65\n")
  cat(paste0("NI=", nrow(irt_par_cal),"\n"))
  cat("IFILE=", paste0(out_file,".ITM\n"))
  cat("SFILE=", paste0(out_file,".CSF\n"))
  cat("ISFILE=", paste0(out_file,".ISF\n"))
  cat("PFILE=", paste0(out_file,".PER\n"))
  cat("DATA=", paste0(idm_path),"\n")
  cat("CODES=", paste(seq(0,max(irt_par_cal$MAX_POINTS)), collapse =  ""),"\n")
  cat("XWIDE= 1\n")
  cat("MODEL=R\n")
  cat("GROUPS=0\n")
  cat("USCALE=1\n")
  cat("UPMEAN=0\n")
  cat("PTBIS=Y\n")
  cat("STBIAS=Y\n")
  cat("UDECIM=4\n")
  cat("CONVERGE=L\n")
  cat("LCONV=.00005\n")

  cat("TABLES=101000000000010000010000000010\n")
  cat("ILFILES=*\n")
  cat(paste0(irt_par_cal[order(irt_par_cal$IDMSEQ),]$UIN[1:ncol(idm_score)], collapse = "\n"))
  cat("\n*\n")
  cat("IDFILE=*\n")
  cat(paste(item_delete,collapse = "\n"))
  cat('\n*\n')
  cat('&END\n')
  cat('END NAMES')
  sink()

  # call winsteps
  return.code <- shell(paste0("C:/Winsteps/Winsteps.exe BATCH=YES ", con_file, " ", out_file))

  ###----------------------------------------------------------------------------------------------------
  ### After Calibration, extract results for future use

  # extract result

  while (!file.exists(out_file)) {
    Sys.sleep(1)
  }

  win_out = out_winsteps(out_file_path = out_file)
  myout = win_out$out_table

  to_consider = myout[which(myout$St !=1),]

  # CTT statistics
  idm_4ctt = idm_score[,paste0("score_",irt_par_4ctt$IDMSEQ)]
  idm_4ctt = apply(idm_4ctt, 2, as.numeric)

  rs = rowSums(idm[,paste0("score_",irt_par_4ctt[which(irt_par_4ctt$ITEM_STATUS != "FT"),]$IDMSEQ)])
  irt_par_4ctt$ITEM_TOTAL_CORRELATION = apply(idm_4ctt,2, function(x) {cor(x,(rs-x), use="complete.obs")})
  irt_par_4ctt$AVERAGE_ITEM_SCORE = apply(idm_4ctt,2,function(x) {mean(x, na.rm = T)})
  irt_par_4ctt$Max_points_obs = apply(idm_4ctt,2,function(x) max(x, na.rm = T))
  irt_par_4ctt$Score_cat_obs = apply(idm_4ctt, 2, function(x) {length(table(x))})

  myout = left_join(irt_par_4ctt[,c("UIN","ITEM_TOTAL_CORRELATION","AVERAGE_ITEM_SCORE","Max_points_obs","Score_cat_obs","IDMSEQ")], myout, by = c("UIN" = "UIN","IDMSEQ" = "IDMseq"))

  final_res = left_join(irt_par[,which(colnames(irt_par)!= "ITEM_TOTAL_CORRELATION")],
                        myout,by = c("IDMSEQ" = "IDMSEQ","UIN" = "UIN"))
  final_res$Score_cat = final_res$MAX_POINTS +1
  # recompute N_Total using current data
  final_res$N_TOTAL = apply(idm_score, 2, function(x) {length(x)})
  # final_res$N_IRT = apply(idm_scores, 2, function(x) {length(x)})


  # U and M distribution
  final_res$Flag_Scr_Dist = NA
  for (i in 1: ncol(idm_score)){
    final_res$Flag_Scr_Dist[i] = um_dist(score_vec = idm_score[,i],
                                          score_cat = final_res$Score_cat[i],
                                          max_point = final_res$MAX_POINTS[i],
                                         N_TOTAL=  final_res$N_TOTAL[i])
  }


  output <- data.frame(
    IRTLOC = final_res$IDMSEQ,
    Admincode = toupper(admin),
    Testcode = toupper(testcode),
    Mode = toupper(mode),
    UIN = final_res$UIN,
    STATUS = final_res$ITEM_STATUS,
    Max_points = final_res$MAX_POINTS,
    Score_cat = final_res$Score_cat,
    Max_points_obs = final_res$Max_points_obs,
    Score_cat_obs = final_res$Score_cat_obs,
    AVERAGE_ITEM_SCORE = final_res$AVERAGE_ITEM_SCORE,
    N_IRT = final_res$N_IRT,
    ITEM_TOTAL_CORRELATION = final_res$ITEM_TOTAL_CORRELATION,

    flag_Scr_Dist = final_res$Flag_Scr_Dist,
    IRT_FIT_Infit = final_res$IRT_FIT_Infit,
    IRT_FIT_Flag_Infit = final_res$IRT_FIT_Flag_Infit,
    IRT_FIT_Outfit = final_res$IRT_FIT_Outfit,
    IRT_FIT_Flag_Outfit = final_res$IRT_FIT_Flag_Outfit,
    EXCLUDED_FROM_IRT_ANALYSIS = final_res$EXCLUDED,

    un_IRT_B = final_res$UN_IRT_B,
    un_IRT_step1 = final_res$Step1,
    un_IRT_step2 = final_res$Step2,
    un_IRT_step3 = final_res$Step3,

    un_IRT_B_SE = final_res$IRT_B_SE,
    un_IRT_step1_SE = final_res$IRT_STEP1_SE,
    un_IRT_step2_SE = final_res$IRT_STEP2_SE,
    un_IRT_step3_SE = final_res$IRT_STEP3_SE
  )


  write.csv(output,mypath$freeirt_path_csv,row.names = F,na="")

  #use this instead
  write.foreign(output,
                mypath$freeirt_path_txt,
                mypath$freeirt_path_sas, package="SAS")

  if (nrow(to_consider) >0){
    print(to_consider)
  } else {
    print("all items have ST == 1")
  }

  return(list(freeirt = output, convergence = win_out$convergence, to_consider = to_consider))

}







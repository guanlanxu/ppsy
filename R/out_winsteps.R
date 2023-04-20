#' Extract Winsteps results.
#'
#' @param out_file_path Output from Winsteps

#' @return out_table Extraction from Winsteps
#' @return convergence: Convergence message from Winsteps.
#' @export


out_winsteps <- function(out_file_path) {


  # convergence
 # conv_file = read.delim(out_file_path)
  conv_file <- readLines(out_file_path)
  line0.2<- which(grepl("^\\s*TABLE 0.2 ",conv_file))
  line0.3 <- which(grepl("^\\s*TABLE 0.3 ",conv_file))
  conv_file = conv_file[line0.2:(line0.3-1)]
  write.table(conv_file,paste0(out_file_path,"_convergence.txt"))  # for consistency, may use mypath$convergence_path

  # IRT B
  itm_file <- read_table(paste0(out_file_path, ".ITM"), col_names = FALSE, skip = 2)
  colnames(itm_file)[c(1,2,3,4,5,6,7,9,24)] = c("IDMseq","UN_IRT_B","St","N_IRT","Score","IRT_B_SE","IRT_FIT_Infit","IRT_FIT_Outfit","UIN")
  itm_file = itm_file[which(itm_file$St != -3),c("IDMseq","UN_IRT_B","St","N_IRT","Score","IRT_B_SE","IRT_FIT_Infit","IRT_FIT_Outfit","UIN")]
  itm_file$IRT_FIT_Flag_Infit = ifelse(itm_file$IRT_FIT_Infit < .5 |itm_file$IRT_FIT_Infit > 1.5, "Y","N" )
  itm_file$IRT_FIT_Flag_Outfit = ifelse(itm_file$IRT_FIT_Outfit < .5 |itm_file$IRT_FIT_Outfit > 1.5, "Y","N" )

  # steps
  isf_file = fread(paste0(out_file_path, ".ISF"),
                   colClasses = "character",
                   sep = "\n",
                   header = FALSE,
                   verbose = FALSE,
                   strip.white = F, skip = 2)
  if (any(as.numeric(substr(isf_file$V1,16,16))>1)){
    csf_file <- read_table(paste0(out_file_path, ".CSF"), col_names = FALSE, skip = 2)
    colnames(csf_file) = c("IDMseq","step","stepDiff")
    csf_file$step = paste0("Step",csf_file$step)
    csf_file_wide = reshape2::dcast(csf_file, IDMseq~step)
    csf_file_wide[which(csf_file_wide$Step1!= 0) ,which(colnames(csf_file_wide) %like% "Step")] =
    csf_file_wide[which(csf_file_wide$Step1!= 0),which(colnames(csf_file_wide) %like% "Step")] * (-1)

    isf_file$IDMseq = as.numeric(substr(isf_file$V1,5,6))
    isf_file$MaxPts_Cal = as.numeric(substr(isf_file$V1,16,16))
    isf_file$IRT_STEP1_SE = as.numeric(substr(isf_file$V1,56,63))
    isf_file$IRT_STEP2_SE = as.numeric(substr(isf_file$V1,114,121))
    isf_file$IRT_STEP3_SE = as.numeric(substr(isf_file$V1,172,179))
    isf_file = isf_file[,c("IDMseq","MaxPts_Cal","IRT_STEP1_SE","IRT_STEP2_SE","IRT_STEP3_SE")]
    isf_file = as.data.frame(isf_file)
  } else {
    csf_file_wide = data.frame(IDMseq = irt_par_cal$IDMSEQ,
                               Step0 = NA,
                               Step1 = NA,
                               Step2 = NA,
                               Step3 = NA)

    isf_file = data.frame(IDMseq = irt_par_cal$IDMSEQ,
                          MaxPts_Cal = 1,
                          IRT_STEP1_SE = NA,
                          IRT_STEP2_SE = NA,
                          IRT_STEP3_SE = NA
    )
  }



  output_list = list(itm_file, csf_file_wide, isf_file)
  myout = Reduce(function(x, y) merge(x, y, all=TRUE), output_list, accumulate=FALSE)

  myout[which(myout$MaxPts_Cal ==1),c("Step1","Step2","Step3","IRT_STEP1_SE","IRT_STEP2_SE","IRT_STEP3_SE")] = NA
  myout[which(myout$MaxPts_Cal ==2),c("Step3","IRT_STEP3_SE")] = NA

  return(list(out_table = myout, convergence = conv_file))

}

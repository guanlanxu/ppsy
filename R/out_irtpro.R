#' Extract IRTpro results.
#'
#' @param out_file_path Output from IRTpro.

#' @return output: Extraction from IRTpro.
#' @return convergence: Convergence message from IRTpro.



out_irtpro <- function(out_file_path){

  # convergence
  conv <- readLines(paste0(out_file_path,".--irt.txt"))
  EngineS <- unlist(strsplit(conv[which(grepl("^\\s*Engine {1}",conv))],": "))
  SEM <- unlist(strsplit(conv[which(grepl("^\\s*SEM {1}",conv))][2],": "))
  FirstOrder <- unlist(strsplit(conv[which(grepl("^\\s*First-order {1}",conv))],": "))

  EngineS <- conv[which(grepl("^\\s*Engine {1}",conv))]
  SEM <- conv[which(grepl("^\\s*SEM {1}",conv))][2]
  FirstOrder <- conv[which(grepl("^\\s*First-order {1}",conv))]
  conv_file = rbind(EngineS,SEM,FirstOrder)
  write.table(conv_file, paste0(out_file_path,"_convergence.txt"),row.names = F,col.names = F)

  if (FirstOrder != "First-order test: Convergence criteria satisfied"){
    print(paste0('!!!!! Warning: ', FirstOrder, "!!!!!"))
  }

  # extract par estimates
  par_est <- read.delim(paste0(out_file_path,".--prm.txt"), header = F, col.names = seq(1,40) )
  par_est <- par_est[-nrow(par_est),]
  par_est$max_points = par_est$X4-1
  par_est$model = ifelse(is.na(par_est$X8), "2PL",
                         ifelse(par_est$max_points == 1,"3PL","GPC"))
  par_est$IDMSEQ = substr(par_est$X1,7,nchar(par_est$X1))

  for (i in 1:nrow(par_est)){
    model = par_est$model[i]
    max_points = par_est$max_points[i]
    if (model == "3PL") {
      par_est[i,"un_irt_A"] = par_est$X5[i]/1.702
      par_est[i,"un_irt_B"] = par_est$X8[i]
      par_est[i,"un_irt_C"] = par_est$X9[i]
      par_est[i,"un_irt_Step1"] = NA
      par_est[i,"un_irt_Step2"] = NA
      par_est[i,"un_irt_Step3"] = NA
      par_est[i,"un_irt_Step4"] = NA
      par_est[i,"un_irt_Step5"] = NA
      par_est[i,"un_irt_Step6"] = NA
    }
    if (model == "2PL") {
      par_est[i,"un_irt_A"] = par_est$X5[i]/1.702
      par_est[i,"un_irt_B"] = par_est$X7[i]
      par_est[i,"un_irt_C"] = 0
      par_est[i,"un_irt_Step1"] = NA
      par_est[i,"un_irt_Step2"] = NA
      par_est[i,"un_irt_Step3"] = NA
      par_est[i,"un_irt_Step4"] = NA
      par_est[i,"un_irt_Step5"] = NA
      par_est[i,"un_irt_Step6"] = NA

    }
    if (model == "GPC" & max_points == 2){
      par_est[i,"un_irt_A"] = par_est$X6[i]/1.702
      par_est[i,"un_irt_B"] = par_est$X19[i]
      par_est[i,"un_irt_Step1"] = par_est$X21[i]
      par_est[i,"un_irt_Step2"] = par_est$X22[i]
      par_est[i,"un_irt_Step3"] = NA
      par_est[i,"un_irt_Step4"] = NA
      par_est[i,"un_irt_Step5"] = NA
      par_est[i,"un_irt_Step6"] = NA
    }
    if (model == "GPC" & max_points == 4){
      par_est[i,"un_irt_A"] = par_est$X6[i]/1.702
      par_est[i,"un_irt_B"] = par_est$X27[i]
      par_est[i,"un_irt_Step1"] = par_est$X29[i]
      par_est[i,"un_irt_Step2"] = par_est$X30[i]
      par_est[i,"un_irt_Step3"] = par_est$X31[i]
      par_est[i,"un_irt_Step4"] = par_est$X32[i]
      par_est[i,"un_irt_Step5"] = NA
      par_est[i,"un_irt_Step6"] = NA
    }
    if (model == "GPC" & max_points == 5){
      par_est[i,"un_irt_A"] = par_est$X6[i]/1.702
      par_est[i,"un_irt_B"] = par_est$X31[i]
      par_est[i,"un_irt_Step1"] = par_est$X33[i]
      par_est[i,"un_irt_Step2"] = par_est$X34[i]
      par_est[i,"un_irt_Step3"] = par_est$X35[i]
      par_est[i,"un_irt_Step4"] = par_est$X36[i]
      par_est[i,"un_irt_Step5"] = par_est$X37[i]
      par_est[i,"un_irt_Step6"] = NA
    }
  }

  par_est = par_est[,c("IDMSEQ","X1","model","max_points","un_irt_A","un_irt_B", "un_irt_C",
             "un_irt_Step1", "un_irt_Step2", "un_irt_Step3", "un_irt_Step4", "un_irt_Step5","un_irt_Step6")]

  # extract SE
  webpage <- read_html(paste0(out_file_path,".--irt.htm"))

  if (all(c("2PL","3PL","GPC") %in% par_est$model)) {
    twopl_est <- webpage %>%
      html_nodes("table") %>%
      .[4] %>%
      html_table(fill = TRUE)

    threepl_est <- webpage %>%
      html_nodes("table") %>%
      .[5] %>%
      html_table(fill = TRUE)

    GPC_est <- webpage %>%
      html_nodes("table") %>%
      .[6] %>%
      html_table(fill = TRUE)

    SX2_est <- webpage %>%
      html_nodes("table") %>%
      .[12] %>%
      html_table(fill = TRUE)
  } else if ((! "2PL" %in% par_est$model) & (all(c("GPC","3PL") %in% par_est$model))) {   # "GPC", "3PL"

    twopl_est <- NULL

    threepl_est <- webpage %>%
      html_nodes("table") %>%
      .[4] %>%
      html_table(fill = TRUE)

    GPC_est <- webpage %>%
      html_nodes("table") %>%
      .[5] %>%
      html_table(fill = TRUE)

    SX2_est <- webpage %>%
      html_nodes("table") %>%
      .[11] %>%
      html_table(fill = TRUE)

  } else if ((! "GPC" %in% par_est$model) & (all(c("2PL","3PL") %in% par_est$model))) {   # "2PL", "3PL"

    twopl_est <- webpage %>%
      html_nodes("table") %>%
      .[4] %>%
      html_table(fill = TRUE)

    threepl_est <- webpage %>%
      html_nodes("table") %>%
      .[5] %>%
      html_table(fill = TRUE)

    GPC_est <- NULL

    SX2_est <- webpage %>%
      html_nodes("table") %>%
      .[7] %>%
      html_table(fill = TRUE)

  } else if ((! "2PL" %in% par_est$model) & (! "GPC" %in% par_est$model)) {   # "3PL"

    twopl_est <- NULL

    threepl_est <- webpage %>%
      html_nodes("table") %>%
      .[4] %>%
      html_table(fill = TRUE)

    GPC_est <- NULL

    SX2_est <- webpage %>%
      html_nodes("table") %>%
      .[6] %>%
      html_table(fill = TRUE)

  }

  # 2PL se
  if (! is.null(twopl_est)){
    twopl_est = as.data.frame(twopl_est)
    twopl_se <- twopl_est[-1,c(2,5,10,8)]
    twopl_se$X5 = as.numeric(twopl_se$X5)/1.702
    twopl_se$X8 = '0'
    colnames(twopl_se) <- c("X1","un_irt_A_se","un_irt_B_se","un_irt_C_se" )
    twopl_se$un_irt_Step1_se = NA
    twopl_se$un_irt_Step2_se = NA
    twopl_se$un_irt_Step3_se = NA
    twopl_se$un_irt_Step4_se = NA
    twopl_se$un_irt_Step5_se = NA
    twopl_se$un_irt_Step6_se = NA
  } else (twopl_se <- NULL)


  # 3PL se
  if (! is.null(threepl_est)){
    threepl_est = as.data.frame(threepl_est)
    threepl_se <- threepl_est[-1,c(2,5,10,15)]
    threepl_se$X5 = as.numeric(threepl_se$X5)/1.702
    colnames(threepl_se) <- c("X1","un_irt_A_se","un_irt_B_se","un_irt_C_se" )
    threepl_se$un_irt_Step1_se = NA
    threepl_se$un_irt_Step2_se = NA
    threepl_se$un_irt_Step3_se = NA
    threepl_se$un_irt_Step4_se = NA
    threepl_se$un_irt_Step5_se = NA
    threepl_se$un_irt_Step6_se = NA
  } else (threepl_se <- NULL)

  # GPC se
  if (! is.null(GPC_est)){
    GPC_est = as.data.frame(GPC_est)
    GPC_est = GPC_est[-1,]
    GPC_se = GPC_est[,c(2,5,7)]
    GPC_se$X5 = as.numeric(GPC_se$X5)/1.702
    if (max(par_est$max_points) == 2){
      GPC_se$un_irt_Step1_se = GPC_est$X10
      GPC_se$un_irt_Step2_se = GPC_est$X12
      GPC_se$un_irt_Step3_se = NA
      GPC_se$un_irt_Step4_se = NA
      GPC_se$un_irt_Step5_se = NA
      GPC_se$un_irt_Step6_se = NA
    } else if (max(par_est$max_points) == 3){
      GPC_se$un_irt_Step1_se = GPC_est$X10
      GPC_se$un_irt_Step2_se = GPC_est$X12
      GPC_se$un_irt_Step3_se = GPC_est$X14
      GPC_se$un_irt_Step4_se = NA
      GPC_se$un_irt_Step5_se = NA
      GPC_se$un_irt_Step6_se = NA
    } else if (max(par_est$max_points) == 4){
      GPC_se$un_irt_Step1_se = GPC_est$X10
      GPC_se$un_irt_Step2_se = GPC_est$X12
      GPC_se$un_irt_Step3_se = GPC_est$X14
      GPC_se$un_irt_Step4_se = GPC_est$X16
      GPC_se$un_irt_Step5_se = NA
      GPC_se$un_irt_Step6_se = NA
    } else if (max(par_est$max_points) == 5){
      GPC_se$un_irt_Step1_se = GPC_est$X10
      GPC_se$un_irt_Step2_se = GPC_est$X12
      GPC_se$un_irt_Step3_se = GPC_est$X14
      GPC_se$un_irt_Step4_se = GPC_est$X16
      GPC_se$un_irt_Step5_se = GPC_est$X18
      GPC_se$un_irt_Step6_se = NA
    }

    colnames(GPC_se) <- c("X1",
                          "un_irt_A_se","un_irt_B_se","un_irt_Step1_se","un_irt_Step2_se","un_irt_Step3_se","un_irt_Step4_se","un_irt_Step5_se","un_irt_Step6_se" )
    GPC_se$un_irt_C_se = NA
  } else (GPC_se <- NULL)

  # combine results
  free_se = rbind(twopl_se, threepl_se,GPC_se)
  # free_se$IDMSEQ = substr(free_se$X1,7,nchar(free_se$X1))
  # free_se$IDMSEQ = as.numeric(free_se$IDMSEQ)
  # free_se = free_se[order(free_se$IDMSEQ),]

  #   # SX2
  SX2_est = as.data.frame(SX2_est)
  if (ncol(SX2_est) < 5) {
    SX2 = data.frame(X1 = free_se$X1, SX2 = NA, SX2_P = NA, SX2_flag = NA)
  } else {
    SX2 <- SX2_est[-1,c(2,3,5)]
    SX2$X5 = as.numeric(SX2$X5)
    SX2$SX2_flag = ifelse(SX2$X5 < 0.05,"Y", "N")
    colnames(SX2) = c("X1","SX2","SX2_P","SX2_flag")

  }
  par_se = left_join(free_se, SX2, by = c("X1" = "X1"))
  myout = left_join(par_est, par_se, by = c("X1" = "X1"))



  return (list(output = myout, convergence = conv_file))
}



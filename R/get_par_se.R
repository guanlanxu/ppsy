get_par_se <- function(irt_pro_html, model){

  if (all(c("2PL","3PL","GPC") %in% model)) {
    twopl_est <- irt_pro_html %>%
      html_nodes("table") %>%
      .[4] %>%
      html_table(fill = TRUE)

    threepl_est <- irt_pro_html %>%
      html_nodes("table") %>%
      .[5] %>%
      html_table(fill = TRUE)

    GPC_est <- irt_pro_html %>%
      html_nodes("table") %>%
      .[6] %>%
      html_table(fill = TRUE)

    SX2_est <- irt_pro_html %>%
      html_nodes("table") %>%
      .[12] %>%
      html_table(fill = TRUE)
  } else if ((! "2PL" %in% model) & (c("GPC","3PL") %in% model)) {   # "GPC", "3PL"

    twopl_est <- NULL

    threepl_est <- irt_pro_html %>%
      html_nodes("table") %>%
      .[4] %>%
      html_table(fill = TRUE)

    GPC_est <- irt_pro_html %>%
      html_nodes("table") %>%
      .[5] %>%
      html_table(fill = TRUE)

    SX2_est <- irt_pro_html %>%
      html_nodes("table") %>%
      .[11] %>%
      html_table(fill = TRUE)

  } else if ((! "GPC" %in% model) & (c("2PL","3PL") %in% model)) {   # "2PL", "3PL"

    twopl_est <- irt_pro_html %>%
      html_nodes("table") %>%
      .[4] %>%
      html_table(fill = TRUE)

    threepl_est <- irt_pro_html %>%
      html_nodes("table") %>%
      .[5] %>%
      html_table(fill = TRUE)

    GPC_est <- NULL

    SX2_est <- irt_pro_html %>%
      html_nodes("table") %>%
      .[7] %>%
      html_table(fill = TRUE)

  } else if ((! "2PL" %in% model) & (! "GPC" %in% model)) {   # "3PL"

    twopl_est <- NULL

    threepl_est <- irt_pro_html %>%
      html_nodes("table") %>%
      .[4] %>%
      html_table(fill = TRUE)

    GPC_est <- NULL

    SX2_est <- irt_pro_html %>%
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
  } else (threepl_se <- NULL)

  # GPC se
  if (! is.null(GPC_est)){
    GPC_est = as.data.frame(GPC_est)
    GPC_se <- GPC_est[-1,c(2,5,7,10,12,14,16,18)]
    GPC_se$X5 = as.numeric(GPC_se$X5)/1.702
    colnames(GPC_se) <- c("X1",
                          "un_irt_A_se","un_irt_B_se","un_irt_Step1_se","un_irt_Step2_se","un_irt_Step3_se","un_irt_Step4_se","un_irt_Step5_se" )
    GPC_se$un_irt_C_se = NA
  } else (GPC_se <- NULL)

  # combine results
  free_se = rbind(twopl_se, threepl_se,GPC_se)

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

  return(par_se)
}





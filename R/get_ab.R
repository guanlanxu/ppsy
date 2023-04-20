#' get initial values for STUIRT.
#'
#' @param ol_par base parameters.
#' @param ne_par unscaled free parameters.
#' @return A and B: initial values for STURIT.
#' @export


get_ab <- function(ol_par,ne_par){
  if ( ! all.equal(ol_par[,c("ID","model2","SCORE_CAT","DW","type","D")], ne_par[,c("ID","model2","SCORE_CAT","DW","type","D")])) {
    print("!!!!! Warning: Check your input data. Items info are not the same across new and old files. !!!!!")
  } else {

    if (all(ol_par$model2 == "L3")) {
      mean_baseb = mean(ol_par$BASE_B)
      sd_baseb = sd(ol_par$BASE_B)
      mean_b = mean(ne_par$UN_IRT_B)
      sd_b = sd(ne_par$UN_IRT_B)
      A=sd_baseb/sd_b
      B= mean_baseb - A * mean_b

    } else {

      mean_baseb = mean(c(ol_par$BASE_B[which(is.na(ol_par$BASE_STEP1))],
                          ol_par$BASE_B[which(!is.na(ol_par$BASE_STEP1))] + ol_par$BASE_STEP1[which(!is.na(ol_par$BASE_STEP1))],
                          ol_par$BASE_B[which(!is.na(ol_par$BASE_STEP1))] + ol_par$BASE_STEP2[which(!is.na(ol_par$BASE_STEP2))],
                          ol_par$BASE_B[which(!is.na(ol_par$BASE_STEP1) & !is.na(ol_par$BASE_STEP3))] + ol_par$BASE_STEP3[which(!is.na(ol_par$BASE_STEP3))],
                          ol_par$BASE_B[which(!is.na(ol_par$BASE_STEP1) & !is.na(ol_par$BASE_STEP3))] + ol_par$BASE_STEP4[which(!is.na(ol_par$BASE_STEP4))],
                          ol_par$BASE_B[which(!is.na(ol_par$BASE_STEP1) & !is.na(ol_par$BASE_STEP5))] + ol_par$BASE_STEP5[which(!is.na(ol_par$BASE_STEP5))]))

      sd_baseb = sd(c(ol_par$BASE_B[which(is.na(ol_par$BASE_STEP1))],
                      ol_par$BASE_B[which(!is.na(ol_par$BASE_STEP1))] + ol_par$BASE_STEP1[which(!is.na(ol_par$BASE_STEP1))],
                      ol_par$BASE_B[which(!is.na(ol_par$BASE_STEP1))] + ol_par$BASE_STEP2[which(!is.na(ol_par$BASE_STEP2))],
                      ol_par$BASE_B[which(!is.na(ol_par$BASE_STEP1) & !is.na(ol_par$BASE_STEP3))] + ol_par$BASE_STEP3[which(!is.na(ol_par$BASE_STEP3))],
                      ol_par$BASE_B[which(!is.na(ol_par$BASE_STEP1) & !is.na(ol_par$BASE_STEP3))] + ol_par$BASE_STEP4[which(!is.na(ol_par$BASE_STEP4))],
                      ol_par$BASE_B[which(!is.na(ol_par$BASE_STEP1) & !is.na(ol_par$BASE_STEP5))] + ol_par$BASE_STEP5[which(!is.na(ol_par$BASE_STEP5))]))

      mean_b = mean(c(ne_par$UN_IRT_B[which(is.na(ne_par$UN_IRT_STEP1))],
                      ne_par$UN_IRT_B[which(!is.na(ne_par$UN_IRT_STEP1))] + ne_par$UN_IRT_STEP1[which(!is.na(ne_par$UN_IRT_STEP1))],
                      ne_par$UN_IRT_B[which(!is.na(ne_par$UN_IRT_STEP1))] + ne_par$UN_IRT_STEP2[which(!is.na(ne_par$UN_IRT_STEP2))],
                      ne_par$UN_IRT_B[which(!is.na(ne_par$UN_IRT_STEP1) & !is.na(ne_par$UN_IRT_STEP3))] + ne_par$UN_IRT_STEP3[which(!is.na(ne_par$UN_IRT_STEP3))],
                      ne_par$UN_IRT_B[which(!is.na(ne_par$UN_IRT_STEP1) & !is.na(ne_par$UN_IRT_STEP3))] + ne_par$UN_IRT_STEP4[which(!is.na(ne_par$UN_IRT_STEP4))],
                      ne_par$UN_IRT_B[which(!is.na(ne_par$UN_IRT_STEP1) & !is.na(ne_par$UN_IRT_STEP5))] + ne_par$UN_IRT_STEP5[which(!is.na(ne_par$UN_IRT_STEP5))]))

      sd_b = sd(c(ne_par$UN_IRT_B[which(is.na(ne_par$UN_IRT_STEP1))],
                  ne_par$UN_IRT_B[which(!is.na(ne_par$UN_IRT_STEP1))] + ne_par$UN_IRT_STEP1[which(!is.na(ne_par$UN_IRT_STEP1))],
                  ne_par$UN_IRT_B[which(!is.na(ne_par$UN_IRT_STEP1))] + ne_par$UN_IRT_STEP2[which(!is.na(ne_par$UN_IRT_STEP2))],
                  ne_par$UN_IRT_B[which(!is.na(ne_par$UN_IRT_STEP1) & !is.na(ne_par$UN_IRT_STEP3))] + ne_par$UN_IRT_STEP3[which(!is.na(ne_par$UN_IRT_STEP3))],
                  ne_par$UN_IRT_B[which(!is.na(ne_par$UN_IRT_STEP1) & !is.na(ne_par$UN_IRT_STEP3))] + ne_par$UN_IRT_STEP4[which(!is.na(ne_par$UN_IRT_STEP4))],
                  ne_par$UN_IRT_B[which(!is.na(ne_par$UN_IRT_STEP1) & !is.na(ne_par$UN_IRT_STEP5))] + ne_par$UN_IRT_STEP5[which(!is.na(ne_par$UN_IRT_STEP5))]))
      A = sd_baseb/sd_b
      B = mean_baseb - A * mean_b
    }


  }

  return(list(A = A, B = B))

}

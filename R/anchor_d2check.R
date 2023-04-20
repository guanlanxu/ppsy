#' compute D2 and WRMSD for drift analysis.
#'
#' @param ne_par unscaled parameters
#' @param ol_par anchor parameters
#' @param con_path input for STUIRT.
#' @param out_path output from STUIRT.
#' @param equate T or F, whether to scale the pars in ne_par file.
#' @param get_plot T or F, whether to plot ICCs.
#' @return m1m2: Stocking-Lord constants.
#' @return anch_prms: anchor item parameters.
#' @return ICC_plot: generate a pdf plot if get_plot is True.
#' @export


anchor_d2check <- function(ne_par,ol_par, con_path, out_path,
                        equate = T,        # if false, only get m1m2
                        get_plot = T){

  ne_par = ne_par[order(ne_par$UIN),]
  ol_par = ol_par[order(ol_par$UIN),]


  # check if ne_par and ol_par have same numner of items
  if (nrow(ne_par) != nrow(ol_par) ){
    print("!!!!!Warning: ne_par and ol_par don't have same number of items!!!!!")
  } else if (! all.equal(ne_par[,c("UIN")],
                       ol_par[,c("UIN")])){
    print("!!!!!Warning: ne_par and ol_par don't have same item info!!!!!")
  } else {

    ne_par_char = ne_par
    ol_par_char = ol_par

    ne_par_char$model2 <- ifelse(ne_par_char$SCORE_CAT == "2", "L3", "PC")
    ne_par_char$type <- ifelse(ne_par_char$model2 == "PC", "LC", "")
    ne_par_char$DW = "DW"
    ne_par_char$D = 1.702
    ne_par_char$ID = seq.int(nrow(ne_par_char))


    ol_par_char$model2 <- ifelse(ol_par_char$SCORE_CAT == "2", "L3", "PC")
    ol_par_char$type <- ifelse(ol_par_char$model2 == "PC", "LC", "")
    ol_par_char$DW = "DW"
    ol_par_char$D = 1.702
    ol_par_char$ID = seq.int(nrow(ol_par_char))

    ne_par_char = ne_par_char[,c("ID","model2","SCORE_CAT","DW","D","UN_IRT_A","type",
                                 "UN_IRT_B","UN_IRT_C","UN_IRT_STEP1","UN_IRT_STEP2","UN_IRT_STEP3","UN_IRT_STEP4","UN_IRT_STEP5","UN_IRT_STEP6")]
    ol_par_char = ol_par_char[,c("ID","model2","SCORE_CAT","DW","D","BASE_A","type",
                                 "BASE_B","BASE_C","BASE_STEP1","BASE_STEP2","BASE_STEP3","BASE_STEP4","BASE_STEP5","BASE_STEP6")]


    # handle C parameters
    ne_par_char$UN_IRT_A = as.numeric(ne_par_char$UN_IRT_A)
    ne_par_char$UN_IRT_B = as.numeric(ne_par_char$UN_IRT_B)
    ne_par_char$UN_IRT_C = as.numeric(ne_par_char$UN_IRT_C)
    ne_par_char$UN_IRT_STEP1 = as.numeric(ne_par_char$UN_IRT_STEP1)
    ne_par_char$UN_IRT_STEP2 = as.numeric(ne_par_char$UN_IRT_STEP2)
    ne_par_char$UN_IRT_STEP3 = as.numeric(ne_par_char$UN_IRT_STEP3)
    ne_par_char$UN_IRT_STEP4 = as.numeric(ne_par_char$UN_IRT_STEP4)
    ne_par_char$UN_IRT_STEP5 = as.numeric(ne_par_char$UN_IRT_STEP5)
    ne_par_char$UN_IRT_STEP6 = as.numeric(ne_par_char$UN_IRT_STEP6)

    ol_par_char$BASE_A = as.numeric(ol_par_char$BASE_A)
    ol_par_char$BASE_B = as.numeric(ol_par_char$BASE_B)
    ol_par_char$BASE_C = as.numeric(ol_par_char$BASE_C)
    ol_par_char$BASE_STEP1 = as.numeric(ol_par_char$BASE_STEP1)
    ol_par_char$BASE_STEP2 = as.numeric(ol_par_char$BASE_STEP2)
    ol_par_char$BASE_STEP3 = as.numeric(ol_par_char$BASE_STEP3)
    ol_par_char$BASE_STEP4 = as.numeric(ol_par_char$BASE_STEP4)
    ol_par_char$BASE_STEP5 = as.numeric(ol_par_char$BASE_STEP5)
    ol_par_char$BASE_STEP6 = as.numeric(ol_par_char$BASE_STEP6)


    ol_par_char[which(is.na(ol_par_char$BASE_C)), "BASE_C"] <- 0
    ne_par_char[which(is.na(ne_par_char$UN_IRT_C)), "UN_IRT_C"] <- 0


   # Comute starting values


    ab = ppsy::get_ab(ol_par_char, ne_par_char)

    #  ol_par$IRT_A = round2(ol_par$IRT_A,5)
    ol_par_char$BASE_B = round_sas(ol_par_char$BASE_B, 5)
    ol_par_char$BASE_C = round_sas(ol_par_char$BASE_C, 5)
    ol_par_char$BASE_STEP1 = round_sas(ol_par_char$BASE_STEP1, 5)
    ol_par_char$BASE_STEP2 = round_sas(ol_par_char$BASE_STEP2, 5)
    ol_par_char$BASE_STEP3 = round_sas(ol_par_char$BASE_STEP3, 5)
    ol_par_char$BASE_STEP4 = round_sas(ol_par_char$BASE_STEP4, 5)
    ol_par_char$BASE_STEP5 = round_sas(ol_par_char$BASE_STEP5, 5)

    #ne_par$UN_IRT_A = round_sas(ne_par$UN_IRT_A, 5)
    ne_par_char$UN_IRT_B = round_sas(ne_par_char$UN_IRT_B, 5)
    ne_par_char$UN_IRT_C = round_sas(ne_par_char$UN_IRT_C, 5)
    ne_par_char$UN_IRT_STEP1 = round_sas(ne_par_char$UN_IRT_STEP1, 5)
    ne_par_char$UN_IRT_STEP2 = round_sas(ne_par_char$UN_IRT_STEP2, 5)
    ne_par_char$UN_IRT_STEP3 = round_sas(ne_par_char$UN_IRT_STEP3, 5)
    ne_par_char$UN_IRT_STEP4 = round_sas(ne_par_char$UN_IRT_STEP4, 5)
    ne_par_char$UN_IRT_STEP5 = round_sas(ne_par_char$UN_IRT_STEP5, 5)

    ol_par_char <- sapply(ol_par_char, as.character)
    ne_par_char <- sapply(ne_par_char, as.character)
    ol_par_char[is.na(ol_par_char)] <- ""
    ne_par_char[is.na(ne_par_char)] <- ""


    # write script

  unlink(con_path)


  sink(con_path, append = T)
  cat(c("NE ", nrow(ne_par_char), "\n"))
  for (i in 1:nrow(ne_par_char)) {
    cat(as.character(ne_par_char[i, ]), "\n")
  }
  cat(c("OL ", nrow(ol_par_char), "\n"))
  for (i in 1:nrow(ol_par_char)) {
    cat(as.character(ol_par_char[i, ]), "\n")
  }
  cat(
    paste0(
      "CI ",
      nrow(ol_par_char),
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
    input = c(con_path, out_path)
  )

  # extract Constants
  stu <- readLines(out_path)
  line <- stu[which(grepl("^\\s*Stocking-Lord {1}", stu))]
  line2 <- unlist(strsplit(line, split = " "))
  line3 = line2[line2 != ""]
  line4 = which(grepl("Iter# TermC", stu))
  line5 = stu[(line4+2)]
  line6  = unlist(strsplit(line5, split = " "))
  line6 = line6[line6!=""]
  m1_check = line6[4]
  m2_check = line6[5]

  m1 = as.numeric(line3[2])
  m2 = as.numeric(line3[3])

  m1m2 = data.frame(
  #  admin = admin,
    #  test = test,
  #  testcode = testcode,
    m1 = m1,
    m2 = m2,
    interation = line6[6],
    TermCode = line6[7],
    m1_check = m1_check,
    m2_check = m2_check

  )

  if (equate == T){

  # equate anchor based on m1m2

  ne_par$EQ_A = ne_par$UN_IRT_A / m1
  ne_par$EQ_B = ne_par$UN_IRT_B * m1 + m2
  ne_par$EQ_C = ne_par$UN_IRT_C
  ne_par$EQ_STEP1 = ne_par$UN_IRT_STEP1 * m1
  ne_par$EQ_STEP2 = ne_par$UN_IRT_STEP2 * m1
  ne_par$EQ_STEP3 = ne_par$UN_IRT_STEP3 * m1
  ne_par$EQ_STEP4 = ne_par$UN_IRT_STEP4 * m1
  ne_par$EQ_STEP5 = ne_par$UN_IRT_STEP5 * m1
  ne_par$EQ_STEP6 = ne_par$UN_IRT_STEP6 * m1

  #  merge ne and ol pars

  anch_prms = inner_join(ne_par[,c("UIN","MODEL","EQ_A","EQ_B","EQ_C",
                       "EQ_STEP1","EQ_STEP2","EQ_STEP3","EQ_STEP4","EQ_STEP5","EQ_STEP6")],
             ol_par[,c("UIN","BASE_A","BASE_B","BASE_C",
                              "BASE_STEP1","BASE_STEP2","BASE_STEP3","BASE_STEP4","BASE_STEP5","BASE_STEP6","SCORE_CAT")], by = "UIN")

  myd2 = apply(anch_prms, 1,
        function(x)
          ppsy::get_d2(model = x[2], theta.vec = seq(-4,4,0.2),
                       D = 1.702,
                       base_a = x[12], base_b = x[13], base_c = x[14], base_steps = x[15:20],
                       equated_a = x[3], equated_b = x[4], equated_c = x[5], equated_steps = x[6:11],
                       uin = x[1],get_plot = get_plot))

  anch_prms$d2 = unlist(sapply(myd2, function(x) x[1]))
  anch_prms$WRMSD = unlist(sapply(myd2, function(x) x[2]))

  anch_prms$MAX_POINTS = anch_prms$SCORE_CAT -1

  anch_prms$cv = ifelse(anch_prms$MAX_POINTS==1, 0.1,
                   ifelse(anch_prms$MAX_POINTS==2,0.15,
                          ifelse(anch_prms$MAX_POINTS==3,0.225,
                                 ifelse(anch_prms$MAX_POINTS==4,0.3,
                                        ifelse(anch_prms$MAX_POINTS ==5, 0.375,NA)))))
  anch_prms$WRMSD_FLAG = ifelse(anch_prms$WRMSD > anch_prms$cv,1,0)
  anch_prms$sortDiff = (anch_prms$WRMSD-anch_prms$cv)/anch_prms$MAX_POINTS
  anch_prms$chng_A = anch_prms$EQ_A - anch_prms$BASE_A
  anch_prms$chng_B = anch_prms$EQ_B - anch_prms$BASE_B

 # anch_prms = anch_prms[,c("UIN","MAX_POINTS","chng_A","chng_B","WRMSD","sortDiff","WRMSD_FLAG")]

  if (get_plot == T){
    myplot = (sapply(myd2, function(x) x[3]))
  } else {
    myplot = NA
  }

  } else {
    anch_prms = NA
    myplot = NA
  }


  return(list(m1m2 = m1m2, anch_prms = anch_prms, ICC_plot = myplot))

  }

}

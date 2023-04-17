#' Drift Analysis for ALT test.
#'
#' @param admin String.
#' @param testcode String.
#' @param mode P or E.
#' @param status OP or FT.
#' @param RSnum RS1-3
#' @param mock T or F.
#' @param dnu Do-NOT-USE item in UIN in Calibration.
#' @param dropuin Items dropped in Calibration
#' @param dropseq IRTLOC dropped in Calibration
#' @param flagDist Items that have flag and dropped in Calibration
#' @param dropuin_anchor Item that to be dropped from anchor set.
#' @param direction Normal or Backward
#' @param get_plot T for F
#' @param anchor_var variable that used to identify anchor set. "preequating" or "linking_status"
#' @param iter nth iteration.Can be blank.
#' @return irt_file: Item parameter file that contains equated pars and base pars.
#' @return mydrift_table: A table of drift items
#' @return drifted_items: List of drifted items
#' @return anchor_set: A table of anchor items.
#
alt_drift <- function(admin, testcode,mode, status,RSnum, mock,
                      dnu = "", flagDist= "", dropuin="", dropseq="",dropuin_anchor = "",
                      direction="normal",get_plot = T, anchor_var = "linking_status", iter = 0){

  library(haven)
  library(dplyr)
  library(ppsy)
  library(foreign)

  mypath = get_path(admin = admin,testcode = testcode, mode = mode, status = status, mock = mock, RSnum = RSnum, iter = iter)

  test = mypath$test

  if(strsplit(mypath$bank_par_path, "[.]")[[1]][2] == "csv"){
    bank_par = read_csv(mypath$bank_par_path)
  } else if (strsplit(mypath$bank_par_path, "[.]")[[1]][2] == "sas7bdat"){
    bank_par = read_sas(mypath$bank_par_path)
  }
  bank_par = as.data.frame(bank_par)
  colnames(bank_par)= toupper(colnames(bank_par))
  bank_par = bank_par[which(bank_par$SPOILED_ITEM == "" & (! bank_par$UIN %in% c("", dnu, dropuin)) & (! bank_par$IDMSEQ %in% c("", dropseq))),]

  free.Cal = read.csv(mypath$freeirt_path_csv)
  colnames(free.Cal) = toupper(colnames(free.Cal))
  free.Cal = left_join(free.Cal, bank_par[,c(toupper(anchor_var), "UIN","IRT_B","IRT_STEP1","IRT_STEP2","IRT_STEP3","IRT_STEP4","IRT_STEP5","IRT_STEP6")],
                       by = c("UIN" = "UIN"))

  # this is only for drift
  mydrift = free.Cal[which((free.Cal[, which(toupper(colnames(free.Cal)) == toupper(anchor_var))] == "Y") &
                             (!free.Cal$UIN %in% dropuin_anchor) &      # drop anchor items
                             (free.Cal$EXCLUDED_FROM_IRT_ANALYSIS != "Y" |  is.na(free.Cal$EXCLUDED_FROM_IRT_ANALYSIS) ) & # drop excluded irt cal items
                             (! is.na(free.Cal$UN_IRT_B))), ] # drop any doesn't have irtpars

   # compute SL
  mydrift$UN_IRT_STEP1[mydrift$UN_IRT_STEP1 == 0] = NA
  mydrift$UN_IRTB_STEP1 = mydrift$UN_IRT_B - mydrift$UN_IRT_STEP1
  mydrift$UN_IRTB_STEP2 = mydrift$UN_IRT_B - mydrift$UN_IRT_STEP2
  mydrift$UN_IRTB_STEP3 = mydrift$UN_IRT_B - mydrift$UN_IRT_STEP3

  mydrift$IRTB_STEP1 = mydrift$IRT_B - mydrift$IRT_STEP1
  mydrift$IRTB_STEP2 = mydrift$IRT_B - mydrift$IRT_STEP2
  mydrift$IRTB_STEP3 = mydrift$IRT_B - mydrift$IRT_STEP3

  if (toupper(direction) == "BACKWARD"){
      mydrift$SL = (
        # base
        sum(mydrift[which(mydrift$MAX_POINTS ==1),]$UN_IRT_B,
            mydrift[which(mydrift$MAX_POINTS >1),]$UN_IRTB_STEP1,
            mydrift[which(mydrift$MAX_POINTS >1),]$UN_IRTB_STEP2,
            mydrift[which(mydrift$MAX_POINTS >1),]$UN_IRTB_STEP3) -
        # to scale back
        sum(mydrift[which(mydrift$MAX_POINTS ==1),]$IRT_B,
            mydrift[which(mydrift$MAX_POINTS >1),]$IRTB_STEP1,
            mydrift[which(mydrift$MAX_POINTS >1),]$IRTB_STEP2,
            mydrift[which(mydrift$MAX_POINTS >1),]$IRTB_STEP3)
        ) / sum(mydrift$MAX_POINTS)

        # base
        mydrift$base_b =  mydrift$UN_IRT_B
        mydrift$base_step1 = mydrift$UN_IRT_STEP1
        mydrift$base_step2 = mydrift$UN_IRT_STEP2
        mydrift$base_step3 = mydrift$UN_IRT_STEP3

        # scaled back
        mydrift$scaleback_b = mydrift$IRT_B + mydrift$SL
        mydrift$scaleback_step1 = mydrift$IRT_STEP1
        mydrift$scaleback_step2 = mydrift$IRT_STEP2
        mydrift$scaleback_step3 = mydrift$IRT_STEP3
        mydrift$scaleback_b_se = NA
        mydrift$scaleback_step1_se = NA
        mydrift$scaleback_step2_se = NA
        mydrift$scaleback_step3_se = NA

        mydrift$absDiff = abs(mydrift$base_b - mydrift$scaleback_b)

  } else {

    mydrift$SL = (
      # base
      sum(mydrift[which(mydrift$MAX_POINTS ==1),]$IRT_B,
          mydrift[which(mydrift$MAX_POINTS >1),]$IRTB_STEP1,
          mydrift[which(mydrift$MAX_POINTS >1),]$IRTB_STEP2,
          mydrift[which(mydrift$MAX_POINTS >1),]$IRTB_STEP3) -

        # free irt
        sum(mydrift[which(mydrift$MAX_POINTS ==1),]$UN_IRT_B,
            mydrift[which(mydrift$MAX_POINTS >1),]$UN_IRTB_STEP1,
            mydrift[which(mydrift$MAX_POINTS >1),]$UN_IRTB_STEP2,
            mydrift[which(mydrift$MAX_POINTS >1),]$UN_IRTB_STEP3)
    ) / sum(mydrift$MAX_POINTS)

    # base
    mydrift$base_b = mydrift$IRT_B
    mydrift$base_step1 = mydrift$IRTB_STEP1
    mydrift$base_step2 = mydrift$IRTB_STEP2
    mydrift$base_step3 = mydrift$IRTB_STEP3

    # equated
    mydrift$equated_b = mydrift$UN_IRT_B + mydrift$SL
    mydrift$equated_step1 = mydrift$UN_IRT_STEP1
    mydrift$equated_step2 = mydrift$UN_IRT_STEP2
    mydrift$equated_step3 = mydrift$UN_IRT_STEP3
    mydrift$equated_b_se = mydrift$UN_IRT_B_SE
    mydrift$equated_step1_se = mydrift$UN_IRT_STEP1_SE
    mydrift$equated_step2_se = mydrift$UN_IRT_STEP2_SE
    mydrift$equated_step3_se = mydrift$UN_IRT_STEP3_SE

    mydrift$absDiff = abs(mydrift$base_b - mydrift$equated_b)

  }
     mydrift$drit_flag = ifelse(mydrift$absDiff >= 0.3, "Y","")
     drifted = mydrift[which(mydrift$drit_flag == "Y"),]

     # Create Plot for anchor items

     if (get_plot == T & nrow(mydrift)>0){
       library(ggplot2)
       library(data.table)

       mydrift$model = ifelse(mydrift$MAX_POINTS == 1, "Rasch", ifelse(mydrift$MAX_POINTS > 1, "GPC", NA))

       if (toupper(direction) == "BACKWARD"){
         mydrift_small = mydrift[,c("model",paste0("base_",c("b","step1","step2","step3")),
                                    paste0("scaleback_",c("b","step1","step2","step3")),"UIN","MAX_POINTS","IRTLOC","absDiff")]
         to_pdf = mydrift[,c("IRTLOC","TESTCODE","UIN","MAX_POINTS","UN_IRT_B","SL","scaleback_b","base_b","absDiff","drit_flag")]
       } else {
         mydrift_small = mydrift[,c("model",paste0("base_",c("b","step1","step2","step3")),
                                    paste0("equated_",c("b","step1","step2","step3")),"UIN","MAX_POINTS","IRTLOC","absDiff")]
         to_pdf = mydrift[,c("IRTLOC","TESTCODE","UIN","MAX_POINTS","UN_IRT_B","SL","equated_b","base_b","absDiff","drit_flag")]
       }

       theta.vec = seq(-4,4,by = 0.01)
       p_base = list(NA)
       p_equated = list(NA)
       drift_plot = list(NA)

       p_base = apply(mydrift_small, 1, function(x) irt_fun(model = x[1],
                                                            theta.vec = theta.vec,
                                                            D = 1, a = 1, b = x[2], c=0,steps = x[3:5])$pi)
       # equated (scaled back)
       p_equated = apply(mydrift_small, 1, function(x) irt_fun(model = x[1],
                                                               theta.vec = theta.vec,
                                                               D = 1, a = 1, b = x[6],
                                                               c=0,steps = x[7:9])$pi)


       for (i in 1: nrow(mydrift_small)){
         for.plot = data.frame(theta = rep(theta.vec,(mydrift_small$MAX_POINTS[i]+1) *2),
                               probability = c(stack((as.data.frame(rbindlist(list(p_base[[i]]))))[,1:(mydrift_small$MAX_POINTS[i]+1)])[,1],
                                               stack((as.data.frame(rbindlist(list(p_equated[[i]]))))[,1:(mydrift_small$MAX_POINTS[i]+1)])[,1]),
                               grp = c(rep("Base",(mydrift_small$MAX_POINTS[i]+1)*length(theta.vec)),
                                       rep("Equated",(mydrift_small$MAX_POINTS[i]+1)*length(theta.vec))),
                               pt = sort(rep(0:mydrift_small$MAX_POINTS[i],length(theta.vec))))


         p1 = ggplot()+
           geom_line(data = for.plot[which(for.plot$pt== 0),],aes(x = theta, y = probability,linetype=grp,color = grp),linewidth = 0.7)+
           geom_line(data = for.plot[which(for.plot$pt== 1),],aes(x = theta, y = probability,linetype=grp,color = grp),linewidth = 0.7)+
           geom_text(data = for.plot[which(for.plot$pt== 0 & for.plot$theta %in% seq(-4,4,1)),],aes(x = theta, y = probability,label  = pt,color = grp),show.legend = F)+
           geom_text(data = for.plot[which(for.plot$pt== 1 & for.plot$theta %in% seq(-4,4,1)),],aes(x = theta, y = probability,label  = pt,color = grp),show.legend = F)+
           theme(legend.position="bottom",legend.title = element_blank())+
           labs(title = "Anchor vs. Equated Item ICCs",
                subtitle = paste0("uin = ",mydrift_small$UIN[i], "; IDMseq = ", mydrift_small$IRTLOC[i],
                                  "; base_b = ", mydrift_small$base_b,
                                  "; ",colnames(mydrift_small)[6]," = ", round_sas(mydrift_small[i,6],4),
                                  "; absDiff = ", round_sas(mydrift_small$absDiff[i],4) ))+

           theme(#plot.title = element_text(size = 12, face = "bold"),
             plot.subtitle = element_text(size = 7))+
           scale_y_continuous(limits = c(0,1),breaks = c(0,0.25,0.5,0.75,1))+
           scale_x_continuous(limits = c(-4,4),breaks = c(-4,-3,-2,-1,0,1,2,3,4))

         if (mydrift_small$MAX_POINTS[i] == 1){
           drift_plot[[i]] = p1

         } else if (mydrift_small$MAX_POINTS[i] == 2){

           drift_plot[[i]] = p1 +
             geom_line(data = for.plot[which(for.plot$pt== 2),],aes(x = theta, y = probability,linetype=grp,color = grp),linewidth = 0.7)+
             geom_text(data = for.plot[which(for.plot$pt== 2 & for.plot$theta %in% seq(-4,4,1)),],aes(x = theta, y = probability,label  = pt,color = grp),show.legend = F)

         } else if (mydrift_small$MAX_POINTS[i] == 3){
           drift_plot[[i]] =  p1+
             geom_line(data = for.plot[which(for.plot$pt== 2),],aes(x = theta, y = probability,linetype=grp,color = grp),linewidth = 0.7)+
             geom_text(data = for.plot[which(for.plot$pt== 2 & for.plot$theta %in% seq(-4,4,1)),],aes(x = theta, y = probability,label  = pt,color = grp),show.legend = F)+
             geom_line(data = for.plot[which(for.plot$pt== 3),],aes(x = theta, y = probability,linetype=grp,color = grp),linewidth = 0.7)+
             geom_text(data = for.plot[which(for.plot$pt== 3 & for.plot$theta %in% seq(-4,4,1)),],aes(x = theta, y = probability,label  = pt,color = grp),show.legend = F)

         }



       }

       library(gridExtra)
       pdf(mypath$alt_drift_plot_path)
       grid.table(to_pdf,rows=NULL,theme =ttheme_default(base_size = 7))
       invisible(lapply(drift_plot, print))
       dev.off()

     }
     write.csv(mydrift,mypath$alt_drift_path,row.names = F, na = "")



     ## summarise results into irt file
     SL = mydrift$SL[1]

     if (toupper(direction) == "BACKWARD"){
       free.Cal$base_b =  free.Cal$UN_IRT_B
       free.Cal$base_step1 = free.Cal$UN_IRT_STEP1
       free.Cal$base_step2 = free.Cal$UN_IRT_STEP2
       free.Cal$base_step3 = free.Cal$UN_IRT_STEP3

       free.Cal$equated_b =  free.Cal$UN_IRT_B
       free.Cal$equated_step1 = free.Cal$UN_IRT_STEP1
       free.Cal$equated_step2 = free.Cal$UN_IRT_STEP2
       free.Cal$equated_step3 = free.Cal$UN_IRT_STEP3
       free.Cal$equated_b_se =  free.Cal$UN_IRT_B_SE
       free.Cal$equated_step1_se = free.Cal$UN_IRT_STEP1_SE
       free.Cal$equated_step2_se = free.Cal$UN_IRT_STEP2_SE
       free.Cal$equated_step3_se = free.Cal$UN_IRT_STEP3_SE

     } else {
       free.Cal$base_b = free.Cal$IRT_B
       free.Cal$base_step1 = free.Cal$IRTB_STEP1
       free.Cal$base_step2 = free.Cal$IRTB_STEP2
       free.Cal$base_step3 = free.Cal$IRTB_STEP3

       free.Cal$equated_b = free.Cal$UN_IRT_B + SL
       free.Cal$equated_step1 = free.Cal$UN_IRT_STEP1
       free.Cal$equated_step2 = free.Cal$UN_IRT_STEP2
       free.Cal$equated_step3 = free.Cal$UN_IRT_STEP3
       free.Cal$equated_b_se = free.Cal$UN_IRT_B_SE
       free.Cal$equated_step1_se = free.Cal$UN_IRT_STEP1_SE
       free.Cal$equated_step2_se = free.Cal$UN_IRT_STEP2_SE
       free.Cal$equated_step3_se = free.Cal$UN_IRT_STEP3_SE

     }

     free.Cal$FLAG_IRT = ifelse(free.Cal$equated_b > 4 | free.Cal$equated_b < -4 , "Y", "")
     free.Cal = left_join(free.Cal,mydrift[,c("UIN","drit_flag")], by = c("UIN"="UIN"))
     free.Cal$IRT_FIT_FLAG_PSY = ifelse(free.Cal$IRT_FIT_FLAG_INFIT == "Y" | free.Cal$IRT_FIT_FLAG_OUTFIT == "Y" | free.Cal$drit_flag == "Y", "Y",  "")

     irt_file = free.Cal[,c("IRTLOC" ,"ADMINCODE","TESTCODE","MODE" ,"UIN",
                           "STATUS", "MAX_POINTS","SCORE_CAT","MAX_POINTS_OBS"  ,"SCORE_CAT_OBS",  "N_IRT",
                           "AVERAGE_ITEM_SCORE",  "ITEM_TOTAL_CORRELATION", "FLAG_SCR_DIST",  "FLAG_IRT", "IRT_FIT_INFIT",
                           "IRT_FIT_FLAG_INFIT", "IRT_FIT_OUTFIT", "IRT_FIT_FLAG_OUTFIT", "IRT_FIT_FLAG_PSY",
                           "EXCLUDED_FROM_IRT_ANALYSIS")]

      irt_file$IRT_B = free.Cal$equated_b
      irt_file$IRT_B_SE = free.Cal$equated_b_se
      irt_file$IRT_Step1 =  free.Cal$equated_step1
      irt_file$IRT_Step1_SE =  free.Cal$equated_step1_se
      irt_file$IRT_Step2 =  free.Cal$equated_step2
      irt_file$IRT_Step2_SE =  free.Cal$equated_step2_se
      irt_file$IRT_Step3 =  free.Cal$equated_step3
      irt_file$IRT_Step3_SE =  free.Cal$equated_step3_se
      irt_file$IRT_Step4 =  NA
      irt_file$IRT_Step4_SE =  NA
      irt_file$IRT_Step5 =  NA
      irt_file$IRT_Step5_SE =  NA
      irt_file$IRT_Step6 =  NA
      irt_file$IRT_Step6_SE = NA
      irt_file$UN_IRT_B = free.Cal$UN_IRT_B
      irt_file$EQ_CONSTANT = SL




    write.csv(irt_file,mypath$final_irt_path_csv,row.names = F, na = "")
    write.foreign(irt_file,
                  mypath$final_irt_path_txt,
                  mypath$final_irt_path_sas, package="SAS")



return(list (irt_file = irt_file, mydrift_table = mydrift, drifted_items = drifted, anchor_set = to_pdf))

}



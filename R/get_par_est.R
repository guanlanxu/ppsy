


get_par_est <- function(irtpro_par_out, model, uin, max_points){
  par_est <- irtpro_par_out[-nrow(irtpro_par_out),]
  par_est$model = model
  par_est$UIN = uin
  par_est$max_points = max_points

  for (i in 1:nrow(par_est)){
    model = par_est$model[i]
    max_points = par_est$max_points[i]
    if (model == "3PL") {
      par_est[i,"un_irt_A"] = par_est$X5[i]/1.702
      par_est[i,"un_irt_B"] = par_est$X8[i]
      par_est[i,"un_irt_C"] = par_est$X9[i]
    }
    if (model == "2PL") {
      par_est[i,"un_irt_A"] = par_est$X5[i]/1.702
      par_est[i,"un_irt_B"] = par_est$X7[i]
    }
    if (model == "GPC" & max_points == 2){
      par_est[i,"un_irt_A"] = par_est$X6[i]/1.702
      par_est[i,"un_irt_B"] = par_est$X19[i]
      par_est[i,"un_irt_Step1"] = par_est$X21[i]
      par_est[i,"un_irt_Step2"] = par_est$X22[i]
    }
    if (model == "GPC" & max_points == 4){
      par_est[i,"un_irt_A"] = par_est$X6[i]/1.702
      par_est[i,"un_irt_B"] = par_est$X27[i]
      par_est[i,"un_irt_Step1"] = par_est$X29[i]
      par_est[i,"un_irt_Step2"] = par_est$X30[i]
      par_est[i,"un_irt_Step3"] = par_est$X31[i]
      par_est[i,"un_irt_Step4"] = par_est$X32[i]
    }
    if (model == "GPC" & max_points == 5){
      par_est[i,"un_irt_A"] = par_est$X6[i]/1.702
      par_est[i,"un_irt_B"] = par_est$X31[i]
      par_est[i,"un_irt_Step1"] = par_est$X33[i]
      par_est[i,"un_irt_Step2"] = par_est$X34[i]
      par_est[i,"un_irt_Step3"] = par_est$X35[i]
      par_est[i,"un_irt_Step4"] = par_est$X36[i]
      par_est[i,"un_irt_Step5"] = par_est$X37[i]
    }
  }
  return(par_est[,c("X1","UIN","MODEL","max_points","un_irt_A","un_irt_B", "un_irt_C",
                    "un_irt_Step1", "un_irt_Step2", "un_irt_Step3", "un_irt_Step4", "un_irt_Step5")])

}

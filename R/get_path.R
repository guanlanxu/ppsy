
#' get path for files. Use this function in conjunction of `newadmin_setup`, which will create directories for the files.
#'
#' @param admin String.
#' @param testcode String.
#' @param mode P or E.
#' @param status OP or FT.
#' @param mock T or F.
#' @param RSnum RS1-3.
#' @param pre_post pre or post.
#' @param purpose normal or maintain.
#' @param iter integer, e.g., 1,2,3... iterations
#' @return list of paths.
#' @export


get_path <- function(admin, testcode,mode, status = NA, mock, RSnum,pre_post = NA,purpose = NA, iter = "") {

  # if (is.na(pre_post)) {pre_post = ""}
  # if (is.na(purpose)) {pre_post = ""}
 if (is.na(iter)){
   iter = ""
 }
  test = toupper(substr(admin, 3, 5))

  if (tolower(substr(admin, 6, 8)) == "spr") {
    admin2 = paste0("Spring", substr(admin, 9, 10))   # admin2 is for path
  } else {
    admin2 = paste0("Fall", substr(admin, 9, 10))
  }
  path = paste0("C:/TDOE/", admin2, "/", toupper(test), "/")


  # maintain is always for OP.
  if (!(is.na(purpose)) & toupper(purpose) == 'MAINTAIN') {
    status  = "OP"
  }

  # setwd(path)


  # Create directories

  newadmin_setup(admin2)



  # dir.create(file.path(paste0("ANALYSES/"), "EQ"), showWarnings = FALSE)
  # dir.create(file.path(paste0("ANALYSES/EQ/"), toupper(status)), showWarnings = FALSE)
  # dir.create(file.path(paste0("ANALYSES/EQ/", toupper(status), "/"), RSnum), showWarnings = FALSE)

  if (mock == T | mock == 1) {
    RSnum2 = paste0(RSnum,"/X_Mock")
    for_idm =  paste0("IDM/MOCK")
  } else {
    RSnum2 = RSnum
    for_idm = "IDM"
  }

  if (toupper( test) == "ALT"){
    cal_path = paste0(path,"ANALYSES/EQ/", toupper(status), "/", RSnum2, "/Calibration/")
    eq_path = paste0(path,"ANALYSES/EQ/", toupper(status), "/", RSnum2, "/Drift/")
    drift_path = paste0(path,"ANALYSES/EQ/", toupper(status), "/", RSnum2, "/Drift/")
  } else if (toupper(test) %in% c('EOC',"ACH")){
    cal_path = paste0(path,"ANALYSES/EQ/", toupper(status), "/", RSnum2, "/Calibration/",toupper(testcode),"/")
    eq_path = paste0(path,"ANALYSES/EQ/", toupper(status), "/", RSnum2, "/STUIRT/")
    drift_path = paste0(path,"ANALYSES/EQ/", toupper(status), "/", RSnum2, "/Drift/")
    fit_path = paste0(path,"ANALYSES/EQ/", toupper(status), "/", RSnum2, "/FIT/")
  }

  log_path = paste0(path,"ANALYSES/EQ/OP/", RSnum2, "/Logs/")

  rsss_path = paste0(path,"ANALYSES/EQ/OP/", RSnum2, "/RSSS/")
  eprs_path = paste0(path,"ANALYSES/EQ/OP/", RSnum2, "/RSSS/ePRS/")  # only OP need ePRS_path
  final_path = paste0(path,"ANALYSES/EQ/OP/", RSnum2, "/Final_Par/")

  #  irt_par path and idm path
  if (toupper(testcode) %in% c(paste0('EL', 3:8), paste0("EN", 1:2))) {
    # use sparse matrix
      idm_path = dir(paste0(path,"ANALYSES/EQ/", toupper(status), "/RA1/",for_idm,"/"),full.names = T,
                     pattern = paste0("^",toupper(admin),"_IDM_SPARSE_",toupper(testcode),"_",toupper(mode),"_",toupper(status)))[1]
      idm_original_path = dir(paste0(path,"ANALYSES/EQ/", toupper(status), "/RA1/",for_idm,"/"),full.names = T,
                              pattern = paste0("^",toupper(admin),"_IDM_",toupper(testcode),"_",toupper(mode),"_",toupper(status)))[1]
    } else {
      idm_path = dir(paste0(path,"ANALYSES/EQ/", toupper(status), "/RA1/",for_idm,"/"),full.names = T,
                     pattern = paste0("^",toupper(admin),"_IDM_",toupper(testcode),"_",toupper(mode),"_",toupper(status)))[1]
      idm_original_path = idm_path
    }



  bank_par_path  = dir(paste0(path,"DATA/ITEM_PARAMETERS/"),full.names = T,
                       pattern = paste0(tolower(admin),"_irt_",tolower(testcode),"_",tolower(mode)))[1]

  temp = as.vector(dir(paste0(path,"DATA/ITEM_PARAMETERS/"),full.names = T,
             pattern = paste0(tolower(admin),"_irt_",tolower(testcode),"_",tolower(mode)))[1])
  bank_par_path = temp

  ptm_path = dir(paste0(path,"DATA/TEST_MAPS/PTM/"),full.names = T,
                 pattern = paste0(tolower(admin),"_ptm_",tolower(testcode),"_",tolower(mode),"_gx"))[1]

  std_path = dir(paste0(path,"DATA/STANDARDIZED/"),full.names = T,
                 pattern = paste0(tolower(admin),"_std_",tolower(testcode)))[length(dir(paste0(path,"DATA/STANDARDIZED/"),full.names = T,
                                                                                        pattern = paste0(tolower(admin),"_std_",tolower(testcode))))]

  sdf_path = dir(paste0(path,"DATA/SDF/"),full.names = T,
                 pattern = paste0(tolower(admin),"_sdf_",tolower(testcode)))[length(dir(paste0(path,"DATA/SDF/"),full.names = T,
                                                                                        pattern = paste0(tolower(admin),"_sdf_",tolower(testcode))))]

  # irtpro

  # used for both irtpro and winsteps
  con_path =  paste0(cal_path,
                            paste0(toupper(admin),"_CAL_",toupper(testcode),"_",toupper(mode),"_",toupper(status),"_GX.CON"))  # used for both irtpro and winsteps

  out_path = paste0(cal_path,
                             toupper(admin),"_CAL_", toupper(testcode),"_",toupper(mode),"_", toupper(status),"_GX")


  convergence_path = paste0(cal_path,
                            paste0(toupper(admin),"_CAL_",toupper(testcode),"_",toupper(mode),"_",toupper(status),"_GX.--irt.txt"))

  convergence_save_path = paste0(cal_path,
                                 toupper(admin),"_CAL_", toupper(testcode),"_",toupper(mode),"_", toupper(status),"_GX_convergence.txt")

  irtpro_par_path = paste0(cal_path,
                           paste0( toupper(admin),"_CAL_",toupper(testcode),"_",toupper(mode),"_",toupper(status),"_GX.--prm.txt"))
  irtpro_web_path = paste0(cal_path,
                           paste0(toupper(admin),"_CAL_",toupper(testcode),"_",toupper(mode),"_",toupper(status), "_GX.--irt.htm"))

  freeirt_path_csv = paste0(cal_path,
                            toupper(admin),"_","freeirt_",toupper(testcode),"_",toupper(mode),"_", toupper(status),"_GX.csv")
  irtwq1_path_csv = paste0(cal_path,
                           toupper(admin),"_","irtwq1_",toupper(testcode),"_",toupper(mode),"_", toupper(status),"_GX.csv")
  freeirt_path_sas = paste0(cal_path,
                            toupper(admin),"_","freeirt_",toupper(testcode),"_",toupper(mode),"_", toupper(status),"_GX.sas")
  irtwq1_path_sas = paste0(cal_path,
                           toupper(admin),"_","irtwq1_",toupper(testcode),"_",toupper(mode),"_", toupper(status),"_GX.sas")
  freeirt_path_txt = paste0(cal_path,
                            toupper(admin),"_","freeirt_",toupper(testcode),"_",toupper(mode),"_", toupper(status),"_GX.txt")
  irtwq1_path_txt = paste0(cal_path,
                           toupper(admin),"_","irtwq1_",toupper(testcode),"_",toupper(mode),"_", toupper(status),"_GX.txt")

  fit_plot_path = paste0(fit_path,
                         toupper(admin),"_FIT_",toupper(testcode),"_",toupper(mode),"_", toupper(status),"_GX.pdf")
  # drift & equating
  stuirt_con_path = paste0(eq_path,toupper(admin),"_",toupper(testcode),"_",toupper(mode),"_",toupper(status),"_GX", iter,".txt")
  stuirt_out_path = paste0(eq_path,toupper(admin),"_",toupper(testcode),"_",toupper(mode),"_",toupper(status),"_GX", iter,".out")

  alt_drift_path = paste0(drift_path,toupper(admin),'_drift_',toupper(testcode),"_GX", iter,".csv")
  alt_drift_plot_path = paste0(drift_path,toupper(admin),'_ICCs_',toupper(testcode),"_GX", iter,".pdf")

  drift_plot_path = paste0(drift_path,toupper(admin),'_ICCs_',toupper(testcode),"_GX", iter,".pdf")
  drift_log_path = paste0(log_path,toupper(admin),'_AnchStability_',toupper(testcode),"_GX", iter,".pdf")
  drift_tcc_path = paste0(drift_path,toupper(admin),'_TCCdrift_',toupper(testcode),"_GX", iter,".pdf")

  d2_path_csv = paste0(drift_path,toupper(admin),'_d2_',toupper(testcode),"_", toupper(mode),"_GX", iter,".csv")
  d2_path_txt = paste0(drift_path,toupper(admin),'_d2_',toupper(testcode),"_", toupper(mode),"_GX", iter,".txt")
  d2_path_sas = paste0(drift_path,toupper(admin),'_d2_',toupper(testcode),"_", toupper(mode),"_GX", iter,".sas")

  m1m2_path = paste0(eq_path,toupper(admin),'_m1m2_',toupper(testcode),"_GX", iter,".csv")

  # final par
  internalirt_path = paste0(eq_path,
                            toupper(admin),"_internalirt_",toupper(testcode),"_",toupper(mode),"_", toupper(status),"_GX.csv")

  final_irt_path_csv = paste0(final_path,
                              toupper(admin),"_irt_",toupper(testcode),"_",toupper(mode),"_", toupper(status),"_GX.csv")
  final_irt_path_sas = paste0(final_path,
                              toupper(admin),"_irt_",toupper(testcode),"_",toupper(mode),"_", toupper(status),"_GX.sas")
  final_irt_path_txt = paste0(final_path,
                              toupper(admin),"_irt_",toupper(testcode),"_",toupper(mode),"_", toupper(status),"_GX.txt")

  # rsss
  if (toupper(pre_post) == "PRE" & mock != T){
    rsss_path_csv =  paste0(rsss_path,
                            toupper(admin),"_pre_",toupper(testcode),"_",toupper(mode),"_OP_GX.csv")
    rsss_rc_path_csv =  paste0(rsss_path,
                               toupper(admin),"_prepl3b_",toupper(testcode),"_",toupper(mode),"_OP_GX.csv")
    rsss_path_sas =  paste0(rsss_path,
                            toupper(admin),"_pre_",toupper(testcode),"_",toupper(mode),"_OP_GX.sas")
    rsss_rc_path_sas =  paste0(rsss_path,
                               toupper(admin),"_prepl3b_",toupper(testcode),"_",toupper(mode),"_OP_GX.sas")
    rsss_path_txt =  paste0(rsss_path,
                            toupper(admin),"_pre_",toupper(testcode),"_",toupper(mode),"_OP_GX.txt")
    rsss_rc_path_txt =  paste0(rsss_path,
                               toupper(admin),"_prepl3b_",toupper(testcode),"_",toupper(mode),"_OP_GX.txt")

    rsss_4TDOE_path = paste0(rsss_path,"For_TDOE/",
                             toupper(admin),"_",toupper(pre_post),"_RSSS_",toupper(mode),"_OP_GX.csv")
    pl3b_4TDOE_path = paste0(rsss_path,"For_TDOE/",
                             toupper(admin),"_",toupper(pre_post),"_subscoreCuts_",toupper(mode),"_OP_GX.csv")

  } else  if (toupper(pre_post) == "PRE" & mock == T){
    # for mock tables
    rsss_path_csv =  paste0(rsss_path,
                            toupper(admin),"_pre_",toupper(testcode),"_",toupper(mode),"_OP_GX_mock.csv")
    rsss_rc_path_csv =  paste0(rsss_path,
                               toupper(admin),"_prepl3b_",toupper(testcode),"_",toupper(mode),"_OP_GX_mock.csv")
    rsss_path_sas =  paste0(rsss_path,
                            toupper(admin),"_pre_",toupper(testcode),"_",toupper(mode),"_OP_GX_mock.sas")
    rsss_rc_path_sas =  paste0(rsss_path,
                               toupper(admin),"_prepl3b_",toupper(testcode),"_",toupper(mode),"_OP_GX_mock.sas")
    rsss_path_txt =  paste0(rsss_path,
                            toupper(admin),"_pre_",toupper(testcode),"_",toupper(mode),"_OP_GX_mock.txt")
    rsss_rc_path_txt =  paste0(rsss_path,
                               toupper(admin),"_prepl3b_",toupper(testcode),"_",toupper(mode),"_OP_GX_mock.txt")

    rsss_4TDOE_path = paste0(rsss_path,"For_TDOE/",
                             toupper(admin),"_",toupper(pre_post),"_RSSS_",toupper(mode),"_OP_GX_mock.csv")
    pl3b_4TDOE_path = paste0(rsss_path,"For_TDOE/",
                             toupper(admin),"_",toupper(pre_post),"_subscoreCuts_",toupper(mode),"_OP_GX_mock.csv")



  } else if (toupper(pre_post) == "POST"){
    # for post equating
    rsss_path_csv =  paste0(rsss_path,
                            toupper(admin),"_rsss_",toupper(testcode),"_",toupper(mode),"_OP_GX.csv")
    rsss_rc_path_csv =  paste0(rsss_path,
                               toupper(admin),"_pl3b_",toupper(testcode),"_",toupper(mode),"_OP_GX.csv")
    rsss_path_sas =  paste0(rsss_path,
                            toupper(admin),"_rsss_",toupper(testcode),"_",toupper(mode),"_OP_GX.sas")
    rsss_rc_path_sas =  paste0(rsss_path,
                               toupper(admin),"_pl3b_",toupper(testcode),"_",toupper(mode),"_OP_GX.sas")
    rsss_path_txt =  paste0(rsss_path,
                            toupper(admin),"_rsss_",toupper(testcode),"_",toupper(mode),"_OP_GX.txt")
    rsss_rc_path_txt =  paste0(rsss_path,
                               toupper(admin),"_pl3b_",toupper(testcode),"_",toupper(mode),"_OP_GX.txt")

    rsss_4TDOE_path = paste0(rsss_path,"For_TDOE/",
                             toupper(admin),"_",toupper(pre_post),"_RSSS_",toupper(mode),"_OP_GX.csv")
    pl3b_4TDOE_path = paste0(rsss_path,"For_TDOE/",
                             toupper(admin),"_",toupper(pre_post),"_subscoreCuts_",toupper(mode),"_OP_GX.csv")
  }




  # eprs
  pl3b_e_battery_path = paste0(eprs_path,
                               toupper(admin),"_PL3B_", pre_post, "eq_E_op_GX_", format(Sys.Date(),"%m%d%Y"),"_Battery.xlsx")
  pl3b_e_Nbattery_path = paste0(eprs_path,
                                toupper(admin),"_PL3B_", pre_post, "eq_E_op_GX_", format(Sys.Date(),"%m%d%Y"),"_NonBattery.xlsx")
  pl3b_p_path = paste0(eprs_path,
                       toupper(admin),"_PL3B_", pre_post, "eq_P_op_GX_", format(Sys.Date(),"%m%d%Y"),".xlsx")

  pl4b_e_battery_path = paste0(eprs_path,
                               toupper(admin),"_PL4B_", pre_post, "eq_E_op_GX_", format(Sys.Date(),"%m%d%Y"),"_Battery.xlsx")
  pl4b_e_Nbattery_path = paste0(eprs_path,
                                toupper(admin),"_PL4B_", pre_post, "eq_E_op_GX_", format(Sys.Date(),"%m%d%Y"),"_NonBattery.xlsx")
  pl4b_p_path = paste0(eprs_path,
                       toupper(admin),"_PL4B_", pre_post, "eq_P_op_GX_", format(Sys.Date(),"%m%d%Y"),".xlsx")

  semb_e_battery_path = paste0(eprs_path,
                               toupper(admin),"_SEMB_", pre_post, "eq_E_op_GX_", format(Sys.Date(),"%m%d%Y"),"_Battery.xlsx")
  semb_e_Nbattery_path = paste0(eprs_path,
                                toupper(admin),"_SEMB_", pre_post, "eq_E_op_GX_", format(Sys.Date(),"%m%d%Y"),"_NonBattery.xlsx")
  semb_p_path = paste0(eprs_path,
                       toupper(admin),"_SEMB_", pre_post, "eq_P_op_GX_", format(Sys.Date(),"%m%d%Y"),".xlsx")

  ssb_e_battery_path = paste0(eprs_path,
                              toupper(admin),"_SSB_", pre_post, "eq_E_op_GX_", format(Sys.Date(),"%m%d%Y"),"_Battery.xlsx")
  ssb_e_Nbattery_path = paste0(eprs_path,
                               toupper(admin),"_SSB_", pre_post, "eq_E_op_GX_", format(Sys.Date(),"%m%d%Y"),"_NonBattery.xlsx")
  ssb_p_path =paste0(eprs_path,
                     toupper(admin),"_SSB_", pre_post, "eq_P_op_GX_", format(Sys.Date(),"%m%d%Y"),".xlsx")

  irtb_e_battery_path = paste0(eprs_path,
                               toupper(admin),"_IRTB_", pre_post, "eq_E_op_GX_", format(Sys.Date(),"%m%d%Y"),"_Battery.xlsx")
  irtb_e_Nbattery_path = paste0(eprs_path,
                                toupper(admin),"_IRTB_", pre_post, "eq_E_op_GX_", format(Sys.Date(),"%m%d%Y"),"_NonBattery.xlsx")
  irtb_p_path = paste0(eprs_path,
                       toupper(admin),"_IRTB_", pre_post, "eq_P_op_GX_", format(Sys.Date(),"%m%d%Y"),".xlsx")


  aberrant_path = paste0(eprs_path,
                         toupper(admin),"_ABERRANT_", pre_post, "eq_P_op_GX_", format(Sys.Date(),"%m%d%Y"),".csv")




  return(
    list(
      test = test,
      path = path,
      cal_path = cal_path,
      eq_path = eq_path,
      drift_path = drift_path,
      rsss_path = rsss_path,
      eprs_path = eprs_path,
      fit_path = fit_path,

      idm_path = idm_path,
      idm_original_path = idm_original_path ,

      bank_par_path  = bank_par_path,
      ptm_path = ptm_path,

      std_path = std_path,
      sdf_path = sdf_path,

      # irtpro

      con_path =  con_path,

      out_path = out_path,

      convergence_path = convergence_path,
      convergence_save_path = convergence_save_path,

      irtpro_par_path = irtpro_par_path,

      irtpro_web_path = irtpro_web_path,

      freeirt_path_csv = freeirt_path_csv,
      irtwq1_path_csv = irtwq1_path_csv,
      freeirt_path_sas = freeirt_path_sas,
      irtwq1_path_sas = irtwq1_path_sas,
      freeirt_path_txt = freeirt_path_txt,
      irtwq1_path_txt = irtwq1_path_txt,
      fit_plot_path = fit_plot_path,

      # equating
      stuirt_con_path = stuirt_con_path,
      stuirt_out_path = stuirt_out_path,
      alt_drift_path =alt_drift_path,
      alt_drift_plot_path = alt_drift_plot_path,

      drift_plot_path = drift_plot_path,
      drift_log_path = drift_log_path,
      d2_path_csv = d2_path_csv,
      d2_path_txt = d2_path_txt,
      d2_path_sas = d2_path_sas,
      drift_tcc_path = drift_tcc_path,

      m1m2_path = m1m2_path,
      # final par
      internalirt_path = internalirt_path,
      final_irt_path_csv = final_irt_path_csv,
      final_irt_path_sas = final_irt_path_sas,
      final_irt_path_txt = final_irt_path_txt,

      # alt_final_irt_path_csv = alt_final_irt_path_csv,
      # alt_final_irt_path_txt = alt_final_irt_path_txt,
      # alt_final_irt_path_sas = alt_final_irt_path_sas,

      # rsss
      rsss_path_csv =  rsss_path_csv,
      rsss_rc_path_csv =  rsss_rc_path_csv,
      rsss_path_sas =  rsss_path_sas,
      rsss_rc_path_sas =  rsss_rc_path_sas,
      rsss_path_txt =  rsss_path_txt,
      rsss_rc_path_txt =  rsss_rc_path_txt,

      rsss_4TDOE_path = rsss_4TDOE_path,
      pl3b_4TDOE_path = pl3b_4TDOE_path,

      # eprs
      pl3b_e_battery_path = pl3b_e_battery_path,
      pl3b_e_Nbattery_path = pl3b_e_Nbattery_path,
      pl3b_p_path = pl3b_p_path,

      pl4b_e_battery_path = pl4b_e_battery_path,
      pl4b_e_Nbattery_path = pl4b_e_Nbattery_path,
      pl4b_p_path = pl4b_p_path,

      semb_e_battery_path = semb_e_battery_path,
      semb_e_Nbattery_path = semb_e_Nbattery_path,
      semb_p_path = semb_p_path,
      ssb_e_battery_path = ssb_e_battery_path,
      ssb_e_Nbattery_path = ssb_e_Nbattery_path,
      ssb_p_path = ssb_p_path,

      irtb_e_battery_path = irtb_e_battery_path,
      irtb_e_Nbattery_path = irtb_e_Nbattery_path,
      irtb_p_path = irtb_p_path,


      aberrant_path = aberrant_path



    )
  )


}


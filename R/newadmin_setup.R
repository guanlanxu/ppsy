#' Set up directories for new administration.
#'
#' @param admin_year administration year. E.g., "Spring19", "Fall20"


newadmin_setup <- function(admin_year){
  dir.create(file.path("C:/TDOE/",admin_year),showWarnings =F)

  if (tolower(substr(admin_year,1,3)) == "spr") {
    tests = c("ACH","EOC","ALT")
  } else {tests = c("EOC")}

  under_tests = c("ANALYSES","DATA")
  under_analyses = c("EQ","SPECIAL_STUDIES","TECH_REPORT")
  under_OP = c("RA1","RS1","RS2")

  under_rs_eocach <- c("Calibration","Drift","STUIRT","RSSS","Compare","CODE","Final_Par","FIT","Logs")
  under_rs_alt <- c("Calibration","Drift","RSSS","Compare","CODE","Final_Par")


  for (i in 1:length(tests)){
    dir.create(file.path(paste0("C:/TDOE/",admin_year,"/"),tests[i]), showWarnings =F)

    for (j in 1:length(under_tests)){
      dir.create(file.path(paste0("C:/TDOE/",admin_year,"/",tests[i],"/"),under_tests[j]), showWarnings =F)
    }

    for (m in 1: length(under_analyses)){
      dir.create(file.path(paste0("C:/TDOE/",admin_year,"/",tests[i],"/ANALYSES"),under_analyses[m]), showWarnings =F)
    }
    dir.create(file.path(paste0("C:/TDOE/",admin_year,"/",tests[i],"/ANALYSES/EQ"),"OP"), showWarnings =F)
    dir.create(file.path(paste0("C:/TDOE/",admin_year,"/",tests[i],"/ANALYSES/EQ"),"FT"), showWarnings =F)

    for (p in 1:length(under_OP)){
      dir.create(file.path(paste0("C:/TDOE/",admin_year,"/",tests[i],"/ANALYSES/EQ/OP"),under_OP[p]), showWarnings =F)
    }

    for (p in 1:length(under_OP)){
      dir.create(file.path(paste0("C:/TDOE/",admin_year,"/",tests[i],"/ANALYSES/EQ/FT"),under_OP[p]), showWarnings =F)
    }

    for (rs in 1:2){
      dir.create(file.path(paste0("C:/TDOE/",admin_year,"/",tests[i],"/ANALYSES/EQ/OP/RS",rs,"/"),"X_Mock"), showWarnings =F)
      if (tests[i] == "ALT"){
        for (f in 1: length(under_rs_alt)){
          dir.create(file.path(paste0("C:/TDOE/",admin_year,"/",tests[i],"/ANALYSES/EQ/OP/RS",rs,"/X_Mock/"),under_rs_alt[f]), showWarnings =F)
          dir.create(file.path(paste0("C:/TDOE/",admin_year,"/",tests[i],"/ANALYSES/EQ/OP/RS",rs,"/"),under_rs_alt[f]), showWarnings =F)
          }
        }else {
          for (f in 1: length(under_rs_eocach)){
          dir.create(file.path(paste0("C:/TDOE/",admin_year,"/",tests[i],"/ANALYSES/EQ/OP/RS",rs,"/X_Mock/"),under_rs_eocach[f]), showWarnings =F)
          dir.create(file.path(paste0("C:/TDOE/",admin_year,"/",tests[i],"/ANALYSES/EQ/OP/RS",rs,"/"),under_rs_eocach[f]), showWarnings =F)
          }
        }
      dir.create(file.path(paste0("C:/TDOE/",admin_year,"/",tests[i],"/ANALYSES/EQ/OP/RS",rs,"/X_Mock/RSSS"),"ePRS"), showWarnings =F)
      dir.create(file.path(paste0("C:/TDOE/",admin_year,"/",tests[i],"/ANALYSES/EQ/OP/RS",rs,"/RSSS"),"ePRS"), showWarnings =F)

      dir.create(file.path(paste0("C:/TDOE/",admin_year,"/",tests[i],"/ANALYSES/EQ/OP/RS",rs,"/X_Mock/RSSS"),"PreEquated"), showWarnings =F)
      dir.create(file.path(paste0("C:/TDOE/",admin_year,"/",tests[i],"/ANALYSES/EQ/OP/RS",rs,"/RSSS"),"PreEquated"), showWarnings =F)

      dir.create(file.path(paste0("C:/TDOE/",admin_year,"/",tests[i],"/ANALYSES/EQ/OP/RS",rs,"/X_Mock/RSSS"),"For_TDOE"), showWarnings =F)
      dir.create(file.path(paste0("C:/TDOE/",admin_year,"/",tests[i],"/ANALYSES/EQ/OP/RS",rs,"/RSSS"),"For_TDOE"), showWarnings =F)


      if (tests[i] != "ALT"){
        dir.create(file.path(paste0("C:/TDOE/",admin_year,"/",tests[i],"/ANALYSES/EQ/OP/RS",rs,"/X_Mock/Drift"),"STUIRT"), showWarnings =F)
        dir.create(file.path(paste0("C:/TDOE/",admin_year,"/",tests[i],"/ANALYSES/EQ/OP/RS",rs,"/Drift"),"STUIRT"), showWarnings =F)
      }


    }

    for (rs in 1:2){
      dir.create(file.path(paste0("C:/TDOE/",admin_year,"/",tests[i],"/ANALYSES/EQ/FT/RS",rs,"/"),"X_Mock"), showWarnings =F)

      if (tests[i] == "ALT"){
        for (f in 1: length(under_rs_alt)){
          dir.create(file.path(paste0("C:/TDOE/",admin_year,"/",tests[i],"/ANALYSES/EQ/FT/RS",rs,"/X_Mock/"),under_rs_alt[f]), showWarnings =F)
          dir.create(file.path(paste0("C:/TDOE/",admin_year,"/",tests[i],"/ANALYSES/EQ/FT/RS",rs,"/"),under_rs_alt[f]), showWarnings =F)
          }
        }else {
          for (f in 1: length(under_rs_eocach)){
          dir.create(file.path(paste0("C:/TDOE/",admin_year,"/",tests[i],"/ANALYSES/EQ/FT/RS",rs,"/X_Mock/"),under_rs_eocach[f]), showWarnings =F)
          dir.create(file.path(paste0("C:/TDOE/",admin_year,"/",tests[i],"/ANALYSES/EQ/FT/RS",rs,"/"),under_rs_eocach[f]), showWarnings =F)
          }
        }
      dir.create(file.path(paste0("C:/TDOE/",admin_year,"/",tests[i],"/ANALYSES/EQ/FT/RS",rs,"/X_Mock/RSSS"),"ePRS"), showWarnings =F)
      dir.create(file.path(paste0("C:/TDOE/",admin_year,"/",tests[i],"/ANALYSES/EQ/FT/RS",rs,"/RSSS"),"ePRS"), showWarnings =F)
    }

    if (tests[i] != "ALT"){
      dir.create(file.path(paste0("C:/TDOE/",admin_year,"/",tests[i],"/ANALYSES/EQ/FT/RS",rs,"/X_Mock/Drift"),"STUIRT"), showWarnings =F)
      dir.create(file.path(paste0("C:/TDOE/",admin_year,"/",tests[i],"/ANALYSES/EQ/FT/RS",rs,"/Drift"),"STUIRT"), showWarnings =F)
    }

  }
}

# test
# newadmin_setup("Spring24")

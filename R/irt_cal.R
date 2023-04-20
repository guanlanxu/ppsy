#' Run IRTpro/Winsteps calibration.
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
#' @return convergence: Convergence message.
#' @import haven readr data.table reshape2 tidyverse
#' @export


irt_cal <- function(admin,testcode,  mode, status,RSnum,mock,
                    dnu = "",dropuin = "",dropseq = "",flagDist = ""){
  # load libraries
  library(haven)
  library(readr)
  library(data.table)
  library(ppsy)
  library(reshape2)
  library(tidyverse)

  test = toupper(substr(admin,3,5))

  # if (tolower(substr(admin,6,8)) == "spr") {
  #   admin2 = paste0("Spring", substr(admin,9,10))   # admin2 is for path
  # } else {admin2 = paste0("Fall", substr(admin,9,10))}
  # path = paste0("C:/TDOE/", admin2,"/", toupper(test),"/")
  # setwd(path)

  if (test == "ALT") {  # use winstep function
    winsteps_fun(admin = admin,
                 testcode = testcode,
                 mode = mode,
                 status = status,
                 RSnum = RSnum,
                 mock = mock,
                 dnu = dnu,
                 dropuin = dropuin,
                 dropseq = dropseq,
                 flagDist = flagDist)
  } else if (test %in% c("EOC","ACH")){
    irtpro_fun(admin = admin,
               testcode = testcode,
               mode = mode,
               status = status,
               RSnum = RSnum,
               mock = mock,
               dnu = dnu,
               dropuin = dropuin,
               dropseq = dropseq,
               flagDist = flagDist)

  }
  return(list(freeirt = output,convergence = conv_file))
}

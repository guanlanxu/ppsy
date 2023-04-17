#' Convert Trait UIN to UIN with Ds.
#'
#' @param trait_uin Trait UIN with Long Strings.
#' @return uin_d: UIN with Ds.

trait2d <- function(trait_uin){
  library(data.table)
  uin = strsplit(trait_uin,"#")[[1]][1]
  trait = strsplit(trait_uin,"#")[[1]][2]

  d = ifelse(trait %like% "SCORE_TRAIT_FOD", "D1",
             ifelse(trait %like% "SCORE_TRAIT_LC", "D2",
                    ifelse(trait %like% "SCORE_TRAIT_FO", "D1",
                           ifelse(trait %like% "SCORE_TRAIT_DEV", "D2",
                                  ifelse(trait %like% "SCORE_TRAIT_LANG", "D3",
                                         ifelse(trait %like% "SCORE_TRAIT_CONV", "D4", NA))))))
  uin_d = paste(uin,d,sep = "_")

  return(uin_d)
}



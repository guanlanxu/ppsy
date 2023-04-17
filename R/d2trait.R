#' Convert UIN with Ds to Trait UIN.
#'
#' @param uin_d String.Trait UIN with Ds.
#' @param testcode String.
#' @return uin_trait: Trait UIN with Long Strings.
#'
#'
#'

d2trait <- function(uin_d, testcode){
  uin = toupper(strsplit(uin_d,"_")[[1]][1])
  d = toupper(strsplit(uin_d,"_")[[1]][2])

  if (toupper(testcode) == "EL3"){
    trait = ifelse(d == "D1", "SCORE_TRAIT_FOD",
                   ifelse(d == "D2", "SCORE_TRAIT_LC",NA))
  } else if (toupper(testcode) %in% c(paste0("EL",3:8), paste0("EN",1:2))){
    trait = ifelse(d == "D1", "SCORE_TRAIT_FO",
                   ifelse(d == "D2", "SCORE_TRAIT_DEV",
                          ifelse(d == "D3", "SCORE_TRAIT_LANG",
                                 ifelse(d == "D4", "SCORE_TRAIT_CONV", NA))))
  }

  uin_trait = paste(uin,trait,sep = "#")

  return(uin_trait)
}

#' Generate special headings for score tables.
#'
#' @param admin String.
#' @param what Type of score tables.
#' @param battery 1 or 0.
#' @return heading: headings that specified in `what` argument.

get_eprs_heading <- function(admin, what,battery){

  if (tolower(what) != "aberrant"){
    if(battery == 0){
      if (tolower(what) == "pl3b"){
        heading <- rbind(c("*Account","TN","","","","","",""),
                         c("*Admin Code",admin,"","","","","",""),
                         c("*Table Type","PL3B","","","","","",""),
                         c("*Score Value", "Lower Bound","","","","","",""),
                         c("#Comments","","","","","","",""),
                         c("@","Form","Test Number","Publish Format","Objective","Proficiency Level","Lower Bound","Upper Bound"))
      } else if (tolower(what) == "pl4b"){

        #non-battery
        heading <- rbind(c("*Account","TN","","","","","",""),
                         c("*Admin Code",admin,"","","","","",""),
                         c("*Table Type","PL4B","","","","","",""),
                         c("*Score Value", "Lower Bound","","","","","",""),
                         c("#Comments","","","","","","",""),
                         c("@","Form","Test Number","Publish Format","Objective","Proficiency Level","Lower Bound","Upper Bound"))

      } else if (tolower(what) == "semb"){
        # non-battery
        heading <- rbind(c("*Account","TN","","","","","",""),
                         c("*Admin Code",admin,"","","","","",""),
                         c("*Table Type","SEMB","","","","","",""),
                         c("*Score Value", "Lower Bound","","","","","",""),
                         c("#Comments","","","","","","",""),
                         c("@","Form","Test Number","Publish Format","Objective","SEM","Lower Bound","Upper Bound"))
      }  else if (tolower(what) == "ssb"){

        # non-battery
        heading <- rbind(c("*Account","TN","","","","","",""),
                         c("*Admin Code",admin,"","","","","",""),
                         c("*Table Type","SSB","","","","","",""),
                         c("*Score Value", "Lower Bound","","","","","",""),
                         c("#Comments","","","","","","",""),
                         c("@","Form","Test Number","Publish Format","Objective","Scale Score","Lower Bound","Upper Bound"))

      } else if (tolower(what) == "irtb"){

        # non-battery
        heading <- rbind(c("*Account","TN","","","","","",""),
                         c("*Admin Code",admin,"","","","","",""),
                         c("*Table Type","IRT-TB","","","","","",""),
                         c("*Score Value", "Lower Bound","","","","","",""),
                         c("#Comments","Summative RS to Theta","","","","","",""),
                         c("@","Form","Test Number","Publish Format","Objective","IRT Theta","Lower Bound","Upper Bound"))

      }
    } else if (battery == 1){
      heading[6,2] = "Battery"
    }
  } else if (tolower(what) == "aberrant"){

      # non-battery
      heading <- c("assetuin",	"formid",	"admin",	"seqno",	"item_status",	"mode",	"testcode",	"max_points",
                   "n_total",	"irt_flag",	"irt_model",	"irt_a",	"irt_a_se",	"irt_b",	"irt_b_se",
                   "irt_c",	"irt_c_se",	"irt_step1",	"irt_step1_se",	"irt_step2",	"irt_step2_se",	"irt_step3",
                   "irt_step3_se",	"irt_step4",	"irt_step4_se",	"irt_step5",	"irt_step5_se",
                   "irt_step6",	"irt_step6_se")
      heading = t(as.data.frame(heading))

    }

  heading = as.data.frame(heading)
  return(heading)
}

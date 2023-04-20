#' Compute distribution abnormal.
#'
#' @param score_vec A vector of scores.
#' @param score_cat Score category. E.g., A binary item would have score_cat = 2 (1,0).
#' @param max_point Max point of an item. E.g.,  A binary item would have max_point = 1.
#' @param N_TOTAL Total N counts.

#' @return um_dist: "U" means "U-shaped" distribution. "M" means missing score category .
#' @export
um_dist <- function(score_vec, score_cat, max_point, N_TOTAL){

  freq = table(score_vec)

  # Check for U
   if (score_cat == 2){
    u_dist = ""
  } else if(score_cat > 2){
      if (any(freq[2:(score_cat-1)] <= (1/(max_point*5)*N_TOTAL))) {
        u_dist = "U"
      } else {
        u_dist = ""
      }
  }

  # check for M (missing)
  if (length(freq) < score_cat){
    um_dist = paste("M",u_dist,sep = ",")
  }else {
    um_dist = u_dist
  }

  return(um_dist)
}



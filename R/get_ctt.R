#' Compute CTT statistics.
#'
#' @param irt_par A table of item parameters.
#' @param idm IDM.

#' @return myctt: A table of CTT statistics.


get_ctt <- function(irt_par,idm){

  #
  colnames(irt_par) = toupper(colnames(irt_par))
  idm = idm[,paste0("score_",irt_par$IDMSEQ)]
  idm = apply(idm, 2, as.numeric)
  idm[idm == -1] <- NA

  # item total correlation

  raw = rowSums(idm[,paste0("score_",irt_par[which(irt_par$ITEM_STATUS != "FT"),]$IDMSEQ)], na.rm = T)

  irt_par$ITEM_TOTAL_CORRELATION = apply(idm, 2, function (x) {cor(x, (raw-x), use="complete.obs")})   # not the spoiled item, but other items should in.

  # average item score
  irt_par$AVERAGE_ITEM_SCORE = colMeans(idm,na.rm = T)

  # observed max points and score cat
  irt_par$score_cat = as.numeric(irt_par$MAX_POINTS)+1
  irt_par$score_cat_obs = apply(idm, 2, function (x) {length(table(x))})
  irt_par$max_points_obs = apply(idm, 2, function (x) {max(as.numeric(names(table(x))))})

  # score distribution flag.
  irt_par$N_TOTAL = apply(idm, 2, function (x) {sum(length(which(x != -1)))})
  irt_par$flag_Scr_Dist <- c(NA)
  for (i in 1: ncol(idm)){
    irt_par$flag_Scr_Dist[i] = um_dist(score_vec = idm[,i],
                                            score_cat = irt_par$score_cat[i],
                                            max_point = irt_par$MAX_POINTS[i],
                                            N_TOTAL  =  irt_par$N_TOTAL[i])
  }

  myctt = irt_par[,c("IDMSEQ","UIN","ITEM_TOTAL_CORRELATION","AVERAGE_ITEM_SCORE","score_cat","score_cat_obs","max_points_obs","N_TOTAL","flag_Scr_Dist")]
  return(myctt)
}

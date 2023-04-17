
#' Compute true points for each points for an item. This function is used in conjuction with model fit.
#'
#' @param theta.vec.short A vector of thetas.
#' @param Ncount A vector of counts.
#' @param item 1,2,3...
#' @param freeirt A table contains irt parameters.

#' @return N_P_item: A list that contains counts of each points for each theta values for the specified item.




G2_int_adj <- function(theta.vec.short, Ncount,item, freeirt = freeirt) {

  theta.vec = theta.vec.short


  N_P_item = list(
    N_P_0 = rep(NA,length(Ncount)),
    N_P_1 = rep(NA,length(Ncount)),
    N_P_2 = rep(NA,length(Ncount)),
    N_P_3 = rep(NA,length(Ncount)),
    N_P_4 = rep(NA,length(Ncount)),
    N_P_5 = rep(NA,length(Ncount))
  )

  ppi = irt_fun(model = freeirt$MODEL[item],
                theta.vec = theta.vec.short,
                D = 1.702, a = as.numeric(freeirt[item,"UN_IRT_A"]),
                b = as.numeric(freeirt[item,"UN_IRT_B"]),
                c = ifelse(is.na(as.numeric(freeirt[item,"UN_IRT_C"])),0,as.numeric(freeirt[item,"UN_IRT_C"])),  # this is for 2PL, code c as 0
                steps = (as.numeric(freeirt[item,paste0("UN_IRT_STEP",1:5)]) ))
  N_P_item[[1]] =  ppi$pi$p0 * as.vector(Ncount[,item])
  N_P_item[[2]] =  ppi$pi$p1 * as.vector(Ncount[,item])
  N_P_item[[3]] =  ppi$pi$p2 * as.vector(Ncount[,item])
  N_P_item[[4]] =  ppi$pi$p3 * as.vector(Ncount[,item])
  N_P_item[[5]] =  ppi$pi$p4 * as.vector(Ncount[,item])
  N_P_item[[6]] =  ppi$pi$p5 * as.vector(Ncount[,item])



  return(N_P_item)
}

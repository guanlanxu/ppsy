
#' get D2 based on base and equated pars.
#'
#' @param model "rasch","2PL","3PL", or "GPC".
#' @param theta.vec A vector of thetas.
#' @param D 1 or 1.702.
#' @param base_a Base A parameter.
#' @param base_b Base B parameter.
#' @param base_c Base C parameter.
#' @param base_steps A vector of Base steps.
#' @param equated_a Equated A parameter .
#' @param equated_b Equated B parameter.
#' @param equated_c Equated C parameter.
#' @param equated_steps A vector of equated steps.
#' @param uin item UIN.
#' @param get_plot T or F.
#' @return d2, wrmsd, and drift_plot.


get_d2 <- function(model, theta.vec,
                   D,
                   base_a,base_b,base_c,base_steps,
                   equated_a,equated_b,equated_c, equated_steps,

                   uin,get_plot = T){
  library(ppsy)
  library(ggplot2)
  library(data.table)

  theta.vec = theta.vec
  # weights = c( 0.000027 ,0.000058 ,0.000122 ,0.000246 ,0.000477 ,0.000886 ,0.001583 ,0.002717
  #              ,0.004479 ,0.007095 ,0.010799 ,0.015791 ,0.022185 ,0.029947 ,0.038839 ,0.048396
  #              ,0.057941 ,0.066648 ,0.073657 ,0.078212 ,0.079792 ,0.078212 ,0.073657 ,0.066648
  #              ,0.057941 ,0.048396 ,0.038839 ,0.029947 ,0.022185 ,0.015791 ,0.010799 ,0.007095
  #              ,0.004479 ,0.002717 ,0.001583 ,0.000886 ,0.000477 ,0.000246 ,0.000122 ,0.000058 ,0.000027)

  weights = (dnorm(seq(-4, 4, length = 41)))
  weights <- weights / sum(weights)
  weights = round_sas(weights,6)

  p_base = irt_fun(model,theta.vec,1.702,base_a,base_b,base_c,base_steps)
   p_equated = irt_fun(model,theta.vec,1.702,equated_a,equated_b,equated_c, equated_steps)
   d2=sum((p_base$p - p_equated$p)^2*weights)
   wrmsd = sqrt(d2)

   base_steps = na.omit(base_steps)
   base_steps = base_steps[which(base_steps!= "")]

   if(length(base_steps) == 0){
     MAX_POINTS =1
   } else {MAX_POINTS = length(base_steps)}

   for.plot = data.frame(theta = rep(theta.vec,(MAX_POINTS+1) *2),
                           probability = c(stack((as.data.frame(rbindlist(list(p_base$pi))))[,1:(MAX_POINTS+1)])[,1],
                                           stack((as.data.frame(rbindlist(list(p_equated$pi))))[,1:(MAX_POINTS+1)])[,1]),
                           grp = c(rep("Base",(MAX_POINTS+1)*length(theta.vec)),
                                   rep("Equated",(MAX_POINTS+1)*length(theta.vec))),
                           pt = sort(rep(0:MAX_POINTS,length(theta.vec))))


     p1 = ggplot()+
       geom_line(data = for.plot[which(for.plot$pt== 0),],aes(x = theta, y = probability,linetype=grp,color = grp),linewidth = 0.7)+
       geom_line(data = for.plot[which(for.plot$pt== 1),],aes(x = theta, y = probability,linetype=grp,color = grp),linewidth = 0.7)+
       geom_text(data = for.plot[which(for.plot$pt== 0 & for.plot$theta %in% seq(-4,4,1)),],aes(x = theta, y = probability,label  = pt,color = grp),show.legend = F)+
       geom_text(data = for.plot[which(for.plot$pt== 1 & for.plot$theta %in% seq(-4,4,1)),],aes(x = theta, y = probability,label  = pt,color = grp),show.legend = F)+
       theme(legend.position="bottom",legend.title = element_blank())+
       labs(title = "Anchor vs. Equated Item ICCs",
            subtitle = paste0("uin = ",uin,  #"; IDMseq = ", mydrift_small$IRTLOC[i],
                              "; d2 = ", d2,
                              "; WRMSD = ", wrmsd,
                              "; chng_A = ", as.numeric(equated_a) - as.numeric(base_a),
                              "; chng_B = ", as.numeric(equated_b) - as.numeric(base_b),
                              "; chng_C = ", as.numeric(equated_c) - as.numeric(base_c)
                              ))+

       theme(#plot.title = element_text(size = 12, face = "bold"),
         plot.subtitle = element_text(size = 6)
  )+
       scale_y_continuous(limits = c(0,1),breaks = c(0,0.25,0.5,0.75,1))+
       scale_x_continuous(limits = c(-4,4),breaks = c(-4,-3,-2,-1,0,1,2,3,4))

     if (MAX_POINTS == 1){
       drift_plot= p1

     } else if (MAX_POINTS == 2){

       drift_plot = p1 +
         geom_line(data = for.plot[which(for.plot$pt== 2),],aes(x = theta, y = probability,linetype=grp,color = grp),linewidth = 0.7)+
         geom_text(data = for.plot[which(for.plot$pt== 2 & for.plot$theta %in% seq(-4,4,1)),],aes(x = theta, y = probability,label  = pt,color = grp),show.legend = F)

     } else if (MAX_POINTS == 3){
       drift_plot=  p1+
         geom_line(data = for.plot[which(for.plot$pt== 2),],aes(x = theta, y = probability,linetype=grp,color = grp),linewidth = 0.7)+
         geom_text(data = for.plot[which(for.plot$pt== 2 & for.plot$theta %in% seq(-4,4,1)),],aes(x = theta, y = probability,label  = pt,color = grp),show.legend = F)+
         geom_line(data = for.plot[which(for.plot$pt== 3),],aes(x = theta, y = probability,linetype=grp,color = grp),linewidth = 0.7)+
         geom_text(data = for.plot[which(for.plot$pt== 3 & for.plot$theta %in% seq(-4,4,1)),],aes(x = theta, y = probability,label  = pt,color = grp),show.legend = F)

     }else if (MAX_POINTS== 4){
       drift_plot=  p1+
         geom_line(data = for.plot[which(for.plot$pt== 2),],aes(x = theta, y = probability,linetype=grp,color = grp),linewidth = 0.7)+
         geom_text(data = for.plot[which(for.plot$pt== 2 & for.plot$theta %in% seq(-4,4,1)),],aes(x = theta, y = probability,label  = pt,color = grp),show.legend = F)+
         geom_line(data = for.plot[which(for.plot$pt== 3),],aes(x = theta, y = probability,linetype=grp,color = grp),linewidth = 0.7)+
         geom_text(data = for.plot[which(for.plot$pt== 3 & for.plot$theta %in% seq(-4,4,1)),],aes(x = theta, y = probability,label  = pt,color = grp),show.legend = F)+

       geom_line(data = for.plot[which(for.plot$pt== 4),],aes(x = theta, y = probability,linetype=grp,color = grp),linewidth = 0.7)+
         geom_text(data = for.plot[which(for.plot$pt== 4 & for.plot$theta %in% seq(-4,4,1)),],aes(x = theta, y = probability,label  = pt,color = grp),show.legend = F)

     }else if (MAX_POINTS == 5){
       drift_plot=  p1+
         geom_line(data = for.plot[which(for.plot$pt== 2),],aes(x = theta, y = probability,linetype=grp,color = grp),linewidth = 0.7)+
         geom_text(data = for.plot[which(for.plot$pt== 2 & for.plot$theta %in% seq(-4,4,1)),],aes(x = theta, y = probability,label  = pt,color = grp),show.legend = F)+
         geom_line(data = for.plot[which(for.plot$pt== 3),],aes(x = theta, y = probability,linetype=grp,color = grp),linewidth = 0.7)+
         geom_text(data = for.plot[which(for.plot$pt== 3 & for.plot$theta %in% seq(-4,4,1)),],aes(x = theta, y = probability,label  = pt,color = grp),show.legend = F)+

         geom_line(data = for.plot[which(for.plot$pt== 4),],aes(x = theta, y = probability,linetype=grp,color = grp),linewidth = 0.7)+
         geom_text(data = for.plot[which(for.plot$pt== 4 & for.plot$theta %in% seq(-4,4,1)),],aes(x = theta, y = probability,label  = pt,color = grp),show.legend = F)+

         geom_line(data = for.plot[which(for.plot$pt== 5),],aes(x = theta, y = probability,linetype=grp,color = grp),linewidth = 0.7)+
         geom_text(data = for.plot[which(for.plot$pt== 5 & for.plot$theta %in% seq(-4,4,1)),],aes(x = theta, y = probability,label  = pt,color = grp),show.legend = F)

     }


   return(list(d2=d2,wrmsd = wrmsd, drift_plot = drift_plot))
}


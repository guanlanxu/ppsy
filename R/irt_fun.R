
#' Compute true score and information based on IRT parameters.
#'
#' @param theta.vec A vector of thetas.
#' @param a A parameter
#' @param b B parameter.
#' @param c C parameter.
#' @param s1 Step1 parameter.
#' @param s2 Step2 parameter.
#' @param s3 Step3 parameter.
#' @param s4 Step4 parameter.
#' @param s5 Step5 parameter.
#' @param D 1.702 or 1.
#' @param steps A vector of steps parameters.
#' @param model String. "Rasch", "2PL", "3PL" or "GPC".
#' @return p: true score.
#' @return info: information
#' @return pi: A list of probabilities of getting ith points.

p.fun <- function(theta.vec,b) {
  p = exp(theta.vec-b)/(1+exp(theta.vec-b))
  # if (is.na(p) & theta.vec-b > 500 ) {p = }
  q = 1-p
  info = p*q
  return (list(p=p,info = info, pi = list(p0 = q, p1 = p,p2 = NA,p3 = NA, p4 = NA, p5 = NA)))
}

## 2PL and 3PL
twopl.fun <-  function(theta.vec,D = 1.702, a,b){
  p = 0+(1-0)/(1+exp(-D*a*(theta.vec-b)))
  q=1-p
  info = D^2*a^2*q*p
  return(list(p=p,info=info, pi = list(p0 = q, p1 = p,p2 = NA,p3 = NA, p4 = NA, p5 = NA)))
}

threepl.fun <-  function(theta.vec,D = 1.702, a,b,c){
  p = c+(1-c)/(1+exp(-D*a*(theta.vec-b)))
  q=1-p
  info = D^2*a^2*(q/p)*((p-c)/(1-c))^2
  return(list(p=p,info=info, pi = list(p0 = q, p1 = p,p2 = NA,p3 = NA, p4 = NA, p5 = NA)))
}
# gpc
gpc.fun2 <- function(theta.vec,D = 1.702,a,b,s1,s2){
  q0 = 1
  q1 = exp(D*a*(theta.vec-b+s1))
  q2 = exp(D*a*(theta.vec-b+s1)+D*a*(theta.vec-b+s2))
  SS = q0+q1+q2
  p0 = q0/SS
  p1 = q1/SS
  p2 = q2/SS
  p = 0*p0+ 1*p1+2*p2
  info = D^2*a^2*((p1+2^2*p2)-(p1+2*p2)^2)
  return(list(p=p,info=info, pi = list(p0 = p0,p1 = p1,p2 = p2,p3 = NA, p4 = NA, p5 = NA)))
}

gpc.fun3 <- function(theta.vec,D = 1.702,a,b,s1,s2,s3){
  q0 = 1
  q1 = exp(D*a*(theta.vec-b+s1))
  q2 = exp(D*a*(theta.vec-b+s1)+D*a*(theta.vec-b+s2))
  q3 = exp(D*a*(theta.vec-b+s1)+D*a*(theta.vec-b+s2)+D*a*(theta.vec-b+s3))
  SS = q0+q1+q2 + q3
  p0 = q0/SS
  p1 = q1/SS
  p2 = q2/SS
  p3 = q3/SS
  p =0*p0+ 1*p1+2*p2 + 3*p3
  info = D^2*a^2*((p1+2^2*p2+3^2*p3)-(p1+2*p2+3*p3)^2)
  return(list(p=p,info=info, pi = list(p0 = p0,p1 = p1,p2 = p2,p3 = p3,p4 = NA, p5 = NA)))
}

gpc.fun4 <- function(theta.vec,D = 1.702,a,b,s1,s2,s3, s4){
  q0 = 1
  q1 = exp(D*a*(theta.vec-b+s1))
  q2 = exp(D*a*(theta.vec-b+s1)+D*a*(theta.vec-b+s2))
  q3 = exp(D*a*(theta.vec-b+s1)+D*a*(theta.vec-b+s2)+D*a*(theta.vec-b+s3))
  q4 = exp(D*a*(theta.vec-b+s1)+D*a*(theta.vec-b+s2)+D*a*(theta.vec-b+s3)+D*a*(theta.vec-b+s4))
  SS = q0+q1+q2 + q3 + q4
  p0 = q0/SS
  p1 = q1/SS
  p2 = q2/SS
  p3 = q3/SS
  p4 = q4/SS
  p = 0*p0+ 1*p1+2*p2 + 3*p3 + 4*p4
  info = D^2*a^2*((p1+2^2*p2+3^2*p3+4^2*p4)-(p1+2*p2+3*p3+4*p4)^2)
  return(list(p=p,info=info,pi = list(p0 = p0,p1 = p1,p2 = p2,p3 = p3, p4 = p4,p5 = NA)))
}

gpc.fun5 <- function(theta.vec,D = 1.702,a,b,s1,s2,s3, s4,s5){
  q0 = 1
  q1 = exp(D*a*(theta.vec-b+s1))
  q2 = exp(D*a*(theta.vec-b+s1)+D*a*(theta.vec-b+s2))
  q3 = exp(D*a*(theta.vec-b+s1)+D*a*(theta.vec-b+s2)+D*a*(theta.vec-b+s3))
  q4 = exp(D*a*(theta.vec-b+s1)+D*a*(theta.vec-b+s2)+D*a*(theta.vec-b+s3)+D*a*(theta.vec-b+s4))
  q5 = exp(D*a*(theta.vec-b+s1)+D*a*(theta.vec-b+s2)+D*a*(theta.vec-b+s3)+D*a*(theta.vec-b+s4)+D*a*(theta.vec-b+s5))
  SS = q0+q1+q2 + q3 + q4 + q5
  p0 = q0/SS
  p1 = q1/SS
  p2 = q2/SS
  p3 = q3/SS
  p4 = q4/SS
  p5 = q5/SS
  p = 0*p0+ 1*p1+2*p2 + 3*p3 + 4*p4+ 5*p5
  info = D^2*a^2*((p1+2^2*p2+3^2*p3+4^2*p4+5^2*p5)-(p1+2*p2+3*p3+4*p4+5*p5)^2)
  return(list(p=p,info=info,pi = list(p0 = p0,p1 = p1,p2 = p2,p3 = p3, p4 = p4,p5 = p5)))
}


irt_fun <- function(model,theta.vec, D, a = 1, b, c = 0, steps) {

  steps = as.numeric(steps)
  steps = na.omit(steps)

  if (is.null(theta.vec)){
    print("theta.vec is not supplied!" )
  } else if ( any(! is.numeric(theta.vec))){
    theta.vec =  sapply(theta.vec, is.numeric)
  } else if (! tolower(model) %in% c("rasch","2pl","3pl","gpc")){
    print("model is not in 'rasch','2pl','3pl', or 'gpc'!")
  } else if (tolower(model) == 'gpc' & length(na.omit(steps)) <2){
    print("steps has to be a vector for gpc!")
  } else {


    if (tolower(model) == "rasch"){
      irt.res = p.fun(theta.vec = as.numeric(theta.vec),b = as.numeric(b ))  # there is no D in this function
    } else if (tolower(model)  %in% c("2pl","3pl")){
      irt.res = threepl.fun(theta.vec = as.numeric(theta.vec), D = as.numeric(D), a = as.numeric(a), b = as.numeric(b), c = ifelse(is.na(as.numeric(c)),0,as.numeric(c)))
    } else if (tolower(model)  %in% c("gpc")) {
      steps = steps[! is.na(steps)]
      if (length(steps) == 2){
        irt.res = gpc.fun2(theta.vec = as.numeric(theta.vec),D = as.numeric(D), a = as.numeric(a), b = as.numeric(b), s1 = as.numeric(steps[1]), s2 =as.numeric(steps[2]))
      } else if (length(steps) == 3){
        irt.res = gpc.fun3(theta.vec = as.numeric(theta.vec),D = as.numeric(D), a = as.numeric(a), b = as.numeric(b), s1 = as.numeric(steps[1]), s2 =as.numeric(steps[2]), s3 = as.numeric(steps[3]))
      } else if (length(steps) == 4){
        irt.res = gpc.fun4(theta.vec = as.numeric(theta.vec),D = as.numeric(D), a = as.numeric(a), b = as.numeric(b), s1 = as.numeric(steps[1]), s2 =as.numeric(steps[2]), s3 = as.numeric(steps[3]), s4 = as.numeric(steps[4]))
      } else if (length(steps) == 5){
        irt.res = gpc.fun5(theta.vec = as.numeric(theta.vec),D = as.numeric(D), a = as.numeric(a), b = as.numeric(b), s1 = as.numeric(steps[1]), s2 =as.numeric(steps[2]), s3 = as.numeric(steps[3]), s4 = as.numeric(steps[4]), s5 = as.numeric(steps[5]))
      }
    }

    p = irt.res$p
    pi = irt.res$pi
    info = irt.res$info
    return(list(p=p,info=info,pi=pi))
  }

}



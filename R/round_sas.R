
#' function to round up at 5 to mimic SAS results

#' @param x number.
#' @param n integer. Which decimal place that want to round to.

#' @return A rounded number

round_sas <- function( x, n )
{
  posneg <- sign( x )
  z <- abs( x ) * 10^n
  z <- z + 0.5 + 10^-14
  z <- trunc( z )
  z <- z / 10^n
  z * posneg
}

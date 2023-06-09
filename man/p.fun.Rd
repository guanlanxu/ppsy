% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/irt_fun.R
\name{p.fun}
\alias{p.fun}
\title{Compute true score and information based on IRT parameters.}
\usage{
p.fun(theta.vec, b)
}
\arguments{
\item{theta.vec}{A vector of thetas.}

\item{b}{B parameter.}

\item{a}{A parameter}

\item{c}{C parameter.}

\item{s1}{Step1 parameter.}

\item{s2}{Step2 parameter.}

\item{s3}{Step3 parameter.}

\item{s4}{Step4 parameter.}

\item{s5}{Step5 parameter.}

\item{D}{1.702 or 1.}

\item{steps}{A vector of steps parameters.}

\item{model}{String. "Rasch", "2PL", "3PL" or "GPC".}
}
\value{
p: true score.

info: information

pi: A list of probabilities of getting ith points.
}
\description{
Compute true score and information based on IRT parameters.
}

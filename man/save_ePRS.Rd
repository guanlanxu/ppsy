% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/save_ePRS.R
\name{save_ePRS}
\alias{save_ePRS}
\title{Save score tables to local drive.}
\usage{
save_ePRS(admin, mock, RSnum, testcode_list, mode_list, battery_list, pre_post)
}
\arguments{
\item{admin}{String. Administration.}

\item{mock}{T or F.}

\item{RSnum}{String. RS1-3.}

\item{testcode_list}{A vector of testcodes.}

\item{mode_list}{A vector of mode.}

\item{battery_list}{A vector of 1s or 0s.}

\item{pre_post}{String.pre or post.}
}
\value{
Files will be written to the local folder. No objects will be returned.
}
\description{
Save score tables to local drive.
}

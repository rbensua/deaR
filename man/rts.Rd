% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rts.R
\name{rts}
\alias{rts}
\title{RTS}
\usage{
rts(deamodel,
    thr = 1e-4)
}
\arguments{
\item{deamodel}{Object of class dea obtained with some of the dea functions.}

\item{thr}{Threshold for the tolerance for considering something = 1. Defults to 1e-4.}
}
\description{
Extract the returns to scale.
}
\examples{
 data("Coll_Blasco_2006")
 data_example <- read_data(Coll_Blasco_2006,
                           ni = 2, 
                           no = 2)
 result <- model_basic(data_example, 
                       orientation = "io", 
                       rts  ="crs")
 rts(result)
 
}
\author{
\strong{Vicente Coll-Serrano} (\email{vicente.coll@uv.es}).
\emph{Quantitative Methods for Measuring Culture (MC2). Applied Economics.}

\strong{Vicente Bolós} (\email{vicente.bolos@uv.es}).
\emph{Department of Business Mathematics}

\strong{Rafael Benítez} (\email{rafael.suarez@uv.es}).
\emph{Department of Business Mathematics}

University of Valencia (Spain)
}

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_XN5statevar_xno.R
\name{get_XN5statevar_xno}
\alias{get_XN5statevar_xno}
\title{Get State Variable Results}
\usage{
get_XN5statevar_xno(
  mylocations,
  state.vars = "output.Plant.Development Stage.BBCH Stage [-]",
  query.return = FALSE
)
}
\arguments{
\item{mylocations}{a list with two named elements. The names matter.
\tabular{lll}{
   \code{my.xno.path}\tab{character specifying the path where the xno files should be searched in. }\cr
   \code{my.output.path}\tab{character specifying the folder where the results should be saved. }\cr
   }}

\item{state.vars}{a character specifying the name of the state variable(s) which should be collected. You might want to
use the function \link[XN5setup]{get_columnNames_xno} to verify if these exist.}

\item{query.return}{default to \code{FALSE}. Specifying \code{TRUE} will result in the function returning the collected results.}
}
\value{
a data.table with the state variable results specified in \code{state.vars} and searched for in location \code{my.xno.path}.
}
\description{
By specifying \code{mylocations$my.xno.path} the function internally will look for ALL .xno files
and search for results of the state variables of interest
}
\examples{
\dontrun{
# initialise list
mylocations <- list()
setwd("C:/Projects/ExpertN/expertn513d/built")
# assign paths:
# 1. location of xno.paths
mylocations$my.xno.path    <- "./cfg/replicateECv10/"
# 2. location of output
mylocations$my.output.path <- "C:/Projects/ExpertN/myresults"

colnam <- get_columnNames_xno("./cfg/replicateECv10/output/replicateECv10_0_0_0_0.xno")
state.vars <- colnam[c(210,197)]

}
}

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_gridDim.R
\name{get_gridDim}
\alias{get_gridDim}
\title{Recuperate the start and end time of a simulation based on available dates in weather files}
\usage{
get_gridDim(x)
}
\arguments{
\item{x}{a list with either character vector of names, or the 4D size. Number of characters may be variable. May be integer
to specify the dimension length.
\itemize{
 \item{weather_NAMES  }{\bold{z-coordinate} name(s) of the weather data file(s).}
 \item{soil_unit_NAMES}{\bold{x-coordinate} name(s) of the spatial unit.}
 \item{stoch_NAMES    }{\bold{y-coordinate} name(s) of the stochastic units.}
 \item{CR_NAMES       }{\bold{tile-coordinate} name(s) of the tile elements/crop rotations.}
 }}
}
\value{
x a list with elements in \code{x} and lengths of vectors.
}
\description{
The weather file has to be in format required by Expert-N. Daily values are given in the format "YYYY-MM-DD"
The function can be used to automatically deduce the maximum possible simulation window, given different weather files.
}
\examples{
\dontrun{
# no example sensible, here.}
}
\author{
Tobias KD Weber , \email{tobias.weber@uni-hohenheim.de}
}

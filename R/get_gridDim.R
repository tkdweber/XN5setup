#' Recuperate the start and end time of a simulation based on available dates in weather files
#'
#'@description The weather file has to be in format required by Expert-N. Daily values are given in the format "YYYY-MM-DD"
#'The function can be used to automatically deduce the maximum possible simulation window, given different weather files.
#'
#'
#' @param x a list with either character vector of names, or the 4D size. Number of characters may be variable. May be integer
#' to specify the dimension length.
#' \itemize{
#'  \item{weather_NAMES  }{\bold{z-coordinate} name(s) of the weather data file(s).}
#'  \item{soil_unit_NAMES}{\bold{x-coordinate} name(s) of the spatial unit.}
#'  \item{stoch_NAMES    }{\bold{y-coordinate} name(s) of the stochastic units.}
#'  \item{CR_NAMES       }{\bold{tile-coordinate} name(s) of the tile elements/crop rotations.}
#'  }
#' @return x a list with elements in \code{x} and lengths of vectors.
#'
#' @author Tobias KD Weber , \email{tobias.weber@uni-hohenheim.de}
#'
#' @examples
#' \dontrun{
#' # no example sensible, here.}
#' @export
#'

get_gridDim  <- function(x){

 if(sum(c(exists("weather_NAMES"  , where=x),
          exists("soil_unit_NAMES", where=x),
          exists("stoch_NAMES"    , where=x),
          exists("CR_NAMES"       , where=x)
          )) == 0){
  stop("Error: x does not contain one element out of weather_NAMES, soil_unit_NAMES, stoch_NAMES, CR_NAMES")}

  count <- list()

  count$weather_NAMES   <- if(c(exists("weather_NAMES", where = x))){
                           ifelse(is.integer(x$weather_NAMES  ), x$weather_NAMES, length(x$weather_NAMES  )) # coordinate  z (grid number)number)
}else{1}

  count$soil_unit_NAMES <- ifelse(is.integer(x$soil_unit_NAMES), x$soil_unit_NAMES, length(x$soil_unit_NAMES)) # coordinate  x
  count$stoch_NAMES     <- ifelse(is.integer(x$stoch_NAMES    ), x$stoch_NAMES    , length(x$stoch_NAMES    )) # coordinate  y
  count$CR_NAMES        <- ifelse(is.integer(x$CR_NAMES       ), x$CR_NAMES       , length(x$CR_NAMES       )) # coordinate  tile elements

 return(count)
}






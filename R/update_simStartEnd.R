#' Update the start and end simulation dates.
#'
#' @description The start and end dates of the simulation experiment can be given. If the climate data files
#' do not cover that period, these will be update. Similarly, if the climate data files contain data
#' beyond the simulation start and end date, the simulation experiment will be done for the time specified.
#' If no simulation start and end dates were specified, these will be set to the first and last timestamp
#' in the climate data, respectively.#'
#'
#' @param x The list with named elements specifying the settings of the simulation experiment.
#'
#'
#' @return x a list with updated elements.
#'
#' @author Tobias KD Weber , \email{tobias.weber@uni-hohenheim.de}
#'
#' @examples
#' \dontrun{
#' }
#' @export
#'

update_simStartEnd <- function(x){

 # x list settings

 # checks
 if(!exists("weather"  , where = x)){stop("weather not an element of x")}
 if(!exists("sim_start", where = x$weather)){stop("sim_start not an element of x")}
 if(!exists("sim_end"  , where = x$weather)){stop("sim_end not an element of x")}

 # update_simStartEnd
 if(is.null(x$sim_start)){
  x$sim_start <- x$weather$sim_start
 }
 if(is.null(x$sim_end)){
  x$sim_end <- x$weather$sim_end
 }

 if(!is.null(x$sim_start)){
  if(lubridate::as_date(x$sim_start) < lubridate::as_date(x$weather$sim_start)){
   x$sim_start <- x$weather$sim_start
  }
 }

 if(!is.null(x$sim_end)){
  if(lubridate::as_date(x$sim_end) < lubridate::as_date(x$weather$sim_end)){
   x$sim_end <- x$weather$sim_end
  }
 }
 # update class
 x$sim_start  <- lubridate::as_date(x$sim_start)
 x$sim_end    <- lubridate::as_date(x$sim_end)
 return(x)
}


#' Update application times of the DEFAULT fertiliser and tillage
#'
#' A simple function used to update DEFAULT dates based on the information given in croprotation, defined by the list returned from croprotationDef
#'
#' @param x a list returned from croprotationDef. If not tinkered with, there is a chornological order in the list which is highly (!) adivised.
#' @param DT (default) DT_min, DT_org, DT_manag which have to contain a column named "date" with the dates of activity as a characrter in "YYYY-MM-DD" format.
#'
#' @details Used in the functions schaaf_managDef and in fertilisationDef
#'
#' @return List with time updated, crop specific DEFAULTs (fertilisation or tillage)
#'
#' @author Tobias KD Weber , \email{tobias.weber@uni-hohenheim.de}
#'
#' @examples
#' ## example 1
#' \dontrun{
#' }
#' @export

update_time <- function(x, DT) {
 variety <- NULL
 # check x
 if(!is.list(x)){ stop("argument x should be a list" )}
 # check DT
 if(!data.table::is.data.table(DT)){ stop("argument DT should be a data.table" )}
 # check if DT has a column with name "date"
 if(!"date"%in%colnames(DT)){ stop("argument DT should contain a column named date" )}

 lapply(x, function(x) {

  # subset DT according to variety
  dt_min <- DT[variety == x$variety]

  # DEFAULT date
  isdat <- as.Date(dt_min$date)

  # SOWDATE of crop in rotation
  sdat <- lubridate::year(x$sow_date)

  # difference in time between thre DEFAULT date and date of the crop rotation
  dtime <- sdat - lubridate::year(isdat)

  # UPDATE
  dt_min$date <- isdat + lubridate::years(dtime) + years(year(isdat) - 1970)

  # RETURN
  return(dt_min)
 } )

}



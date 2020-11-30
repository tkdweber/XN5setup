#' Calculate Annual average Temperature and Annual Monthly Temperature Amplitude
#'
#'@description Based on the weather data stored in settings, the Annual Average Temperature is calculated for the time period of complete years only.
#' There is no adjustment over the years (climate change), and it is not possible to do single years in this ewq
#'The function can be used to automatically deduce the maximum possible simulation window, given different weather files.
#'
#'
#' @param x a list with at least the following 4 element
#' \itemize{
#'  \item{count$weather_NAMES}{The nams of the weather data files. Several can be provided }
#'  \item{sim_start}{Start date of the simulation Date class, format "YYYY-MM-DD"}
#'  \item{sim_end}{End date of the simulation Date class, format "YYYY-MM-DD"}
#'  \item{weather$data}{weather data from the weather.csv files}
#' }

#' @return a list with 2 elements
#'\itemize{
#'  \item{\code{AveYearTemp}} {List with Average Annual Temperature for each climate file.}
#'  \item{\code{MonthTempAmp}}{List with Annual Monthly Temperature Amplitude}
#' }
#'
#' @author Tobias KD Weber, \email{tobias.weber@uni-hohenheim.de}
#' @author Michelle Viswanathan, \email{michelle.viswanathan@uni-hohenheim.de}
#'
#' @examples
#' \dontrun{
#' # no sensible example available. Provide x.
#' }
#' @importFrom lubridate year month
#' @export
#'



get_cfgClimate <- function(x){

  AveYearTemp  <- list()
  MonthTempAmp <- list()


  for (kweather in 1:length(x$weather$data)){

    # ensure complete years are considered only, but only if at least one full year is present in the data
    if((x$sim_end %>% year -
        x$sim_start %>% year) > 0){

      cut_lo <- as.Date(ISOdate(year(x$sim_start), 1, 1  ))
      cut_up <- as.Date(ISOdate(year(x$sim_end)  , 12, 31))

      sel_loc    <- x$weather$data[[kweather]]$Date >= cut_lo & x$weather$data[[kweather]]$Date <= cut_up

    }else{
      # this is, so that the code works
      sel_loc <- TRUE
      message("Attention: calculation of AveYearTemp and MonthTempAmp based on incomplete year information \n Adjust manually in cfg.ini")}

    dat_temp <- x$weather$data[[kweather]][sel_loc, ]

    AveYearTemp[[kweather]]  <- aggregate(dat_temp[,9], list(dat_temp$Date%>%year), mean) %>% dplyr::summarise(mean(x)) %>% round(.,2)

    a  <- aggregate(dat_temp[,9], list(dat_temp$Date %>% year, dat_temp$Date %>% month), min)
    b  <- aggregate(dat_temp[,9], list(dat_temp$Date %>% year, dat_temp$Date %>% month), max)
    MonthTempAmp[[kweather]] <- mean((b$x - a$x)/2) %>% round(., 2)

  }

  AveYearTemp  %<>% unlist
  MonthTempAmp %<>% unlist
  names(AveYearTemp) <- names(MonthTempAmp) <- x$weather$file_name

  # unorthodox. because it checks if an object is in the global environment. Need to change this.
  if(exists("weather", sel$soil)) {

    if(length(sel$soil$weather) != length(AveYearTemp)){

      DT <- merge(data.table::data.table("name" = sel$soil$weather)
                  , data.table::data.table("name" = names(AveYearTemp), "AveYearTemp" = AveYearTemp, "MonthTempAmp" =MonthTempAmp)
                  , by = "name")

      AveYearTemp  <-  unlist(DT$AveYearTemp) %>% set_names(., sel$soil$weather )
      MonthTempAmp <-  unlist(DT$MonthTempAmp) %>% set_names(., sel$soil$weather )

    }

  }


  return(list("AveYearTemp" = unlist(AveYearTemp), "MonthTempAmp" = unlist(MonthTempAmp)))
}







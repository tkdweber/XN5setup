#' Setup cfg input
#'
#' Soil parameterisation setup and returns a vector of characters for each line in the _cfg.ini file. Main inputs are i) soil hydraulic properties, and ii) initial conditions,
#' Through the arguments, the Default settings can be changed. arguments soil and start_values have to be provided
#'
#'
#' @param simulation a vector with two named elements start and end, both characters, giving the dates (YYYY-MM-DD) of the simulation (should coincide with the weather file)
#' @param location a vector with named elements specifying location of the simulations, as well as the wind measurement height (wind_measure_height) and
#' the temperature measurement height (temp_measure_height)
#' @param climate a vector with named elements specifying the average annual temperature (AveYearTemp; from weather file) and monthly Temperature amplitude (MonthTempAmp; from weather file)
#' @param preceding_crop a vector with named elements giving details on the preceeding crop. Best left unchanged for long simulations
#' @param soil the list returned from the fucntion which determines the model setup of ini_cfgsoilDef
#' @param start_values the list returned from the function which determines the initial conditions ini_cfgicDef
#' @param farm a vector of named elements giving details which should be changed
#'
#' @return Setup the cfg.ini given several arguments
#' @author Tobias KD Weber , \email{tobias.weber@uni-hohenheim.de}
#'
#'
#' @examples
#' \dontrun{
#' }
#' @export

ini_cfgsoilDef <- function(x){

 cfg <- list()
 # [soil] ----------------------------------------------------------
 cfg$'soil'$layers           =  1
 cfg$'soil'$water_content    =  29.77
 cfg$'soil'$matrix_potential =  -99
 cfg$'soil'$soil_temperature =  10
 cfg$'soil'$nh4_content      =  0.03
 cfg$'soil'$no3_content      =  2.22
 cfg$'soil'$root_density     =  -99

}








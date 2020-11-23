#' setup cfg input
#'
#' Soil parameterisation setup and returns a vector of characters for each line in the _cfg.ini file. Main inputs are i) soil hydraulic properties, and ii) initial conditions,
#' Through the arguments, the Default settings can be changed. arguments soil and start_values have to be provided
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
#' # no sensible example}
#' @export

ini_cfgDef <- function(simulation = NA, location = NA, climate = NA, preceding_crop = NA, soil, start_values, farm = NA){


 # initialise  ----------------------------------------------------
 cfg <- list()

 # DEFAULT SETTINGS -----------------------------------------------
 ## [simulation] ---------------------------------------------------
 cfg$'simulation' <- list()
 cfg$'simulation'$start             = "1990-08-15"
 cfg$'simulation'$end               = "2018-09-30"


 cfg$'location'$lat                 = 48.9
 cfg$'location'$lon                 = 8.7
 cfg$'location'$alt                 = 319
 cfg$'location'$exposition          = "S"
 cfg$'location'$inclination         = 0
 cfg$'location'$size                = 1.0
 cfg$'location'$plot_number         = 1
 cfg$'location'$plot_name           = "dummy"
 cfg$'location'$sub_plot_number     = 1
 cfg$'location'$sub_plot_name       = "dummy"
 cfg$'location'$sub_plot_size       = 1.0
 cfg$'location'$area_number         = "dummy"
 cfg$'location'$wind_measure_height = 2.0
 cfg$'location'$temp_measure_height = 2.0

 # (climate) -------------------------------------------------------
 cfg$'climate'$AveYearTemp  = 7.4
 cfg$'climate'$MonthTempAmp = 6

 # [preceding_crop] ------------------------------------------------
 cfg$'preceding_crop'$crop_name         = "Wheat"
 cfg$'preceding_crop'$harvest_date      = -99
 cfg$'preceding_crop'$yield             = -99
 cfg$'preceding_crop'$res_removal       = -99
 cfg$'preceding_crop'$above_residual    = -99
 cfg$'preceding_crop'$root_residual     = -99
 cfg$'preceding_crop'$above_CN_residual = -99
 cfg$'preceding_crop'$root_CN_residual  = -99

 # [soil] ----------------------------------------------------------
 cfg$'soil'$layer_count     =  1
 cfg$'soil'$layer_thickness =  5
 cfg$'soil'$layers          =  1
 cfg$'soil'$clay            =  18.2
 cfg$'soil'$silt            =  79.4
 cfg$'soil'$sand            =  2.4
 cfg$'soil'$organic_matter  =  1.75
 cfg$'soil'$bulk_density    =  1.37
 cfg$'soil'$rock_fraction   =  0
 cfg$'soil'$ph              =  -99
 cfg$'soil'$soil_type       =  "Ut4"
 cfg$'soil'$wilting_point   =  16.2
 cfg$'soil'$field_capacity  =  36.9
 cfg$'soil'$porosity        =  48.3
 cfg$'soil'$cond_sat        =  179.9
 cfg$'soil'$res_water_cont  =  7.5
 cfg$'soil'$cont_sat        =  46
 cfg$'soil'$camp_a          =  -0.05
 cfg$'soil'$camp_b          =  7.48
 cfg$'soil'$van_gen_a       =  0.008
 cfg$'soil'$van_gen_n       =  1.633

 # [start values] --------------------------------------------------
 cfg$'start values'$layers           =  1
 cfg$'start values'$water_content    =  29.77
 cfg$'start values'$matrix_potential =  -99
 cfg$'start values'$soil_temperature =  10
 cfg$'start values'$nh4_content      =  0.03
 cfg$'start values'$no3_content      =  2.22
 cfg$'start values'$root_density     =  -99

 # [farm] ----------------------------------------------------------
 cfg$'farm'$name       = "stochastic soils"
 cfg$'farm'$number     = 1
 cfg$'farm'$last_name  = "DFG CRC 1253 CAMPOS"
 cfg$'farm'$first_name = "University of Hohenheim"
 cfg$'farm'$street     = "Emil-Wolff-Strasse 27"
 cfg$'farm'$post_code  = "70599"
 cfg$'farm'$city       = "Stuttgart"
 cfg$'farm'$phone      = "0049 711 459 22466"
 cfg$'farm'$fax        = "orcid 0000-0002-3448-5208"
 cfg$'farm'$mobile     = "0049 711 459 0"
 cfg$'farm'$email      = "tobias.weber_at_uni-hohenheim.de"

 # 3 CHECKS ----------------------------
 {
  # simulation
  if(exists("simulation") & !all(names(simulation) %in% names(cfg$'simulation')))  stop({paste("In the simulation argument, the element name   ",  names(simulation)[!names(simulation) %in% names(cfg$'simulation')], "   was ill-specified")})
  # location
  if(exists("location") & !all(names(farm) %in% names(cfg$'location')))  stop({paste("In the location argument, the element name   ",  names(location)[!names(location) %in% names(cfg$'location')], "   was ill-specified")})
  # climate
  if(exists("climate") & !all(names(farm) %in% names(cfg$'climate')))  stop({paste("In the climate argument, the element name   ",  names(farm)[!names(climate) %in% names(cfg$'climate')], "   was ill-specified")})
  # preceding_crop
  if(exists("preceding_crop") & !all(names(preceding_crop) %in% names(cfg$'preceding crop')))  stop({paste("In the preceding_crop argument, the element name   ",  names(farm)[!names(preceding_crop) %in% names(cfg$'preceding crop')], "   was ill-specified")})
  # soil
  if(exists("soil") & !all(colnames(soil) %in% names(cfg$'soil')))  stop({paste("In the soil argument, the element name   ",  colnames(soil)[!colnames(soil) %in% names(cfg$'soil')], "   was ill-specified")})
  # starting_values
  if(exists("start_values") & !all(colnames(start_values) %in% names(cfg$'start values')))  stop({paste("In the start_values argument, the element name   ",  colnames(start_values)[!colnames(start_values) %in% names(cfg$'start values')], "   was ill-specified")})
  # farm
  if(exists("farm") & !all(names(farm) %in% names(cfg$'farm')))  stop({paste("In the farm argument, the element name   ",  names(farm)[!names(farm) %in% names(cfg$'farm')], "   was ill-specified")})
 }





}








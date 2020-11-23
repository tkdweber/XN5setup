#' Setup cfg input
#' @description Soil parameterisation setup and returns a vector of characters for each line in the _cfg.ini file. Main inputs are i) soil hydraulic properties, and ii) initial conditions,
#' Through the arguments, the Default settings can be changed. arguments soil and start_values have to be provided
#' @param x a named list with named elements. Defaults are specified in the function. Named elements are replaced if existing in the default inside the function. Else, they are added to the return.
#' @return Setup the cfg.ini given several arguments
#' @author Tobias KD Weber , \email{tobias.weber@uni-hohenheim.de}
#' @examples
#' \dontrun{
#' # no good example possible.
#' result <- ini_cfgsoilDef()
#' }
#' @export

ini_cfgsoilDef <- function(x){

 cfg <- list()
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
}








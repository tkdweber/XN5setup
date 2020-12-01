#' setup XPN options line
#'
#' A simple function that sets up the [options] block in an .xnp file. A DEFAULT setting is given, additional
#' moduls can be added by providing a list with named elements.
#' The name of the list element has to correspond to new line added.
#'
#' @param x i) a list with named elements. The names of the elements will be used as the LHS and
#' the vector element will be the RHS of the equal sign in extending [options].
#' ii) The default options can be updated by named x elements equalling the [options] LHS
#' @param query.plant.model character to specify the plant model. Supported: GECROS, GECROS_h
#' @return The options specification for the .xnp file
#'
#' @author Tobias KD Weber , \email{tobias.weber@uni-hohenheim.de}
#'
#' @examples
#' # Default settings can be seen by executing the following line
#' opts <- xpn_optionsDef(x = NA)
#'
#' @export

xpn_optionsDef <- function(x = NA, query.plant.model = "gecros"){

 if(all(!is.na(x))){
  x <- lapply(x, as.character)
 }
 options    <- list()
 # DEFAULT settings

 # code to replace the project name
 # gsub("$PROJECT_NAME", "", "$<$PROJECT_PATH/$PROJECT_NAME__barley_sommergerste_gecros.ini$>", fixed = TRUE)
 query.plant.model %<>% tolower
 # [options]
 options$'Barley_Sommergerste_gecros'   = "$<$PROJECT_PATH/$PROJECT_NAME__barley_sommergerste_gecros.ini$>"
 options$'Barley_Wintergerste_gecros'   = "$<$PROJECT_PATH/$PROJECT_NAME__barley_wintergerste_gecros.ini$>"
 options$'Maize_SilageKRG_gecros'       = "$<$PROJECT_PATH/$PROJECT_NAME__maize_silageKRG_gecros.ini$>"
 options$'Maize_SilageMSA_gecros'       = "$<$PROJECT_PATH/$PROJECT_NAME__maize_silageMSA_gecros.ini$>"
 options$'Rapeseed_Winterrape_gecros'   = "$<$PROJECT_PATH/$PROJECT_NAME__rapeseed_winterrape_gecros.ini$>"
 options$'SugarBeet_zuckerruebe1_gecros'= "$<$PROJECT_PATH/$PROJECT_NAME__sugarbeet_zuckerruebe1_gecros.ini$>"
 options$'Soy_gecros'                   = "$<$PROJECT_PATH/$PROJECT_NAME__soy_gecros.ini$>"
 options$'Wheat_Spelt_gecros'           = "$<$PROJECT_PATH/$PROJECT_NAME__wheat_spelt_gecros.ini$>"
 options$'Wheat_Winterdurum_gecros'     = "$<$PROJECT_PATH/$PROJECT_NAME__wheat_winterdurum_gecros.ini$>"
 options$'Wheat_Winterweizen_gecros'    = "$<$PROJECT_PATH/$PROJECT_NAME__wheat_winterweizen_gecros.ini$>"
 options$'CoverCrop_Ackersenf_gecros'   = "$<$PROJECT_PATH/$PROJECT_NAME__covercrop_ackersenf_gecros.ini$>"
 options$'Potato_Kartoffel_gecros'      = "$<$PROJECT_PATH/$PROJECT_NAME__potato_kartoffel_gecros.ini$>"
 options$'varlist'                      = "$<$PROJECT_PATH/$PROJECT_NAME_varlist.var$>"
 options$'time step'                    = "0.1"
 options$'output time step'             = "daily"
 options$'create_auto_init_files'       = "0"
 options$'t_base'                       = 3.5
 options$'start_dlen_phenology'         = 10.0
 options$'gdd_start_crit'               = 600.0
 options$'gdd_end_crit'                 = 100.0
 options$'start_day_phenology'          = "01.03"
 options$'output netcdf path'           = "$PROJECT_PATH/output"
 options$'no output netcdf file'        = 0
 options$'output text path'             = "$PROJECT_PATH/output"
 options$'no output txt file'           = 1
 options$'always dump nc file'          = 1
 options$'create xno files'             = 0
 options$'special_output_def'           = "$<$PROJECT_PATH/$PROJECT_NAME_special_output_def.ini$>"
 options$'measure time'                 = 0
 options$'output time span'             = "2000-08-01:2020-11-10"

 if(query.plant.model == "gecros_h"){
   options %<>% lapply(., stringr::str_replace, "gecros", "gecros_h")
   names(options) %<>% stringr::str_replace(., "gecros", "gecros_h")
 }




 if(all(!is.na(x))){
  # add to default list
  stopifnot(is.list(x))
  if(any(!names(x) %in% names(options))){
   x_short <-  rlist::list.remove(x ,   names(x)%in%names(options))
   options <- c(options, x_short)
  }

  # update the default
  xnamex <- names(x)
  for(j in xnamex){
   options[j] <- x[[j]]
   #print(paste0("updated: ",  "'",names(options[j]),"'", " to ", x[[j]]))
  }

  # removes empty named elements
  # options <- rlist::list.remove(options ,  which(names(options) == ""))
 }

 res <- c("[options]"
          , paste(names(options), do.call("rbind", options), sep = " = ")
          , ""
 )
 return(res)
}






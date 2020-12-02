#' setup XPN system line
#'
#' A simple function that sets up the [system] block in an .xnp file. A DEFAULT setting is given, additional
#' moduls can be added by providing a list with named elements.
#' The name of the list element has to correspond to new line added.
#'
#' @param x NA - Default values are used i) a list with named elements. The names of the elements will be used as the LHS and
#' the vector element will be the RHS of the equal sign in extending [system].
#' ii) The default system can be updated by named x elements equalling the [system] LHS
#' @param query.plant.model character to specify the plant model. Supported: GECROS, GECROS_h, SPASS, CERES
#' @param query.miner.model character to specify the mineralisation model. Supported: DAISY and LEACHN.
#' @return The system specification for the .xnp file
#'
#' @author Tobias KD Weber , \email{tobias.weber@uni-hohenheim.de}
#'
#' @examples
#' # Default settings can be seen by executing the following line
#' syst <- xpn_systemDef(x = NA)
#'
#' @export

xpi_Def <- function(x = NA, query.plant.model = "gecros", query.miner.model = "DAISY"){

  # if(all(!is.na(x))){
  #  x <- lapply(x, as.character)
  # }
  query.plant.model %<>% toupper
  query.miner.model %<>% toupper

  if(!(query.plant.model %in% c("GECROS", "GECROS_H", "SPASS", "CERES"))){stop("plant model not supported")}
  if(!(query.miner.model %in% c("DAISY", "HANSEN ET AL. (DAISY_MINER)", "LEACHN"))){stop("mineralisation model not supported")}
  # _____ DEFAULT settings ----------------
  {
    xpi.list    <- list()

    #1 INITIALISE ######
    # [control] ----------------------
    xpi.list$control$'balance'      <- "BALANCE"
    xpi.list$control$'database'     <-  "Expert N Standard Read INI"
    xpi.list$control$'pedotransfer' <-  "Campbell"
    xpi.list$control$'output'       <-  "XPN_OUTPUT"

    # [Expert N Standard Read INI] ----------------------
    xpi.list$'Expert N Standard Read INI'$'use high resolution climate data' <- 0
    xpi.list$'Expert N Standard Read INI'$'interpolate climate data'         <- 0
    xpi.list$'Expert N Standard Read INI'$'filename'                         <- "$<$PROJECT_PATH/$PROJECT_NAME_$REG_STR_cfg.ini$>"
    xpi.list$'Expert N Standard Read INI'$'time zone'                        <- 0
    xpi.list$'Expert N Standard Read INI'$'climate file'                     <- "$<$PROJECT_PATH/weather.csv$>"

    # [expertn_database] ----------------------
    # not implemented
    # [water] ----------------------
    xpi.list$water$'potential evapotranspiration' <- "Penman Monteith (FAO)"
    xpi.list$water$'potential evaporation'        <- "Penman Monteith"
    xpi.list$water$'actual evaporation'           <- "Penman Monteith"
    xpi.list$water$'kc factor'                    <- "dev stage"
    xpi.list$water$'hydraulic functions'          <- "van Genuchten and Mualem"
    xpi.list$water$'flow module'                  <- "HYDRUS Flow"

    # [const] ----------------------
    xpi.list$const$'kc factor'                    <- 0

    # [HYDRUS Flow] ----------------------
    # [hydrus] ----------------------
    xpi.list$hydrus$'bottombc'                 <- 1
    xpi.list$hydrus$'mobil'                    <- 0
    xpi.list$hydrus$'infiltration_limit'       <- 0
    xpi.list$hydrus$'infiltration_layer_limit' <- 0

    # [Penman Monteith] ----------------------
    xpi.list$'Penman Monteith'$'soil cover'      <- 0.0

    # [evapotranspiration_pm] ----------------------

    # [LEACHN] ----------------------

    # [leachn] ----------------------
    xpi.list$leachn$'ini_filename'        <- "$<$PROJECT_PATH/$PROJECT_NAME_$REG_STR_leachn.ini$>"

    # [heat] ----------------------
    xpi.list$heat$'heat transfer'       <- "DAISY Modul Heat Transfer"
    xpi.list$heat$'albedo'              <- "Vegetation const (0.25)"
    xpi.list$heat$'surface temperature' <- "First Soil Layer"
    xpi.list$heat$'ground heat'         <- "Penman Monteith"
    xpi.list$heat$'net radiation'       <- "Penman Monteith"
    xpi.list$heat$'emissivity'          <- "Penman Monteith"

    # [nitrogen] ----------------------
    xpi.list$nitrogen$'nitrogen transport'  <- "LEACHN"
    xpi.list$nitrogen$'nitrification'       <- "LEACHN"
    xpi.list$nitrogen$'denitrification'     <- "LEACHN"
    xpi.list$nitrogen$'urea hydrolysis'     <- "LEACHN"
    xpi.list$nitrogen$'deposition'          <- "Constant Deposition"

    xpi.list$nitrogen$'mineralisation' <- switch(query.miner.model,
           "DAISY"                       = "Hansen et al. (DAISY_Miner)",
           "HANSEN ET AL. (DAISY_MINER)" = "Hansen et al. (DAISY_Miner)",
           "LEACHN"                      = "LEACHN"
    )
    # [management] ----------------------
    xpi.list$management$'application fertilizers' <- "Schaaf"
    xpi.list$management$'mixing incorporation'    <- "Williams et al. (EPIC)"

    # [Schaaf] ----------------------
    # [schaaf] ----------------------
    xpi.list$schaaf$'filename'                 <- "$<$PROJECT_PATH/$PROJECT_NAME_$REG_STR_fertilization.ini$>"

    # [schaaf_manag] ----------------------
    xpi.list$schaaf_manag$'ini_filename'       <- "$<$PROJECT_PATH/$PROJECT_NAME_$REG_STR_schaaf_manag.ini$>"

    # [DAISY Modul Heat Transfer] ----------------------
    xpi.list$'DAISY Modul Heat Transfer'$'frost_rad_flag'           <- 2
    xpi.list$'DAISY Modul Heat Transfer'$'lower_boundary_condition' <- 2

    # [daisy] ----------------------

    # [plant] ----------------------
    # GECROS
      xpi.list$plant$'potential transpiration'     <- "Penman Monteith"
      xpi.list$plant$'phenological development'    <- "GECROS Development"
      xpi.list$plant$'biomass growth'              <- "GECROS BiomassGrowth"
      xpi.list$plant$'canopy gross photosynthesis' <- "GECROS Gross Photosynthesis"
      xpi.list$plant$'canopy formation'            <- "GECROS Canopy Formation"
      xpi.list$plant$'root length growth'          <- "GECROS Root System Formation"
      xpi.list$plant$'crop senescence'             <- "GECROS Crop Senescence"
      xpi.list$plant$'nitrogen demand'             <- "GECROS Nitrogen Demand"
      xpi.list$plant$'nitrogen uptake'             <- "GECROS Nitrogen Uptake"
      xpi.list$plant$'actual transpiration'        <- "GECROS Actual Transpiration"

    # GECROS_h: lazy short coding.
    if(query.plant.model == "GECROS_H"){
      xpi.list$plant %<>% lapply(., stringr::str_replace, "GECROS", "GECROS_h")
    }
      if(query.plant.model == "CERES"){
      xpi.list$plant %<>% lapply(., function(x) "CERES")
      }
      if(query.plant.model == "SPASS"){
        xpi.list$plant %<>% lapply(., function(x) "WANG (SPASS)")
      }
    # [gecros] ----------------------
    xpi.list$gecros$'filename' <- "$<$PROJECT_PATH/$PROJECT_NAME_$REG_STR_crop_rotation.ini$>"

    # [daisy_miner] ----------------------
    xpi.list$'daisy_miner'$'ini_filename'<- "$<$PROJECT_PATH/$PROJECT_NAME_$REG_STR_daisy_miner_nitrogen.ini$>"

    # [dev stage] ----------------------
    # xpi.list$'dev stage'$'kc_param_file' <- "$<$PROJECT_PATH/$PROJECT_NAME_$REG_STR_kc_dev_stage.ini$>"
    xpi.list$'dev stage'$'kc_param_file' <- "$<$PROJECT_PATH/$PROJECT_NAME_$REG_STR_kc_dev_stage.ini$>"

    # [Wang (SPASS)] ----------------------
    xpi.list$'Wang (SPASS)'$'harvest_at_maturity'         <- 0
    xpi.list$'Wang (SPASS)'$'set_LAI_to_0_after_maturity' <- 0

    # [spass] ----------------------
    xpi.list$'spass'$'Maize'               <- "$<$PROJECT_PATH/$PROJECT_NAME_$REG_STR_maize.ini$>"
    xpi.list$'spass'$'Wheat'               <- "$<$PROJECT_PATH/$PROJECT_NAME_$REG_STR_wheat.ini$>"
    xpi.list$'spass'$'Rapeseed_Winterrape' <- "$<$PROJECT_PATH/$PROJECT_NAME_$REG_STR_winterrape.ini$>"
    xpi.list$'spass'$'CoverCrop'           <- "$<$PROJECT_PATH/$PROJECT_NAME_$REG_STR_zwifru.ini$>"
    xpi.list$'spass'$'filename'            <- "$<$PROJECT_PATH/$PROJECT_NAME_$REG_STR_crop_rotation.ini$>"

    # [Penman Monteith ASCE 81 crop] ----------------------

    #  [Constant Deposition] ------------------------------
    xpi.list$'Constant Deposition'$'no3'   <- 6.0
    xpi.list$'Constant Deposition'$'nh4'   <- 12.0

  }

  # _____ UPDATE ----------------
  if(all(!is.na(x)) & length(x) != 1){
    switch_names <- names(x)
    for(i in 1:length(switch_names)){

      names_it <- switch_names[i]

      # removes empty named elements
      system <- rlist::list.remove(system ,  which(names(system) == ""))
      xpi.list[[names_it]] <- update_list(x[[names_it]], xpi.list[[names_it]])

    }; rm(switch_names, names_it)
  }
  # _____ Prepare print out ----------------
  # _____ UPDATE ----------------

  switch_names_xpi <- names(xpi.list)
  res <- ""
  for(i in 1:length(switch_names_xpi)){

    names_it <- switch_names_xpi[i]

    res <- c(res,
             c(paste0("[", names_it, "]")
               , paste(names(xpi.list[[names_it]]), do.call("rbind", xpi.list[[names_it]]), sep = " = ")
               , ""
             ))

  }

  return(res)

}

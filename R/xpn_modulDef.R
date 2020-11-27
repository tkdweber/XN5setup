#' Setup XPN module line
#'
#' A simple function that sets up the [modul] libs = line in an .xnp file. A DEFAULT setting is given, additional
#' moduls can be added by providing a list with named elements. The name should correspond to new module which should be additionally loaded
#'
#' @param x a list with named elements. The names of the elements will be used as modules to be loaded.
#'
#' @return The libs specification in the .xnp file
#'
#' @author Tobias KD Weber , \email{tobias.weber@uni-hohenheim.de}
#'
#' @examples
#' \dontrun{
#' ## example 1
#' libsreturn1 <- xpn_modulDef()
#' ## example 2
#' libsini <- list()
#' libsini$testmodule <- 1
#' libsreturn2 <- xpn_modulDef(libsini)
#' }
#' @export
#'
#'
xpn_modulDef <- function(x = NA){
 libs    <- list()
 if(is.na(x)){
  # DEFAULT settings

 # [modul]
 libs$evapotranspiration_pm <- 1
 libs$expertn_database      <- 1
 libs$hydraulic_functions   <- 1
 libs$leachn                <- 1
 libs$pedotransfer          <- 1
 libs$schaaf                <- 1
 libs$schaaf_manag          <- 1
 libs$spass                 <- 1
 libs$daisy                 <- 1
 libs$daisy_miner           <- 1
 libs$depos                 <- 1
 libs$gecros                <- 1
 libs$gecros_h              <- 1
 libs$hydrus                <- 1
 libs$xpn_output            <- 1
 libs$balance               <- 1
 }else{
  # add to default
  stopifnot(is.list(x))
  libs <- c(libs,x)
 }

 # names(libs) %>% paste(., collapse = ";") %>% paste0("libs = ", .,";;", collapse = "") %>% c("[modul]",.) %>% return
 res <- c("[modul]"
          , paste0("libs = "
                   ,  paste( names(libs)
                             , collapse = ";")
                   ,";"
                   , collapse = ""
                   )
          , "")

 return(res)
}


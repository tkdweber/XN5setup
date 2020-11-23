#' setup XPN system line
#'
#' A simple function that sets up the [system] block in an .xnp file. A DEFAULT setting is given, additional
#' moduls can be added by providing a list with named elements.
#' The name of the list element has to correspond to new line added.
#'
#' @param x NA - Default values are used i) a list with named elements. The names of the elements will be used as the LHS and
#' the vector element will be the RHS of the equal sign in extending [system].
#' ii) The default system can be updated by named x elements equalling the [system] LHS
#'
#' @return The system specification for the .xnp file
#'
#' @author Tobias KD Weber , \email{tobias.weber@uni-hohenheim.de}
#'
#' @examples
#' # Default settings can be seen by executing the following line
#' syts <- xpn_systemDef(x = NA)
#'
#' @export

xpn_systemDef <- function(x = NA){
 if(all(!is.na(x))){
  x <- lapply(x, as.character)
 }
 system    <- list()
 # DEFAULT settings

 # [system]
 system$'module_def_file' <- "cfg_system/modules.def"
 system$'num_proc'        <- 0
 system$'debug_level'     <- 1

 if(all(!is.na(x))){
  # add to default list
  stopifnot(is.list(x))
  if(any(!names(x) %in% names(system))){
   x_short <-  rlist::list.remove(x ,   names(x)%in%names(system))
   system <- c(system, x_short)
  }

  # update the default
  xnamex <- names(x)
  for(j in xnamex){
   system[j] <- x[[j]]
   #print(paste0("updated: ",  "'",names(system[j]),"'", " to ", x[[j]]))
  }

  # removes empty named elements
  # system <- rlist::list.remove(system ,  which(names(system) == ""))
 }

 res <- c("[system]"
          , paste(names(system), do.call("rbind", system), sep = " = ")
          , ""
 )
 return(res)
}






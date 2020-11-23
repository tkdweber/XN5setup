
#' Get State Variable Results
#' @description By specifying \code{mylocations$my.xno.path} the function internally will look for ALL .xno files
#' and search for results of the state variables of interest
#' @param mylocations a list with two named elements. The names matter.
#'\tabular{lll}{
#'    \code{my.xno.path}\tab{character specifying the path where the xno files should be searched in. }\cr
#'    \code{my.output.path}\tab{character specifying the folder where the results should be saved. }\cr
#'    }
#' @param state.vars a character specifying the name of the state variable(s) which should be collected. You might want to
#' use the function \link[XN5setup]{get_columnNames_xno} to verify if these exist.
#' @param query.return default to \code{FALSE}. Specifying \code{TRUE} will result in the function returning the collected results.
#'
#' @return a data.table with the state variable results specified in \code{state.vars} and searched for in location \code{my.xno.path}.
#' @examples
#' \dontrun{
#' # initialise list
#' mylocations <- list()
#' setwd("C:/Projects/ExpertN/expertn513d/built")
#' # assign paths:
#' # 1. location of xno.paths
#' mylocations$my.xno.path    <- "./cfg/replicateECv10/"
#' # 2. location of output
#' mylocations$my.output.path <- "C:/Projects/ExpertN/myresults"
#'
#' colnam <- get_columnNames_xno("./cfg/replicateECv10/output/replicateECv10_0_0_0_0.xno")
#' state.vars <- colnam[c(210,197)]
#'
#' }
#' @importFrom magrittr %<>%
#' @importFrom data.table data.table
#' @export
#'
get_XN5statevar_xno <- function(mylocations, state.vars ="output.Plant.Development Stage.BBCH Stage [-]", query.return = FALSE){


 ## checks
 if(!exists(x = "my.xno.path", where = mylocations))
  stop("my.xno.path has not been provided in the list mylocations")

 if(!is.logical(query.return)){
  stop("query.return is not logical")
 }

 if(!exists(x = "my.output.path", where = mylocations))
  stop("my.output.path has not been provided in the list mylocations")

 stopifnot(length(mylocations$my.xno.path) == 1)
 stopifnot(length(mylocations$my.output.path) == 1)

 # collect data
 out.l <- list()
 out    <- list.files(mylocations$my.xno.path, pattern = ".xno", recursive = TRUE, full.names = TRUE)
 my.xno <- list.files(mylocations$my.xno.path, pattern = ".xno", recursive = TRUE, full.names = FALSE, include.dirs = FALSE) %>% stringr::str_replace( ., pattern = ".xno", replacement = "")

 for(i in 1:length(out)){

  out.dt <- fread(out[i])

  out.dt %<>% data.table::data.table("file.name"  = my.xno[i], .)

  date_cols <- c("Time and Date")

  out.dt[, (date_cols) := lapply(.SD, function(x) {as.character(x) %>% stringr::str_split(., " ") %>% do.call("rbind", .) %>% .[,1]}

  ), .SDcols = date_cols]

  out.l[[i]] <- out.dt

 }

 # time is needed
 out.lk <- list()
 for (k in 1:length(state.vars)){
  # flexible for multiple state variables
  timestamp    <- out.l[[1]]$`Time and Date`
  out.l.short  <- lapply(out.l, function(x)  x[, state.vars[k], with = FALSE] %>% t %>% data.table)
  out.dt.short <- data.table::rbindlist(out.l.short, fill = TRUE)

  colnames(out.dt.short) <- timestamp
  rownames(out.dt.short) <- my.xno

  if(isFALSE(dir.exists(mylocations$my.output.path))){
   dir.create(mylocations$my.output.path)
   message(paste("created dir", mylocations$my.output.path))
  } %>% invisible

  state.vars[k] <- stringr::str_replace(state.vars[k], "\\/", " per ")

  data.table::fwrite(out.dt.short
                     , file.path(mylocations$my.output.path, paste0(state.vars[k],".OUT")), sep = ",", quote = FALSE, row.names = TRUE)

  out.lk[[k]] <- out.dt.short
 }

 # output

 if(isTRUE(query.return)){
  return(out.lk)
 }

}




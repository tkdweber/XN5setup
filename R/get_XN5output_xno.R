#' setup XPN options line
#'
#' A simple function that sets up the [options] block in an .xnp file. A DEFAULT setting is given, additional
#' moduls can be added by providing a list with named elements.
#' The name of the list element has to correspond to new line added.
#'
#' @param mylocations list
#' \itemize{
#'   \item{"my.xno.path"}{Character vector specifying the location of the result files. If a higher order folder is used, this will be included in the output. Possibility to make results unique.}
#'   \item{"my.output.path}{Character vector specifying the location of the output files. The files are ascii files and will be supplemented by \emph{.OUT}.}
#' }
#' @param timestamps character vector specifying the date(s) which are to be returned. Format "YYYY-MM-DD". No checks, careful. Most probably cause of errors, here.
#' @param query.return logical. FALSE is the default. If TRUE, a list is returned.
#' @return If query.return == TRUE, returns list with number of elements as .xno files. Each element contains a data.table with the read timestamps.
#'
#' @author Tobias KD Weber , \email{tobias.weber@uni-hohenheim.de}
#'
#' @examples
#' \dontrun{
#' # initialise list
#' mylocations <- list()
#'
#' # assign paths:
#' # 1. location of xno.paths
#' mylocations$my.xno.path <- "C:/Projects/ExpertN/expertn513d/built/cfg/replicateECv10/"
#' # 2. location of output
#' mylocations$my.output.path <- "C:/Projects/ExpertN/myresults"
#'
#' timestamps<- c("2009-08-02", "2026-09-03")
#' get_XN5output_xno(mylocations, timestamps)
#'}
#' @importFrom data.table := .SD fread
#' @importFrom utils write.table
#' @importFrom magrittr %<>%
#' @export

get_XN5output_xno <- function(mylocations, timestamps, query.return = FALSE){


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

  out.l[[i]] <- out.dt[(out.dt[,2] == timestamps)%>% drop]

 }
 out.dt <- data.table::rbindlist(out.l, fill = TRUE)
 # write data
 for (i in 1:length(timestamps)){

  out.dt.short <- out.dt[(out.dt[,2] == timestamps[i])%>% drop]

  write.table(out.dt.short, file.path(mylocations$my.output.path, paste0(timestamps[i],".OUT")), sep = ",", quote = FALSE, row.names = FALSE)
 }

 if(isTRUE(query.return)){
  return(out.l)
 }

}




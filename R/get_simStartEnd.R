#' Recuperate the start and end time of a simulation based on available dates in weather files
#'
#'@description The weather file has to be in format required by Expert-N. Daily values are given in the format "YYYY-MM-DD"
#'The function can be used to automatically deduce the maximum possible simulation window, given different weather files.
#'
#'
#' @param x a list with at least two elements
#' \itemize{
#'  \item{file_name}{The name of the weather data file. Several can be provided }
#'  \item{path}{The path of where the weather data file is stored. Either the same amount as provided in \code{file_name}
#'  or 1.}
#' }
#' @param sep_arg The column seperator in the weather file \code{DEFAULT = ,}.
#' @param query.head \code{TRUE/FALSE} If the weather file has a header or not. \code{DEFAULT = TRUE}.
#'
#' @return x List with time updated, crop specific DEFAULTs (fertilisation or tillage)
#'\itemize{
#'  \item{\code{file_name}}{ Character vector with the names of the weather data file. Several can be provided }
#'  \item{\code{path}} {Character vector with the names of the paths  where the weather data file is stored. Either the same amount as provided in \code{file_name}
#'   or 1.}
#' }
#'
#' @author Tobias KD Weber , \email{tobias.weber@uni-hohenheim.de}
#'
#' @examples
#' \dontrun{
#' }
#' @importFrom utils read.table
#' @export
#'
get_simStartEnd <- function(x
                            , sep_arg = ","
                            , query.head = TRUE
){

 # x   list  has to contain element file_name and path
 # checks

 # if(sum(c(exists("path"     , where = x),
 #          exists("file_name", where = x)))!=2 |
 #    sum(c(exists("path"     , where = x),
 #          exists("pattern"  , where = x)))!=2){
 #  stop("Error: settings$weather does not contain list elements file_name and path")}

 if(!is.character(x$file_name) & !is.character(x$pattern)){
  stop("In settings$weather either pattern or file_name has to be specified as a character and the resp. other as NULL")
 }

 if(length(x$file_name) < length(x$path) & !is.null(x$file_name)){
  stop("Do not provide more paths for weather files than weather file names")
 }

 # for output
 x_StartEnd <- list()

 if(!is.null(x$file_path)) {
  read_these_files <- file.path(x$path, x$file_path)} else if(is.null(x$file_path)) {
  read_these_files <- list.files(x$path, pattern = paste0(x$pattern, collapse = "|"),  recursive = TRUE, full.names = TRUE)
 }

 x <- lapply(read_these_files, function(x) {df     <- read.table(x, sep = sep_arg, header = query.head)
                                            df[,1] <- lubridate::as_date(df[,1])

df
                                            # minDate <- df$Date %>% lubridate::as_date(.) %>% min
                                            # maxDate <- df$Date %>% lubridate::as_date(.) %>% max
                                            # return(c(minDate, maxDate))
                                            } #%>% do.call("rbind", .)
             )

 # convert factor to
 start <- lapply(x, function(x_i) {x_i$Date %>% lubridate::as_date(.) %>% min})
 end   <- lapply(x, function(x_i) {x_i$Date %>% lubridate::as_date(.) %>% max})

 x_StartEnd$sim_start        <-do.call("c", start) %>% max #  max(x[,1]) #
 x_StartEnd$sim_end          <-do.call("c", end) %>% min   #  min(x[,2])

 x_StartEnd$data             <- x
 x_StartEnd$paths_full       <- read_these_files
 x_StartEnd$file_name_short  <- strsplit(x_StartEnd$paths_full , "\\/") %>% do.call("rbind", .) %>%
  .[, ncol(.)]

 return(x_StartEnd)
}

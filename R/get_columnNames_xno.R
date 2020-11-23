#' Extract column names from xno output file.
#'
#' A simple function to quickly access the available output column names in the xno files.
#'
#' @param file.path character vector specifying the at least one files path.
#' @return vector with character of output state variables.
#'
#' @author Tobias KD Weber , \email{tobias.weber@uni-hohenheim.de}
#'
#' @examples
#' \dontrun{
#' # initialise list
#'
#' # assign paths:
#' colnam <- get_columnNames_xno("my.xno")
#'}
#' @export

get_columnNames_xno <- function(file.path = "my.xnos"){
 
 if(file.path ==1){
  message("file.path length is >1, first element used to extract the columnnames")
 }
 
 return(data.table::fread(file.path[1],
                             nrows = 0) %>% colnames)
 
}

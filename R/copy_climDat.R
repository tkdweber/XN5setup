#' The function is used to copy the climate files from the various, specified sources to PROJECT_PATH
#'
#' @param x The list where the project settings are stored. It has to contain elements \code{PROJECT_PATH},
#' \code{weather$file_name}, and \code{weather$paths_full}.
#' \itemize{
#'   \item{"PROJECT_PATH"}{character with path specifying the location of the simulation experiment}
#'   \item{"weather$file_name"}{Stuff}
#'   \item{"weather$paths_full"}{Stuff}
#' }
#' @param query.overwrite query if existing files in PROJECT_PATH should be overwritten. Default to \code{TRUE}.
#'
#' @details Used in the setup of the simulation experiment.
#'
#' @return Vector of characters specifying the file.path names path names of the location of the climate files where copied, too.
#' The location of the file in the PROJECT_PATH
#'
#' @author Tobias KD Weber , \email{tobias.weber@uni-hohenheim.de}
#'
#' @examples
#' \dontrun{
#' # No sensible example, system specific
#' }
#' @export

copy_climDat <- function(x, query.overwrite = TRUE){

 # introduce checks
 if(!exists("PROJECT_PATH"      , where = x)){stop("PROJECT_PATH not an element of x")}
 if(!exists("weather"           , where = x)){stop("weather not an element of x")}
 if(!exists("file_name_short"   , where = x$weather)){stop("file_name not an element of x$weather" )}
 if(!exists("paths_full"        , where = x$weather)){stop("paths_full not an element of x$weather")}

 paths_full_new <- file.path(x$PROJECT_PATH, x$weather$file_name_short)

 file.copy(from        = x$weather$paths_full
           , to        = paths_full_new
           , overwrite = query.overwrite)

 return(list("paths_full_new" = paths_full_new))
}

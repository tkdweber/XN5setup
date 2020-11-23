#' Convert and write to files the cfg, leachn, daisy_miner_nitrogen initialisations.
#'
#' @description Intended for internal use.
#' @param xlist list with croprotations to be written to the file specified in \code{file_paths}.
#' @param file_header character, header of the .ini file, e.g. "[plant management]" in the croprotation.ini.
#' @param file_paths character vector with the file names corresponding to the crop rotations in \code{xlist}.
#' @param append.query \code{TRUE} or \code{FALSE}, specifying if the output should be appended to existing files.
#'
#' @details This function is specific to the list of returned elements from \code{croprotationDef}
#'
#' @return If \code{file_paths = NA} then the character vector is returned, which of \code{file_paths}
#' are provided, is saved to a file.
#'
#' @author Tobias KD Weber , \email{tobias.weber@uni-hohenheim.de}
#'
#' @examples
#' \dontrun{
#' }
#' @export
#'
#'

write_iniList <- function(xlist, file_header, file_paths = NA, append.query = FALSE){

 stopifnot(is.list(xlist))
 stopifnot(is.character(file_header))
 stopifnot(is.logical(append.query))
 stopifnot((is.na(file_paths)) | all(is.character(file_paths)))

 xx       <- lapply(xlist,  function(x){

  if(exists('start', where=x)){
   x$start <- NULL
  }
  if(exists('end', where=x)){
   x$end   <- NULL
  }


  return(lapply(x, dplyr::bind_rows))

 }) %>%  lapply(., function(x) lapply(x, function(x) {
  # xcolnames <- colnames(x)
  apply(t(x),1, collapse) %>%
   paste(names(.), . , sep = " = ") %>%
   c(file_header,.,
     "")
 }))

 if(all(!is.na(file_paths))){
  mapply(function(x, y) {write(x[[1]], y
                               , append = append.query)
  }, xx, file_paths) %>% invisible

 }else(return(xx))

}

#' Activate/Deactive individual cells from outside the XN5 gui
#'
#' A simple function that enables to rapiud selection/deselection of simulated cells in the xpn.
#'
#' @param xpn_file_path .xpn file in question, full path names ahas to be given.
#' @param xpi_activate character to specify the plant model. Supported: GECROS, GECROS_h, SPASS, CERES
#' @param flip character to specify the mineralisation model. Supported: DAISY and LEACHN.
#' @param xpn_file_path_out optional chaarcter to write a new xpn, default \code{NULL}, means overwriting of .xpn specified in \code{xpn_file_path}. Make sure to provide a full path, otherwise the new xpn will be written into the current working directory.
#' @return nothing. The function overwrites the xpn if \code{xpn_file_path_out} is not specified-.
#'
#' @author Tobias KD Weber , \email{tobias.weber@uni-hohenheim.de}
#'
#' @examples
#'
#' #' # xpn_modelActivate(xpn_file_path, xpi_activate = c("0_0_0_0.xpi", "0_1_0_0.xpi"), flip = TRUE)
#'
#' @export

xpn_modelActivate <- function(xpn_file_path, xpi_activate = "all", flip = FALSE, xpn_file_path_out = NULL){

  stopifnot(is.logical(flip))

  # xpn_file_path     chr     the name and path of the xpn file in question
  # xpi_activate      num     the names of the xpi to activate:
  # flip              chr     flip the selection of xpi_activate, i.e. enable deselction. Not possible to deselect all, as it is no use.

  xpn        <- readLines(xpn_file_path)
  xpn_detect <- stringi::stri_detect(str = xpn, regex = "grid")
  line.no    <- (1:length(xpn))[xpn_detect]
  xpn_split  <- xpn[xpn_detect] %>% str_remove(., "grid = ")

  xpn_split  <- stringr::str_split(xpn_split, ";") %>% unlist

  if( "all" %in% xpi_activate){
    xpn_split %<>% sub(., pattern = "^.", replacement = "1" )

  }else{
    if(isFALSE(flip)){
      query.replacement = 0
    }else{
      query.replacement = 1
    }

    xpn_split         %<>% sub(., pattern = "^.", replacement = query.replacement )
    nwhich            <- str_detect(xpn_split, paste(xpi_activate, collapse="|"))
    xpn_split[nwhich] %<>% sub(., pattern = "^.", replacement = as.numeric(!query.replacement) )
  }

  xpn[xpn_detect] <- paste0("grid = ", paste(xpn_split, collapse = ";"),";")


  if(!is.null(xpn_file_path)){
    xpn_file_path <- xpn_file_path_out
  }
  write(x = xpn, file = xpn_file_path)


}

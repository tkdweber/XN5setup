#' get XN3 weather file climate info
#'
#'Get XN3 climate information (Marker 20000) for XN5 cfg input.
#'
#' @param xn3_xnw character xn3.xnw. Careful of paths
#' @param path_in directory of XN3 weather files. If xn3_xnw is \code{NA}, xnw files in (sub-)directories are non-recursively searched and converted.
#'
#' @author Tobias KD Weber , \email{tobias.weber@uni-hohenheim.de}
#'
#' @examples
#'
#' @export

xn3xn5_climate <- function(xn3_xnw = NA, path_in = NA){

  stopifnot((!is.na(xn3_xnw) & is.na(path_in)) | (is.na(xn3_xnw) & !is.na(path_in)))

  stopifnot(is.na(path_in) | (is.character(path_in) & length(path_in) == 1))

 # load the xn5 header file
 if(is.na(path_in)){
  path_in <- getwd()
 }

 # if no files are supplied, then list.files
 if(all(is.na(xn3_xnw))){
  xn3_xnw <- list.files(path_in, recursive = FALSE, pattern = ".xnw", full.names = TRUE)
   }

 res     <- lapply(xn3_xnw, fread, skip = "20000", nrow = 1, header = TRUE) %>% do.call(what = "rbind")
 res[,1] <- str_split(xn3_xnw, "\\/") %>% do.call(what = "rbind") %>% .[, ncol(.)]
 res     <- res[,1:8] %>% set_colnames(c("station_name", "lat", "lon", "alt", "anual_mean_T", "anual_T_amp", "ws_H", "T_H"))
 # return only if desired
 return(res)


}




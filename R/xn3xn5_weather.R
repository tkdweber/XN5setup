#' convert XN3 weather files to XN5 weather files.
#'
#' Use to convert XN3 weather files (.xnw) to XN5 weather files (.csv).
#'
#' @param xn3_xnw character xn3.xnw. Careful of paths
#' @param print_to_file \code{TRUE} result is printed to XN5 file, \code{FALSE}, time series of the XN3 weather file are returned.
#' @param path_in directory of XN3 weather files. If xn3_xnw is \code{NA}, xnw files in (sub-)directories are non-recursively searched and converted.
#' @param path_out directory to store XN5 weather files. If \code{NA}, then directory specified in path_in is used, else, current directory.
#'
#' @author Tobias KD Weber , \email{tobias.weber@uni-hohenheim.de}
#'
#' @examples
#'
#' @export

xn3xn5_weather <- function(xn3_xnw = NA, print_to_file = TRUE, path_in = NA, path_out = NA){

 stopifnot((!is.na(xn3_xnw) & is.na(path_in)) | (is.na(xn3_xnw) & !is.na(path_in)))

 stopifnot(isTRUE(print_to_file) & is.character(path_out))

 stopifnot(is.na(path_out) | (is.character(path_out) & length(path_out) == 1))

 stopifnot(is.na(path_in) | (is.character(path_in) & length(path_in) == 1))

 if(!is.na(path_out)){stopifnot(dir.exists(path_out))}
 # load the xn5 header file
 # header_XN5weather <- data(header_xnweather)

 if(is.na(path_in)){
  path_in <- getwd()
 }

 if(is.na(path_out)){
  path_out <- path_in
 }

 xn3_xnw_old <- xn3_xnw
 if(all(!is.na(xn3_xnw_old))){

   xn3_xnw <- file.path(path_in, xn3_xnw)
   xn5_out <- xn3_xnw
 }

 # if no files are supplied, then list.files
 if(all(is.na(xn3_xnw_old))){

  xn3_xnw <- list.files(path_in, recursive = FALSE, pattern = ".xnw", full.names = TRUE)
  xn5_out <- list.files(path_in, recursive = FALSE, pattern = ".xnw", full.names = FALSE) %>% str_replace(.,".xnw", ".csv")
 }



 res    <- lapply(xn3_xnw, fread, skip = "20001") %>%

  lapply(., function(x){

   # make date
   dateDT <- data.table(Date = {x[,1] %>% unlist %>%
     sprintf(., fmt = "%04d") %>%
     paste0(.
            , x[,2] %>% unlist  %>%
             {ifelse(.>79, .+1900, .+2000)}) %>%
     dmy(.)
   }
   )
   # drop to many soil temperatures
   x[ ,(colnames(x)[1:3]) :=NULL]
   # order the columns
   setcolorder( x, c("V4", "V5", "V6", "V7", "V11", "V13", "V9", "V12", "V8", "V15", "V14", "V16", "V10", "V17", "V18", "V19"))
   x <- cbind(x, tempsoil= -99)
   #  merge data frame and rename the column headers
   data.table(dateDT, x ) %>% set_colnames(header_XN5weather) %>%
    return


  })

 if(isTRUE(print_to_file)){
  owd <- getwd()
  if(!is.na(path_out)){ setwd(path_out)}
  mapply(fwrite
         , file = xn5_out
         , x = res
  )
  setwd(owd)
 }

 # return only if desired
 if(isFALSE(print_to_file)){
  return(res)
 }

}


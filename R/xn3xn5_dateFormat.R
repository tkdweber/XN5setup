#' convert XN3 date format to XN5 date format
#'
#' Use to convert XN3 date format "ddmmyy" to XN5 "yyy-mm-dd"
#'
#' @param xn3_dates single numeric value or vector of numerical values.
#' @param cutoff In XN3, only 2 digits are used for the Year. This makes it complicated. \code{cutoff} is the two digit value,
#' after which all values are thought of belonging to the 20th centure (i.e. 19xx). Otherwise, it is interpreted to be a year from 2000.

#' @author Tobias KD Weber , \email{tobias.weber@uni-hohenheim.de}
#'
#' @examples
#' result <- xn3xn5_dateFormat(xn3_dates = 10100, cutoff = 79)
#' @export

xn3xn5_dateFormat <- function(xn3_dates = 10100, cutoff = 79){

 stopifnot(all(is.numeric(xn3_dates)))
 stopifnot(is.vector(xn3_dates))
 # convert
 xn3_dates %<>% sprintf(., fmt = "%06d")

 paste(    lapply(xn3_dates, substr,start=5,stop=6) %>% unlist %>%  as.numeric%>% {ifelse(.>cutoff, .+1900, .+2000)}
         , lapply(xn3_dates, substr,start=3,stop=4)
         , lapply(xn3_dates, substr,start=1,stop=2)
         , sep = "-") %>% return

}




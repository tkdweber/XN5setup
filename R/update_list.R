#' Update the DEFAULT settings of the xpi and xpn lists, fixed in various function.
#'
#' @description Intended for internal use.
#' @param x a list with named elements passed to the function. Will update opt.list accordingls.
#' @param opt.list named list to be updated by x
#'
#' @details Used in definition functions
#'
#' @return character vector, which can be concatenated to, e.g. the xpi and xpn definition files
#'
#' @author Tobias KD Weber , \email{tobias.weber@uni-hohenheim.de}
#'
#' @examples
#' \dontrun{
#' }
#' @export


update_list <- function(x, opt.list) {

 if(all(!is.na(x))){

  # add to default list
  stopifnot(is.list(x))
  if(any(!names(x) %in% names(opt.list))){
   x_short <-  rlist::list.remove(x ,   names(x)%in%names(opt.list))
   opt.list <- c(opt.list, x_short)
  }

  # update the default
  xnamex <- names(x)
  for(j in xnamex){
   opt.list[j] <- x[[j]]
   print(paste0("updated: ",  "'",names(opt.list[j]),"'", " to ", x[[j]]))
  }
 }

 opts <- paste(names(opt.list), do.call("rbind", opt.list), sep = " = ")

 return(opts)
}


#' Convert cfg, leachn, daisy_miner_nitrogen template lists to data.frame
#'
#' @description Intended for internal use.
#' @param x list, the modified cfg, leachn, daisy_miner_nitrogen template lists.
#'
#' @details Used to setup the variable which can be written to the _cfg.ini, _daisy_miner_nitrogen.ini, and _leachn.ini
#'
#' @return Character vector of merged elements seperated by sep.
#'
#' @author Tobias KD Weber , \email{tobias.weber@uni-hohenheim.de}
#'
#' @examples
#' \dontrun{
#' }
#' @export
#'
#'
merge_iniList <- function(x){

 stopifnot(is.list(x))
 x <- lapply(x, collapse)

 # merge list names and list elements
 lhs <- unlist(x) %>% names
 rhs <- unlist(x) %>% unname

 # workaround: Add a character, enabling deletion later
 rhs[rhs ==""] <- "="
 # merge the sides
 out <- paste(lhs, rhs , sep = " = ")
 # final remove the specific characters
 out <- stringr::str_replace(string = out, pattern = " = =", replacement = "")
 return(out)
}

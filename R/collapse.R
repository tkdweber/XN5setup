#' Collapse a vector to a string
#'
#' @description Intended for internal use.
#' @param x a 1 column or 1 row data.frame or data.table, or a vector with n elements.
#' @param sep the seperator of the collapsed elements
#'
#' @details Used to setup the cfg.ini, daisy_miner_nitrogen.ini, and leachn.ini
#'
#' @return character vector of collapsed elements seperated by sep.
#'
#' @author Tobias KD Weber , \email{tobias.weber@uni-hohenheim.de}
#'
#' @examples
#' \dontrun{
#' result <- collapse(c("f", "g"))
#' }
#' @export
#'
#'
collapse <- function(x, sep = ";"){
 paste(unlist(x), collapse = sep)
}

#' Read template files from the cfg_template folder in built
#'
#'@description The XN5 cfg_built folder contains template files to e.g. cfg.ini, leachn.ini, and
#'  daisy_miner_nitrogen.ini files. With this function, these files are read and made use of.
#'  As such, all comment lines in the template files (rows starting with #) are deleted.
#'
#'
#' @param path a character specifying the path to the template file location. Default to "./cfg_template".
#' @param file.name a character specifying the template file name.

#' @return A named list with list element names the variable names (left of the =-sign),
#' and the elements being the template elements to the right of the =-sign.
#' These can be replaced.
#'
#' @author Tobias KD Weber, \email{tobias.weber@uni-hohenheim.de}
#'
#' @examples
#' ## No example possible/sensble
#' @export
#' @importFrom stringr str_detect
#'

read_tpl <- function(path = "./cfg_template", file.name = NA){

 if(is.na(file.name)){
  myfile <- path
 }else{
  myfile <- file.path(path,file.name)
 }

 in.tpl <- readLines(myfile) %>%
  .[stringr::str_detect(., "#", negate = TRUE)] %>%
  .[. != ""] %>% stringr::str_split(., "=") %>% do.call("rbind", .)
 # index of [-occurence
 sel             <- stringr::str_detect(in.tpl, "\\[", negate = TRUE)
 # remove all entries with [] in second column
 in.tpl[!sel[1:(length(sel)/2)], 2] <- ""
 # remove all spaces
 in.tpl[sel]    <- gsub("[[:space:]]", "", in.tpl[sel])
 # convert to list
 out.tpl <- as.list(in.tpl[, 2]) %>% set_names(., value = in.tpl[, 1])

 return(out.tpl)
}




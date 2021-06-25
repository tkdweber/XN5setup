#' Split the .xpn (i.e. Master) file into several smaller projects
#'
#'@description The function enable to sperate the xpn grid into sub-elements, so that
#'
#'
#' @param xpn_file_path  .xpn path in question, full path names ahas to be given.
#' @param splits.no number of splits along the xpn.
#' @param chunks.len number of simulation units per xpn chunk
#'
#' @return NULL
#' @author Tobias KD Weber, \email{tobias.weber@uni-hohenheim.de}
#'
#' @examples
#' \dontrun{
#' # no sensible example available. Provide x.
#' }
#' @export
#'

xpn_modelCut <- function(xpn_file_path, splits.no = NULL, chunks.len = NULL ) {


  if(is.null(splits.no) & is.null(chunks.len)){stop("one of splits.no or chunks.len have to be numeric")}
  if(is.numeric(splits.no) & is.numeric(chunks.len)){warning("both splits.no and chunks.len provided as numeric, used splits.no")}
  # xpn_file_path     chr     the name and path of the xpn file in question
  # no.splits         num

  xpn       <- readLines(xpn_file_path)
  xpn_detect<- stringi::stri_detect(str = xpn, regex = "grid")
  line.no   <- (1:length(xpn))[xpn_detect]
  xpn_split <- xpn[xpn_detect] %>% str_remove(., "grid = ")

  xpn_split <- stringr::str_split(xpn_split, ";") %>% unlist

  if(is.numeric(splits.no)){

    splits.len      <- length(xpn_split)/splits.no
    xpn_split_sub   <- split(xpn_split, ceiling(seq_along(xpn_split)/splits.len))
    files_split_sub <- gsub(xpn_file_path, pattern = ".xpn", paste0(".xpn",1:splits.no))

  }else{
    xpn_split_sub   <- split(xpn_split, ceiling(seq_along(xpn_split)/chunks.len))
    files_split_sub <- gsub(xpn_file_path, pattern = ".xpn", paste0(".xpn",1:length(xpn_split_sub)))
  }

  for(i in 1:length(files_split_sub)){
    xpn[xpn_detect] <- paste0("grid = ", paste(xpn_split_sub[[i]], collapse = ";"),";")

    xpn <- str_replace_all(xpn, "/output", paste0("/output", i))

    write(x =xpn , file = files_split_sub[i])
  }

  return(NULL)

}

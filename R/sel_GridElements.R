#' Pre-select grid elements)
#' @description The function can be used to select individual elements from the stochastic 4D dimensions.
#'
#' @param data.vector character vector specifying the element names.
#' @param select.vector null, numbers or named elements of length > 1 to select from data.vector. These will be selected
#' using \code{sel$soilunit},\code{sel$weather},\code{sel$ptf},\code{sel$croprotation}. Character elements only sensible, if in data.vector character elements.
#'
#' @return returns a reduced data.vector, depending on select.vector
#' @export
#'
#' @examples
#' \dontrun{
#' # Difficiult to concrete examples
#' # res <- sel_GridElements(data.vector     = sel$BK50$soilunits
#' #        , select.vector = NULL )
#'
#' # res <- sel_GridElements(data.vector     = sel$BK50$soilunits
#' #        , select.vector =  1)
#'
#' # res <- sel_GridElements(data.vector     = sel$BK50$soilunits
#' #        , select.vector = c(1,2) )
#'
#' # res <- sel_GridElements(data.vector     = sel$BK50$soilunits
#' #        , select.vector = "g34" )
#'
#' # res <- sel_GridElements(data.vector     = sel$BK50$soilunits
#' #        , select.vector = c("g34". "g35") )
#'
#' }
#'
#'
sel_GridElements <- function(data.vector, select.vector){

 stopifnot(!all(is.numeric(select.vector)) | !all(is.character(select.vector)) | !is.null(select.vector))

 if(is.null(select.vector)){

  select.vector <- 1:length(data.vector)

  result <- data.vector[select.vector]

 }else{

  if(all(is.numeric(select.vector))){
   result <- data.vector[select.vector]
  }

  if(all(is.character(select.vector))){
   result <- data.vector[data.vector %in% select.vector]
  }

 }
 stopifnot(length(result) > 0)

 return(result)
}
attr(sel_GridElements, "purpose") <- rbind("[select element of z_x_y_tile]", paste(   "sel$soilunit "
                                                                          , "sel$weather"
                                                                          , "sel$ptf"
                                                                          , "sel$croproation"
                                                                          ,  sep = " | "))



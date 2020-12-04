#' Cap and Trade and Threshold and Trade Precipiation
#'
#'@description A function to produce lower and upper caps on the precipitation in XN5 weather files.
#' Overwrites former files. If a path is provided, and non-weather files are stored as .csv, it will not work.
#' Then, the explicit paths to the weather files need to be provided explicitly.
#'
#'
#' @param path a character specifying the path to the location of climate files. Can be folder or just one file
#' @param ncrit.up a numeric value which specifies the maximum precipitation (unit according to input file). If \code{NA}, cap not used
#' @param ncrit.lo a numeric value which specifies the minimum precipitation (unit according to input file), default \code{NA}, meaning the cap is not used.
#' @param query.recursive if a folder is given, option exists to look for sub-folders, default \code{FALSE}.

#' @return nothing, saves and overwrites weather files.
#'
#' @author Tobias KD Weber, \email{tobias.weber@uni-hohenheim.de}
#'
#' @examples
#' \dontrun{
#' path <-  "./"
#' cap_precip(path, ncrit.up = 20, ncrit.lo = .5)
#' }
#'
#' @export
#' @importFrom stringr str_detect
#'

cap_precip <- function(path, ncrit.up = 20, ncrit.lo = NA, query.recursive = FALSE) {

  # checks
  stopifnot(is.character(path))


  # get files
  if(isFALSE(file_test("-f", path))){
    input_files <- list.files(path, pattern = ".csv", recursive = query.recursive, full.names = TRUE)
  }else{
    input_files <- path
  }

  # length of for loop
  nj <- length(input_files)

  # set progressbar
  pb  <- txtProgressBar(min = 0, max = nj, initial = 0)
  pbj <- 0

  # for loop to correct
  for(j in 1:nj){

    pbj %<>%  "+"(1)
    setTxtProgressBar(pb, pbj)

    jdat <- data.table::fread(input_files[j])

    # the capping to a max
    nl.prec.v <- nrow(jdat)


    if(!is.na(ncrit.lo)){

      for(k in 1:(nl.prec.v-1)) {
        # LOWER CAP
        # only reassign rain to next day, if measured value > ncrit.
        if(jdat$'precipitation [mm]'[k] <= ncrit.lo) {

          jdat$'precipitation [mm]'[k + 1] <- jdat$'precipitation [mm]'[k + 1] + jdat$'precipitation [mm]'[k]
          jdat$'precipitation [mm]'[k]     <- 0
        }
      }# end of k loop
    }

    if(!is.na(ncrit.up)){
      for(k in 1:(nl.prec.v-1)) {

        # UPPER CAP
        # only reassign rain to next day, if measured value > ncrit.
        if(jdat$'precipitation [mm]'[k]>= ncrit.up) {
          jdat$'precipitation [mm]'[k+1] <- jdat$'precipitation [mm]'[k+1] + (jdat$'precipitation [mm]'[k] - ncrit.up)
          jdat$'precipitation [mm]'[k]   <- ncrit.up
        }

      }# end of k loop
    }

    data.table::fwrite(jdat, input_files[j])

  } # end of j loop

}





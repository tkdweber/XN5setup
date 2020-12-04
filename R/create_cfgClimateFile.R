#' Calculate Annual Average Temperature and Annual Monthly Temperature Amplitude
#'
#'@description The function is intended for use on the climate files in a given folder, to calculate the
#' \emph{AveYearTemp} and \emph{MonthTempAmp} for the cfg files. The output is stored in a file.
#'
#' @param path path to folder with csvs only climate files OR individual files
#' @param query.write defaults to \code{FALSE}. If TRUE a result file will be written , path_file_out = "cfgClimate.IN" a character specifying the path to the location of climate files. Can be
#' @param path_file_out name of the file
#' @return nothing, saves and overwrites weather files.
#'
#' @author Tobias KD Weber, \email{tobias.weber@uni-hohenheim.de}
#' @author Michelle Viswanathan, \email{michelle.viswanathan@uni-hohenheim.de}
#'
#' @examples
#' \dontrun{
#' path <- c("./100_485 .csv", "./60_518 .csv")
#' path <- c("./100_485 .csv")
#' path <- c("./")
#' create_cfgClimateFile(path, query.write = TRUE)
#' }
#' @importFrom lubridate year month
#' @export
#'

create_cfgClimateFile <- function(path, query.write = FALSE, path_file_out = "cfgClimate.IN"){

  stopifnot(is.logical(query.write))

  # get files
  if(isFALSE(file_test("-f", path))){
    input_files <- list.files(path, pattern = ".csv", full.names = TRUE)
    input_names <- list.files(path, pattern = ".csv", full.names = FALSE)
    out_path    <- file.path(path, path_file_out)
  }else{
    input_files <- path
print("here")
    # out_path    <- file.path(path, path_file_out)
   out_stub <- stringr::str_split(path, "/") %>% do.call(., what = "rbind")

   input_names  <- out_stub %>%  .[, ncol(out_stub)]
   outfilenames <- input_names %>% stringr::str_replace(., ".csv", "cfgClimate.ini")
   outpaths     <- out_stub %>%  .[, -ncol(out_stub)] %>% matrix(., ncol = ncol(out_stub)-1) %>% apply(., 1,function(x) paste(x, collapse = "/"))

   out_path <- file.path(outpaths, outfilenames)

  }

  AveYearTemp  <- list()
  MonthTempAmp <- list()

  x <- list()

  for (kweather in 1:length(input_files)){

    jdat <- data.table::fread(input_files[kweather])

    x$sim_start <- min(jdat$Date)
    x$sim_end   <- max(jdat$Date)

    # ensure complete years are considered only, but only if at least one full year is present in the data
    if((x$sim_end %>% year -
        x$sim_start %>% year) > 0){

      cut_lo <- as.Date(ISOdate(year(x$sim_start), 1, 1  ))
      cut_up <- as.Date(ISOdate(year(x$sim_end)  , 12, 31))

      sel_loc    <- jdat$Date >= cut_lo & jdat$Date <= cut_up

    }else{
      # this is, so that the code works
      sel_loc <- TRUE
      message("Attention: calculation of AveYearTemp and MonthTempAmp based on incomplete year information \n Adjust manually in cfg.ini")
      }

    dat_temp <- jdat[sel_loc, ]

    # average weather==> METHOD: Averages of annual averages
    AveYearTemp[[kweather]]  <- aggregate(dat_temp[, 9 ], list(dat_temp$Date%>%year), mean) %>% .[,2] %>% mean %>% round(.,2)

    # average weather ==> METHOD: Average of 50% of the difference
    a  <- aggregate(dat_temp[, 9], list(dat_temp$Date %>% month), min) %>% .[,2]
    b  <- aggregate(dat_temp[, 9], list(dat_temp$Date %>% month), max)%>% .[,2]
    MonthTempAmp[[kweather]]   <- mean((b - a)/2) %>% round(., 2)
  }


  AveYearTemp  %<>% unlist
  MonthTempAmp %<>% unlist

  out <- data.table("weatherID" = input_names, "AveYearTemp" = AveYearTemp, "MonthTempAmp" = MonthTempAmp)

  # names(AveYearTemp) <- names(MonthTempAmp) <- x$weather$file_name

  # unorthodox. because it checks if an object is in the global environment. Need to change this.
  # if(exists("weather", sel$soil)) {
  #
  #   if(length(sel$soil$weather) != length(AveYearTemp)){
  #
  #     DT <- merge(data.table::data.table("name" = sel$soil$weather)
  #                 , data.table::data.table("name" = names(AveYearTemp), "AveYearTemp" = AveYearTemp, "MonthTempAmp" =MonthTempAmp)
  #                 , by = "name")
  #
  #     AveYearTemp  <-  unlist(DT$AveYearTemp) %>% set_names(., sel$soil$weather )
  #     MonthTempAmp <-  unlist(DT$MonthTempAmp) %>% set_names(., sel$soil$weather )
  #
  #   }
  # return(list("AveYearTemp" = unlist(AveYearTemp), "MonthTempAmp" = unlist(MonthTempAmp)))


  if(isTRUE(query.write)){

    if(isFALSE(file_test("-f", path))){
      data.table::fwrite(out, file.path(path, path_file_out))
    }else{

      for(i in 1:nrow(out)){

        data.table::fwrite(x = out[i], file = out_path[i])
    }
  }
  }else{
    return(out)
}

}







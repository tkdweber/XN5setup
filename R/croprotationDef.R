#' Setup crop_rotations.ini for XN5, based on DEFAULT management information
#'
#'
#' @param x a character vector. Characters specify varieties, which should be incorporated
#' into the crop rotation. Varieties supported can be seen in the Details section.
#' The character vector will be recylced, until the end date, specified by the end arguments.
#' @param start character specifying the starting date of the crop rotation; format "yyyy-mm-dd": Has to be before the first sowing date corresponding to the first crop.
#' @param end character specifying the end date of the crop rotation; "yyyy-mm-dd": Set after to the last harvest date of the last crop.
#'
#' @details
#' Accepted varieties are "Kartoffel", "Wintergerste", "SilageKRG", "Winterweizen","Sommergerste", "Winterrape", "zuckerruebe1", "Ackersenf"
#'
#' @return A list with three elements vector which can be used to produce the crop_rotation.ini
#' \item{start}{The date of the start of the crop rotation}
#' \item{end}{The date of the end of the crop rotation}
#' \item{CR}{The data.frame of the crop rotation to be written to crop_rotation.ini}
#' @author Tobias KD Weber , \email{tobias.weber@uni-hohenheim.de}
#' @examples
#' # example 1
#' @export
croprotationDef <- function(x, start = "2014-08-1", end = "2019-08-1"){

 #' @importFrom magrittr "%>%"
 #' @importFrom data.table setnames
 #' @importFrom lubridate ymd

 # Initialise
 WinterCrop <- NULL

 start <- as.Date(start)
 end   <- as.Date(end)

 # add checks, i.e. no cover crops directly after SilageKRG (Maize) or zuckerruebe1 (Sugarbeet)
 if(is.data.frame(x)){x <- unlist(x)}

 #### HOW TO ADD CROPS (VARIETIES) ####
 # 0. Update CHECK BLOCK
 # 1. Add element to each of the variables in CROP BLOCK
 # 2. Add list element in lookup.c und lookup.l
 # 3. update counter.Wd | Ii specifies if a given crop runs over new year lines

 # When adding crops, add one element to each of the following lines.
 # Dates have to be specified based relative to 1970. In other words, set sow_date 1970-mm-dd, and the harvest +1year,
 # if it is a wintercrop.
 # Do not use 29th of Febuary!!!
 # Define the crops

 # CHECK BLOCK ------------
 {
  lookup.c            <- list()
  lookup.c[["PO"]]    <- "Kartoffel"
  lookup.c[["MZSB"]]  <- c("SilageKRG","zuckerruebe1")
  lookup.c[["WG"]]    <- c("Winterweizen","Wintergerste")
  lookup.c[["SG"]]    <- "Sommergerste"
  lookup.c[["WR"]]    <- "Winterrape"
  lookup.c[["CC"]]    <- "Ackersenf"
 }

 # PUT THIS INTO A .data file::: https://www.r-bloggers.com/no-visible-binding-for-global-variable/
 # CROP BLOCK DATA TABLE------------
 DT <- data.table::data.table(rbind(
  #                  Crop    Crop         sow_       variety       sow_    row_ sow   max_         max_    emerg_      harvest_      max_biom_     max_ro_  biom_   Winter
  #                  Code    Name         date                     density dist depth plant_height root    date        date          date          date     remove  Crop
  "Kartoffel"   =c("PO" ,"Potato"   ,"1970-05-10","Kartoffel"    ,  6     , 60 ,  10,   50        ,150 ,"1970-05-25" ,"1970-08-29" ,"1970-08-29" ,"1970-08-29" ,0  , 0    ),
  "Wintergerste"=c("BA" ,"Barley"   ,"1970-10-04","Wintergerste" ,  400   , 12 ,  3 ,   100       ,150 ,"1970-09-22" ,"1971-07-26" ,"1971-07-26" ,"1971-07-26" ,0  , 1    ),
  "SilageKRG"   =c("MZ" ,"Maize"    ,"1970-04-18","SilageKRG"    ,  13    , 70 ,  5 ,   300       ,150 ,"1970-04-18" ,"1970-10-01" ,"1970-10-01" ,"1970-10-01" ,1  , 0    ),
  "Winterweizen"=c("WH" ,"Wheat"    ,"1970-10-15","Winterweizen" ,  390   , 12 ,  3 ,   100       ,150 ,"1970-11-23" ,"1971-08-15" ,"1971-08-15" ,"1971-08-15" ,0  , 1    ),
  "Sommergerste"=c("BA" ,"Barley"   ,"1970-03-21","Sommergerste" ,  380   , 12 ,  3 ,   100       ,150 ,"1970-03-21" ,"1970-08-14" ,"1970-08-14" ,"1970-08-14" ,0  , 0    ),
  "Winterrape"  =c("RP" ,"Rapeseed" ,"1970-08-24","Winterrape"   ,  63    , 24 ,  2 ,   150       ,150 ,"1970-08-24" ,"1971-08-02" ,"1971-08-02" ,"1971-08-02" ,0  , 1    ),
  "zuckerruebe1"=c("SB" ,"SugarBeet","1970-04-10","zuckerruebe1" ,  11    , 45 ,  5 ,   60        ,200 ,"1970-04-30" ,"1970-10-15" ,"1970-06-06" ,"1970-06-06" ,0  , 0    ),
  "Ackersenf"   =c("WM" ,"CoverCrop","1970-09-05","Ackersenf"    ,  300   , 12 ,  2 ,   120       ,150 ,"1970-09-19" ,"1971-02-15" ,"1971-02-15" ,"1971-02-15" ,0  , 1    )
 )); data.table::setnames(DT, c("CropCode" ,"CropName" ,"sow_date" ,"variety",  "sow_dens"
                                ,"row_dist" ,"sow_depth" ,"max_plant_height" ,"max_root"
                                ,"emerg_date" ,"harvest_date" ,"max_biom_date" ,"max_ro_date" ,"biom_remove","WinterCrop"))

 # selector where the dates are:

 date.names <-  names(DT)[stringr::str_detect(names(DT), "date")]

 # counter for the Wintercrops
 counter.Wd <- DT[WinterCrop == 1, "variety"]$variety # new with DT
 # ------------------------------------------------- -------
 #### CREATING THE CROP ROTATION ####

 ## INITIALISE
 # look up (for positions of colums)
 lookup.l <- as.list( DT[,"variety"][[1]])

 # the lookup
 sel <- as.integer(qdapTools::lookup(x, lookup.l))
 # ensure variety vector is at least of length 2.
 if(length(sel) == 1){sel <- c(sel, sel)}
 CR.sel <- as.data.frame(DT[sel])

 # break-out date counter for while loop
 max.date <-  CR.sel[, "harvest_date"] %>% ymd %>% max
 # time counter
 dtime <- lubridate::year(start) - 1970
 # index counters
 j <- 1
 i <- 1
 # results storage
 out.l <- list()

 # Construct the crop rotation
 while(end > max.date)
 {
  # Correct the DEFAULT dates, according to arguments
  df.int <- CR.sel[i,]
  df.int[, date.names] <- as.character(lubridate::ymd(df.int[, date.names]) + lubridate::years(dtime))

  # add index so long until the end of the variety vector, then restart.
  i_old <- i
  if(i < nrow(CR.sel)){
   i <- i + 1
  }else{i <- 1}

  out.l[[j]] <- df.int

  if(CR.sel[i_old, "variety"] %in% counter.Wd & CR.sel[i, "variety"]%in%counter.Wd |
     !CR.sel[i_old, "variety"] %in% counter.Wd & !CR.sel[i, "variety"]%in%counter.Wd ){
   dtime <- dtime + 1
  }
  if(!CR.sel[i_old, "variety"] %in% counter.Wd & CR.sel[i, "variety"]%in%counter.Wd){
   dtime <- dtime
  }
  if(CR.sel[i_old, "variety"] %in% counter.Wd & !CR.sel[i, "variety"]%in%counter.Wd){
   dtime <- dtime + 2
   # remedy for CoverCrops, which are wintercrops, but enable a new seeding of summer crops immediate with no year break
   if(CR.sel[i_old, "CropName"] == "CoverCrop"){
    dtime <- dtime - 1
   }
  }

  # update max.time
  max.date <- CR.sel[, "harvest_date"] %>% as.matrix %>% ymd %>% max + lubridate::years(dtime)

  # j counter for the list.
  # Fail safe implemented ensures, break out from loop.
  stopifnot(j < 200)
  j <- j + 1
 };

 # update the end date, to return and give back for _cfg.ini
 end <- max.date

 out.x <- do.call("rbind", out.l)["variety"]
 # check_ini <- qdapTools::lookup(x, lookup.c)
 # check <- c(check_ini, check_ini[1])
 check <- qdapTools::lookup(out.x, lookup.c)

 # check
 for (i in 1:length(check)){
  check.mod <- check[i]
  switch(check.mod,
         "CC" = if(check[(i)] %in% c("MZSB")){stop(paste(check[i+1], "may not follow", check[(i)]))}
         , "WR" = if(check[(i)] %in% c("PO"))  {stop(paste(check[i+1], "may not follow", check[(i)]))}
         #, "WG" = if(check[(i-1)] %in% c("MZSB")){stop(paste(check[i], "may not follow", check[(i-1)]))}
  )
 }

 return(list("CR" = out.l, "end" = end, "start" = start))

}
attr(croprotationDef, "varieties") <- rbind("[varieties included]", paste("Winterweizen", "Sommergerste"
                                                                          , "Winterrape", "SilageKRG"
                                                                          , "SilageKRG", "Kartoffel"
                                                                          , "Ackersenf"
                                                                          , "zuckerruebe1", "Wintergerste", sep = " | "))

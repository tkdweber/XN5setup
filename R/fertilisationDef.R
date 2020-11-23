#' Setup fertilisation.ini for XN5, based on DEFAULT fertilisation information and a prescribed
#' crop rotion dataframe from croprotationDef()
#'
#' @param x the crop rotation data.frame from croprotationDef()

#' @details
#' Accepted varieties are "Kartoffel", "Wintergerste","SilageKRG" ,"Winterweizen","Sommergerste","Winterrape","zuckerruebe1","Ackersenf"
#'
#' @return
#' \item{fertilisation}{A dataframe with information on fertilisation for a given crop.}
#'
#' @examples
#' # example 1
#' @export

fertilisationDef <- function(x){

out.l <- list()

 #### ARGUMENT

 DT_min <- data.table::data.table(rbind(
  #                    "variety"     , "fert_date" , "fertilizer"                  , "code" , "n_tot_min","no3n" ,"nh4n" ,"urea"
   "Winterweizen" <- c("Winterweizen","1971-03-14" ,"NPK15/13/13"                  ,"FE034" ,60        ,30   ,30   ,0    ),
   "Winterweizen" <- c("Winterweizen","1971-04-29" ,"Harnstoff"                    ,"FE005" ,72        ,0    ,0    ,72   ),
   "Winterweizen" <- c("Winterweizen","1971-06-01" ,"Kalkammonsalpeter"            ,"FE011" ,54        ,27   ,27   ,0    ),
   "Sommergerste" <- c("Sommergerste","1970-03-19" ,"NPK+S15/13/13/5"              ,"FE034" ,45.0      ,22.5 ,22.5 ,0.0  ),
   "Sommergerste" <- c("Sommergerste","1970-04-20" ,"Ammonnitrat-Harnstoff-Loesung","FE010" ,56        ,14   ,14   ,28   ),
   "Winterrape"   <- c("Winterrape"  ,"1970-08-20" ,"Kalkammonsalpeter"            ,"FE011" ,40        ,20   ,20   ,0    ),
   "Winterrape"   <- c("Winterrape"  ,"1971-03-24" ,"Ammonsulfatsalpeter"          ,"FE003" ,107       ,27   ,80   ,0    ),
   "Winterrape"   <- c("Winterrape"  ,"1971-04-07" ,"Harnstoff"                    ,"FE005" ,140.3     ,0    ,0    ,140.3),
   "SilageKRG"    <- c("SilageKRG"   ,"1970-04-28" ,"NPK_Nitrop."                  ,"FE034" ,48        ,24   ,24   ,0    ),
   "SilageKRG"    <- c("SilageKRG"   ,"1970-05-18" ,"Harnstoff"                    ,"FE005" ,150       ,0    ,0    ,150  ),
   "Kartoffel"    <- c("Kartoffel"   ,"1970-05-15" ,"Ammonnitrat-Harnstoff-Loesung","FE010" ,75        ,37.5 ,37.5 ,0    ),
   "Kartoffel"    <- c("Kartoffel"   ,"1970-02-06" ,"Ammonnitrat-Harnstoff-Loesung","FE010" ,75        ,37.5 ,37.5 ,0    ),
   "zuckerruebe1" <- c("zuckerruebe1","1970-04-23" ,"AHL"                          ,"FE011" ,72        ,18   ,18   ,36   ),
   "zuckerruebe1" <- c("zuckerruebe1","1970-06-06" ,"AHL"                          ,"FE011" ,54        ,13.5 ,13.5 ,27   ),
   "Wintergerste" <- c("Wintergerste","1971-03-10" ,"Kalkammonsalpeter"            ,"FE011" ,59        ,29.5 ,29.5 ,0    ),
   "Wintergerste" <- c("Wintergerste","1971-04-05" ,"Kalkammonsalpeter"            ,"FE011" ,54        ,27   ,27   ,0    )))
 setnames(DT_min, c("variety", "date", "fertilizer" , "code", "n_tot_min", "no3n", "nh4n", "urea"))


 DT_org <- data.table::data.table(rbind(
  #                   "variety"    ,"fert_date"  ,"fertilizer"    ,"code"  ,"amount","dry_matter","org_subst","n_tot_org","nh4n"
  "Winterweizen" <- c("Winterweizen","1971-03-26" ,"Biogasslurry"  ,"RE006" ,    1   ,1200        ,876        ,86         ,52    ),
  "SilageKRG"    <- c("SilageKRG"   ,"1970-04-16" ,"Rinderguelle_5","RE005" ,    1   ,1461        ,1052       ,38         ,67    ),
  "Wintergerste" <- c("Wintergerste","1970-10-18" ,"Biogasslurry"  ,"RE006" ,    1   ,1438        ,1035       ,65         ,37    ),
  "Wintergerste" <- c("Wintergerste","1971-04-05" ,"Rinderguelle_5","RE005" ,    1   ,2411        ,1735       ,109        ,62    )))
 setnames(DT_org, c("variety", "date"  ,"fertilizer"    ,"code"  ,"amount","dry_matter","org_subst","n_tot_org","nh4n"))

 out.l$min <- update_time(x = x, DT = DT_min)
 out.l$org <- update_time(x = x, DT = DT_org)
 return(out.l)

}
attr(fertilisationDef, "min") <- rbind("[varieties included]", paste("Winterweizen", "Sommergerste"
                                   , "Winterrape", "SilageKRG"
                                   , "SilageKRG", "Kartoffel"
                                   , "zuckerruebe1", "Wintergerste", sep = " | "))

attr(fertilisationDef, "org") <- c("Winterweizen", "SilageKRG", "Wintergerste")

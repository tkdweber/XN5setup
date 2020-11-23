#' Setup schaaf_manag.ini for XN5, based on DEFAULT management information and a prescribed
#' crop rotion dataframe from croprotationDef()
#'
#' @param x the crop rotation data.frame from croprotationDef()

#' @details
#' Accepted varieties are "Kartoffel", "Wintergerste","SilageKRG" ,"Winterweizen","Sommergerste","Winterrape","zuckerruebe1","Ackersenf"
#'
#' @return
#' \item{schaaf_manag}{A dataframe with information on the schaaf_manag for a given crop}.
#'
#' @examples
#' # example 1
#' @export

schaaf_managDef <- function(x){

 out.l <- list()
 # this can be replaced by readin from Christian Troosts mySQL
 DT_manag <- data.table::data.table(rbind(
                     # variety		    ,date		       ,implement	            ,code		  ,depth
"Winterweizen"		<- c("Winterweizen"	,"1970-10-02"	,"Scheibenegge"			     ,"TI031"	,12),
"Winterweizen"		<- c("Winterweizen"	,"1971-03-27"	,"Scheibenegge"			     ,"TI031"	,12),
"SilageKRG"	    <- c("SilageKRG"	   ,"1970-04-15"	,"Volldrehpflug"			    ,"TI005"	,25),
"SilageKRG"		   <- c("SilageKRG"		  ,"1970-04-17"	,"Saatbettkombination"	,"TI032"	,8 ),
"Ackersenf"	  	 <- c("Ackersenf"	  	,"1970-08-27"	,"Schaelgrubber"			    ,"TI010"	,15),
"Ackersenf"		   <- c("Ackersenf"		  ,"1971-02-16"	,"Schaelgrubber"			    ,"TI010"	,15),
"zuckerruebe1"	 <- c("zuckerruebe1"	,"1970-04-01"	,"Volldrehpflug"			    ,"TI005"	,20),
"Sommergerste"	 <- c("Sommergerste"	,"1970-03-25"	,"Saatbettkombination"	,"TI032"	,8 ),
"Sommergerste"	 <- c("Sommergerste"	,"1970-04-03"	,"Scheibenegge"			     ,"TI031"	,6 ),
"Kartoffel"		   <- c("Kartoffel"		  ,"1970-05-14"	,"Schaelgrubber"		     ,"TI010"	,5 ),
"Kartoffel"		   <- c("Kartoffel"		  ,"1970-06-02"	,"Schaelgrubber"		     ,"TI010"	,5 ),
"Winterrape"		  <- c("Winterrape"		 ,"1970-08-25"	,"Scheibenegge"			     ,"TI031"	,15),
"Winterrape"		  <- c("Winterrape"		 ,"1970-08-28"	,"Saatbettkombination"	,"TI032"	,8 ),
"Wintergerste"	 <- c("Wintergerste"	,"1970-09-22"	,"Saatbettkombination"	,"TI032"	,8 ),
"Wintergerste"	 <- c("Wintergerste"	,"1970-10-19"	,"Scheibenegge"			     ,"TI031"	,5 ),
"Wintergerste"	 <- c("Wintergerste"	,"1971-04-06" ,"Scheibenegge"			     ,"TI031"	,5 )))
setnames(DT_manag, c("variety"		,"date"		,"implement"	,"code"		,"depth"))

out.l$manag <- update_time(x = x, DT = DT_manag)

return(out.l)

}
attr(schaaf_managDef, "varieties") <- rbind("[varieties included]", paste("Winterweizen", "Sommergerste"
                                                                          , "Winterrape", "SilageKRG"
                                                                          , "SilageKRG", "Kartoffel"
                                                                          , "zuckerruebe1", "Wintergerste", sep = " | "))



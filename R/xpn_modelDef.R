#' setup XPN model line (the grid)
#' @description Defaults to all active
#' A simple function that sets up the [model] grid = line in an .xnp file.
#' Default and only setup form is:
#' grid coordinate j ('x') for the soil units, e.g. BK50, BÜK200, BÜK1000,
#' grid coordinate i ('y') for simple stochastification, e.g. PTFs
#' grid stack number ('z') the number of weather scenarios.
#' tile element ('tile') for the crop rotations
#'
#' @param x a list with named elements. The names of the elements will be used to construct the grid layout in the .xpn.
#' All grid cells are activated by default. See \code{details}
#' @param PROJECT_NAME character specifying the name of the project. May be included in x, instead.
#'
#' @details
#' x$weather_NAMES  the names of the different weather scenarios OR the integer number of grid layers
#' x$soil_unit_NAMES the names of the different soil units to be used OR the total number
#' x$stoch_NAMES the names of the different stochastic untis to be used (e.g. number of PTF) OR the total number
#' x$CR_NAMES the names of tiles
#'
#' Alternative to the vector of names, the integer number of unique elements can be given in x.
#'
#' @return The grid to be created in the .xnp file
#'
#' @author Tobias KD Weber , \email{tobias.weber@uni-hohenheim.de}
#'
#' @examples
#' ## example 1
#' x <- list()
#' x$PROJECT_NAME    <- "test_XN5setup"
#' x$soil_unit_NAMES <- 10L
#' x$stoch_NAMES     <- 3L
#' x$weather_NAMES   <- 2L
#' x$CR_NAMES        <- 3L
#' res <- xpn_modelDef(x)
#' @export

xpn_modelDef <- function(x, PROJECT_NAME = NA){

 if(!is.na(PROJECT_NAME)){
  x$PROJECT_NAME <- PROJECT_NAME
 }

 # check that the project name exists
 stopifnot(exists("PROJECT_NAME", where = x) & is.character(x$PROJECT_NAME) )

 # check that at least one dimension is set
 if(sum(c(exists("soil_unit_NAMES", where=x),
          exists("stoch_NAMES", where=x),
          exists("weather_NAMES", where=x),
          exists("CR_NAMES", where=x)))==0){
  stop("Error: at least one dimensional argument has to be specified")}

 if(exists("CR_NUMBER", where=x)){}

 # set x_coordinate ('i' in XN5)
 x_coords <- (-1)+if(exists("soil_unit_NAMES", where = x)){
  stopifnot(is.character(x$soil_unit_NAMES)| x$soil_unit_NAMES%%1==0)
  if(is.character(x$soil_unit_NAMES)){
   1:length(x$soil_unit_NAMES) }
  if(x$soil_unit_NAMES%%1==0){
   1:x$soil_unit_NAMES}
 }else{1}

 # set y_coordinate ('j' in XN5)
 y_coords <- (-1)+if(exists("stoch_NAMES", where = x)){
  stopifnot(is.character(x$stoch_NAMES)| x$stoch_NAMES%%1==0)
  if(is.character(x$stoch_NAMES)){
   1:length(x$stoch_NAMES) }
  if(x$stoch_NAMES%%1==0){
   1:x$stoch_NAMES }
 }else{1}

 # set z_coordinate (grid_ID in XN5)
 z_coords <- (-1)+if(exists("weather_NAMES", where = x)){
  stopifnot(is.character(x$weather_NAMES)| x$weather_NAMES%%1==0)
  if(is.character(x$weather_NAMES)){
   1:length(x$weather_NAMES) }
  if(x$weather_NAMES%%1==0){
   1:x$weather_NAMES }
 }else{1}

 tile_coords <- (-1)+if(exists("CR_NAMES", where = x)){
  stopifnot(is.character(x$CR_NAMES) | x$CR_NAMES%%1==0)
  if(is.character(x$CR_NAMES)){
   1:length(x$CR_NAMES) }
  if(x$CR_NAMES%%1==0){
   1:x$CR_NAMES }
 }else{1}

#print(tile_coords)
#print(length(tile_coords))

 tile_fraction <- rep(signif(100/length(tile_coords), 4), length(tile_coords))

 # correction, because XN cannot handle tile sum != 100.
 tile_fraction_sum <- sum(tile_fraction)
# print(tile_fraction_sum)
 if( tile_fraction_sum != 100) {

  tile_fraction[1] <- ifelse(tile_fraction_sum > 100, signif(tile_fraction[1] - abs((100 - tile_fraction_sum)), 4), signif(tile_fraction[1] + abs((100 - tile_fraction_sum)), 4))

 }
 #print(tile_fraction)

 # TEMPLATE
 # activate,grid,x,y      grid,x,y,tile
 # grid = 1,0,0,0,
 # $PROJECT_PATH/$PROJECT_NAME_
 # 0_0_0_0.xpi,100;

 PROJECT_NAME <- if(exists("PROJECT_NAME", where = x)){
  x$PROJECT_NAME
 }else{
  "$PROJECT_NAME"
 }
 sep1 <- ifelse(stringr::str_sub(PROJECT_NAME, -1) != "_", "_","" )

 PROJECT_PATH <- if(exists("PROJECT_PATH", where = x)){
  paste0("$", x$PROJECT_PATH)
 }else{
  "$PROJECT_PATH"
 }

 res.l <- list()
 item3.l <- list()
 counter <- 0
 for(z.it in z_coords){
  for(x.it in x_coords){
   for(y.it in y_coords){
    for(tile.it in tile_coords){
     counter <- counter +1
     # grid ID X cell ID pointer
     if(tile.it==0){
      item1 <- paste0(paste("1", z.it, x.it, y.it, sep = ","),",")
     }else{item1=","}

     item2 <- paste(PROJECT_PATH,"/",PROJECT_NAME, sep1, sep = "")

     # item3 <- paste(z_coords[z.it]
     #                , x_coords[x.it]
     #                , y_coords[y.it]
     #                , tile_coords[tile.it]
     #                , sep = "_")
     item3 <- paste(z.it
                    , x.it
                    , y.it
                    , tile.it
                    , sep = "_")
     if(max(tile_coords) == tile.it){
      addc = ";"
     }else{addc = ""}
     item3.l[[counter]] <- item3
     item4 <- paste0(".xpi",",", tile_fraction[(tile.it+1)], addc)

     item12 <- paste0(item1,item2, sep = "")
     item34 <- paste0(item3,item4)

     item1234 <- paste0(item12,item34)
     res.l[[counter]] <- item1234


    } # end tile_coords
   } # end y_coords
  } # end x_coords
 } # end z_coords

 # grid element
 res <- c("[model]",
          paste0("grid = " , do.call("paste0", res.l)),
          ""
 )

 item3.res <- paste0(PROJECT_NAME,"_", item3.l, ".xpi")

 return(list("modelDef" = res, "xpi_names" = item3.res))
}

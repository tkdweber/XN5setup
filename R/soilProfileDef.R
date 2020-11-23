#' Calculate depth distributions
#'
#' Synthetic distribution of carbon and nitrogen contents in soilscontent with depth
#'
#' @param zdepth depth of the soil profile [cm].
#' @param layer_thickness thickness of the simulation layers.
#' @param pars list of three named elements. Parameters determining the shape of the profile distribution.
#'#' \describe{
#'   \item{Adrepth}{depth of A horizon [cm] }
#'   \item{n}{sharpness of drop at Adepth [-]}
#'   \item{fact}{factor by which the entire profile (top = 1), is multiplied. For example by a humus content [-]}
#' }
#' @param plot.query      query if a plot of the depth profile should be made. TRUE = plot is provided. FALSE, no plot.
#' @param rescale.query   query if the matter content in the soil should be rescaled to 0 at the bottom. TRUE, rescales.
#'
#'
#' @details Used in the functions schaaf_managDef and in fertilisationDef
#'
#' @return Vector of calculated values ordered from top to bottom, in depth increments provided by \code{layer_thickness}
#'
#' @author Tobias KD Weber , \email{tobias.weber@uni-hohenheim.de}
#'
#' @examples
#' \dontrun{result <- soilProfileDef(200, plot.query = TRUE, rescale.query = FALSE)}
#' @export
#' @importFrom graphics abline plot
#'

soilProfileDef <- function(zdepth, layer_thickness = 5, pars = list("Adepth" = 25, "n" = 2, "fact" = 1), plot.query = FALSE, rescale.query = FALSE){

 # assign variables
 Adepth   <- pars$Adepth                  # approximately depth of Ap horizon expressed by: log10(alf1)*lyr.incr
 hc.stoch <- pars$fact
 n        <- pars$n

 # calculate variables
 zmax     <- zdepth-layer_thickness*.5  # [cm] length of simulation profile
 zini     <- layer_thickness*.5         # [cm] length of simulation profile
 lyr.no   <- zdepth/layer_thickness     # [-] number of simulation lyrs
 z        <- -seq(zini, zmax, length = lyr.no)

 # check
 if((zmax + layer_thickness*.5)%%lyr.no != 0){stop("Either change zmax or lyr.incr. Current setting does permit 5 cm lyrs")}

 # full conversion factor
 #          [%] -> [-] | [cm]->[m] | [g]->[kg] |
 # convfac <-   1e-2   *    1e-2   *    1e3    * 1e4    # [-]

 alf <- (10^(- Adepth/layer_thickness))^(1/(2.1 + 0.01 * Adepth))
 #      (10^(-Adepth/lyr.incr))^(1/(2.1+0.01*Adepth))


 # Function to calculate C with depth
 # cdepth <- function(x){(log(exp(1) + (alf1 * x)^n))^-(1-1/n)
 # }

 # update z
 z.calc <- z/100 * 6.8
 x      <- 10^abs(z.calc)
 u      <- (log(exp(1) + (alf * x)^n))^-(1-1/n)

 # Rescaling. Ensures bottom of profile to contain no Corg.
 cdepth.eff  <- if(isTRUE(rescale.query)){
  u0 <- u[length(x)]
  u <- (u - u0)/(1 - u0)
  u
 }else{u}

 cdepth.full <- cdepth.eff
 cprofile    <- hc.stoch*cdepth.full

 if(isTRUE(plot.query)){
  plot(cprofile, z, xlim = c(0,4))
  abline(h = -Adepth)
 }

 return(cprofile)
}
soilProfileDef(200, plot.query = TRUE, rescale.query = FALSE)

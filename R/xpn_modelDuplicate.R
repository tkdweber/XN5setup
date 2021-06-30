#' Duplicate an m-dimensional grid to make a m(+1)-dimensional grid
#'
#' This function is used for setting up massively parallel inverse modelling projects (e.g. on bwHPC).
#' Ensure your project has columns .xpi-names with the format grid_i_j_tile (0_0_0_0). Else the code will not work.
#'
#' @param xpn_file_path .xpn path in question, full path names ahas to be given.
#' @param Npar Number of repetitions.
#' @param dim_extend in which dimension to extend the xpn project.
#' @param crop_ini_file_name the crop file name in the xpi file.
#' @details \code{dim_query} has to be a numeric or integer of 1, 2, 3, 4. Alternatively,
#' a character \code{grid}, \code{i}, \code{j}, \code{tile}. It specifies into which direction, an n-dimensional existing grid
#' will be expanded. Example: An XN5 grid which extends into 1D with J simulation columns on the 4D grid:  grid, i, j, tile,
#' would mean grid = 1, i= 1, j = J, tile = 1, in short, 1,1,J,1. This can now be conviently be extended by \code{Npar}
#' into any dimension specified in \code{dim_query}. If we extend it into the direction of i, by setting Npar = I, the result is an
#' grid = 1, i= I, j = J grid, i.e. 1,I,J,1. Careful, intuition has it, that i, j, would make a MxN matrix, in the sence of M signifies the
#' rows, and N signifies the columns, but in fact, it is flipped.
#' @return vector with parameter file names of dimension 1 x Npar.
#'
#' @author Tobias KD Weber , \email{tobias.weber@uni-hohenheim.de}
#'
#' @examples
#'
#' # xpn_modelActivate(xpn_file_path, xpi_activate = c("0_0_0_0.xpi", "0_1_0_0.xpi"), flip = TRUE)
#'
#' @export

xpn_modelDuplicate <- function(xpn_file_path, Npop = 10, dim_extend, crop_ini_file_name = "wheat_winterweizen_gecros"){

  # xpn_file_path <- "C:/Projects/ExpertN/expertn514e/built/cfg/replicateECv11_flat/replicateECv11.xpn"
  # dim_extend    <- 2
  # Npar          <- 3
  # crop_ini_file_name = "wheat_winterweizen_gecros"
  Npar <- Npop
  # CHECKS
  xpi_path      <- strsplit(xpn_file_path, "/") %>% unlist %>%  .[-length(.)] %>% paste(., collapse = "/")

  if(dim_extend == "tile" | dim_extend == 4) {stop("method not implemented for tile.")}

  # convert character dim_extend to a numeric
  if(is.character(dim_extend)){
    dim_extend <- switch(dim_extend,
                         "grid" = 1,
                         "i"    = 2,
                         "j"    = 3,
                         "tile" = 4
    )
  }

  # READ CURRENT XPN -----
  xpn        <- readLines(xpn_file_path)
  xpn_detect <- stringi::stri_detect(str = xpn, regex = "grid")
  line.no    <- (1:length(xpn))[xpn_detect]
  xpn_split  <- xpn[xpn_detect] %>% str_remove(., "grid = ")

  xpn_split  <- stringr::str_split(xpn_split, ";") %>% unlist
  # remove empties
  xpn_split  <- xpn_split[xpn_split != ""]

  # more checks
  if(!all(grepl("[[:digit:]]\\_[[:digit:]]\\_[[:digit:]]\\_[[:digit:]]", xpn_split))){stop("not all xpi in correct names format\n must be xpn_split grid_i_j_tile")}

  # get the xpi elements
  xpi_ALL  <- strsplit(xpn_split, "\\/|.xpi")
  xpi_ROOT <-  lapply(xpi_ALL, "[[", length(xpi_ALL[[1]])-1)  %>% unlist

  # STRING POSITIONS
  # start
  nstart <- gregexpr("\\_[[:digit:]]\\_[[:digit:]]\\_[[:digit:]]", xpi_ROOT ) %>% unlist %>% "+"(1)
  # end
  nend   <- gregexpr("[[:digit:]]\\_[[:digit:]]\\_[[:digit:]]\\_[[:digit:]]",xpi_ROOT ) %>% lapply(., "attr", "match.length") %>% unlist %>%  "+"(nstart-1)

  # XPN_GRID -----
  # make grid elements
  NxpiL  <-  list()
  for(nxpi in 1:length(xpi_ROOT)){
    NxpiL[nxpi] <-  substr(xpi_ROOT[nxpi], nstart[nxpi], nend[nxpi])
  }
  NxpiL %<>% unlist
  NxpiL %<>% strsplit(., "_")
  # loop over the dimensions
  Nxpi_Npar_L <-  lapply(NxpiL, function(x){
    # print(x)
    X <- lapply(1:Npar, function(y){
      x[dim_extend] <- y
      return(paste(x, collapse = "_"))
    }
    )
    return(unlist(X))
  }
  )

  # string manipulations (defaults to activate grid element)
  Nxpi_Npar_v     <- unlist(Nxpi_Npar_L)
  Nxpi_Npar_front <- strsplit(Nxpi_Npar_v, "_")     %>% do.call(what = "rbind")
  Nxpi_Npar_front <- cbind(1, Nxpi_Npar_front[,-4]) %>% apply(., 1, paste, collapse = ",")
  xpi_merge       <- strsplit(xpn_split, "\\/|[[:digit:]]\\_[[:digit:]]\\_[[:digit:]]\\_[[:digit:]].xpi")[[1]]
  front           <- xpi_merge[1]
  front_ROOT      <- substr(front, 8, nchar(front))
  project_NAME    <- xpi_merge[(length(xpi_merge)-1)]

  # ASSEMBLE GRID
  new_grid        <- paste0(Nxpi_Npar_front, front_ROOT,"/", project_NAME, Nxpi_Npar_v, ".xpi", tail(xpi_merge,1))
  xpn[xpn_detect] <- paste0("grid = ", paste(new_grid, collapse = ";"),";")

  # XPN_WRITE ----
  data.table::fwrite(x = data.table::as.data.table(xpn), file = xpn_file_path, quote = FALSE, col.names = FALSE)

  #
  ## XPI ---------
  #
  Nxpi_v       <- lapply(NxpiL, paste0, collapse = "_") %>% unlist
  xpi_ROOT     <- paste0(project_NAME, Nxpi_v, ".xpi")

  # NEW XPI FILE NAMES
  write_out_m  <- matrix(Nxpi_Npar_v, nrow = Npar)
  write_out_m  <- paste0(file.path(xpi_path, project_NAME) , write_out_m, ".xpi") %>% matrix(nrow = Npar)

  xpi_ROOT_len <- length(xpi_ROOT)
  for(n_xpi_ROOT in 1:xpi_ROOT_len){

    xpi_in               <- readLines(file.path(xpi_path, xpi_ROOT[n_xpi_ROOT]))
    n_crop_ini_file_name <- stringr::str_detect(xpi_in, crop_ini_file_name)
    xpi_par_name         <- xpi_in[n_crop_ini_file_name]

    # query checking if xpi and/or n_crop_ini_file_name find a match
    if(length(xpi_par_name) == 0){
      stop("ill-specified crop_ini_file_name or not existant in xpi")
    }

    for(npar in 1:Npar){
      xpi_in[n_crop_ini_file_name] <-  stringr::str_replace(xpi_par_name, ".ini", paste0("_", npar ,".ini"))

      proj_ROOT <- stringr::str_remove(string    = xpi_ROOT[n_xpi_ROOT]
                                       , pattern = ".xpi")

      # replace generic pointers to the ini files.
      xpi_in <- stringr::str_replace_all(string        = xpi_in
                                         , pattern     = "\\$PROJECT_NAME_\\$REG_STR"
                                         , replacement = proj_ROOT)

      # write new xpi
      data.table::fwrite(data.table::as.data.table(xpi_in)
                         , file      = write_out_m[npar,n_xpi_ROOT]
                         , quote     = FALSE
                         , col.names = FALSE
      )
    }
  }
  ## RETURN -----
  # Vector of parameter file names.
  return(list("crop_par_file_names" = paste0(crop_ini_file_name, paste0("_", 1:Npar,".ini"))
              , "grid_new"  = Nxpi_Npar_v
              , "grid_old"  = Nxpi_v
              , "grid_match"= matrix(Nxpi_Npar_v, nrow = Npar)
         ))
}


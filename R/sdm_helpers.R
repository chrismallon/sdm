library(raster)
library(stringr)

#' Make Test raster
#'
#' @param width The width of the test raster
#' @param resolution The resolution of the test raster
#' @param file_name The name of the raster file to be created (do not include file time, always produces '.tif' files)
#' @param directory The directory of the raster file to be created.
#'
#' @return
#' @export
#'
#' @examples
make_test_raster = function(width, resolution, file_name, directory){
  x_min = -1260000
  x_max = x_min + width * resolution
  y_min = 7500000
  y_max = y_min + width * resolution

  m = matrix(sample(c(0,1),
                    size = width^2,
                    replace = T,
                    prob = c(0.5,0.5)),
             nrow = width, ncol = width)
  if (!dir.exists(directory)) dir.create(directory)

  out = raster::raster(m,
               crs = "+proj=lcc +lat_0=0 +lon_0=-95 +lat_1=49 +lat_2=77 +x_0=0 +y_0=0 +ellps=GRS80 +units=m",
               xmn= x_min, xmx = x_max, ymn = y_min, ymx = y_max)
  raster::writeRaster(x = out, filename = file.path(directory, paste0(file_name, ".tif")), overwrite=TRUE)
  #browser()
  return(file.path(directory, paste0(file_name, ".tif")))
}


#' Load Disturbance Raster
#'
#' @param file_path The path of the folder with all the disturbance rasters.
#' @param base_file_name The start of the name of the disturbance rasters, from beginning to the start of the numeric year.
#' @param year The numeric year. Can be a string or number.
#' @param file_name_end_and_type The end of the file name, after the year. Includes the file type, such as '.tif'.
#'
#' @return A raster object representing the disturbance for a particular year.
#' @export
#'
#' @examples
#' disturbance <- load_disturbance_raster("outputs/static_fire_regime", "rstCurrentBurn_year", 4, ".tif")
load_disturbance_raster <- function(file_path, base_file_name, year, file_name_end_and_type){
  str_year <- stringr::str_pad(year, 4, side="left", pad="0")
  drast <- raster::raster(file.path(file_path, paste0(base_file_name, str_year, file_name_end_and_type)))
  return(drast)
}

add_list_of_fire_sizes <- function(fire_size_list, fire_raster){
  fc <- raster::clump(fire_raster)
  ff <- raster::freq(fc)
  ff <- as.data.frame(ff)
  ff <- ff[!is.na(ff$value),]
  annual_fire_sizes = list(size = ff$count, maxSize = ff$count)
  fire_size_list <- append(fire_size_list, list(annual_fire_sizes))
  return(fire_size_list)
}

#' Load and Prep a Shapefile
#'
#' This function loads a shapefile and preps it so that it can be used to cut up rasters.
#' ****This needs to be updated to account for multi-part polygons
#' @param SHPNAME the name of the shapefile to be loaded
#' @param PROJECT.PATH the filepath of the shapefile
#' @param PROJ.USE the \code{\link[rgdal]{CRS}} projection object of the shapefile
#'
#' @return The output is a Large SpatialPolygon S4 object
#' @export
#'
#' @import sp
#' @import rgdal
#' @importFrom rgeos gBuffer gUnaryUnion gIsValid
#'
#' @examples
#' LOAD.AND.PREP.GIS("East_Cst_cropped",
#'     "C:/Users/dennis.corvi/Documents/R/Projects/east_cst_crop",
#'     sp::CRS('+proj=aea +lat_1=28 +lat_2=42 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83
#'     +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0 '))
#'
LOAD.AND.PREP.GIS <- function(SHPNAME, PROJECT.PATH = PROJECT.PATH, PROJ.USE = PROJ.USE) {
  SHP = rgdal::readOGR(dsn=file.path(PROJECT.PATH), layer=SHPNAME, verbose=F)
  SHP = sp::spTransform(SHP, CRS=PROJ.USE)
  if(NROW(SHP)>1) {
    SHP = rgeos::gBuffer(SHP, width=1, byid=T)
    SHP = rgeos::gUnaryUnion(SHP) }
  stopifnot(rgeos::gIsValid(SHP))
  return(SHP)
}
.onLoad <- function(libnam, pkgname) {
  packageStartupMessage(paste0("Welcome to my test package. Number of cores on this machine:  ",parallel::detectCores()))
}

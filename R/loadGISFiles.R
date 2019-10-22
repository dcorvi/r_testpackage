#' Load and Prep a Shapefile
#'
#' This function loads a shapefile and preps it so that it can be used in the function
#' @param SHPNAME the name of the shapefile to be loaded
#' @param PROJECT.PATH the filepath of the shapefile
#' @param PROJ.USE the \code{\link{CRS}} projection object of the shapefile
#'
#' @return The output is a Large SpatialPolygon S4 object
#' @export
#'
#' @import rgdal
#' @importFrom rgeos gBuffer gUnaryUnion gIsValid
#'
#' @examples
#' LOAD.AND.PREP.GIS("clipArea", "C:/Users/dennis.corvi/Documents/R/Projects", albersProj)
#'
LOAD.AND.PREP.GIS <- function(SHPNAME, PROJECT.PATH = PROJECT.PATH, PROJ.USE = PROJ.USE) {
  SHP = rgdal::readOGR(dsn=file.path(PROJECT.PATH), layer=SHPNAME, verbose=F)
  SHP = spTransform(SHP, CRS=PROJ.USE)
  if(NROW(SHP)>1) {
    SHP = rgeos::gBuffer(SHP, width=1, byid=T)
    SHP = rgeos::gUnaryUnion(SHP) }
  stopifnot(rgeos::gIsValid(SHP))
  return(SHP)
}

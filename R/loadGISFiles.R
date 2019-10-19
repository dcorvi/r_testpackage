#' Load and Prep a Shapefile
#'
#' This function loads a shapefile and preps it so that it can be used in the function
#' @param SHPNAME the name of the shapefile to be loaded
#' @param PROJECT.PATH the filepath of the shapefile
#' @param PROJ.USE the CRS projection of the shapefile
#'
#' @return The output from \code{{}}
#' @export
#'
#' @examples
#'
#'


PKG <- c('rgdal','rgeos')


for (p in PKG) {
  if(!require(p,character.only = TRUE)) {
    install.packages(p)
    require(p,character.only = TRUE) }
}

LOAD.AND.PREP.GIS <- function(SHPNAME, PROJECT.PATH = PROJECT.PATH, PROJ.USE = PROJ.USE) {
  SHP = readOGR(dsn=file.path(PROJECT.PATH), layer=SHPNAME, verbose=F)
  SHP = spTransform(SHP, CRS=PROJ.USE)
  if(NROW(SHP)>1) {
    SHP = gBuffer(SHP, width=1, byid=T)
    SHP = gUnaryUnion(SHP) }
  stopifnot(gIsValid(SHP))
  return(SHP)
}

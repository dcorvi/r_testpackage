

#' Title
#'
#' @param SHPNAME
#' @param PROJECT.PATH
#' @param PROJ.USE
#'
#' @return
#' @export
#'
#' @examples
#'
#'
LOAD.AND.PREP.GIS <- function(SHPNAME, PROJECT.PATH = PROJECT.PATH, PROJ.USE = PROJ.USE) {
  SHP = readOGR(dsn=file.path(PROJECT.PATH), layer=SHPNAME, verbose=F)
  SHP = spTransform(SHP, CRS=PROJ.USE)
  if(NROW(SHP)>1) {
    SHP = gBuffer(SHP, width=1, byid=T)
    SHP = gUnaryUnion(SHP) }
  stopifnot(gIsValid(SHP))
  return(SHP)
}

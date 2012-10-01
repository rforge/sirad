cstRead <- function(lat,lon) {
    require(raster)
    p <- extract(CSTmap, matrix(c(lon,lat),1,2))
    p}
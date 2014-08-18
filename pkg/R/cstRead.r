cstRead <- function(lat,lon) {
  p <- extract(CSTmap, matrix(c(lon,lat),1,2))
  if (is.na(p)) { warning("Lat/lon outside the CSTmap!")}
  p}
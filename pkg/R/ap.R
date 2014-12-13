ap <-
  function(days, lat, lon, extraT=NULL, A=NA, B=NA, SSD) {
    i <- dayOfYear(days)
    latt <- radians(lat)
    if (is.null(extraT)) extraT <- extrat(i=i,lat=latt)$ExtraTerrestrialSolarRadiationDaily
    
    if (is.na(A)) A <- extract(APa_map, matrix(c(lon,lat),1,2))
    if (is.na(B)) B <- extract(APb_map, matrix(c(lon,lat),1,2))
    
    ap <- extraT*(A+B*(SSD/dayLength(lat=latt,i=i)))
    ap 
  }


su <-
  function(days,lat,extraT=NULL,A=NA,B=NA,C=NA,tmax,tmin,CC) {
    i <- dayOfYear(days)
    latt <- radians(lat)
    if (is.null(extraT)) extraT <- extrat(i=i,lat=latt)$ExtraTerrestrialSolarRadiationDaily
    su <- extraT*(A*sqrt(tmax-tmin)+B*sqrt(1-CC/8)) + C
    su 
  }


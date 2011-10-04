bc <-
function(days,lat,BCb,Tmax,Tmin,BCc=2,tal) {
      i <- dayOfYear(days)
      Sd <- extrat(i=i,lat=radians(lat))$ExtraTerrestrialSolarRadiationDaily
      le <- length(Tmax) 
      dtemp <- c(Tmax[-le]-(Tmin[-le]+Tmin[-1])/2,Tmax[le]-(Tmin[le-1]+Tmin[le])/2)   # for the last day of time series dtemp=Tmax-(Tmin-1+Tmin)/2 where Tmin-1 is a Tmin for a previous day
      Zdtemp <- zoo(Tmax-Tmin,order.by=days)
      dtempM <- mean(as.numeric( aggregate( Zdtemp, by= format( time(Zdtemp), "%m" ), FUN=mean, na.rm= TRUE )),na.rm=T)
      bc <- Sd*tal*(1-exp(-BCb*(dtemp^BCc)/dtempM))
      bc 
}


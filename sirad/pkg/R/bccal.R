bccal <-
function(lat, days, rad_mea, Tmax, Tmin,tal)  {
        latt <- radians(lat)
        i <- dayOfYear(days)
        Sd <- extrat(i=i,lat=latt)$ExtraTerrestrialSolarRadiationDaily
        le <- length(Tmax) 
        dtemp <- c(Tmax[-le]-(Tmin[-le]+Tmin[-1])/2,Tmax[le]-(Tmin[le-1]+Tmin[le])/2)
        Zdtemp <- zoo(Tmax-Tmin,order.by=days)
        dtempM <- mean(as.numeric( aggregate( Zdtemp, by= format( time(Zdtemp), "%m" ), FUN=mean, na.rm= TRUE )),na.rm=T)
        m <- nls(rad_mea ~ Sd*tal*(1-exp((-b*dtemp^2)/dtempM)),start=list(b=0.05),trace=F)
        rval <- c(coef(m))
        names(rval) <- c("BCb_nls")
        rval
}


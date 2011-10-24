bcauto <-
function(lat, days, Tmax, Tmin,tal,BCb_guess=0.13,epsilon=0.5,perce=1,dcoast=NA)  {
        if (!is.na(dcoast) & dcoast <= 15) epsilon <- 0.1
        if (!is.na(dcoast) & dcoast > 15) epsilon <- 0.5
        latt <- radians(lat)
        i <- dayOfYear(days)
        Sd <- extrat(i=i,lat=latt)$ExtraTerrestrialSolarRadiationDaily
        le <- length(Tmax) 
        dtemp <- c(Tmax[-le]-(Tmin[-le]+Tmin[-1])/2,Tmax[le]-(Tmin[le-1]+Tmin[le])/2)
        Zdtemp <- zoo(Tmax-Tmin,order.by=days)
        dtempM <- mean(as.numeric( aggregate( Zdtemp, by= format( time(Zdtemp), "%m" ), FUN=mean, na.rm= TRUE )),na.rm=T)
        rv_BC <- bc(days,lat,BCb_guess,Tmax,Tmin,BCc=2,tal)
        pot <- Sd*tal
        dif <- pot-rv_BC
        wh <- which(dif < sort(dif)[round(length(dif)*(perce/100))]) # around 10 days per year
        dif <- dif[wh]
        dtemp <- dtemp[wh]
        Sd <- Sd[wh]
        rad_mea <- Sd*tal-epsilon
        m <- nls(rad_mea ~ Sd*tal*(1-exp((-b*dtemp^2)/dtempM)),start=list(b=0.05),trace=F)
        rval <- c(coef(m))
        names(rval) <- c("BCb_auto")
        rval
}


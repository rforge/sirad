hacal <-
function(lat,days,rad_mea,tmax,tmin) {
    
        i <- dayOfYear(days)
        DL <- extrat(lat=radians(lat),i)$DayLength   #[hours]
        Sd <- extrat(lat=radians(lat),i)$ExtraTerrestrialSolarRadiationDaily  # [MJ]

        Y <- rad_mea/Sd
        dtemp <- sqrt(tmax-tmin) 
        m <- lm(Y ~ dtemp)
        rval <- c(m$coefficients[c(2,1)],summary(m)$r.squared)
        names(rval) <- c("Ha","Hb","Hr2")
        rval
}


apcal <-
function(lat,days,rad_mea,SSD) {
        i <- dayOfYear(days)
        ex <- extrat(lat=radians(lat),i)
        DL <- ex$DayLength   #[hours]
        Sd <- ex$ExtraTerrestrialSolarRadiationDaily  # [MJ]
        Y <- rad_mea/Sd      
        X <- SSD/DL
        m <- lm(Y ~ X)
        rval <- c(m$coefficients,summary(m)$r.squared)
        names(rval) <- c("APa","APb","APr2")
        rval
}


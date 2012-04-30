hauto <- function (lat, days, Tmax, Tmin, tal, Ha_guess = 0.16,Hb_guess=0.1, epsilon = 0.5, 
    perce = 1, dcoast = NA) 
{
    if (!is.na(dcoast) & dcoast <= 15) 
        epsilon <- 0.1
    if (!is.na(dcoast) & dcoast > 15) 
        epsilon <- 0.5
    latt <- radians(lat)
    i <- dayOfYear(days)
    Sd <- extrat(i = i, lat = latt)$ExtraTerrestrialSolarRadiationDaily
    dtemp <- sqrt(Tmax-Tmin)    
    rv_ha <- Sd * Ha_guess * dtemp + Hb_guess      
    pot <- Sd * tal
    dif <- pot - rv_ha
    wh <- which(dif < sort(dif)[round(length(dif) * (perce/100))])
    dif <- dif[wh]
    dtemp <- dtemp[wh]
    Sd <- Sd[wh]
    rad_mea <- Sd * tal - epsilon
    m <- lm(rad_mea ~ I(Sd * dtemp))
    rval <- c(m$coefficients[c(2, 1)], summary(m)$r.squared)
    names(rval) <- c("Ha_auto", "Hb_auto", "Hr2_auto")
    rval
}

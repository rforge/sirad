cst <-
function(RefRad,days,lat,perce=3,sepYear=FALSE) {
 i <- dayOfYear(days) 
 
 if (sepYear == TRUE) {
 
 years <- unique(format(as.Date(days,origin="1970-01-01"),'%Y'))

 
 mtalsALL <- vector()
 ex <- extrat(i,lat)$ExtraTerrestrialSolarRadiationDaily
 for (y in years) {
 wh <- which(format(as.Date(days,origin="1970-01-01"),'%Y')==y)
 tals <- RefRad[wh]/ex[wh]
 stals <- sort(tals)
 
 ll <- length(stals)
 mtals <- stals[(ll-round(ll*(perce/100))):ll]
 mtalsALL <- c(mtalsALL,mtals)
 }
 rval <- median(mtalsALL)
 }
 
 if (sepYear == FALSE) {
 tals <- RefRad/extrat(i,lat)$ExtraTerrestrialSolarRadiationDaily
 ll <- length(sort(tals))
 mtals <- sort(tals)[(ll-round(ll*(perce/100))):ll]
 rval <- median(mtals)
 }
 rval
}


\name{sirad-package}
\alias{sirad-package}
\alias{sirad}
\docType{package}
\title{
Functions for calculating daily solar radiation}
\description{
Package provides functions to calculate daily solar radiation at horizontal surface using several well-known models. It also icludes functions for model calibration based on groud-truth data as well as a function for auto-calibration.}
\details{
\tabular{ll}{
Package: \tab sirad\cr
Type: \tab Package\cr
Version: \tab 1.0-7\cr
Date: \tab 2011-10-20\cr
License: \tab GPL-2\cr
LazyLoad: \tab yes\cr
}
This package provides functions to calculate daily solar radiation at horizontal surface using several well-known models (Bristow&Campbell, Hargreaves, Supit-Van Kappel, Mahmood-Hubbard, Angrstrom-Prescott). It also includes functions for model calibration based on groud-truth data as well as a function for auto-calibration.
}
\author{
Jedrzej Bojanowski

Maintainer: Jedrzej Bojanowski <jedrzej.bojanowski@gmail.com>

}
\references{

}

\keyword{ package }
\seealso{

}
\examples{
require(zoo)
data(Metdata)
A <- 0.21
B <- 0.57
sunshine <- Metdata$meteo$SUNSHINE
days <- Metdata$meteo$DAY
lat <- Metdata$LATITUDE
plot(zoo(ap(days=days,lat=lat,A=A,B=B,SSD=sunshine),order.by=days))
}

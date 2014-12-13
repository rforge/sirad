\name{APa_map}
\alias{APa_map}
\docType{data}
\title{
Map of Angstrom-Prescott model A coefficient
}
\description{
This dataset contains a raster of Angstrom-Prescott model A coefficient 
}
\usage{
data(APa_map)
}
%\format{ }
\details{
Map of Angstrom-Prescott model coefficient was derived from Meteosat Second Generation solar radiation.
}

%\source{}

\references{Bojanowski, J.S., Vrieling, A., Skidmore, A.K., 2013. Calibration of solar radiation models for Europe using Meteosat Second Generation and weather station data. Agricultural and Forest Meteorology 176:1-9}

\examples{
if(require(raster))
{
  data(APa_map)
  show(APa_map)
}
}
\keyword{datasets}

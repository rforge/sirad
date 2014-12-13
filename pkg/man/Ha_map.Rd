\name{Ha_map}
\alias{Ha_map}
\docType{data}
\title{
Map of Hargreaves model A coefficient
}
\description{
This dataset contains a raster of Hargreaves model A coefficient 
}
\usage{
data(Ha_map)
}
%\format{ }
\details{
Map of Hargreaves model coefficient was derived from Meteosat Second Generation solar radiation.
}

%\source{}

\references{Bojanowski, J.S., Vrieling, A., Skidmore, A.K., 2013. Calibration of solar radiation models for Europe using Meteosat Second Generation and weather station data. Agricultural and Forest Meteorology 176:1-9}

\examples{
if(require(raster))
{
  data(Ha_map)
  show(Ha_map)
}
}
\keyword{datasets}

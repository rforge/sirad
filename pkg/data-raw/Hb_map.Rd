\name{Hb_map}
\alias{Hb_map}
\docType{data}
\title{
Map of Hargreaves model B coefficient
}
\description{
This dataset contains a raster of Hargreaves model B coefficient 
}
\usage{
data(Hb_map)
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
  data(Hb_map)
  show(Hb_map)
}
}
\keyword{datasets}

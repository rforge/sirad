\name{Sa_map}
\alias{Sa_map}
\docType{data}
\title{
Map of Supit-Van Kappel model A coefficient
}
\description{
This dataset contains a raster of Supit-Van Kappel model A coefficient 
}
\usage{
data(Sa_map)
}
%\format{ }
\details{
Map of Supit-Van Kappel model coefficient was derived from Meteosat Second Generation solar radiation.
}

%\source{}

\references{Bojanowski, J.S., Vrieling, A., Skidmore, A.K., 2013. Calibration of solar radiation models for Europe using Meteosat Second Generation and weather station data. Agricultural and Forest Meteorology 176:1-9}

\examples{
if(require(raster))
{
  data(Sa_map)
  show(Sa_map)
}
}
\keyword{datasets}

\name{Sc_map}
\alias{Sc_map}
\docType{data}
\title{
Map of Supit-Van Kappel model C coefficient
}
\description{
This dataset contains a raster of Supit-Van Kappel model C coefficient 
}
\usage{
data(Sc_map)
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
  data(Sc_map)
  show(Sc_map)
}
}
\keyword{datasets}

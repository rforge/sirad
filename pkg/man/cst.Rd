\name{cst}
\alias{cst}

\title{
Estimate clear sky transmissivity
}
\description{
Function estimates a clear sky transmissivity based on reference data (e.g. measured)
}
\usage{
cst(RefRad, days, lat, perce = 3, sepYear = FALSE)
}

\arguments{
  \item{RefRad}{
Vector of length n of reference solar radiation data [MJm-2]
}
  \item{days}{
Vector of class 'Date' of length n.
}
  \item{lat}{
Latitude in radians
}
  \item{perce}{
Percent of days to be chosen as clear days
}
  \item{sepYear}{
Logical value. If 'TRUE' percent of days given by 'perce' of every single year are taken for calculation. If 'FALSE' percent of days given by 'perce' of all years are taken for calculation.
}
}
\details{

}
\value{
Numeric. Clear sky transmissivity.}
\references{

}
\author{
Jedrzej Bojanowski
}
\note{

}



\seealso{

}
\examples{

data(Metdata)
ref <- Metdata$meteo$RAD_MEA
i <- dayOfYear(Metdata$meteo$DAY)
latr <- radians(Metdata$LATITUDE)
cst(ref,i,latr) 
}

\keyword{}


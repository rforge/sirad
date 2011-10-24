\name{Metdata}
\alias{Metdata}
\docType{data}
\title{
Weather data
}
\description{
This dataset contains two years of daily data of sunshine hours, solar radiation, minimum temperature, maximum temperature, and cloud coverage.
}
\usage{data(Metdata)}
\format{
\tabular{rlll}{
         [,1] \tab DAY    \tab Date \tab Date \cr
         [,2] \tab SUNSHINE  \tab numeric \tab Sunshine (hours)\cr
         [,3] \tab RAD_MEA     \tab numeric \tab Solar radiation (MJm-2)\cr
         [,4] \tab TEMP_MIN    \tab numeric \tab Minimum temperature (degrees C)\cr
         [,5] \tab TEMP_MAX   \tab numeric \tab Maximum temperature (degrees C)\cr
         [,6] \tab CLOUD_DAYTIME_TOTAL     \tab numeric \tab Cloud coverage (octas)
       }       

}
\details{

}
\source{

}
\references{

}
\examples{
data(Metdata)
str(Metdata)

}
\keyword{datasets}

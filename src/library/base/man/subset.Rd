\name{subset}
\alias{subset}
\alias{subset.default}
\alias{subset.data.frame}
\title{Subsetting Vectors and Data Frames}
\usage{
subset(x, ...)
subset.default(x, subset)
subset.data.frame(x, subset, select)
}
\arguments{
    \item{x}{Object to be subsetted}
    \item{\dots}{How to subset, depends on object}
    \item{subset}{logical expression}
    \item{select}{expression, indicating variables to select from a
	dataframe}
}
\details{
    For ordinary vectors, the result is simply 
    \code{x[subset & !is.na(subset)]}. 

    For dataframes, the \code{subset} argument works similarly on the
    rows.  Note that \code{subset} will be evaluated in the dataframe.

    The \code{select} argument exists only for dataframes.  It works by
    first replacing variable names in the selection expression with the
    corresponding column numbers in the dataframe and then using the
    resulting integer vector to index the columns.  This allows the use
    of the standard indexing conventions so that e.g. ranges of
    variables can be specified easily.
}
\value{
    Selected rows and columns of the object \code{x}.
}
\author{Peter Dalgaard}
\seealso{
    \code{\link{[}},
    \code{\link{transform}}
}
\examples{
data(airquality)
subset(airquality, Temp > 80, select = c(Ozone, Temp))
subset(airquality, Day == 1, select = -Temp)
subset(airquality, select = Ozone:Wind)

attach(airquality)
subset(Ozone, Temp > 80)
}
\keyword{manip}

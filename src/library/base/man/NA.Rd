\name{NA}
\title{Not Available / ``Missing'' Values}
\usage{
NA
is.na(x)
is.na.data.frame(x)

is.na(x) <- value
}
\alias{NA}
\alias{is.na}
\alias{is.na.data.frame}
\alias{is.na<-}
\alias{is.na<-.default}
\description{
\code{NA} is a logical constant of length 1 which contains
a missing value indicator.  \code{NA} can be freely coerced to any
other vector type.

The generic function \code{is.na} returns a logical vector
of the same ``form'' as its argument \code{x}, containing \code{TRUE}
for those elements marked \code{NA} or \code{\link{NaN}} (!)
and \code{FALSE} otherwise.
\code{dim}, \code{dimnames} and \code{names} attributes are preserved.

The generic function \code{is.na<-} sets elements to \code{NA}.
}
\arguments{
  \item{x}{an \R object to be tested.}
  \item{value}{a suitable index vector for use with \code{x}.}
}
\details{
  For character vectors the value \code{"NA"} represents missingness.

  \code{is.na(x)} works elementwise when \code{x} is a \code{\link{list}}.
  The method dispatching is C-internal, rather than via
  \code{\link{UseMethod}}.

  Function \code{is.na<-} may provide a safer way to set missingness.
  It behaves differently for factors, for example.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole.

  Chambers, J. M. (1998)
  \emph{Programming with Data. A Guide to the S Language}.
  Springer.
}
\seealso{
  \code{\link{NaN}}, \code{\link{is.nan}}, etc.
  and the utility function \code{\link{complete.cases}}.

  \code{\link{na.action}}, \code{\link{na.omit}}, \code{\link{na.fail}}
  on how methods can be tuned to deal with missing values.
}
\examples{
is.na(c(1,NA))       #> F TRUE
is.na(paste(c(1,NA)))#> F FALSE
}
\keyword{NA}
\keyword{logic}
\keyword{manip}

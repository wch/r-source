\name{Extremes}
\alias{max}
\alias{min}
\alias{pmax}
\alias{pmin}
\title{Maxima and Minima}
\description{
  Returns the (parallel) maxima and minima of the input values.
}
\usage{
max(\dots, na.rm=FALSE)
min(\dots, na.rm=FALSE)

pmax(\dots, na.rm=FALSE)
pmin(\dots, na.rm=FALSE)
}
\arguments{
  \item{\dots}{numeric arguments.}
  \item{na.rm}{a logical indicating whether missing values should be
    removed.}
}
\value{
  \code{max} and \code{min} return the maximum or minimum of \emph{all}
  the  values present in their arguments, as \code{\link{integer}} if
  all are \code{integer}, % and at least one is nonempty
  or as \code{\link{double}} otherwise.

  The minimum and maximum of an empty set are \code{+Inf} and \code{-Inf}
  (in this order!) which ensures \emph{transitivity}, e.g.,
  \code{min(x1, min(x2)) == min(x1,x2)}.
  In \R versions before 1.5, \code{min(integer(0)) == .Machine$integer.max},
  and analogously for \code{max}, preserving argument \emph{type},
  whereas from \R version 1.5.0, \code{max(x) == -Inf} and
  \code{min(x) == +Inf} whenever \code{length(x) == 0} (after removing
  missing values if requested).

  If \code{na.rm} is \code{FALSE} an \code{NA} value in any of the
  arguments will cause a value of \code{NA} to be returned, otherwise
  \code{NA} values are ignored.

  \code{pmax} and \code{pmin} take several vectors (or matrices) as arguments and
  return a single vector giving the \dQuote{parallel} maxima (or minima) of the
  vectors.  The first element of the result is the maximum (minimum) of
  the first elements of all the arguments, the second element of the
  result is the maximum (minimum) of the second elements of all the
  arguments and so on.  Shorter vectors are recycled if necessary.  If
  \code{na.rm} is \code{FALSE}, \code{NA} values in the input vectors
  will produce \code{NA} values in the output.  If \code{na.rm} is
  \code{TRUE}, \code{NA} values are ignored.
  \code{\link{attributes}} (such as \code{\link{names}} or
  \code{\link{dim}}) are transferred from the first argument (if applicable).

  \code{max} and \code{min} are generic functions: methods can be defined
  for them individually or via the \code{\link{Summary}} group generic.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole.
}
\seealso{
  \code{\link{range}} (\emph{both} min and max) and
  \code{\link{which.min}} (\code{which.max}) for the \emph{arg min},
  i.e., the location where an extreme value occurs.
}
\examples{
require(stats)
 min(5:1, pi) #-> one number
pmin(5:1, pi) #->  5  numbers

x <- sort(rnorm(100));  cH <- 1.35
pmin(cH, quantile(x)) # no names
pmin(quantile(x), cH) # has names
plot(x, pmin(cH, pmax(-cH, x)), type='b', main= "Huber's function")
}
\keyword{univar}
\keyword{arith}

% File src/library/base/man/Extremes.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2013 R Core Team
% Distributed under GPL 2 or later

\name{Extremes}
\alias{max}
\alias{min}
\alias{pmax}
\alias{pmin}
\alias{pmax.int}
\alias{pmin.int}
\title{Maxima and Minima}
\description{
  Returns the (parallel) maxima and minima of the input values.
}
\usage{
max(\dots, na.rm = FALSE)
min(\dots, na.rm = FALSE)

pmax(\dots, na.rm = FALSE)
pmin(\dots, na.rm = FALSE)

pmax.int(\dots, na.rm = FALSE)
pmin.int(\dots, na.rm = FALSE)
}
\arguments{
  \item{\dots}{numeric or character arguments (see Note).}
  \item{na.rm}{a logical indicating whether missing values should be
    removed.}
}
\details{
  \code{max} and \code{min} return the maximum or minimum of \emph{all}
  the  values present in their arguments, as \code{\link{integer}} if
  all are \code{logical} or \code{integer}, as \code{\link{double}} if
  all are numeric, and character otherwise.

  If \code{na.rm} is \code{FALSE} an \code{NA} value in any of the
  arguments will cause a value of \code{NA} to be returned, otherwise
  \code{NA} values are ignored.

  The minimum and maximum of a numeric empty set are \code{+Inf} and
  \code{-Inf} (in this order!) which ensures \emph{transitivity}, e.g.,
  \code{min(x1, min(x2)) == min(x1, x2)}.  For numeric \code{x}
  \code{max(x) == -Inf} and \code{min(x) == +Inf}
  whenever \code{length(x) == 0} (after removing missing values if
  requested).  However, \code{pmax} and \code{pmin} return
  \code{NA} if all the parallel elements are \code{NA} even for
  \code{na.rm = TRUE}.

  \code{pmax} and \code{pmin} take one or more vectors (or matrices) as
  arguments and return a single vector giving the \sQuote{parallel}
  maxima (or minima) of the vectors.  The first element of the result is
  the maximum (minimum) of the first elements of all the arguments, the
  second element of the result is the maximum (minimum) of the second
  elements of all the arguments and so on.  Shorter inputs (of non-zero
  length) are recycled if necessary.  Attributes (see
  \code{\link{attributes}}: such as \code{\link{names}} or
  \code{\link{dim}}) are copied from the first argument (if applicable).

  \code{pmax.int} and \code{pmin.int} are faster internal versions only
  used when all arguments are atomic vectors and there are no classes:
  they drop all attributes.  (Note that all versions fail for raw and
  complex vectors since these have no ordering.)

  \code{max} and \code{min} are generic functions: methods can be
  defined for them individually or via the
  \code{\link[=S3groupGeneric]{Summary}} group generic.  For this to
  work properly, the arguments \code{\dots} should be unnamed, and
  dispatch is on the first argument.

  By definition the min/max of a numeric vector containing an \code{NaN}
  is \code{NaN}, except that the min/max of any vector containing an
  \code{NA} is \code{NA} even if it also contains an \code{NaN}.
  Note that \code{max(NA, Inf) == NA} even though the maximum would be
  \code{Inf} whatever the missing value actually is.

  Character versions are sorted lexicographically, and this depends on
  the collating sequence of the locale in use: the help for
  \sQuote{\link{Comparison}} gives details.  The max/min of an empty
  character vector is defined to be character \code{NA}.  (One could
  argue that as \code{""} is the smallest character element, the maximum
  should be \code{""}, but there is no obvious candidate for the
  minimum.)
}
\value{
  For \code{min} or \code{max}, a length-one vector.  For \code{pmin} or
  \code{pmax}, a vector of length the longest of the input vectors, or
  length zero if one of the inputs had zero length.

  The type of the result will be that of the highest of the inputs in
  the hierarchy integer < double < character.

  For \code{min} and \code{max} if there are only numeric inputs and all
  are empty (after possible removal of \code{NA}s), the result is double
  (\code{Inf} or \code{-Inf}).
}
\section{S4 methods}{
  \code{max} and \code{min} are part of the S4
  \code{\link[=S4groupGeneric]{Summary}} group generic.  Methods
  for them must use the signature \code{x, \dots, na.rm}.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.
}
\note{
  \sQuote{Numeric} arguments are vectors of type integer and numeric,
  and logical (coerced to integer).  For historical reasons, \code{NULL}
  is accepted as equivalent to \code{integer(0)}.% PR#1283

  \code{pmax} and \code{pmin} will also work on classed objects with
  appropriate methods for comparison, \code{is.na} and \code{rep} (if
  recycling of arguments is needed).
}
\seealso{
  \code{\link{range}} (\emph{both} min and max) and
  \code{\link{which.min}} (\code{which.max}) for the \emph{arg min},
  i.e., the location where an extreme value occurs.

  \sQuote{\link{plotmath}} for the use of \code{min} in plot annotation.
}
\examples{
require(stats); require(graphics)
 min(5:1, pi) #-> one number
pmin(5:1, pi) #->  5  numbers

x <- sort(rnorm(100));  cH <- 1.35
pmin(cH, quantile(x)) # no names
pmin(quantile(x), cH) # has names
plot(x, pmin(cH, pmax(-cH, x)), type = "b", main =  "Huber's function")

cut01 <- function(x) pmax(pmin(x, 1), 0)
curve(      x^2 - 1/4, -1.4, 1.5, col = 2)
curve(cut01(x^2 - 1/4), col = "blue", add = TRUE, n = 500)
## pmax(), pmin() preserve attributes of *first* argument
D <- diag(x = (3:1)/4) ; n0 <- numeric()
stopifnot(identical(D,  cut01(D) ),
          identical(n0, cut01(n0)),
          identical(n0, cut01(NULL)),
          identical(n0, pmax(3:1, n0, 2)),
          identical(n0, pmax(n0, 4)))
}
\keyword{univar}
\keyword{arith}

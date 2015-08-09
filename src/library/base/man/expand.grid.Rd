% File src/library/base/man/expand.grid.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2010 R Core Team
% Distributed under GPL 2 or later

\name{expand.grid}
\title{Create a Data Frame from All Combinations of Factors}
\usage{
expand.grid(\dots, KEEP.OUT.ATTRS = TRUE, stringsAsFactors = TRUE)
}
\alias{expand.grid}
\arguments{
  \item{\dots}{vectors, factors or a list containing these.}
  \item{KEEP.OUT.ATTRS}{a logical indicating the \code{"out.attrs"}
    attribute (see below) should be computed and returned.}
  \item{stringsAsFactors}{logical specifying if character vectors are
    converted to factors.}
}
\description{
  Create a data frame from all combinations of the supplied vectors or
  factors.  See the description of the return value for precise details of
  the way this is done.
}
\value{
  A data frame containing one row for each combination of the supplied
  factors.  The first factors vary fastest.  The columns are labelled by
  the factors if these are supplied as named arguments or named
  components of a list.  The row names are \sQuote{automatic}.

  Attribute \code{"out.attrs"} is a list which gives the dimension and
  dimnames for use by \code{\link{predict}} methods.
}
%% currently, from "base R", only predict.loess() makes use of it.
\note{
  Conversion to a factor is done with levels in the order
  they occur in the character vectors (and not alphabetically, as is
  most common when converting to factors).
}

\references{
  Chambers, J. M. and Hastie, T. J. (1992)
  \emph{Statistical Models in S.}
  Wadsworth & Brooks/Cole.
}
% there is a 'combn' in package combinat
\seealso{
  \code{\link[utils]{combn}} (package \code{utils}) for the generation
  of all combinations of n elements, taken m at a time.
}
\examples{
require(utils)

expand.grid(height = seq(60, 80, 5), weight = seq(100, 300, 50),
            sex = c("Male","Female"))

x <- seq(0, 10, length.out = 100)
y <- seq(-1, 1, length.out = 20)
d1 <- expand.grid(x = x, y = y)
d2 <- expand.grid(x = x, y = y, KEEP.OUT.ATTRS = FALSE)
object.size(d1) - object.size(d2)
##-> 5992 or 8832 (on 32- / 64-bit platform)
\dontshow{stopifnot(object.size(d1) > object.size(d2))}
}
\keyword{models}
\keyword{array}

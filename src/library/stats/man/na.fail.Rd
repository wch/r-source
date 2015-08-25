% File src/library/stats/man/na.fail.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{na.fail}
\alias{na.fail}
\alias{na.fail.default}
\alias{na.omit}
\alias{na.omit.data.frame}
\alias{na.omit.default}
\alias{na.exclude}
\alias{na.exclude.data.frame}
\alias{na.exclude.default}
\alias{na.pass}
\title{Handle Missing Values in Objects}
\usage{
na.fail(object, \dots)
na.omit(object, \dots)
na.exclude(object, \dots)
na.pass(object, \dots)
}
\arguments{
  \item{object}{an \R object, typically a data frame}
  \item{\dots}{further arguments special methods could require.}
}
\description{
  These generic functions are useful for dealing with \code{\link{NA}}s
  in e.g., data frames.
  \code{na.fail} returns the object if it does not contain any
  missing values, and signals an error otherwise.
  \code{na.omit} returns the object with incomplete cases removed.
  \code{na.pass} returns the object unchanged.
}
\details{
  At present these will handle vectors, matrices and data frames
  comprising vectors and matrices (only).

  If \code{na.omit} removes cases, the row numbers of the cases form the
  \code{"na.action"} attribute of the result, of class \code{"omit"}.

  \code{na.exclude} differs from \code{na.omit} only in the class of the
  \code{"na.action"} attribute of the result, which is
  \code{"exclude"}.  This gives different behaviour in functions making
  use of \code{\link{naresid}} and \code{\link{napredict}}: when
  \code{na.exclude} is used the residuals and predictions are padded to
  the correct length by inserting \code{NA}s for cases omitted by
  \code{na.exclude}.
}
\references{
  Chambers, J. M. and Hastie, T. J. (1992)
  \emph{Statistical Models in S.}
  Wadsworth & Brooks/Cole.
}
\seealso{
  \code{\link{na.action}};
  \code{\link{options}} with argument \code{na.action} for setting NA actions;
  and \code{\link{lm}} and \code{\link{glm}} for functions using these.
  \code{\link{na.contiguous}} as alternative for time series.
}
\examples{
DF <- data.frame(x = c(1, 2, 3), y = c(0, 10, NA))
na.omit(DF)
m <- as.matrix(DF)
na.omit(m)
stopifnot(all(na.omit(1:3) == 1:3))  # does not affect objects with no NA's
try(na.fail(DF))   #> Error: missing values in ...

options("na.action")
}
\keyword{NA}

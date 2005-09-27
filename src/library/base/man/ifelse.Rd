\name{ifelse}
\title{Conditional Element Selection}
\usage{
ifelse(test, yes, no)
}
\alias{ifelse}
\description{
\code{ifelse} returns a value with the same shape as
\code{test} which is filled with elements selected
from either \code{yes} or \code{no}
depending on whether the element of \code{test}
is \code{TRUE} or \code{FALSE}.
}
\arguments{
  \item{test}{an object which can be coerced to logical mode.}
  \item{yes}{return values for true elements of \code{test}.}
  \item{no}{return values for false elements of \code{test}.}
}
\details{
  If \code{yes} or \code{no} are too short, their elements are recycled.
  \code{yes} will be evaluated if and only if any element of \code{test}
  is true, and analogously for \code{no}.

  Missing values in \code{test} giving missing values in the result.
}
\value{
  A vector of the same length and attributes (including
  class) as \code{test} and data values from the values of
  \code{yes} or \code{no}.  The mode of the answer will be coerced from
  logical to accommodate first any values taken from \code{yes} and then
  any values taken from \code{no}.
}
\section{Warning}{
  The mode of the result may depend on the value of \code{test}, and the
  class attribute of the result is taken from \code{test} and may be
  inappropriate for the values selected from \code{yes} and \code{no}.

  Sometimes it is better to use a construction such as
  \code{(tmp <- yes; tmp[!test] <- no[!test]; tmp)}, possibly extended
  to handle missing values in \code{test}.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole.
}
\seealso{
\code{\link{if}}.
}
\examples{
x <- c(6:-4)
sqrt(x)#- gives warning
sqrt(ifelse(x >= 0, x, NA))# no warning

## Note: the following also gives the warning !
ifelse(x >= 0, sqrt(x), NA)

## example of different return modes:
yes <- 1:3
no <- pi^(0:3)
typeof(ifelse(NA, yes, no))   # logical
typeof(ifelse(TRUE, yes, no)) # integer
typeof(ifelse(FALSE, yes, no))# double
}
\keyword{logic}
\keyword{programming}

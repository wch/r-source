% File src/library/base/man/ifelse.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2015 R Core Team
% Distributed under GPL 2 or later

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

  Missing values in \code{test} give missing values in the result.
}
\value{
  A vector of the same length and attributes (including dimensions and
  \code{"class"}) as \code{test} and data values from the values of
  \code{yes} or \code{no}.  The mode of the answer will be coerced from
  logical to accommodate first any values taken from \code{yes} and then
  any values taken from \code{no}.
}
\section{Warning}{
  The mode of the result may depend on the value of \code{test} (see the
  examples), and the class attribute (see \code{\link{oldClass}}) of the
  result is taken from \code{test} and may be inappropriate for the
  values selected from \code{yes} and \code{no}.

  Sometimes it is better to use a construction such as
\preformatted{  (tmp <- yes; tmp[!test] <- no[!test]; tmp)
}, possibly extended to handle missing values in \code{test}.

  Further note that \code{if(test) yes else no}  is much more efficient
  and often much preferable to \code{ifelse(test, yes, no)} whenever
  \code{test} is a simple true/false result, i.e., when
  \code{length(test) == 1}.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.
}
\seealso{
\code{\link{if}}.
}
\examples{
x <- c(6:-4)
sqrt(x)  #- gives warning
sqrt(ifelse(x >= 0, x, NA))  # no warning

## Note: the following also gives the warning !
ifelse(x >= 0, sqrt(x), NA)


## ifelse() strips attributes
## This is important when working with Dates and factors
x <- seq(as.Date("2000-02-29"), as.Date("2004-10-04"), by = "1 month")
## has many "yyyy-mm-29", but a few "yyyy-03-01" in the non-leap years
y <- ifelse(as.POSIXlt(x)$mday == 29, x, NA)
head(y) # not what you expected ... ==> need restore the class attribute:
class(y) <- class(x)
y
## ==> Again a case where it is better *not* to use ifelse(), but
## both more efficient and clear:
y2 <- x
y2[as.POSIXlt(x)$mday != 29] <- NA
stopifnot(identical(y2, y))


## example of different return modes:
yes <- 1:3
no <- pi^(0:3)
typeof(ifelse(NA,    yes, no)) # logical
typeof(ifelse(TRUE,  yes, no)) # integer
typeof(ifelse(FALSE, yes, no)) # double
}
\keyword{logic}
\keyword{programming}

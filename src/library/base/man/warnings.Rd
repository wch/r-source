% File src/library/base/man/warnings.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2014 R Core Team
% Distributed under GPL 2 or later

\name{warnings}
\title{Print Warning Messages}
\alias{warnings}
\alias{last.warning}
\alias{print.warnings}
\alias{[.warnings}
\alias{c.warnings}
\alias{unique.warnings}
\description{
  \code{warnings} and its \code{print} method print the
  variable \code{last.warning} in a pleasing form.
}
\usage{
warnings(\dots)
}
\arguments{
  \item{\dots}{arguments to be passed to \code{\link{cat}}.}
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.
}
\details{
  See the description of \code{\link{options}("warn")} for the
  circumstances under which there is a \code{last.warning} object and
  \code{warnings()} is used.  In essence this is if \code{options(warn =
    0)} and \code{warning} has been called at least once.

  It is possible that \code{last.warning} refers to the last recorded
  warning and not to the last warning, for example if \code{options(warn)} has
  been changed or if a catastrophic error occurred.
}
\section{Warning}{
  It is undocumented where \code{last.warning} is stored nor that it is
  visible, and this is subject to change.
}
\seealso{
  \code{\link{warning}}.
}
\value{an object of S3 class \code{"warnings"}, basically a named
  \code{\link{list}}.
}
\examples{
## NB this example is intended to be pasted in,
##    rather than run by example()
ow <- options("warn")
for(w in -1:1) {
   options(warn = w); cat("\n warn =", w, "\n")
   for(i in 1:3) { cat(i,"..\n"); m <- matrix(1:7, 3,4) }
}
warnings()
options(ow) # reset
tail(warnings(), 2) # see the last two warnings only (via '[' method)
\dontshow{
ww <- warnings()
uw <- unique(ww)
stopifnot(identical(c(ww[1], ww[3]), ww[c(1, 3)]),
          length(uw) == 1, nchar(names(uw)) > 10)
}
}
\keyword{programming}
\keyword{error}

% File src/library/base/man/noquote.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2017 R Core Team
% Distributed under GPL 2 or later

\name{noquote}
\alias{noquote}
\alias{print.noquote}
\alias{as.matrix.noquote}
\alias{c.noquote}
\alias{[.noquote}
\title{Class for \sQuote{no quote} Printing of Character Strings}
\description{
  Print character strings without quotes.
}
\usage{
noquote(obj, right = FALSE)

\method{print}{noquote}(x, \dots)

\method{c}{noquote}(\dots, recursive = FALSE)
}
\arguments{
  \item{obj}{any \R object, typically a vector of
    \code{\link{character}} strings.}
  \item{right}{optional \code{\link{logical}} eventually to be passed to
    \code{print()}, used by \code{\link{print.default}()}, indicating
    whether or not strings should be right aligned.}
  \item{x}{an object of class \code{"noquote"}.}
  \item{\dots}{further options passed to next methods, such as \code{\link{print}}.}
  \item{recursive}{for compatibility with the generic \code{\link{c}} function.}
}
\details{
  \code{noquote} returns its argument as an object of class
  \code{"noquote"}.  There is a method for \code{c()} and subscript
  method (\code{"[.noquote"}) which ensures that the class is not lost
  by subsetting.  The print method (\code{print.noquote}) prints
  character strings \emph{without} quotes (\code{"\dots"}).

  These functions exist both as utilities and as an example of using (S3)
  \code{\link{class}} and object orientation.
}
\author{Martin Maechler \email{maechler@stat.math.ethz.ch}}
\seealso{
  \code{\link{methods}}, \code{\link{class}}, \code{\link{print}}.
}
\examples{
letters
nql <- noquote(letters)
nql
nql[1:4] <- "oh"
nql[1:12]

cmp.logical <- function(log.v)
{
  ## Purpose: compact printing of logicals
  log.v <- as.logical(log.v)
  noquote(if(length(log.v) == 0)"()" else c(".","|")[1 + log.v])
}
cmp.logical(stats::runif(20) > 0.8)

chmat <- as.matrix(format(stackloss)) # a "typical" character matrix
## noquote(*, right=TRUE)  so it prints exactly like a data frame
chmat <- noquote(chmat, right = TRUE)
chmat
%% \dontshow{ ========   FIXME(?) problem in capture.output()
%% stopifnot(identical(
%%    capture.output(stackloss),
%%    capture.output(chmat)))
%% }%dont
}
\keyword{print}
\keyword{methods}
\keyword{utilities}

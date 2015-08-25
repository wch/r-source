% File src/library/stats/man/case.names.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2011 R Core Team
% Distributed under GPL 2 or later

\name{case+variable.names}
\alias{case.names}
\alias{case.names.lm}
\alias{variable.names}
\alias{variable.names.lm}
\title{Case and Variable Names of Fitted Models}
\description{
  Simple utilities returning (non-missing) case names, and
  (non-eliminated) variable names.
}
\usage{
case.names(object, \dots)
\method{case.names}{lm}(object, full = FALSE, \dots)

variable.names(object, \dots)
\method{variable.names}{lm}(object, full = FALSE, \dots)
}
\arguments{
 \item{object}{an \R object, typically a fitted model.}
 \item{full}{logical; if \code{TRUE}, all names (including zero weights,
   \dots) are returned.}
 \item{\dots}{further arguments passed to or from other methods.}
}
\value{
  A character vector.
}
\seealso{\code{\link{lm}}; further, \code{\link{all.names}},
  \code{\link{all.vars}} for functions with a similar name but only slightly
  related purpose.
}
\examples{
x <- 1:20
y <-  setNames(x + (x/4 - 2)^3 + rnorm(20, sd = 3),
               paste("O", x, sep = "."))
ww <- rep(1, 20); ww[13] <- 0
summary(lmxy <- lm(y ~ x + I(x^2)+I(x^3) + I((x-10)^2), weights = ww),
        cor = TRUE)
variable.names(lmxy)
variable.names(lmxy, full = TRUE)  # includes the last
case.names(lmxy)
case.names(lmxy, full = TRUE)      # includes the 0-weight case
}
\keyword{regression}
\keyword{models}

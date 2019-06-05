% File src/library/stats/man/smoothEnds.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2019 R Core Team
% Distributed under GPL 2 or later

\name{smoothEnds}
\title{End Points Smoothing (for Running Medians)}
\alias{smoothEnds}
\usage{
smoothEnds(y, k = 3)
}
\description{
  Smooth end points of a vector \code{y} using subsequently smaller
  medians and Tukey's end point rule at the very end. (of odd span),
}
\arguments{
  \item{y}{dependent variable to be smoothed (vector).}
  \item{k}{width of largest median window; must be odd.}
}
\value{vector of smoothed values, the same length as \code{y}.
}
\details{
  \code{smoothEnds} is used to only do the \sQuote{end point smoothing},
  i.e., change at most the observations closer to the beginning/end
  than half the window \code{k}.  The first and last value are computed using
  \emph{Tukey's end point rule}, i.e.,
  \code{sm[1] = median(y[1], sm[2], 3*sm[2] - 2*sm[3], na.rm=TRUE)}.

  In \R versions 3.6.0 and earlier, missing values (\code{\link{NA}})
  in \code{y} typically lead to an error, whereas now the equivalent of
  \code{\link{median}(*, na.rm=TRUE)} is used.
}
\references{
  John W. Tukey (1977)
  \emph{Exploratory Data Analysis}, Addison.

  Velleman, P.F., and Hoaglin, D.C. (1981)
  \emph{ABC of EDA (Applications, Basics, and Computing of Exploratory
    Data Analysis)}; Duxbury.
}
\author{Martin Maechler}
\seealso{
  \code{\link{runmed}(*, endrule = "median")} which calls
  \code{smoothEnds()}.
}
\examples{
require(graphics)

y <- ys <- (-20:20)^2
y [c(1,10,21,41)] <-  c(100, 30, 400, 470)
s7k <- runmed(y, 7, endrule = "keep")
s7. <- runmed(y, 7, endrule = "const")
s7m <- runmed(y, 7)
col3 <- c("midnightblue","blue","steelblue")
plot(y, main = "Running Medians -- runmed(*, k=7, endrule = X)")
lines(ys, col = "light gray")
matlines(cbind(s7k, s7.,s7m), lwd = 1.5, lty = 1, col = col3)
eRules <- c("keep","constant","median")
legend("topleft", paste("endrule", eRules, sep = " = "),
       col = col3, lwd = 1.5, lty = 1, bty = "n")

stopifnot(identical(s7m, smoothEnds(s7k, 7)))

## With missing values (for R >= 3.6.1):
yN <- y; yN[c(2,40)] <- NA
rN <- sapply(eRules, function(R) runmed(yN, 7, endrule=R))
matlines(rN, type = "b", pch = 4, lwd = 3, lty=2,
         col = adjustcolor(c("red", "orange4", "orange1"), 0.5))
yN[c(1, 20:21)] <- NA # additionally
rN. <- sapply(eRules, function(R) runmed(yN, 7, endrule=R))
head(rN., 4); tail(rN.) # more NA's too, still not *so* many:
stopifnot(exprs = {
   !anyNA(rN[,2:3])
   identical(which(is.na(rN[,"keep"])), c(2L, 40L))
   identical(which(is.na(rN.), arr.ind=TRUE, useNames=FALSE),
             rbind(cbind(c(1:2,37:38,40L), 1L),
                   cbind(37:41, 2L), cbind(37:38, 3L)))
   identical(rN.[39:41, "median"], c(324, 397, 470))
})
}
\keyword{smooth}
\keyword{robust}

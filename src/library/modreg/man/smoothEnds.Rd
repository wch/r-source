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
  \code{smoothEnds} is used to only do the \dQuote{end point smoothing},
  i.e., change at most the observations closer to the beginning/end
  than half the window \code{k}.  The first and last value are computed using
  \dQuote{\emph{Tukey's end point rule}}, i.e.,
  \code{sm[1] = median(y[1], sm[2], 3*sm[2] - 2*sm[3])}.
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
  \code{\link{runmed}(*, end.rule = "median")} which calls
  \code{smoothEnds()}.
}
\examples{
y <- ys <- (-20:20)^2
y [c(1,10,21,41)] <-  c(100, 30, 400, 470)
s7k <- runmed(y,7, end = "keep")
s7. <- runmed(y,7, end = "const")
s7m <- runmed(y,7)
col3 <- c("midnightblue","blue","steelblue")
plot(y, main = "Running Medians -- runmed(*, k=7, end.rule = X)")
lines(ys, col = "light gray")
matlines(cbind(s7k,s7.,s7m), lwd= 1.5, lty = 1, col = col3)
legend(1,470, paste("end.rule",c("keep","constant","median"),sep=" = "),
       col = col3, lwd = 1.5, lty = 1)

stopifnot(identical(s7m, smoothEnds(s7k, 7)))
}
\keyword{smooth}
\keyword{robust}

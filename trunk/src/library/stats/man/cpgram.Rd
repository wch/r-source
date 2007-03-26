% file cpgram.Rd
% copyright (C) 1994-9 W. N. Venables and B. D. Ripley
%
\name{cpgram}
\alias{cpgram}
\title{
  Plot Cumulative Periodogram
}
\description{
  Plots a cumulative periodogram.
}
\usage{
cpgram(ts, taper = 0.1,
       main = paste("Series: ", deparse(substitute(ts))),
       ci.col = "blue")
}
\arguments{
  \item{ts}{a univariate time series}
  \item{taper}{proportion tapered in forming the periodogram}
  \item{main}{main title}
  \item{ci.col}{colour for confidence band.}
}
\value{
  None.
}
\section{Side Effects}{
  Plots the cumulative periodogram in a square plot.
}
\author{B.D. Ripley}
\note{
  From package \pkg{MASS}.
}
\examples{
par(pty = "s", mfrow = c(1,2))
cpgram(lh)
lh.ar <- ar(lh, order.max = 9)
cpgram(lh.ar$resid, main = "AR(3) fit to lh")

cpgram(ldeaths)
}
\keyword{ts}
\keyword{hplot}

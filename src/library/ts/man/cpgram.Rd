% file MASS/cpgram.d
% copyright (C) 1994-9 W. N. Venables and B. D. Ripley
%
\name{cpgram}
\alias{cpgram}
\title{
Plot Cumulative Periodogram
}
\description{
Plots a cumulative periodogram
}
\usage{
cpgram(ts, taper=0.1, main=
         paste("Series: ", deparse(substitute(ts))))
}
\arguments{
\item{ts}{
time series
}
\item{taper}{
proportion tapered in forming the periodogram
}
\item{main}{
main title
}}
\value{
old parameter settings
}
\section{Side Effects}{
plots the cumulative periodogram in a square plot.
}
\author{B.D. Ripley}
\note{
From package \code{MASS}.
}
\examples{
par(pty="s", mfrow=c(1,2))
data(lh)
cpgram(lh)
lh.ar <- ar(lh, order.max=9)
cpgram(lh.ar$resid, main="AR(3) fit to lh")

data(deaths)
cpgram(deaths)
}
\keyword{ts}
\keyword{hplot}

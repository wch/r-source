% file modreg/man/plot.ppr.Rd
% copyright (C) 1995-8 B. D. Ripley
%
\name{plot.ppr}
\alias{plot.ppr}
\title{
Plot Ridge Functions for Projection Pursuit Regression Fit
}
\usage{
plot.ppr(fit, ask, type="o", \dots)
}
\arguments{
\item{fit}{
A fit of class \code{"ppr"} as produced by a call to \code{ppr}.
}
\item{ask}{
the graphics parameter \code{ask}: see \code{par} for details. If set to \code{True}
will ask between the plot of each cross-section.
}
\item{type}{
the type of line to draw
}
\item{...}{
further graphical parameters
}}
\value{
None
}
\section{Side Effects}{
A series of plots are drawn on the current graphical device, one for
each term in the fit.
}
\seealso{
\code{\link{ppr}}, \code{\link{par}}
}
\examples{
}
\keyword{hplot}
% Converted by Sd2Rd version 0.2-a5.

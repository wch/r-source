% File src/library/stats/man/plot.ppr.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

% file modreg/man/plot.ppr.Rd
% copyright (C) 1995-8 B. D. Ripley
%
\name{plot.ppr}
\alias{plot.ppr}
\title{
Plot Ridge Functions for Projection Pursuit Regression Fit
}
\description{
Plot ridge functions for projection pursuit regression fit.
}
\usage{
\method{plot}{ppr}(x, ask, type = "o", \dots)
}
\arguments{
  \item{x}{
    A fit of class \code{"ppr"} as produced by a call to \code{ppr}.
  }
  \item{ask}{
    the graphics parameter \code{ask}: see \code{par} for details.
    If set to \code{TRUE} will ask between the plot of each cross-section.
  }
  \item{type}{
    the type of line to draw
  }
  \item{\dots}{
    further graphical parameters
  }
}
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
require(graphics)

with(rock, {
area1 <- area/10000; peri1 <- peri/10000
par(mfrow = c(3,2)) # maybe: , pty = "s")
rock.ppr <- ppr(log(perm) ~ area1 + peri1 + shape,
                data = rock, nterms = 2, max.terms = 5)
plot(rock.ppr, main = "ppr(log(perm)~ ., nterms=2, max.terms=5)")
plot(update(rock.ppr, bass = 5), main = "update(..., bass = 5)")
plot(update(rock.ppr, sm.method = "gcv", gcvpen = 2),
     main = "update(..., sm.method=\"gcv\", gcvpen=2)")
})
}
\keyword{hplot}

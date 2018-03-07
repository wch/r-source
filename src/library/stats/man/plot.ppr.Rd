% File src/library/stats/man/plot.ppr.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2016 R Core Team
% Distributed under GPL 2 or later

% file modreg/man/plot.ppr.Rd
% copyright (C) 1995-8 B. D. Ripley
%
\name{plot.ppr}
\alias{plot.ppr}
\title{Plot Ridge Functions for Projection Pursuit Regression Fit}
\description{
  Plot the ridge functions for a projection pursuit regression
  (\code{\link{ppr}}) fit.
}
\usage{
\method{plot}{ppr}(x, ask, type = "o", cex = 1/2,
     main = quote(bquote(
         "term"[.(i)]*":" ~~ hat(beta[.(i)]) == .(bet.i))),
     xlab = quote(bquote(bold(alpha)[.(i)]^T * bold(x))),
     ylab = "", \dots)
}
\arguments{
  \item{x}{an \R object of class \code{"ppr"} as produced by a call to
    \code{ppr}.}
  \item{ask}{
    the graphics parameter \code{ask}: see \code{\link{par}} for details.
    If set to \code{TRUE} will ask between the plot of each cross-section.
  }
  \item{type}{the type of line (see \code{\link{plot.default}}) to draw.}
  \item{cex}{plot symbol expansion factor (\emph{relative} to
    \code{\link{par}("cex")}).}
  \item{main, xlab, ylab}{axis annotations, see also
    \code{\link{title}}.  Can be an expression (depending on \code{i} and
    \code{bet.i}), as by default which will be \code{eval()}uated.}
  \item{\dots}{further graphical parameters, passed to
    \code{\link{plot}()}.}
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

rock1 <- within(rock, { area1 <- area/10000; peri1 <- peri/10000 })
par(mfrow = c(3,2)) # maybe: , pty = "s"
rock.ppr <- ppr(log(perm) ~ area1 + peri1 + shape,
                data = rock1, nterms = 2, max.terms = 5)
plot(rock.ppr, main = "ppr(log(perm)~ ., nterms=2, max.terms=5)")
plot(update(rock.ppr, bass = 5), main = "update(..., bass = 5)")
plot(update(rock.ppr, sm.method = "gcv", gcvpen = 2),
     main = "update(..., sm.method=\"gcv\", gcvpen=2)")
}
\keyword{hplot}

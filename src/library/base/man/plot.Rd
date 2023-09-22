% File src/library/base/man/plot.Rd
% (Previously src/library/graphics/man/plot.Rd for R < 4.0.0)
% Part of the R package, https://www.R-project.org
% Copyright 1995-2020 R Core Team
% Distributed under GPL 2 or later

\name{plot}
\title{Generic X-Y Plotting}
\alias{plot}
\description{
  Generic function for plotting of \R objects.

  For simple scatter plots, \code{\link{plot.default}} will be used.
  However, there are \code{plot} methods for many \R objects,
  including \code{\link{function}}s, \code{\link{data.frame}}s,
  \code{\link{density}} objects, etc.  Use \code{methods(plot)} and
  the documentation for these. Most of these methods are implemented
  using traditional graphics (the \pkg{graphics} package), but this is
  not mandatory.

  For more details about graphical parameter arguments used by
  traditional graphics, see \code{\link{par}}.
}
\usage{
plot(x, y, \dots)
}
\arguments{
  \item{x}{the coordinates of points in the plot. Alternatively, a
      single plotting structure, function or \emph{any \R object with a
        \code{plot} method} can be provided.}
  \item{y}{the y coordinates of points in the plot, \emph{optional}
    if \code{x} is an appropriate structure.}
  \item{\dots}{arguments to be passed to methods, such as
    \link{graphical parameters} (see \code{\link{par}}).
    Many methods will accept the following arguments:
    \describe{
    \item{\code{type}}{what type of plot should be drawn.  Possible types are
      \itemize{
        \item \code{"p"} for \bold{p}oints,
        \item \code{"l"} for \bold{l}ines,
        \item \code{"b"} for \bold{b}oth,
        \item \code{"c"} for the lines part alone of \code{"b"},
        \item \code{"o"} for both \sQuote{\bold{o}verplotted},
        \item \code{"h"} for \sQuote{\bold{h}istogram} like (or
        \sQuote{high-density}) vertical lines,
        \item \code{"s"} for stair \bold{s}teps,
        \item \code{"S"} for other \bold{s}teps, see \sQuote{Details} below,
        \item \code{"n"} for no plotting.
      }
      All other \code{type}s give a warning or an error; using, e.g.,
      \code{type = "punkte"} being equivalent to \code{type = "p"} for S
      compatibility.  Note that some methods,
      e.g.\sspace{}\code{\link{plot.factor}}, do not accept this.
    }
    \item{\code{main}}{an overall title for the plot: see \code{\link{title}}.}
    \item{\code{sub}}{a subtitle for the plot: see \code{\link{title}}.}
    \item{\code{xlab}}{a title for the x axis: see \code{\link{title}}.}
    \item{\code{ylab}}{a title for the y axis: see \code{\link{title}}.}
    \item{\code{asp}}{the \eqn{y/x} aspect ratio,
      see \code{\link{plot.window}}.}
    }
  }
}
\details{
  The two step types differ in their x-y preference: Going from
  \eqn{(x1,y1)} to \eqn{(x2,y2)} with \eqn{x1 < x2}, \code{type = "s"}
  moves first horizontal, then vertical, whereas \code{type = "S"} moves
  the other way around.
}
\note{
  The \code{plot} generic was moved from the \pkg{graphics} package to
  the \pkg{base} package in \R 4.0.0. It is currently re-exported from
  the \pkg{graphics} namespace to allow packages importing it from there
  to continue working, but this may change in future versions of \R.
}
\seealso{
  \code{\link{plot.default}}, \code{\link{plot.formula}} and other
  methods; \code{\link{points}}, \code{\link{lines}}, \code{\link{par}}.
  For thousands of points, consider using \code{\link{smoothScatter}()}
  instead of \code{plot()}.

  For X-Y-Z plotting see \code{\link{contour}}, \code{\link{persp}} and
  \code{\link{image}}.
}
\examples{
require(stats) # for lowess, rpois, rnorm
require(graphics) # for plot methods
plot(cars)
lines(lowess(cars))

plot(sin, -pi, 2*pi) # see ?plot.function

## Discrete Distribution Plot:
plot(table(rpois(100, 5)), type = "h", col = "red", lwd = 10,
     main = "rpois(100, lambda = 5)")

## Simple quantiles/ECDF, see ecdf() {library(stats)} for a better one:
plot(x <- sort(rnorm(47)), type = "s", main = "plot(x, type = \"s\")")
points(x, cex = .5, col = "dark red")
}
\keyword{hplot}

\name{plot}
\title{Generic X-Y Plotting}
\alias{plot}
\usage{
%-- this is not really ok:  plot(x , ...)  is now the GENERIC function
plot(x, \dots)
plot(x, y, xlim=range(x), ylim=range(y), type="p",
        main, xlab, ylab, \dots)
plot(y ~ x, \dots)
}
\arguments{
  \item{x}{the coordinates of points in the plot. Alternatively, a
      single plotting structure or \emph{any \R object with a
	  \code{plot} method} can be provided.}
  \item{y}{the y coordinates of points in the plot, \emph{optional}
      if \code{x} is an appropriate structure.}
  \item{xlim, ylim}{the ranges to be encompassed by the x and y axes.}
  \item{type}{what type of plot should be drawn. Possible types are
    \itemize{
      \item \code{"p"} for \bold{p}oints,
      \item \code{"l"} for \bold{l}ines,
      \item \code{"b"} for \bold{b}oth,
      \item \code{"o"} for both ``\bold{o}verplotted'',
      \item \code{"h"} for ``\bold{h}istogram'' like vertical lines,
      \item \code{"s"} for \bold{s}teps,
      \item \code{"S"} for other \bold{s}teps,
      \item \code{"n"} for no plotting.
    }
  }
  \item{main}{an overall title for the plot.}
  \item{xlab}{a title for the x axis.}
  \item{ylab}{a title for the y axis.}
  \item{\dots}{graphical parameters can be given as arguments to
    \code{plot}.}
}
\description{
  Generic function for plotting of \R objects.  For more details about
  the graphical parameter arguments, see \code{\link{par}}.
}
\details{
    For simple scatter plots, \code{\link{plot.default}} will be used.
    However, there are \code{plot} methods for many \R objects,
    including \code{\link{function}}s, \code{\link{data.frame}}s,
    \code{\link{density}} objects, etc.  Use \code{methods(plot)} and
    the documentation for these.
}
\seealso{
  \code{\link{plot.default}}, \code{\link{plot.formula}} and other
  methods; \code{\link{points}}, \code{\link{lines}}, \code{\link{par}}.
}
\examples{
data(cars)
plot(cars)
lines(lowess(cars))

plot(sin, -pi, 2*pi)

## Discrete Distribution Plot:
plot(table(rpois(100,5)), type = "h", col = "red", lwd=10,
     main="rpois(100,lambda=5)")
}
\keyword{hplot}

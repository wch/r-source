\name{plot}
\title{X-Y Plotting}
\alias{plot}
\usage{
%-- this is not really ok:  plot(x , ...)  is now the GENERIC function
plot(x, y, xlim=range(x), ylim=range(y), type="p",
        main, xlab, ylab, \dots)
plot(y ~ x, \dots)
}
\arguments{
  \item{x,y}{the coordinates of points in the plot. Alternatively, a
    single plotting structure can be provided.}
  \item{xlim,ylim}{the ranges to be encompased by the x and y axes.}
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

  A longer description will go here eventually.
}
\seealso{
  \code{\link{plot.default}}, \code{\link{plot.formula}} and other
  methods; \code{\link{points}}, \code{\link{lines}}, \code{\link{par}}.
}
\examples{
data(cars)
plot(cars)
lines(lowess(cars))
}
\keyword{hplot}

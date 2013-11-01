% File src/library/grid/man/legendGrob.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2013 R Core Team
% Distributed under GPL 2 or later

\name{legendGrob}
\title{Constructing a Legend Grob}
\alias{legendGrob}
\alias{grid.legend}
\usage{
legendGrob(labels, ncol = 1, nrow, byrow = FALSE,
           do.lines = has.lty || has.lwd, lines.first = TRUE,
           hgap = unit(1, "lines"), vgap = unit(1, "lines"),
           default.units = "lines", pch, gp = gpar(), vp = NULL)

grid.legend(\dots, draw=TRUE)
}
\arguments{
  \item{labels}{legend labels (expressions)}
  \item{ncol, nrow}{integer; the number of columns or rows, respectively
    of the legend \dQuote{layout}.  \code{nrow} is optional and
    typically computed from the number of labels and \code{ncol}.}
  \item{byrow}{logical indicating whether rows of the legend are filled first.}
  \item{do.lines}{logical indicating whether legend lines are drawn.}
  \item{lines.first}{logical indicating whether legend lines are drawn
    first and hence in a plain \dQuote{below} legend symbols.}
  \item{hgap}{horizontal space between the legend entries}
  \item{vgap}{vertical space between the legend entries}
  \item{default.units}{default units, see \code{\link{unit}}.}
  \item{pch}{legend symbol, numeric or character, passed to
    \code{\link{pointsGrob}()}; see also \code{\link[graphics]{points}} for
    interpreation of the numeric codes.}
  \item{gp}{an \R object of class \code{gpar}, typically the output
    from a call to the function \code{gpar}, is basically a list of
    graphical parameter settings.}
  \item{vp}{a Grid \code{\link{viewport}} object (or \code{NULL}).}
  \item{\dots}{for \code{grid.legend()}: all the arguments above are
    passed to \code{legendGrob()}.}
  \item{draw}{logical indicating whether graphics output should be produced.}
}
\value{
  Both functions create a legend \code{\link{grob}} (a graphical object
  describing a plot legend), but only \code{grid.legend} draws it (only
  if \code{draw} is \code{TRUE}).
}
%\author{Martin Maechler assisted by Marius Hofert, building on Paul Murrell's}
\seealso{
  \link{Grid}, \code{\link{viewport}};
  \code{\link{pointsGrob}}, \code{\link{linesGrob}}.
}
\examples{
## add !
}
\keyword{aplot}


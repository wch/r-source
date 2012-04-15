% File src/library/grDevices/man/colors.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2011 R Core Team
% Distributed under GPL 2 or later

\name{colors}
\alias{colors}
\alias{colours}
\title{Color Names}
\description{
  Returns the built-in color names which \R knows about.
}
\usage{
colors()
colours()
}
\details{
  These color names can be used with a \code{col=} specification in
  graphics functions.

  An even wider variety of colors can be created with primitives
  \code{rgb} and \code{hsv} or the derived \code{rainbow},
  \code{heat.colors}, etc.
}
\value{
  A character vector containing all the built-in color names.
}
\seealso{
  \code{\link{palette}} for setting the \sQuote{palette} of colors for
  \code{par(col=}\emph{<num>}\code{)};
  \code{\link{rgb}}, \code{\link{hsv}}, \code{\link{hcl}}, \code{\link{gray}};
  \code{\link{rainbow}} for a nice example;
  and \code{\link{heat.colors}}, \code{\link{topo.colors}} for images.

  \code{\link{col2rgb}} for translating to RGB numbers and extended
  examples.
}
\examples{
cl <- colors()
length(cl); cl[1:20]

### ----------- Show (almost) all named colors ---------------------

## 1) with traditional 'graphics' package:
showCols1 <- function(bg = "gray", cex = 0.75, srt = 30) {
    m <- ceiling(sqrt(n <- length(cl <- colors())))
    length(cl) <- m*m; cm <- matrix(cl, m)
    ##
    require("graphics")
    op <- par(mar=rep(0,4), ann=FALSE, bg = bg); on.exit(par(op))
    plot(1:m,1:m, type="n", axes=FALSE)
    text(col(cm), rev(row(cm)), cm,  col = cl, cex=cex, srt=srt)
}
showCols1()

## 2) with 'grid' package:
showCols2 <- function(bg = "grey", cex = 0.75, rot = 30) {
    m <- ceiling(sqrt(n <- length(cl <- colors())))
    length(cl) <- m*m; cm <- matrix(cl, m)
    ##
    require("grid")
    grid.newpage(); vp <- viewport(w = .92, h = .92)
    grid.rect(gp=gpar(fill=bg))
    grid.text(cm, x = col(cm)/m, y = rev(row(cm))/m, rot = rot,
              vp=vp, gp=gpar(cex = cex, col = cm))
}
showCols2()
showCols2(bg = "gray33")
}
\keyword{color}
\keyword{dplot}
\keyword{sysdata}

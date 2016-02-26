% File src/library/graphics/man/title.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2016 R Core Team
% Distributed under GPL 2 or later

\name{title}
\title{Plot Annotation}
\usage{
title(main = NULL, sub = NULL, xlab = NULL, ylab = NULL,
      line = NA, outer = FALSE, \dots)
}
\alias{title}
\arguments{
  \item{main}{The main title (on top) using font, size (character
    expansion) and color \code{par(c("font.main", "cex.main", "col.main"))}.}
  \item{sub}{Sub-title (at bottom) using font, size and color 
    \code{par(c("font.sub", "cex.sub", "col.sub"))}.}
  \item{xlab}{X axis label using font, size and color 
    \code{par(c("font.lab", "cex.lab", "col.lab"))}.}
  \item{ylab}{Y axis label, same font attributes as \code{xlab}.}
  \item{line}{specifying a value for \code{line} overrides the
    default placement of labels, and places them this many lines
    outwards from the plot edge.}
  \item{outer}{a logical value.  If \code{TRUE}, the titles are
    placed in the outer margins of the plot.}
  \item{\dots}{further \link{graphical parameters} from \code{\link{par}}.  Use
    e.g., \code{col.main} or \code{cex.sub} instead of just \code{col}
    or \code{cex}.  \code{adj} controls the justification of the titles.
    \code{xpd} can be used to set the clipping region:
    this defaults to the figure region unless \code{outer = TRUE},
    otherwise the device region and can only be increased.  \code{mgp}
    controls the default placing of the axis titles.
  }
}
\description{
  This function can be used to add labels to a plot.  Its first four
  principal arguments can also be used as arguments in most high-level
  plotting functions.  They must be of type \code{\link{character}} or
  \code{\link{expression}}. In the latter case, quite a bit of
  mathematical notation is available such as sub- and superscripts,
  greek letters, fractions, etc: see \link{plotmath}
}
\details{
  The labels passed to \code{title} can be character strings or
  language objects (names, calls or expressions), or a list
  containing the string to be plotted, and a selection of the optional
  modifying \link{graphical parameters} \code{cex=}, \code{col=} and
  \code{font=}.  Other objects will be coerced by
  \code{\link{as.graphicsAnnot}}.

  The position of \code{main} defaults to being vertically centered in
  (outer) margin 3 and justified horizontally according to
  \code{par("adj")} on the plot region (device region for
  \code{outer = TRUE}).

  The positions of \code{xlab}, \code{ylab} and \code{sub} are
  \code{line} (default for \code{xlab} and \code{ylab} being
  \code{par("mgp")[1]} and increased by \code{1} for \code{sub}) lines
  (of height \code{par("mex")}) into the appropriate margin, justified
  in the text direction according to \code{par("adj")} on the
  plot/device region.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.
}
\seealso{
  \code{\link{mtext}}, \code{\link{text}};
  \code{\link{plotmath}} for details on mathematical annotation.
}
\examples{
plot(cars, main = "") # here, could use main directly
title(main = "Stopping Distance versus Speed")

plot(cars, main = "")
title(main = list("Stopping Distance versus Speed", cex = 1.5,
                  col = "red", font = 3))

## Specifying "..." :
plot(1, col.axis = "sky blue", col.lab = "thistle")
title("Main Title", sub = "sub title",
      cex.main = 2,   font.main= 4, col.main= "blue",
      cex.sub = 0.75, font.sub = 3, col.sub = "red")


x <- seq(-4, 4, len = 101)
y <- cbind(sin(x), cos(x))
matplot(x, y, type = "l", xaxt = "n",
        main = expression(paste(plain(sin) * phi, "  and  ",
                                plain(cos) * phi)),
        ylab = expression("sin" * phi, "cos" * phi), # only 1st is taken
        xlab = expression(paste("Phase Angle ", phi)),
        col.main = "blue")
axis(1, at = c(-pi, -pi/2, 0, pi/2, pi),
     labels = expression(-pi, -pi/2, 0, pi/2, pi))
abline(h = 0, v = pi/2 * c(-1,1), lty = 2, lwd = .1, col = "gray70")
}
\keyword{aplot}

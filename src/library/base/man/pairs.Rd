\name{pairs}
\title{Scatterplot Matrices}
\alias{pairs}
\alias{pairs.default}
\usage{
pairs(x, \dots)
pairs.default(x, labels = colnames(x), panel = points, \dots,
              lower.panel = panel, upper.panel = panel,
              diag.panel = NULL, text.panel = textPanel,
              label.pos = 0.5 + has.diag/3,
              cex.labels = NULL, font.labels = 1,
              row1attop = TRUE)
}
\synopsis{
pairs(x, \dots)
pairs.default(x, labels = colnames(x), panel = points, \dots,
              main = NULL, oma = NULL,
              font.main = par("font.main"), cex.main = par("cex.main"),
              lower.panel = panel, upper.panel = panel,
              diag.panel = NULL, text.panel = textPanel,
              label.pos = 0.5 + has.diag/3,
              cex.labels = NULL, font.labels = 1,
              row1attop = TRUE)
}
}
\arguments{
  \item{x}{the coordinates of points given as columns of a matrix.}
  \item{labels}{the names of the variables.}
  \item{panel}{\code{function(x,y,\dots)} which is used to plot
    the contents of each panel of the display.}
  \item{\dots}{graphical parameters can be given as arguments to
    \code{plot}.}
  \item{lower.panel, upper.panel}{separate panel functions to be used
    below and above the diagonal respectively.}
  \item{diag.panel}{optional \code{function(x, \dots)} to be
    applied on the diagonals.}
  \item{text.panel}{optional \code{function(x, y, labels, cex,
      font, \dots)} to be applied on the diagonals.}
  \item{label.pos}{\code{y} position of labels in the text panel.}
  \item{cex.labels, font.labels}{graphics parameters for the text panel.}
  \item{row1attop}{logical. Should the layout be matrix-like with row 1
    at the top, or graph-like with row 1 at the bottom?}
}
\description{
  A matrix of scatterplots is produced.
}
\details{
  The \eqn{ij}th scatterplot contains \code{x[,i]} plotted against
  \code{x[,j]}.  The ``scatterplot' can be customised by setting panel
  functions to appear as something completely different. The
  off-diagonal panel functions are passed the appropriate columns of
  \code{x} as \code{x} and \code{y}: the diagonal panel function (if
  any) is passed a single column, and the \code{text.panel} function is
  passed a single \code{(x, y)} location and the column name.

  The graphical parameters \code{pch} and \code{col} can be used to
  specify a vector of plotting symbols and colors to be used in the
  plots.

  The graphical parameter \code{oma} will be set by \code{pairs.default}
  unless supplied as an argument.
}
\author{
  Enhancements for \R 1.0.0 contributed by Dr. Jens
  Oehlschlaegel-Akiyoshi and R-core members.
}
\examples{
data(iris)
pairs(iris[1:4], main = "Anderson's Iris Data -- 3 species", 
      pch = 21, bg = c("red", "green3", "blue")[codes(iris$Species)])

data(USJudgeRatings)
pairs(USJudgeRatings)

## put histograms on the diagonal
panel.hist <- function(x, ...)
{
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(usr[1:2], 0, 1.5) )
    h <- hist(x, plot = FALSE)
    breaks <- h$breaks; nB <- length(breaks)
    y <- h$counts; y <- y/max(y)
    rect(breaks[-nB], 0, breaks[-1], y, col="cyan", ...)
}
pairs(USJudgeRatings[1:5], panel=panel.smooth,
      cex = 1.5, pch = 24, bg="light blue",
      diag.panel=panel.hist, cex.labels = 2, font.labels=2)

## put (absolute) correlations on the upper panels,
## with size proportional to the correlations.
panel.cor <- function(x, y, digits=2, prefix="", cex.cor)
{
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    r <- abs(cor(x, y))
    txt <- format(c(r, 0.123456789), digits=digits)[1]
    txt <- paste(prefix, txt, sep="")
    if(missing(cex.cor)) cex <- 0.8/strwidth(txt)
    text(0.5, 0.5, txt, cex = cex * r)
}
pairs(USJudgeRatings, lower.panel=panel.smooth, upper.panel=panel.cor)
}
\keyword{hplot}

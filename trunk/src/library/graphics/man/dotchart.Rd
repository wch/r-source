\name{dotchart}
\alias{dotchart}
\title{Cleveland Dot Plots}
\description{
  Draw a Cleveland dot plot.
}
\usage{
dotchart(x, labels = NULL, groups = NULL, gdata = NULL,
         cex = par("cex"), pch = 21, gpch = 21, bg = par("bg"),
         color = par("fg"), gcolor = par("fg"), lcolor = "gray",
         xlim = range(x[is.finite(x)]),
         main = NULL, xlab = NULL, ylab = NULL, \dots)
}
\arguments{
  \item{x}{either a vector or matrix of numeric values (\code{NA}s are
    allowed).  If \code{x} is a matrix the overall plot consists of
    juxtaposed dotplots for each row.}
  \item{labels}{a vector of labels for each point.
    For vectors the default is to use \code{names(x)} and for matrices
    the row labels \code{dimnames(x)[[1]]}.}
  \item{groups}{an optional factor indicating how the elements of
    \code{x} are grouped.
    If \code{x} is a matrix, \code{groups} will default to the columns
    of \code{x}.}
  \item{gdata}{data values for the groups.
    This is typically a summary such as the median or mean
    of each group.}
  \item{cex}{the character size to be used.  Setting \code{cex}
    to a value smaller than one can be a useful way of avoiding label
    overlap.  Unlike many other graphics functions, this sets the actual
    size, not a multiple of \code{par("cex")}.}
  \item{pch}{the plotting character or symbol to be used.}
  \item{gpch}{the plotting character or symbol to be used for group
    values.}
  \item{bg}{the background color of plotting characters or symbols to be
    used; use \code{\link{par}(bg= *)} to set the background color of
    the whole plot.}
  \item{color}{the color(s) to be used for points and labels.}
  \item{gcolor}{the single color to be used for group labels and
    values.}
  \item{lcolor}{the color(s) to be used for the horizontal lines.}
  \item{xlim}{horizontal range for the plot, see
    \code{\link{plot.window}}, e.g.}
  \item{main}{overall title for the plot, see \code{\link{title}}.}
  \item{xlab, ylab}{axis annotations as in \code{title}.}
  \item{\dots}{graphical parameters can also be specified as arguments.}
}
\value{
  This function is invoked for its side effect, which is to produce two
  variants of dotplots as described in Cleveland (1985).

  Dot plots are a reasonable substitute for bar plots.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole.

  Cleveland, W. S. (1985)
  \emph{The Elements of Graphing Data.}
  Monterey, CA: Wadsworth.

  Murrell, P. (2005) \emph{R Graphics}. Chapman & Hall/CRC Press.
}
\examples{
dotchart(VADeaths, main = "Death Rates in Virginia - 1940")
op <- par(xaxs="i")# 0 -- 100\%
dotchart(t(VADeaths), xlim = c(0,100),
         main = "Death Rates in Virginia - 1940")
par(op)
}
\keyword{hplot}

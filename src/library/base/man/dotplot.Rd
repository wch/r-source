\name{dotplot}
\title{Cleveland Dot Plots}
\usage{
dotplot(x, labels = NULL, groups = NULL, gdata = NULL, cex = par("cex"),
        pch = 21, gpch = 21, bg = par("bg"), color = par("fg"),
        gcolor = par("fg"), lcolor = "gray", ...)
}
\alias{dotplot}
\arguments{
  \item{x}{either a vector or matrix of numeric values
    (\code{NA}s are allowed).
    If \code{x} is a matrix the overall plot consists of
    juxtaposed dotplots for each row.}
  \item{labels}{a vector of labels for each point.
    For vectors the default is to use \code{names(x)} and for
    matrices the row labels \code{dimnames(x)[[1]]}.}
  \item{groups}{an optional factor indicating how the
    elements of \code{x} are grouped.
    If \code{x} is a matrix, \code{groups} will default to the columns
    of \code{x}.}
  \item{gdata}{data values for the groups.
    This is typically a summary such as the median or mean
    of each group.}
  \item{cex}{the character size to be used.  Setting \code{cex}
    to a value smaller than one can be a useful way of avoiding label
    overlap.}
  \item{pch}{the plotting character or symbol to be used.}
  \item{gpch}{the plotting character or symbol to be usea for group
    values.}
  \item{bg}{the background color to be used.}
  \item{color}{the color to be used for points an labels.}
  \item{gcolor}{the color to be used for group labels and values.}
  \item{lcolor}{the color to be used for the horizontal lines.}
  \item{\dots}{graphical parameters can also be specified as arguments.}
}
\value{
  This function is invoked for its side effect, which is to produce two
  variants of dotplots as described in Cleveland's graphics book.

  Dot plots are a reasonable substitute for bar plots.}
\references{
  Cleveland, W. S. (1985).
  \emph{The Elements of Graphing Data},
  Monterey, CA: Wadsworth.
}
\examples{
data(deaths)
dotplot(deaths, main = "Death Rates in Virginia - 1940")
dotplot(t(deaths), main = "Death Rates in Virginia - 1940")
}
\keyword{hplot}

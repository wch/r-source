\name{mosaicplot}
\alias{mosaicplot}
\title{Mosaic Plots}
\usage{
mosaicplot(x, main = NA, sort = NA, off = NA, dir = NA, color = FALSE)
}
\arguments{
    \item{x}{a contingency table, with optional category labels
	specified in the \code{dimnames(x)} attribute.  The table is
	best created by the \code{table()} command, which produces an
	object of type array.}
    \item{main}{character string for the mosaic title.}
    \item{sort}{vector ordering of the variables, containing a
	permutation of the integers \code{1:length(dim(x))} (the
	default).}
    \item{off}{vector of offsets to determine percentage spacing at each
	level of the mosaic (appropriate values are between 0 and 20,
	and the default is 10 at each level).  There should be one
	offset for each dimension of the contingency table.}
    \item{dir}{vector of split directions (\code{"v"} for vertical and
	\code{"h"} for horizontal) for each level of the mosaic, one
	direction for each dimension of the contingency table.  The
	default consists of alternating directions, beginning with a
	vertical split.}
    \item{color}{(\code{TRUE} or vector of integer colors) for color
	shading or (\code{FALSE}, the default) for empty boxes with no
	shading.}
}
\description{Plots a mosaic on the current graphics device.}
\details{
    See Emerson (1998) for more information and a case study with
    television viewer data from Nielsen Media Research.
}
\author{
    S-PLUS original by John Emerson \email{emerson@stat.yale.edu}.
    Slightly modified for R by KH.
}
\references{
    John W. Emerson (1998).
    Mosaic displays in S-PLUS: a general implementation and a case
    study.
    \emph{Statistical Computing and Graphics Newsletter}, \bold{9}, 1,
    17--23.
    
    The home page of Michael Friendly
    (\url{http://hotspur.psych.yorku.ca/SCS/friendly.html}) provides
    information on various aspects of graphical methods for analyzing
    categorical data, including mosaic plots.
}
\examples{
Y <- table(trunc(3*runif(1000)), trunc(3*runif(1000)),
           trunc(5*runif(1000))-10, trunc(3*runif(1000)))
dimnames(Y)[[2]] <- c("Cat", "Dog", "Horse")
mosaicplot(Y, main = "Sample Mosaic", color = TRUE)
}

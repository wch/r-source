\name{layout}
\title{Specifying complex plot arrangements}
\author{Paul R. Murrell}
\usage{
layout(mat,
       widths = rep(1, dim(mat)[2]),
       heights= rep(1, dim(mat)[1]),
       respect= FALSE)
layout.show(n = 1)
}
\alias{layout}
\alias{layout.show}
\arguments{
\item{mat}{a matrix object specifying the location of the next \eqn{N} figures
  on the output device.  Each value in the matrix must be \code{0} or a
  positive integer.	If \eqn{N} is the largest positive integer in
  the matrix, then the integers \eqn{\{1,\dots,N-1\}} must also appear
  at least once in the matrix.}
\item{widths}{a vector of values for the widths of columns on the
  device.  Relative widths are specified with numeric values.	 Absolute
  widths (in centimetres) are specified with the \code{lcm()} function (see
  examples).}
\item{heights}{a vector of values for the heights of rows on the device.
  Relative and absolute heights can be specified, see \code{widths} above.}
\item{respect}{either a logical value or a matrix object.  If the
  latter, then it must have the same dimensions as \code{mat} and
  each value in the matrix must be either \code{0} or \code{1}.}
}
\description{
\code{layout} divides the device up into as many rows and columns as
there are in \code{mat}, with the column-widths and the row-heights specified
in the respective arguments.  Figure i is allocated a region composed
from a subset
of these rows and columns, based on the rows and columns in which i
occurs
in \code{mat}.	The respect argument controls whether a unit column-width is
the same physical measurement on the device as a unit row-height.
}
\value{
The number of figures, \eqn{N}, see above, is returned.
}
\references{
Chapter 5 of Paul Murrell's Ph.D. thesis.
}
\seealso{
\code{\link{par}(mfrow=..)}, \code{\link{par}(mfcol=..)}
and \code{\link{par}(mfg=..)}
}
\examples{
 def.par <- par()# save default, for resetting...

 ## divide the device into two rows and two columns
 ## allocate figure 1 all of row 1
 ## allocate figure 2 the intersection of column 2 and row 2
 layout(matrix(c(1,1,0,2), 2, 2, byrow=T))
 ## show the regions that have been allocated to each plot
 layout.show(2)

 ## divide device into two rows and two columns
 ## allocate figure 1 and figure 2 as above
 ## respect relations between widths and heights
 nf <- layout(matrix(c(1,1,0,2), 2, 2, byrow=T), respect=T)
 layout.show(nf)

 ## create single figure which is 5cm square
 nf <- layout(matrix(1), widths=lcm(5), heights=lcm(5))
 layout.show(nf)


 ##-- Create a scatterplot with marginal histograms -----

 x <- rnorm(50)
 y <- rnorm(50)
 xhist <- hist(x, breaks=seq(-3,3,0.5), plot=F)
 yhist <- hist(y, breaks=seq(-3,3,0.5), plot=F)
 top <- max(c(xhist$counts, yhist$counts))
 xrange <- c(-3,3)
 yrange <- c(-3,3)
 nf <- layout(matrix(c(2,0,1,3),2,2,T), c(3,1), c(1,3), T)
 layout.show(nf)

 par(mar=c(3,3,1,1))
 plot(x, y, xlim=xrange, ylim=yrange, xlab="", ylab="")
 par(mar=c(0,3,1,1))
 barplot(xhist$counts, axes=F, ylim=c(0, top), space=0)
 par(mar=c(3,0,1,1))
 barplot(yhist$counts, axes=F, xlim=c(0, top), space=0, horiz=T)

 par(def.par)#- reset to default
}
\keyword{iplot}
\keyword{dplot}
\keyword{environment}

\name{plot.stepfun}
\title{Plot Step Functions}
\usage{
plot.stepfun(Fn, x, xlim, xlab = "x", ylab = "f(x)", main = NULL,
     add = FALSE, verticals = TRUE, do.points = TRUE,
     pch = par("pch"), col.points=par("col"), cex.points=par("cex"),
     col.hor = par("col"), col.vert= par("col"), \dots)
}
\alias{plot.stepfun}
\arguments{
 \item{Fn}{an \R object inheriting from \code{"stepfun"}.}
 \item{x}{numeric vector of abscissa values at which to evaluate
   \code{Fn}.  Defaults to \code{\link{knots}(Fn)} restricted to \code{xlim}.}
 \item{xlim}{numeric(2); range of \code{x} values to use.}
 \item{add}{logical; if \code{TRUE} only \emph{add} to an existing plot.}
 \item{verticals}{logical;  if \code{TRUE}, draw vertical lines at steps.}
 \item{do.points}{logical;  if \code{true}, also draw points at the
   (\code{xlim} restricted) knot locations.}
 \item{pch}{character; point character if \code{do.points}.}
 \item{col.points}{character or integer code; color of points if \code{do.points}.}
 \item{cex.points}{numeric; character expansion factor if \code{do.points}.}
 \item{col.hor}{color of horizontal lines.}
 \item{col.vert}{color of vertical lines.}
 \item{xlab,ylab}{labels of x and y axis.}
 \item{main}{main title.}
 \item{\dots}{further arguments of \code{\link{plot}(.)}, or if\code{(add)}
   \code{\link{segments}(.)}.}
}
\description{
  Method of the generic \code{\link{plot}} for \code{\link{stepfun}}
  objects and utility for plotting piecewise constant functions.

}
\value{
  A list with two components
  \item{t}{abscissa (x) values, including the two outermost ones.}
  \item{y}{y values `in between' the \code{t[]}.}
}
\seealso{\code{\link{ecdf}} for empricial distribution functions as
  special step functions,
  \code{\link{approxfun}} and \code{\link{splinefun}}.
}
\examples{
y0 <- c(1,2,4,3)
sfun0  <- stepfun(1:3, y0, f = 0)
sfun.2 <- stepfun(1:3, y0, f = .2)
sfun1  <- stepfun(1:3, y0, f = 1)

tt <- seq(0,3, by=0.1)
op <- par(mfrow=c(2,2))
plot(sfun0); plot(sfun0, x=tt, add=T, col.h="bisque")
plot(sfun.2);plot(sfun.2,x=tt, add=T, col.h="orange")
plot(sfun1); plot(sfun1, x=tt, add=T, col.h="coral")
##-- This is  revealing :
plot(sfun0, verticals= FALSE,
     main = "stepfun(x, y0, f=f)  for f = 0, .2, 1")
for(i in 1:3)
  plot(list(sfun0,sfun.2,sfun1)[[i]], add=TRUE, col.h=i, col.v=i)
legend(2.5, 1.9, paste("f =", c(0,0.2,1)), col=1:3, lty=1, y.inter=1); par(op)

##-- this works too (automatic call to  ecdf(.)):
plot.stepfun(rt(50, df=3), col.vert = "gray20")
}
\keyword{hplot}

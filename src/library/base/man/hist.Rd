\name{hist}
\title{Histograms}
\usage{
hist(x, breaks, freq = NULL, col = NULL, border = par("fg"),
     main = paste("Histogram of" , deparse(substitute(x))),
     xlim = range(breaks),  ylim = range(counts, 0),
     xlab = deparse(substitute(x)),  ylab,
     axes = TRUE, plot = TRUE, \dots)
}
\alias{hist}
\arguments{
\item{x}{a vector of values for which the histogram is desired.}
\item{breaks}{either a single number giving the
approximate number of cells for the histogram
or a vector giving the breakpoints between histogram cells.}
\item{freq}{a logical vector indicating whether the
  histogram is to present a representation of frequencies
  (the default \emph{if} \code{breaks} are equidistant) or relative
  frequencies (``probabilities'').}
\item{col}{a colour to be used to fill the bars.
  The default of \code{NULL} yields unfilled bars.}
\item{border}{the color of the border around the bars.}
\item{main,xlab,ylab}{these arguments to \code{title} have useful
  defaults here.}
\item{xlim,ylim}{the range of x and y values with sensible defaults.}
\item{plot}{logical.  If \code{TRUE} (default), a histogram is
  plotted, otherwise a list of breaks and counts is returned.}
\item{\dots}{further graphical parameters to \code{title} and \code{axis}.}
}
\description{
\code{hist} computes and plots a histogram of the given data values.
The histogram cells are intervals of the form
\code{[a,b)}, i.e. they include their left-hand endpoint,
but not their right one.
}
\value{
  a list with components
  \item{breaks}{the \eqn{n+1} cell boundaries (= \code{breaks} if that
  was a vector).}
  \item{counts}{\eqn{n} integers; for each cell, the number of
    \code{x[]} inside.}
  \item{rel.freqs}{the \emph{relative} frequencies}
  \item{mids}{the \eqn{n} cell midpoints; useful for plotting.}
}
\examples{
data(islands)
op <- par(mfrow=c(2,2))
hist(islands)
r <- hist(islands, col="gray")
text(r$mids,r$counts,r$counts,adj=c(.5,-.5))

hist(sqrt(islands), br = 12, col="lightblue", border="pink")
##-- For non-equidistant breaks, counts should NOT be graphed unscaled:
r <- hist(sqrt(islands), br = c(4* 0:5,10* 3:5,70,100,140), col='blue1')
text(r$mids, r$rel.fr, r$counts, adj=c(.5,-.5), col='blue3')
par(op)

str(hist(islands, plot= F))
str(hist(islands, br=12, plot= F))
}
\keyword{hplot}
\keyword{iplot}
\keyword{distribution}

\name{hist}
\title{Histograms}
\usage{
hist(x, breaks, freq = NULL, probability = !freq,
     include.lowest = TRUE, col = NULL, border = par("fg"),
     main = paste("Histogram of" , deparse(substitute(x))),
     xlim = range(breaks),  ylim = range(counts, 0),
     xlab = deparse(substitute(x)),  ylab,
     axes = TRUE, plot = TRUE, labels = FALSE, \dots)
}
\alias{hist}
\arguments{
\item{x}{a vector of values for which the histogram is desired.}
\item{breaks}{either a single number giving the
approximate number of cells for the histogram
or a vector giving the breakpoints between histogram cells.}
\item{freq}{logical; if \code{TRUE}, the
  histogram graphic is to present a representation of frequencies, i.e,
  the \code{counts} component of the result; if \code{FALSE},
  \emph{relative} frequencies (``probabilities''), the \code{rel.freqs},
  are plotted.   Defaults to \code{TRUE} \emph{iff} \code{breaks} are
  equidistant.}
\item{probability}{an \emph{alias} for \code{!freq}, for S compatibility.}
\item{include.lowest}{logical; if \code{TRUE},
  an `x[i]' equal to the `breaks' value will be included in the first bar.}
\item{col}{a colour to be used to fill the bars.
  The default of \code{NULL} yields unfilled bars.}
\item{border}{the color of the border around the bars.}
\item{main,xlab,ylab}{these arguments to \code{title} have useful
  defaults here.}
\item{xlim,ylim}{the range of x and y values with sensible defaults.}
\item{plot}{logical.  If \code{TRUE} (default), a histogram is
  plotted, otherwise a list of breaks and counts is returned.}
\item{labels}{logical.  Additionaly draw labels on top of bars, if \code{TRUE}.}
\item{\dots}{further graphical parameters to \code{title} and \code{axis}.}
}
\description{
\code{hist} computes and plots a histogram of the given data values.
The histogram cells are intervals of the form
\code{(a,b]}, i.e. they include their right-hand endpoint,
but not their left one, with the exception of the first cell when
\code{include.lowest} is \code{TRUE}.
}
\value{
  a list with components
  \item{breaks}{the \eqn{n+1} cell boundaries (= \code{breaks} if that
  was a vector).}
  \item{counts}{\eqn{n} integers; for each cell, the number of
    \code{x[]} inside.}
  \item{intensities}{values \eqn{\hat f(x_i)}{f^(x[i])}, as estimated
    density values. If \code{all(diff(breaks) == 1)}, they are the
    relative frequencies \code{counts/n} and in general satisfy
    \eqn{\sum_i \hat f(x_i) (b_{i+1}-b_i) = 1}{sum[i; f^(x[i])
      (b[i+1]-b[i])] = 1}, where \eqn{b_i}{b[i]}=\code{breaks[i]}.}
  \item{mids}{the \eqn{n} cell midpoints; useful for plotting.}
}
\note{
  The resulting value does \emph{not}
  depend on the values of the arguments \code{freq} (or \code{probability})
  or \code{plot}.  This is intentionally different from S.
}
\examples{
data(islands)
op <- par(mfrow=c(2,2))
hist(islands)
str(hist(islands, col="gray", labels = TRUE))

hist(sqrt(islands), br = 12, col="lightblue", border="pink")
##-- For non-equidistant breaks, counts should NOT be graphed unscaled:
r <- hist(sqrt(islands), br = c(4* 0:5,10* 3:5,70,100,140), col='blue1')
text(r$mids, r$intensities, r$counts, adj=c(.5,-.5), col='blue3')
sapply(r[2:3],sum)
sum(r$intensities * diff(r$breaks)) # == 1
par(op)

str(hist(islands, plot= F))
str(hist(islands, br=12, plot= F))
str(hist(islands, br=c(12,20,36,80,200,1000,17000), plot = F))
str(hist(islands, br=c(12,20,36,80,200,1000,17000), freq = TRUE))#warning
}
\keyword{hplot}
\keyword{iplot}
\keyword{distribution}

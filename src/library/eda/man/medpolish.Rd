\name{medpolish}
\title{Median Polish of a Matrix}
\usage{
medpolish(x, eps=0.01, maxiter=10, trace.iter = TRUE)

plot(medpolish.obj)
print(medpolish.obj)
}
\alias{plot.medpolish}
\alias{print.medpolish}
\description{
    \code{medpolish} fits an additive "two-way" (constant + rows + columns)
    model to the values given in \code{x} using Tukey's
    \emph{median polish} procedure.
}
\details{
    Sweeping out for row and column effects continues
    until the proportional reduction in the sum
    of absolute residuals is less than \code{eps}
    or until there have been \code{maxiter} iterations.
    The sum of absolute residuals is printed at
    each iteration of the fitting process, if \code{trace.iter} is \code{TRUE}.

    \code{medpolish} returns an object of class \code{medpolish} (see below).
    There are printing and plotting methods for this
    class, which are invoked via by the generics
    \code{\link{print}} and \code{\link{plot}}.
}
\value{
    An object of class \code{medpolish} with the following named components:
    \item{overall}{the fitted constant term.}
    \item{row}{the fitted row effects.}
    \item{col}{the fitted column effects.}
    \item{residuals}{the residuals.}
    \item{name}{the name of the dataset.}
}
\seealso{\code{\link{median}}; \code{\link{aov}} which for a \emph{mean}
    instead of \emph{median} decomposition.}
\references{Tukey (1977). \emph{E.D.A}; see ref.\ in \code{\link{line}}.}
\examples{
## Deaths from sport parachuting;  from ABC of EDA, p.224:
deaths <-
    rbind(c(14,15,14),
          c( 7, 4, 7),
          c( 8, 2,10),
          c(15, 9,10),
          c( 0, 2, 0))
dimnames(deaths) <- list(c("1-24", "25-74", "75-199", "200++", "NA"),
                         paste(1973:1975))
deaths
(med.d <- medpolish(deaths))
plot(med.d)
## Check decomposition:
all(deaths == med.d$overall + outer(med.d$row,med.d$col, "+") + med.d$resid)
}
\keyword{robust}

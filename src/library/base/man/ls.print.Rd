\name{ls.print}
\title{Print a regression result as from lsfit(..)}
\usage{
ls.print(ls.out, digits=4, print.it=TRUE)
}
\alias{ls.print}
\arguments{
\item{ls.out}{Typically the result of \code{\link{lsfit}(..)}}
\item{digits}{The number of significant digits used for printing}
\item{print.it}{logical; ~~Describe print.it here~~}
}
\description{
Computes basic statistics, including standard errors, \eqn{t-} and \eqn{P-}
values for the regression coefficients and prints them if \code{print.it} is
\code{TRUE}.
}
\value{
If  \code{print.it} is not \code{TRUE}, a list with the components
\item{summary}{The ANOVA table of the regression}
\item{coef.table}{matrix with regression coefficients, standard errors,
\eqn{t-} and \eqn{P-}values}
is returned.
}
\note{
Usually, you'd rather use  \code{summary(lm(\ldots))} and
\code{anova(lm(\ldots))} for obtaining similar output.
}
\seealso{
\code{\link{ls.diag}}, \code{\link{lsfit}};
\code{\link{lm}}, \code{\link{lm.influence}} which usually are preferable.
}
\examples{
## See help for \link{lsfit}
}

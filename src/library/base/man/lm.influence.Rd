\name{lm.influence}
\title{Regression Diagnostics}
\usage{
lm.influence(lm.obj)
}
\alias{lm.influence}
\arguments{
\item{lm.obj}{an object as returned by \code{\link{lm}}.}
}
\description{
  This function provides the basic quantities which are
  used in forming a wide variety of diagnostics for
  checking the quality of regression fits.
}
\details{
  The \code{\link{influence.measures}()} and other functions listed in
  \bold{See Also} provide a more user oriented way of computing a
  variety of regression diagnostics.
}
\value{
  A list containing the following components:
  \item{hat}{a vector containing the diagonal of the ``hat'' matrix.}
  \item{coefficients}{the change in the estimated coefficients which results
    when the i-th case is dropped from the regression is contained in
    the i-th row of this matrix.}
  \item{sigma}{a vector whose i-th element contains the estimate
    of the residual standard deviation obtained when the i-th
    case is dropped from the regression.}
}
\note{
  The \code{coefficients} returned by the \R version
  of \code{lm.influence} differ from those computed by S.
  Rather than returning the coefficients which result
  from dropping each case, we return the changes in the coefficients.
  This is more directly useful in many diagnostic measures.

  Note that cases with \code{weights == 0} are \emph{dropped} (contrary
  to the situation in S).
}
\references{
  Belsley, D. A., Kuh, E. and Welsch, R. E. (1980)
  \emph{Regression Diagnostics.}
  New York: Wiley.
}
\seealso{
\code{\link{summary.lm}} for \code{\link{summary}} and related methods;\cr
\code{\link{influence.measures}},\cr
\code{\link{hat}} for the hat matrix diagonals,\cr
\code{\link{dfbetas}},
\code{\link{dffits}},
\code{\link{covratio}},
\code{\link{cooks.distance}},
\code{\link{lm}}.
}
\examples{
## Analysis of the life-cycle savings data
## given in Belsley, Kuh and Welsch.
data(LifeCycleSavings)
summary(lm.SR <- lm(sr ~ pop15 + pop75 + dpi + ddpi,
                    data = LifeCycleSavings),
        corr = TRUE)
str(lmI <- lm.influence(lm.SR))

## For more `user level' examples, use example(influence.measures)
}
\keyword{regression}

\name{friedman.test}
\alias{friedman.test}
\alias{friedman.test.default}
\alias{friedman.test.formula}
\title{Friedman Rank Sum Test}
\description{
  Performs a Friedman rank sum test with unreplicated blocked data.
}
\usage{
friedman.test(y, \dots)

\method{friedman.test}{default}(y, groups, blocks, \dots)

\method{friedman.test}{formula}(formula, data, subset, na.action, \dots)
}
\arguments{
  \item{y}{either a numeric vector of data values, or a data matrix.}
  \item{groups}{a vector giving the group for the corresponding
    elements of \code{y} if this is a vector;  ignored if \code{y}
    is a matrix.  If not a factor object, it is coerced to one.}
  \item{blocks}{a vector giving the block for the corresponding
    elements of \code{y} if this is a vector;  ignored if \code{y}
    is a matrix.  If not a factor object, it is coerced to one.}
  \item{formula}{a formula of the form \code{a ~ b | c}, where \code{a},
    \code{b} and \code{c} give the data values and corresponding groups
    and blocks, respectively.}
  \item{data}{an optional matrix or data frame (or similar: see
    \code{\link{model.frame}}) containing the variables in the
    formula \code{formula}.  By default the variables are taken from
    \code{environment(formula)}.}
  \item{subset}{an optional vector specifying a subset of observations
    to be used.}
  \item{na.action}{a function which indicates what should happen when
    the data contain \code{NA}s.  Defaults to
    \code{getOption("na.action")}.}
  \item{\dots}{further arguments to be passed to or from methods.}    
}
\details{
  \code{friedman.test} can be used for analyzing unreplicated complete
  block designs (i.e., there is exactly one observation in \code{y}
  for each combination of levels of \code{groups} and \code{blocks})
  where the normality assumption may be violated.

  The null hypothesis is that apart from an effect of \code{blocks},
  the location parameter of \code{y} is the same in each of the
  \code{groups}. 

  If \code{y} is a matrix, \code{groups} and \code{blocks} are
  obtained from the column and row indices, respectively.  \code{NA}'s
  are not allowed in \code{groups} or \code{blocks};  if \code{y}
  contains \code{NA}'s, corresponding blocks are removed.
}
\value{
  A list with class \code{"htest"} containing the following components:
  \item{statistic}{the value of Friedman's chi-squared statistic.}
  \item{parameter}{the degrees of freedom of the approximate
    chi-squared distribution of the test statistic.}
  \item{p.value}{the p-value of the test.}
  \item{method}{the character string \code{"Friedman rank sum test"}.}
  \item{data.name}{a character string giving the names of the data.}
}
\references{
  Myles Hollander & Douglas A. Wolfe (1973),
  \emph{Nonparametric statistical inference}.
  New York: John Wiley & Sons.
  Pages 139--146.
}
\seealso{
  \code{\link{quade.test}}.
}
\examples{
## Hollander & Wolfe (1973), p. 140ff.
## Comparison of three methods ("round out", "narrow angle", and
##  "wide angle") for rounding first base.  For each of 18 players
##  and the three method, the average time of two runs from a point on
##  the first base line 35ft from home plate to a point 15ft short of
##  second base is recorded.
RoundingTimes <-
matrix(c(5.40, 5.50, 5.55,
         5.85, 5.70, 5.75,
         5.20, 5.60, 5.50,
         5.55, 5.50, 5.40,
         5.90, 5.85, 5.70,
         5.45, 5.55, 5.60,
         5.40, 5.40, 5.35,
         5.45, 5.50, 5.35,
         5.25, 5.15, 5.00,
         5.85, 5.80, 5.70,
         5.25, 5.20, 5.10,
         5.65, 5.55, 5.45,
         5.60, 5.35, 5.45,
         5.05, 5.00, 4.95,
         5.50, 5.50, 5.40,
         5.45, 5.55, 5.50,
         5.55, 5.55, 5.35,
         5.45, 5.50, 5.55,
         5.50, 5.45, 5.25,
         5.65, 5.60, 5.40,
         5.70, 5.65, 5.55,
         6.30, 6.30, 6.25),
       nr = 22,
       byrow = TRUE,
       dimnames = list(1 : 22,
                       c("Round Out", "Narrow Angle", "Wide Angle")))
friedman.test(RoundingTimes)
## => strong evidence against the null that the methods are equivalent
##    with respect to speed

wb <- aggregate(warpbreaks$breaks,
                by = list(w = warpbreaks$wool,
                          t = warpbreaks$tension),
                FUN = mean)
wb
friedman.test(wb$x, wb$w, wb$t)
friedman.test(x ~ w | t, data = wb)
}
\keyword{htest}

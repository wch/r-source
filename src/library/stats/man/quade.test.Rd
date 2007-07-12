\name{quade.test}
\alias{quade.test}
\alias{quade.test.default}
\alias{quade.test.formula}
\title{Quade Test}
\description{
  Performs a Quade test with unreplicated blocked data.
}
\usage{
quade.test(y, \dots)

\method{quade.test}{default}(y, groups, blocks, \dots)

\method{quade.test}{formula}(formula, data, subset, na.action, \dots)
}
\arguments{
  \item{y}{either a numeric vector of data values, or a data matrix.}
  \item{groups}{a vector giving the group for the corresponding	elements
    of \code{y} if this is a vector;  ignored if \code{y} is a matrix.
    If not a factor object, it is coerced to one.}
  \item{blocks}{a vector giving the block for the corresponding	elements
    of \code{y} if this is a vector;  ignored if \code{y} is a matrix.
    If not a factor object, it is coerced to one.}
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
  \code{quade.test} can be used for analyzing unreplicated complete
  block designs (i.e., there is exactly one observation in \code{y}
  for each combination of levels of \code{groups} and \code{blocks})
  where the normality assumption may be violated.

  The null hypothesis is that apart from an effect of \code{blocks},
  the location parameter of \code{y} is the same in each of the
  \code{groups}.

  If \code{y} is a matrix, \code{groups} and \code{blocks} are obtained
  from the column and row indices, respectively.  \code{NA}'s are not
  allowed in \code{groups} or \code{blocks};  if \code{y} contains
  \code{NA}'s, corresponding blocks are removed.
}
\value{
  A list with class \code{"htest"} containing the following components:
  \item{statistic}{the value of Quade's F statistic.}
  \item{parameter}{a vector with the numerator and denominator degrees 
    of freedom of the approximate F distribution of the test statistic.}
  \item{p.value}{the p-value of the test.}
  \item{method}{the character string \code{"Quade test"}.}
  \item{data.name}{a character string giving the names of the data.}
}
\references{
  D. Quade (1979),
  Using weighted rankings in the analysis of complete blocks with
  additive block effects.
  \emph{Journal of the American Statistical Association}, \bold{74},
  680--683.
    
  William J. Conover (1999),
  \emph{Practical nonparametric statistics}.
  New York: John Wiley & Sons.
  Pages 373--380.
}
\seealso{
  \code{\link{friedman.test}}.
}
\examples{
## Conover (1999, p. 375f):
## Numbers of five brands of a new hand lotion sold in seven stores
## during one week.
y <- matrix(c( 5,  4,  7, 10, 12,
               1,  3,  1,  0,  2,
              16, 12, 22, 22, 35,
               5,  4,  3,  5,  4,
              10,  9,  7, 13, 10,
              19, 18, 28, 37, 58,
              10,  7,  6,  8,  7),
            nrow = 7, byrow = TRUE,
            dimnames =
            list(Store = as.character(1:7),
                 Brand = LETTERS[1:5]))
y
quade.test(y)
}
\keyword{htest}

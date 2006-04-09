\name{kruskal.test}
\alias{kruskal.test}
\alias{kruskal.test.default}
\alias{kruskal.test.formula}
\title{Kruskal-Wallis Rank Sum Test}
\description{
  Performs a Kruskal-Wallis rank sum test.
}
\usage{
kruskal.test(x, \dots)

\method{kruskal.test}{default}(x, g, \dots)

\method{kruskal.test}{formula}(formula, data, subset, na.action, \dots)
}
\arguments{
  \item{x}{a numeric vector of data values, or a list of numeric data
    vectors.}
  \item{g}{a vector or factor object giving the group for the
    corresponding elements of \code{x}.  Ignored if \code{x} is a
    list.}
  \item{formula}{a formula of the form \code{lhs ~ rhs} where \code{lhs}
    gives the data values and \code{rhs} the corresponding groups.}
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
  \code{kruskal.test} performs a Kruskal-Wallis rank sum test of the
  null that the location parameters of the distribution of \code{x}
  are the same in each group (sample).  The alternative is that they
  differ in at least one.
  
  If \code{x} is a list, its elements are taken as the samples to be
  compared, and hence have to be numeric data vectors.  In this case,
  \code{g} is ignored, and one can simply use \code{kruskal.test(x)}
  to perform the test.  If the samples are not yet contained in a
  list, use \code{kruskal.test(list(x, ...))}.
  
  Otherwise, \code{x} must be a numeric data vector, and \code{g} must
  be a vector or factor object of the same length as \code{x} giving
  the group for the corresponding elements of \code{x}.
}
\value{
  A list with class \code{"htest"} containing the following components:
  \item{statistic}{the Kruskal-Wallis rank sum statistic.}
  \item{parameter}{the degrees of freedom of the approximate
    chi-squared distribution of the test statistic.}
  \item{p.value}{the p-value of the test.}
  \item{method}{the character string \code{"Kruskal-Wallis rank sum test"}.}
  \item{data.name}{a character string giving the names of the data.}
}
\references{
  Myles Hollander & Douglas A. Wolfe (1973),
  \emph{Nonparametric statistical inference}.
  New York: John Wiley & Sons.
  Pages 115--120.
}
\seealso{
  The Wilcoxon rank sum test (\code{\link{wilcox.test}}) as the special
  case for two samples;
  \code{\link{lm}} together with \code{\link{anova}} for performing
  one-way location analysis under normality assumptions; with Student's
  t test (\code{\link{t.test}}) as the special case for two samples.
}
\examples{
## Hollander & Wolfe (1973), 116.
## Mucociliary efficiency from the rate of removal of dust in normal
##  subjects, subjects with obstructive airway disease, and subjects
##  with asbestosis.
x <- c(2.9, 3.0, 2.5, 2.6, 3.2)	# normal subjects
y <- c(3.8, 2.7, 4.0, 2.4)      # with obstructive airway disease
z <- c(2.8, 3.4, 3.7, 2.2, 2.0) # with asbestosis
kruskal.test(list(x, y, z))
## Equivalently,
x <- c(x, y, z)
g <- factor(rep(1:3, c(5, 4, 5)),
            labels = c("Normal subjects",
                       "Subjects with obstructive airway disease",
                       "Subjects with asbestosis"))
kruskal.test(x, g)

## Formula interface.
boxplot(Ozone ~ Month, data = airquality)
kruskal.test(Ozone ~ Month, data = airquality)
}
\keyword{htest}

\name{kruskal.test}
\alias{kruskal.test}
\title{Kruskal-Wallis Rank Sum Test}
\description{
  Performs a Kruskal-Wallis rank sum test.
}
\usage{
kruskal.test(x, g)
}
\arguments{
  \item{x}{a numeric vector of data values, or a list of numeric data
    vectors.}
  \item{g}{a vector or factor object giving the group for the
    corresponding elements of \code{x}.  Ignored if \code{x} is a
    list.}
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
    chi-square distribution of the test statistic.}
  \item{p.value}{the p-value of the test.}
  \item{method}{the string \code{"Kruskal-Wallis rank sum test"}.}
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
  \code{\link[base]{lm}} together with \code{\link[base]{anova}} for
  performing
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
}
\keyword{htest}

\name{HairEyeColor}
\docType{data}
\alias{HairEyeColor}
\title{Hair and Eye Color of Statistics Students}
\description{
  Distribution of hair and eye color and sex in 592 statistics students.
}
\usage{HairEyeColor}
\format{
  A 3-dimensional array resulting from cross-tabulating 592 observations
  on 3 variables.  The variables and their levels are as follows:

  \tabular{rll}{
    No \tab Name \tab Levels \cr
    1 \tab Hair \tab Black, Brown, Red, Blond \cr
    2 \tab Eye \tab Brown, Blue, Hazel, Green \cr
    3 \tab Sex \tab Male, Female
  }
}
\details{
  This data set is useful for illustrating various techniques for the
  analysis of contingency tables, such as the standard chi-squared test
  or, more generally, log-linear modelling, and graphical methods such
  as mosaic plots, sieve diagrams or association plots.
}
\references{
  Snee, R. D. (1974),
  Graphical display of two-way contingency tables.
  \emph{The American Statistician}, \bold{28}, 9--12.

  Friendly, M. (1992),
  Graphical methods for categorical data.
  \emph{SAS User Group International Conference Proceedings}, \bold{17},
  190--200.
  \url{http://www.math.yorku.ca/SCS/sugi/sugi17-paper.html}

  Friendly, M. (1992),
  Mosaic displays for loglinear models.
  \emph{Proceedings of the Statistical Graphics Section},
  American Statistical Association, pp. 61--68.
  \url{http://www.math.yorku.ca/SCS/Papers/asa92.html}
}
\seealso{
  \code{\link{chisq.test}},
  \code{\link{loglin}},
  \code{\link{mosaicplot}}
}
\examples{
require(graphics)
## Full mosaic
mosaicplot(HairEyeColor)
## Aggregate over sex:
x <- apply(HairEyeColor, c(1, 2), sum)
x
mosaicplot(x, main = "Relation between hair and eye color")
}
\keyword{datasets}

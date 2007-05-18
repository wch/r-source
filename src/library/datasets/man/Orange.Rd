\name{Orange}
\docType{data}
\alias{Orange}
\title{Growth of orange trees}
\description{
  The \code{Orange} data frame has 35 rows and 3 columns of records of
  the growth of orange trees.
}
\usage{Orange}
\format{
  This data frame contains the following columns:
  \describe{
    \item{Tree}{
      an ordered factor indicating the tree on which the measurement is
      made.  The ordering is according to increasing maximum diameter.
    }
    \item{age}{
      a numeric vector giving the age of the tree (days since 1968/12/31)
    }
    \item{circumference}{
      a numeric vector of trunk circumferences (mm).  This is probably
      \dQuote{circumference at breast height}, a standard measurement in
      forestry.
    }
  }
}
\source{
  Draper, N. R. and Smith, H. (1998), \emph{Applied Regression Analysis
    (3rd ed)}, Wiley (exercise 24.N).

  Pinheiro, J. C. and Bates, D. M. (2000) \emph{Mixed-effects Models
    in S and S-PLUS}, Springer.
}
\examples{
require(stats); require(graphics)
coplot(circumference ~ age | Tree, data = Orange, show.given = FALSE)
fm1 <- nls(circumference ~ SSlogis(age, Asym, xmid, scal),
           data = Orange, subset = Tree == 3)
plot(circumference ~ age, data = Orange, subset = Tree == 3,
     xlab = "Tree age (days since 1968/12/31)",
     ylab = "Tree circumference (mm)", las = 1,
     main = "Orange tree data and fitted model (Tree 3 only)")
age <- seq(0, 1600, len = 101)
lines(age, predict(fm1, list(age = age)))
}
\keyword{datasets}

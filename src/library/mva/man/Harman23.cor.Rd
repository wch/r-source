\name{Harman23.cor}
\alias{Harman23.cor}
\non_function{}
\title{Harman Example 2.3}
\description{
  A correlation matrix of eight physical measurements on 305 girls between
  ages seven and seventeen.
}
\usage{data(Harman23.cor)}
\source{
  Harman, H. H. (1976)
  \emph{Modern Factor Analysis}, Third Edition Revised,
  University of Chicago Press, Table 2.3.
}
\examples{
data(Harman23.cor)
(Harman23.FA <- factanal(factors = 1, covmat = Harman23.cor))
for(factors in 2:4) print(update(Harman23.FA, factors = factors))
}
\keyword{datasets}

% File src/library/datasets/man/Harman23.cor.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{Harman23.cor}
\docType{data}
\alias{Harman23.cor}
\title{Harman Example 2.3}
\description{
  A correlation matrix of eight physical measurements on 305 girls between
  ages seven and seventeen.
}
\usage{Harman23.cor}
\source{
  Harman, H. H. (1976)
  \emph{Modern Factor Analysis}, Third Edition Revised,
  University of Chicago Press, Table 2.3.
}
\examples{
require(stats)
(Harman23.FA <- factanal(factors = 1, covmat = Harman23.cor))
for(factors in 2:4) print(update(Harman23.FA, factors = factors))
}
\keyword{datasets}

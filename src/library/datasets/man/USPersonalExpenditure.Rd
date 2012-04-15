% File src/library/datasets/man/USPersonalExpenditure.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{USPersonalExpenditure}
\docType{data}
\alias{USPersonalExpenditure}
\title{Personal Expenditure Data}
\description{
  This data set consists of United States personal expenditures (in
  billions of dollars) in the categories; food and tobacco, household
  operation, medical and health, personal care, and private education
  for the years 1940, 1945, 1950, 1955 and 1960.
}
\usage{USPersonalExpenditure}
\format{A matrix with 5 rows and 5 columns.}
\source{The World Almanac and Book of Facts, 1962, page 756.}
\references{
  Tukey, J. W. (1977)
  \emph{Exploratory Data Analysis}.
  Addison-Wesley.

  McNeil, D. R. (1977)
  \emph{Interactive Data Analysis}.
  Wiley.
}
\examples{
require(stats) # for medpolish
USPersonalExpenditure
medpolish(log10(USPersonalExpenditure))
}
\keyword{datasets}

\name{consume}
\title{Personal Expenditure Data}
\usage{data(consume)}
\alias{consume}
\format{A matrix with 5 rows and 5 columns.
}
\source{The World Almanac and Book of Facts, 1962, page 756.
}
\description{This data set consists of United States personal expenditures (in
billions of dollars) in the categories; food and tobacco, household
operation, medical and health, personal care, and private education
for the years 1940, 1945, 1950, and 1960.
}
\references{Tukey, J. W. (1977).  Exploratory Data Analysis.  Addison-Wesley.
McNeil, D. R. (1977).  Interactive Data Analysis.  Wiley.
}
\examples{
data(consume); consume
require(eda)
medpolish(log10(consume))
}
\keyword{datasets}

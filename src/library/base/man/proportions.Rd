% File src/library/base/man/proportions.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2020 R Core Team
% Distributed under GPL 2 or later

\name{proportions}
\alias{proportions}
\title{Express Table Entries as Fraction of Marginal Table}
\concept{proportions}
\usage{
proportions(x, margin = NULL)
}
\arguments{
 \item{x}{ table }
 \item{margin}{a vector giving the margins to split by.
    E.g., for a matrix \code{1} indicates rows, \code{2} indicates
    columns, \code{c(1, 2)} indicates rows and columns.
    When \code{x} has named dimnames, it can be a character vector
    selecting dimension names.}
}
\description{
    Returns conditional proportions given \code{margins}, i.e. entries of \code{x}, divided by the appropriate marginal sums. 
}
\value{
  Table like \code{x} expressed relative to \code{margin}
}
\author{ Peter Dalgaard }

\seealso{\code{\link{marginSums}}}

\examples{
m <- matrix(1:4, 2)
m
proportions(m, 1)

DF <- as.data.frame(UCBAdmissions)
tbl <- xtabs(Freq ~ Gender + Admit, DF)

proportions(tbl, "Gender")
}
\keyword{ array }

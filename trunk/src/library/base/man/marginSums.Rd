% File src/library/base/man/marginSums.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2020 R Core Team
% Distributed under GPL 2 or later

\name{marginSums}
\alias{marginSums}
\alias{margin.table}
\title{Compute table margins}
\description{
  For a contingency table in array form, compute the sum of table
  entries for a given margin or set of margins.
}
\usage{
marginSums(x, margin = NULL)
margin.table(x, margin = NULL)
}
\arguments{
  \item{x}{an array}
  \item{margin}{a vector giving the margins to compute sums for.
    E.g., for a matrix \code{1} indicates rows, \code{2} indicates
    columns, \code{c(1, 2)} indicates rows and columns.
    When \code{x} has named dimnames, it can be a character vector
    selecting dimension names.}
}
\seealso{
  \code{\link{proportions}} and \code{\link{addmargins}}.
}
\value{
  The relevant marginal table, or just the sum of all entries if \code{margin} has length zero.  The class of \code{x} is copied to the
  output table if \code{margin} is non-NULL.
}
\note{\code{margin.table} is an earlier name, retained for back-compatibility.}
\author{Peter Dalgaard}
\examples{
m <- matrix(1:4, 2)
marginSums(m, 1)
marginSums(m, 2)


DF <- as.data.frame(UCBAdmissions)
tbl <- xtabs(Freq ~ Gender + Admit, DF)

marginSums(tbl, "Gender")
proportions(tbl, "Gender")
}
\keyword{array}

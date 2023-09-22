% File src/library/base/man/proportions.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2023 R Core Team
% Distributed under GPL 2 or later

\name{proportions}
\alias{proportions}
\alias{prop.table}
\title{Express Table Entries as Fraction of Marginal Table}
\concept{proportions}
\usage{
proportions(x, margin = NULL)
prop.table(x, margin = NULL)
}
\arguments{
 \item{x}{an array, usually a \code{\link{table}}.}
 \item{margin}{a vector giving the margins to split by.
    E.g., for a matrix \code{1} indicates rows, \code{2} indicates
    columns, \code{c(1, 2)} indicates rows and columns.
    When \code{x} has named \code{\link{dimnames}}, it can be a character vector
    selecting dimension names.}
}
\description{
  Returns conditional proportions given \code{margins}, i.e.,
  entries of \code{x}, divided by the appropriate marginal sums.
}
\value{
  A table or array like \code{x}, expressed relative to \code{margin}.
}
\note{\code{prop.table} is an earlier name, retained for back-compatibility.}
\author{ Peter Dalgaard }

\seealso{
  \code{\link{marginSums}}.

  \code{\link{apply}} and \code{\link{sweep}} are more general
  mechanisms for sweeping out marginal statistics.
}

\examples{
m <- matrix(1:4, 2)
m
proportions(m, 1)

DF <- as.data.frame(UCBAdmissions)
tbl <- xtabs(Freq ~ Gender + Admit, DF)
tbl
proportions(tbl, "Gender")
}
\keyword{ array }

% File src/library/stats/man/pairwise.table.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2017 R Core Team
% Distributed under GPL 2 or later

\name{pairwise.table}
\alias{pairwise.table}
\title{Tabulate p values for pairwise comparisons}
\description{
 Creates  table of p values for pairwise comparisons
 with corrections for multiple testing.
}
\usage{
pairwise.table(compare.levels, level.names, p.adjust.method)
}
\arguments{
 \item{compare.levels}{a \code{\link{function}} to compute (raw) p value
   given indices \code{i} and \code{j}.}
 \item{level.names}{names of the group levels}
 \item{p.adjust.method}{a character string specifying the method for
   multiple testing adjustment; almost always one of
   \code{\link{p.adjust.methods}}.  Can be abbreviated.}
}
\details{
  Functions that do multiple group comparisons create separate
  \code{compare.levels} functions (assumed to be symmetrical in \code{i}
  and \code{j}) and passes them to this function.
}
\value{
  Table of p values in lower triangular form.
}
\seealso{ \code{\link{pairwise.t.test}} }

\keyword{htest}

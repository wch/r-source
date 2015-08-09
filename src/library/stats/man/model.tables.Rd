% File src/library/stats/man/model.tables.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2013 R Core Team
% Distributed under GPL 2 or later

\name{model.tables}
\alias{model.tables}
\alias{model.tables.aov}
\alias{model.tables.aovlist}
\title{Compute Tables of Results from an Aov Model Fit}
\description{
  Computes summary tables for model fits, especially complex \code{aov}
  fits.
}
\usage{
model.tables(x, \dots)

\method{model.tables}{aov}(x, type = "effects", se = FALSE, cterms, \dots)

\method{model.tables}{aovlist}(x, type = "effects", se = FALSE, \dots)
}
\arguments{
  \item{x}{a model object, usually produced by \code{aov}}
  \item{type}{type of table: currently only \code{"effects"} and
    \code{"means"} are implemented.  Can be abbreviated.}
  \item{se}{should standard errors be computed?}
  \item{cterms}{A character vector giving the names of the terms for
    which tables should be computed. The default is all tables.}
  \item{\dots}{further arguments passed to or from other methods.}
}
\details{
  For \code{type = "effects"} give tables of the coefficients for each
  term, optionally with standard errors.

  For \code{type = "means"} give tables of the mean response for each
  combinations of levels of the factors in a term.

  The \code{"aov"} method cannot be applied to components of a
  \code{"aovlist"} fit.
}
\value{
  An object of class \code{"tables.aov"}, as list which may contain components
  \item{tables}{A list of tables for each requested term.}
  \item{n}{The replication information for each term.}
  \item{se}{Standard error information.}
}
\section{Warning}{
  The implementation is incomplete, and only the simpler cases have been
  tested thoroughly.

  Weighted \code{aov} fits are not supported.
}

\seealso{
  \code{\link{aov}}, \code{\link{proj}},
  \code{\link{replications}}, \code{\link{TukeyHSD}},
  \code{\link{se.contrast}}
}

\examples{\donttest{
options(contrasts = c("contr.helmert", "contr.treatment"))
npk.aov <- aov(yield ~ block + N*P*K, npk)
model.tables(npk.aov, "means", se = TRUE)

## as a test, not particularly sensible statistically
npk.aovE <- aov(yield ~  N*P*K + Error(block), npk)
model.tables(npk.aovE, se = TRUE)
model.tables(npk.aovE, "means")
}}
\keyword{models}

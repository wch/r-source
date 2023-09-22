% File src/library/stats/man/terms.formula.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2023 R Core Team
% Distributed under GPL 2 or later

\name{terms.formula}
\alias{terms.formula}
\title{Construct a terms Object from a Formula}
\description{
  This function takes a formula and some optional arguments and
  constructs a terms object. The terms object can then be used to
  construct a \code{\link{model.matrix}}.
}
\usage{
\method{terms}{formula}(x, specials = NULL, abb = NULL, data = NULL, neg.out = TRUE,
      keep.order = FALSE, simplify = FALSE, \dots,
      allowDotAsName = FALSE)
}
\arguments{
  \item{x}{a \code{\link{formula}}.}
  \item{specials}{which functions in the formula should be marked as
    special in the \code{terms} object?  A character vector or \code{NULL}.}
  \item{abb}{Not implemented in \R; deprecated.}
  \item{data}{a data frame from which the meaning of the special symbol
    \code{.} can be inferred.  It is used only if there is a \code{.} in
    the formula.}
  \item{neg.out}{Not implemented in \R; deprecated.}
  \item{keep.order}{a logical value indicating whether the terms should
    keep their positions.  By default, when \code{FALSE}, the terms are reordered so
    that main effects come first, followed by the interactions,
    all second-order, all third-order and so on.  Effects of a given
    order are kept in the order specified.}
  \item{simplify}{should the formula be expanded and simplified, the
    pre-1.7.0 behaviour?}
  \item{\dots}{further arguments passed to or from other methods.}
  \item{allowDotAsName}{normally \code{.} in a formula refers to the
    remaining variables contained in \code{data}.  Exceptionally,
    \code{.} can be treated as a name for non-standard uses of formulae.}
}
\details{
  Not all of the options work in the same way that they do in S and not
  all are implemented.
}
\value{
  A \code{terms} object is returned.  It is the re-ordered formula (unless
  \code{keep.order = TRUE}) with several \code{\link{attributes}}, see
  \code{\link{terms.object}} for details.  In all
  cases variables within an interaction term in the formula are
  re-ordered by the ordering of the \code{"variables"} attribute, which
  is the order in which the variables occur in the formula.
}
\seealso{
  \code{\link{terms}}, \code{\link{terms.object}}, also for examples.
}
\keyword{models}

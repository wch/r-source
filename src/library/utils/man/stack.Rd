% File src/library/utils/man/stack.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2011 R Core Team
% Distributed under GPL 2 or later

\name{stack}
\alias{stack}
\alias{stack.default}
\alias{stack.data.frame}
\alias{unstack}
\alias{unstack.default}
\alias{unstack.data.frame}
\title{Stack or Unstack Vectors from a Data Frame or List}
\description{
  Stacking vectors concatenates multiple vectors into a single vector
  along with a factor indicating where each observation originated.
  Unstacking reverses this operation.
}
\usage{
stack(x, \dots)
\method{stack}{default}(x, \dots)
\method{stack}{data.frame}(x, select, \dots)

unstack(x, \dots)
\method{unstack}{default}(x, form, \dots)
\method{unstack}{data.frame}(x, form, \dots)
}
\arguments{
  \item{x}{a list or data frame to be stacked or unstacked.}
  \item{select}{an expression, indicating which variable(s) to select from a
    data frame.}
  \item{form}{a two-sided formula whose left side evaluates to the
    vector to be unstacked and whose right side evaluates to the
    indicator of the groups to create.  Defaults to \code{\link{formula}(x)}
    in the data frame method for \code{unstack}.}
  \item{\dots}{further arguments passed to or from other methods.}
}
\details{
  The \code{stack} function is used to transform data available as
  separate columns in a data frame or list into a single column that can
  be used in an analysis of variance model or other linear model.  The
  \code{unstack} function reverses this operation.

  Note that \code{stack} applies to \emph{vectors} (as determined by
  \code{\link{is.vector}}): non-vector columns (e.g., factors) will be
  ignored (with a warning as from \R 2.15.0).  Where vectors of
  different types are selected they are concatenated by
  \code{\link{unlist}} whose help page explains how the type of the
  result is chosen.

  These functions are generic: the supplied methods handle data frames
  and objects coercible to lists by \code{\link{as.list}}.
}

\value{
  \code{unstack} produces a list of columns according to the formula
  \code{form}.  If all the columns have the same length, the resulting
  list is coerced to a data frame.

  \code{stack} produces a data frame with two columns:
  \item{values}{the result of concatenating the selected vectors in
    \code{x}.}
  \item{ind}{a factor indicating from which vector in \code{x} the
    observation originated.}
}
\author{Douglas Bates}
\seealso{
  \code{\link{lm}}, \code{\link{reshape}}
}
\examples{
require(stats)
formula(PlantGrowth)         # check the default formula
pg <- unstack(PlantGrowth)   # unstack according to this formula
pg
stack(pg)                    # now put it back together
stack(pg, select = -ctrl)    # omitting one vector
}
\keyword{manip}

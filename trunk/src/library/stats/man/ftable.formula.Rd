\name{ftable.formula}
\alias{ftable.formula}
\title{Formula Notation for Flat Contingency Tables}
\description{Produce or manipulate a flat contingency table using
  formula notation.}
\usage{
\method{ftable}{formula}(formula, data = NULL, subset, na.action, \dots)
}
\arguments{
  \item{formula}{a formula object with both left and right hand sides
    specifying the column and row variables of the flat table.}
  \item{data}{a data frame, list or environment (or similar: see
    \code{\link{model.frame}}) containing the variables
    to be cross-tabulated, or a contingency table (see below).}
  \item{subset}{an optional vector specifying a subset of observations
    to be used.
    Ignored if \code{data} is a contingency table.}
  \item{na.action}{a function which indicates what should happen when
    the data contain \code{NA}s.
    Ignored if \code{data} is a contingency table.}
  \item{\dots}{further arguments to the default ftable method may also
    be passed as arguments, see \code{\link{ftable.default}}.}
}
\details{
  This is a method of the generic function \code{\link{ftable}}.

  The left and right hand side of \code{formula} specify the column and
  row variables, respectively, of the flat contingency table to be
  created.  Only the \code{+} operator is allowed for combining the
  variables.  A \code{.} may be used once in the formula to indicate
  inclusion of all the \dQuote{remaining} variables.

  If \code{data} is an object of class \code{"table"} or an array with
  more than 2 dimensions, it is taken as a contingency table, and hence
  all entries should be nonnegative.  Otherwise, if it is not a flat
  contingency table (i.e., an object of class \code{"ftable"}), it
  should be a data frame or matrix, list or environment containing the
  variables to be cross-tabulated.  In this case, \code{na.action} is
  applied to the data to handle missing values, and, after possibly
  selecting a subset of the data as specified by the \code{subset}
  argument, a contingency table is computed from the variables.

  The contingency table is then collapsed to a flat table, according to
  the row and column variables specified by \code{formula}.
}
\value{
  A flat contingency table which contains the counts of each combination
  of the levels of the variables, collapsed into a matrix for suitably
  displaying the counts.
}
\seealso{
  \code{\link{ftable}},
  \code{\link{ftable.default}};
  \code{\link{table}}.
}
\examples{
Titanic
x <- ftable(Survived ~ ., data = Titanic)
x
ftable(Sex ~ Class + Age, data = x)
}
\keyword{category}

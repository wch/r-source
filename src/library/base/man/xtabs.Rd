\name{xtabs}
\alias{xtabs}
\alias{print.xtabs}
\title{Cross Tabulation}
\description{
  Create a contingency table from cross-classifying factors, usually
  contained in a data frame, using a formula interface.
}
\usage{
xtabs(formula = ~., data, subset, na.action, exclude = c(NA, NaN),
      drop.unused.levels = FALSE)
}
\arguments{
  \item{formula}{a formula object with the cross-classifying variables,
    separated by \code{+}, on the right hand side.  Interactions are not
    allowed.  On the left hand side, one may optionally give a vector or
    a matrix of counts; in the latter case, the columns are interpreted
    as corresponding to the levels of a variable.  This is useful if the
    data has already been tabulated, see the examples below.}
  \item{data}{a data frame, list or environment containing the variables
    to be cross-tabulated.}
  \item{subset}{an optional vector specifying a subset of observations
    to be used.}
  \item{na.action}{a function which indicates what should happen when
    the data contain \code{NA}s.}
  \item{exclude}{a vector of values to be excluded when forming the
    set of levels of the classifying factors.}
  \item{drop.unused.levels}{a logical indicating whether to drop unused
    levels in the classifying factors.  If this is \code{FALSE} and
    there are unused levels, the table will contain zero marginals, and
    a subsequent chi-squared test for independence of the factors will
    not work.}
}
\details{
  There is a \code{summary} method for contingency table objects created
  by \code{table} or \code{xtabs}, which gives basic information and
  performs a chi-squared test for independence of factors (note that the
  function \code{\link[ctest]{chisq.test}} in package \bold{ctest}
  currently only handles 2-d tables).
}
\value{
  A contingency table in array representation of class \code{c("xtabs",
    "table")}, with a \code{"call"} attribute storing the matched call.
}
\seealso{
  \code{\link{table}} for ``traditional'' cross-tabulation, and
  \code{\link{as.data.frame.table}} which is the inverse operation of
  \code{xtabs} (see the \code{DF} example below).
}
\examples{
data(esoph)
## `esoph' has the frequencies of cases and controls for all levels of
## the variables `agegp', `alcgp', and `tobgp'.
xtabs(cbind(ncases, ncontrols) ~ ., data = esoph)
## Output is not really helpful ... flat tables are better:
ftable(xtabs(cbind(ncases, ncontrols) ~ ., data = esoph))
## In particular if we have fewer factors ...
ftable(xtabs(cbind(ncases, ncontrols) ~ agegp, data = esoph))

data(UCBAdmissions)
## This is already a contingency table in array form.
DF <- as.data.frame(UCBAdmissions)
## Now `DF' is a data frame with a grid of the factors and the counts
## in variable `Freq'.
DF
## Nice for taking margins ...
xtabs(Freq ~ Gender + Admit, DF)
## And for testing independece ...
summary(xtabs(Freq ~ ., DF))
}
\keyword{category}

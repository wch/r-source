\name{table}
\title{Cross Tabulation and Table Creation}
\alias{table}
\alias{summary.table}
\alias{print.summary.table}
\alias{as.data.frame.table}
\alias{as.table}
\alias{as.table.default}
\alias{is.table}
\description{
  \code{table} uses the cross-classifying factors to build a contingency
  table of the counts at each combination of factor levels.
}
\usage{
table(\dots, exclude = c(NA, NaN), dnn, deparse.level = 1)
as.table(x, \dots)
is.table(x)
as.data.frame.table(x, row.names = NULL, optional = FALSE, \dots)
}
\arguments{
  \item{\dots}{objects which can be interpreted as factors (including
    character strings), or a list (or data frame) whose components can
    be so interpreted}
  \item{exclude}{values to use in the exclude argument of \code{factor}
    when interpreting non-factor objects; if specified, levels to remove
    from all factors in \dots.}
  \item{dnn}{the names to be given to the dimensions in the result (the
    \emph{dimnames names}).}
  \item{deparse.level}{controls how the default \code{dnn} is
    constructed.  See details.}
  \item{x}{an arbitrary \R object.}
  \item{row.names}{a character vector giving the row names for the data
    frame.}
  \item{optional}{a logical controlling whether row names are set.
    Currently not used.}
}
\details{
  If the argument \code{dnn} is not supplied, the internal function
  \code{list.names} is called to compute the `dimname names'.  If the
  arguments in \code{\dots} are named, those names are used.  For the
  remaining arguments, \code{deparse.level = 0} gives an empty name,
  \code{deparse.level = 1} uses the supplied argument if it is a symbol,
  and \code{deparse.level = 2} will deparse the argument.

  Only when \code{exclude} is specified (i.e., not by default), will
  \code{table} drop levels of factor arguments potentially.

  There is a \code{summary} method for contingency table objects created
  by \code{table} or \code{xtabs}, which gives basic information and
  performs a chi-squared test for independence of factors (note that the
  function \code{\link[ctest]{chisq.test}} in package \bold{ctest}
  currently only handles 2-d tables).

  \code{as.table} and \code{is.table} coerce to and test for contingency
  table, respectively.

  \code{as.data.frame.table} is a method for the generic function
  \code{as.data.frame} to convert the array-based representation of a
  contingency table to a data frame containing the classifying factors
  and the corresponding counts (the latter as component \code{Freq}).
  This is the inverse of \code{\link{xtabs}}.
}
\examples{
## Simple frequency distribution
table(rpois(100,5))
data(warpbreaks)
attach(warpbreaks)
## Check the design:
table(wool, tension)
data(state)
table(state.division, state.region)

data(airquality)
attach(airquality)
# simple two-way contingency table
table(cut(Temp, quantile(Temp)), Month)

a <- letters[1:3]
table(a, sample(a))                    # dnn is c("a", "")
table(a, sample(a), deparse.level = 0) # dnn is c("", "")
table(a, sample(a), deparse.level = 2) # dnn is c("a", "sample(a)")

## xtabs() <-> as.data.frame.table() :
data(UCBAdmissions) ## already a contingency table
DF <- as.data.frame(UCBAdmissions)
class(tab <- xtabs(Freq ~ ., DF))# xtabs & table
## tab *is* ``the same'' as the original table:
all(tab == UCBAdmissions)
all.equal(dimnames(tab), dimnames(UCBAdmissions))

a <- rep(c(NA, 1/0:3), 10)
table(a)
table(a, exclude=NULL)
b <- factor(rep(c("A","B","C"), 10))
table(b)
table(b, exclude="B")
d <- factor(rep(c("A","B","C"), 10), levels=c("A","B","C","D","E"))
table(d, exclude="B")

## NA counting:
is.na(d) <- 3:4
d <- factor(d, exclude=NULL)
d[1:7]
table(d, exclude = NULL)
}
\keyword{category}

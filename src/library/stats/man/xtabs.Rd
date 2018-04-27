% File src/library/stats/man/xtabs.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2018 R Core Team
% Distributed under GPL 2 or later

\name{xtabs}
\alias{xtabs}
\alias{print.xtabs}
\title{Cross Tabulation}
\description{
  Create a contingency table (optionally a sparse matrix) from
  cross-classifying factors, usually contained in a data frame,
  using a formula interface.
}
\usage{
xtabs(formula = ~., data = parent.frame(), subset, sparse = FALSE,
      na.action, addNA = FALSE, exclude = if(!addNA) c(NA, NaN),
      drop.unused.levels = FALSE)

\method{print}{xtabs}(x, na.print = "", \dots)
}
\arguments{
  \item{formula}{a \link{formula} object with the cross-classifying variables
    (separated by \code{+}) on the right hand side (or an object which
    can be coerced to a formula).  Interactions are not allowed.  On the
    left hand side, one may optionally give a vector or a matrix of
    counts; in the latter case, the columns are interpreted as
    corresponding to the levels of a variable.  This is useful if the
    data have already been tabulated, see the examples below.}
  \item{data}{an optional matrix or data frame (or similar: see
    \code{\link{model.frame}}) containing the variables in the
    formula \code{formula}.  By default the variables are taken from
    \code{environment(formula)}.}
  \item{subset}{an optional vector specifying a subset of observations
    to be used.}
  \item{sparse}{logical specifying if the result should be a
    \emph{sparse} matrix, i.e., inheriting from
    \code{\link[Matrix:sparseMatrix-class]{sparseMatrix}}%\linkS4class{sparseMatrix}.
    Only works for two factors (since there
    are no higher-order sparse array classes yet).
  }
  \item{na.action}{a function which indicates what should happen when
    the data contain \code{\link{NA}}s.  If unspecified, and
    \code{addNA} is true, this is set to \code{\link{na.pass}}.  When it
    is \code{na.pass} and \code{formula} has a left hand side (with
    counts), \code{\link{sum}(*, na.rm = TRUE)} is used instead of
    \code{sum(*)} for the counts.}
  \item{addNA}{logical indicating if \code{NA}s should get a separate
    level and be counted, using \code{\link{addNA}(*, ifany=TRUE)} and
    setting the default for \code{na.action}.}
  \item{exclude}{a vector of values to be excluded when forming the
    set of levels of the classifying factors.}
  \item{drop.unused.levels}{a logical indicating whether to drop unused
    levels in the classifying factors.  If this is \code{FALSE} and
    there are unused levels, the table will contain zero marginals, and
    a subsequent chi-squared test for independence of the factors will
    not work.}

  \item{x}{an object of class \code{"xtabs"}.}
  \item{na.print}{character string (or \code{NULL}) indicating how
    \code{\link{NA}} are printed.  The default (\code{""}) does not show
    \code{NA}s clearly, and \code{na.print = "NA"} maybe advisable
    instead.}
  \item{\dots}{further arguments passed to or from other methods.}
}
\details{
  There is a \code{summary} method for contingency table objects created
  by \code{table} or \code{xtabs(*, sparse = FALSE)}, which gives basic
  information and performs a chi-squared test for independence of
  factors (note that the function \code{\link{chisq.test}} currently
  only handles 2-d tables).

  If a left hand side is given in \code{formula}, its entries are simply
  summed over the cells corresponding to the right hand side; this also
  works if the lhs does not give counts.

  For variables in \code{formula} which are factors, \code{exclude}
  must be specified explicitly; the default exclusions will not be used.

  In \R versions before 3.4.0, e.g., when \code{na.action = na.pass},
  sometimes zeroes (\code{0}) were returned instead of \code{NA}s.
}
\value{
  By default, when \code{sparse = FALSE},
  a contingency table in array representation of S3 class \code{c("xtabs",
    "table")}, with a \code{"call"} attribute storing the matched call.

  When \code{sparse = TRUE}, a sparse numeric matrix, specifically an
  object of S4 class %\linkS4class{dgTMatrix}
  \code{\link[Matrix:dgTMatrix-class]{dgTMatrix}} from package
  \CRANpkg{Matrix}.
}
\seealso{
  \code{\link{table}} for traditional cross-tabulation, and
  \code{\link{as.data.frame.table}} which is the inverse operation of
  \code{xtabs} (see the \code{DF} example below).

  \code{\link[Matrix:sparseMatrix-class]{sparseMatrix}} on sparse
  matrices in package \CRANpkg{Matrix}.
}
\examples{
## 'esoph' has the frequencies of cases and controls for all levels of
## the variables 'agegp', 'alcgp', and 'tobgp'.
xtabs(cbind(ncases, ncontrols) ~ ., data = esoph)
## Output is not really helpful ... flat tables are better:
ftable(xtabs(cbind(ncases, ncontrols) ~ ., data = esoph))
## In particular if we have fewer factors ...
ftable(xtabs(cbind(ncases, ncontrols) ~ agegp, data = esoph))

## This is already a contingency table in array form.
DF <- as.data.frame(UCBAdmissions)
## Now 'DF' is a data frame with a grid of the factors and the counts
## in variable 'Freq'.
DF
## Nice for taking margins ...
xtabs(Freq ~ Gender + Admit, DF)
## And for testing independence ...
summary(xtabs(Freq ~ ., DF))

## with NA's
DN <- DF; DN[cbind(6:9, c(1:2,4,1))] <- NA; DN
tools::assertError(# 'na.fail' should fail :
     xtabs(Freq ~ Gender + Admit, DN, na.action=na.fail))
xtabs(Freq ~ Gender + Admit, DN)
xtabs(Freq ~ Gender + Admit, DN, na.action = na.pass)
## The Female:Rejected combination has NA 'Freq' (and NA prints 'invisibly' as "")
xtabs(Freq ~ Gender + Admit, DN, addNA = TRUE) # ==> count NAs

## Create a nice display for the warp break data.
warpbreaks$replicate <- rep_len(1:9, 54)
ftable(xtabs(breaks ~ wool + tension + replicate, data = warpbreaks))

### ---- Sparse Examples ----

\donttest{if(require("Matrix")) withAutoprint({
 ## similar to "nlme"s  'ergoStool' :
 d.ergo <- data.frame(Type = paste0("T", rep(1:4, 9*4)),
                      Subj = gl(9, 4, 36*4))
 xtabs(~ Type + Subj, data = d.ergo) # 4 replicates each
 set.seed(15) # a subset of cases:
 xtabs(~ Type + Subj, data = d.ergo[sample(36, 10), ], sparse = TRUE)

 ## Hypothetical two-level setup:
 inner <- factor(sample(letters[1:25], 100, replace = TRUE))
 inout <- factor(sample(LETTERS[1:5], 25, replace = TRUE))
 fr <- data.frame(inner = inner, outer = inout[as.integer(inner)])
 xtabs(~ inner + outer, fr, sparse = TRUE)
})}% only if Matrix is available
}
\keyword{category}

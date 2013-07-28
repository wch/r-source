% File src/library/stats/man/xtabs.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2013 R Core Team
% Distributed under GPL 2 or later

\newcommand{\CRANpkg}{\href{http://CRAN.R-project.org/package=#1}{\pkg{#1}}}

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
      na.action, exclude = c(NA, NaN), drop.unused.levels = FALSE)
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
  by \code{table} or \code{xtabs(*, sparse = FALSE)}, which gives basic
  information and performs a chi-squared test for independence of
  factors (note that the function \code{\link{chisq.test}} currently
  only handles 2-d tables).

  If a left hand side is given in \code{formula}, its entries are simply
  summed over the cells corresponding to the right hand side; this also
  works if the lhs does not give counts.
  
  For variables in \code{formula} which are factors, \code{exclude}
  must be specified explicitly; the default exclusions will not be used.  
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

## Create a nice display for the warp break data.
warpbreaks$replicate <- rep(1:9, len = 54)
ftable(xtabs(breaks ~ wool + tension + replicate, data = warpbreaks))

### ---- Sparse Examples ----

\donttest{if(require("Matrix")) {
 ## similar to "nlme"s  'ergoStool' :
 d.ergo <- data.frame(Type = paste0("T", rep(1:4, 9*4)),
                      Subj = gl(9, 4, 36*4))
 print(xtabs(~ Type + Subj, data = d.ergo)) # 4 replicates each
 set.seed(15) # a subset of cases:
 print(xtabs(~ Type + Subj, data = d.ergo[sample(36, 10), ], sparse = TRUE))

 ## Hypothetical two level setup:
 inner <- factor(sample(letters[1:25], 100, replace = TRUE))
 inout <- factor(sample(LETTERS[1:5], 25, replace = TRUE))
 fr <- data.frame(inner = inner, outer = inout[as.integer(inner)])
 print(xtabs(~ inner + outer, fr, sparse = TRUE))
}}% only if Matrix is available
}
\keyword{category}

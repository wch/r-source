% File src/library/base/man/warnings.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2017 R Core Team
% Distributed under GPL 2 or later

\name{warnings}
\title{Print Warning Messages}
\alias{warnings}%%--> ../R/warnings.R
\alias{last.warning}
\alias{print.warnings}
\alias{[.warnings}
\alias{c.warnings}
\alias{duplicated.warnings}
\alias{unique.warnings}
\alias{summary.warnings}
\alias{print.summary.warnings}
\description{
  \code{warnings} and its \code{print} method print the
  variable \code{last.warning} in a pleasing form.
}
\usage{
warnings(\dots)

\S3method{summary}{warnings}(object, \dots)

\S3method{print}{warnings}(x, tags,
      header = ngettext(n, "Warning message:\n", "Warning messages:\n"),
      \dots)
\S3method{print}{summary.warnings}(x, \dots)
}
\arguments{
  \item{\dots}{arguments to be passed to \code{\link{cat}} (for
    \code{warnings()}).}
  \item{object}{a \code{"warnings"} object as returned by
    \code{warnings()}.}
  \item{x}{a \code{"warnings"} or \code{"summary.warnings"} object.}
  \item{tags}{if not \code{\link{missing}}, a \code{\link{character}}
    vector of the same \code{\link{length}} as \code{x}, to \dQuote{label}
    the messages.  Defaults to \code{paste0(seq_len(n), ": ")} for
    \eqn{n \ge 2}{n >= 2} where \code{n <- length(x)}.}
  \item{header}{a character string \code{\link{cat}()}ed before the
      messages are printed.}
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.
}
\details{
  See the description of \code{\link{options}("warn")} for the
  circumstances under which there is a \code{last.warning} object and
  \code{warnings()} is used.  In essence this is if \code{options(warn =
    0)} and \code{warning} has been called at least once.

  Note that the \code{\link{length}(last.warning)} is maximally
  \code{\link{getOption}("nwarnings")} (at the time the warnings are
  generated) which is \code{50} by default.  To increase, use something
  like \preformatted{  options(nwarnings = 10000)  }

  It is possible that \code{last.warning} refers to the last recorded
  warning and not to the last warning, for example if \code{options(warn)} has
  been changed or if a catastrophic error occurred.
}
\section{Warning}{
  It is undocumented where \code{last.warning} is stored nor that it is
  visible, and this is subject to change.
}
\seealso{
  \code{\link{warning}}.
}
\value{\code{warnings()} returns an object of S3 class \code{"warnings"}, basically a named
  \code{\link{list}}.

  \code{summary(<warnings>)} returns a \code{"summary.warnings"}
  object which is basically the \code{\link{list}} of unique warnings
  (\code{unique(object)}) with a \code{"counts"} attribute, somewhat
  experimentally.
}
\examples{
## NB this example is intended to be pasted in,
##    rather than run by example()
ow <- options("warn")
for(w in -1:1) {
   options(warn = w); cat("\n warn =", w, "\n")
   for(i in 1:3) { cat(i,"..\n"); m <- matrix(1:7, 3,4) }
   cat("--=--=--\n")
}
## at the end prints all three warnings, from the 'option(warn = 0)' above
options(ow) # reset to previous, typically 'warn = 0'
tail(warnings(), 2) # see the last two warnings only (via '[' method)

## Often the most useful way to look at many warnings:
summary(warnings())
\dontshow{
ww <- warnings()
uw <- unique(ww)
sw <- summary(ww)
stopifnot(identical(c(ww[1], ww[3]), ww[c(1, 3)]),
          length(uw) == 1, nchar(names(uw)) > 10,
          length(sw) == 1, attr(sw, "counts") == 3)
}
op <- options(nwarnings = 10000) ## <- get "full statistics"
x <- 1:36; for(n in 1:13) for(m in 1:12) A <- matrix(x, n,m) # There were 105 warnings ...
summary(warnings())
options(op) # revert to previous (keeping 50 messages by default)
}
\keyword{programming}
\keyword{error}

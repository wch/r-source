% File src/library/base/man/deparse.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2019 R Core Team
% Distributed under GPL 2 or later

\name{deparse}
\alias{deparse}
\alias{deparse1}
\title{Expression Deparsing}
\description{
  Turn unevaluated expressions into character strings.
}
\usage{
deparse(expr, width.cutoff = 60L,
        backtick = mode(expr) \%in\% c("call", "expression", "(", "function"),
        control = c("keepNA", "keepInteger", "niceNames", "showAttributes"),
        nlines = -1L)

deparse1(expr, collapse = " ", width.cutoff = 500L, ...)
}
\arguments{
  \item{expr}{any \R expression.}
  \item{width.cutoff}{integer in \eqn{[20, 500]} determining the cutoff
    (in bytes) at which line-breaking is tried.}
  \item{backtick}{logical indicating whether symbolic names should be
    enclosed in backticks if they do not follow the standard syntax.}
  \item{control}{character vector (or \code{NULL}) of deparsing options.
    See \code{\link{.deparseOpts}}.}
  \item{nlines}{integer: the maximum number of lines to produce.
    Negative values indicate no limit.}
  \item{collapse}{a string, passed to \code{\link{parse}()}.}
  \item{\dots}{further arguments passed to \code{deparse()}.}
}
\details{
  These functions turn unevaluated expressions (where \sQuote{expression}
  is taken in a wider sense than the strict concept of a vector of
  \code{\link{mode}} and type (\code{\link{typeof}})
  \code{"expression"} used in \code{\link{expression}}) into character
  strings (a kind of inverse to \code{\link{parse}}).

  A typical use of this is to create informative labels for data sets
  and plots.  The example shows a simple use of this facility.  It uses
  the functions \code{deparse} and \code{substitute} to create labels
  for a plot which are character string versions of the actual arguments
  to the function \code{myplot}.

  The default for the \code{backtick} option is not to quote single
  symbols but only composite expressions.  This is a compromise to
  avoid breaking existing code.

  Using \code{control = "all"} comes closest to making \code{deparse()}
  an inverse of \code{parse()}.  However, not all objects are
  deparse-able even with this option and a warning will be issued if the
  function recognizes that it is being asked to do the impossible.

  Numeric and complex vectors are converted using 15 significant digits:
  see \code{\link{as.character}} for more details.

  \code{width.cutoff} is a lower bound for the line lengths: deparsing a
  line proceeds until at least \code{width.cutoff} \emph{bytes} have
  been output and e.g.\sspace{}\code{arg = value} expressions will not be split
  across lines.

  \code{deparse1()} is a simple utility to ensure a string result
  (\code{\link{character}} vector of length one), typically used in
  name construction, as \code{deparse1(\link{substitute}(.))}.
}
\note{
  To avoid the risk of a source attribute out of sync with the actual
  function definition, the source attribute of a function will never
  be deparsed as an attribute.

  Deparsing internal structures may not be accurate: for example the
  graphics display list recorded by \code{\link{recordPlot}} is not
  intended to be deparsed and \code{.Internal} calls will be shown as
  primitive calls.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.
}
\seealso{
  \code{\link{substitute}},
  \code{\link{parse}},
  \code{\link{expression}}.

  \code{Quotes} for quoting conventions, including backticks.
}
\examples{
require(stats); require(graphics)

deparse(args(lm))
deparse(args(lm), width = 500)

myplot <- function(x, y) {
    plot(x, y, xlab = deparse1(substitute(x)),
               ylab = deparse1(substitute(y)))
}

e <- quote(`foo bar`)
deparse(e)
deparse(e, backtick = TRUE)
e <- quote(`foo bar`+1)
deparse(e)
deparse(e, control = "all") # wraps it w/ quote( . )
}
\keyword{programming}
\keyword{manip}
\keyword{data}

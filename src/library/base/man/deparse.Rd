% File src/library/base/man/deparse.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2022 R Core Team
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
    \code{control = "all"} is thorough, see \code{\link{.deparseOpts}}.}
  \item{nlines}{integer: the maximum number of lines to produce.  Negative
    values indicate no limit.} 
  \item{collapse}{a string, passed to \code{\link{paste}()}.}
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

  \code{width.cutoff} is a lower bound for the line lengths: deparsing a
  line proceeds until at least \code{width.cutoff} \emph{bytes} have
  been output and e.g.\sspace{}\code{arg = value} expressions will not be split
  across lines.

  \code{deparse1()} is a simple utility added in \R{} 4.0.0 to ensure a
  string result (\code{\link{character}} vector of length one),
  typically used in name construction, as
  \code{deparse1(\link{substitute}(.))}.
}
\note{
  To avoid the risk of a source attribute out of sync with the actual
  function definition, the source attribute of a function will never
  be deparsed as an attribute.

  Deparsing internal structures may not be accurate: for example the
  graphics display list recorded by \code{\link{recordPlot}} is not
  intended to be deparsed and \code{.Internal} calls will be shown as
  primitive calls.

  %% Prior to \R 4.2.0, attributes named \code{dim}, \code{dimnames},
  %% \code{levels}, \code{names} and \code{tsp} were deparsed to
  %% \code{.Dim}, \code{.Dimnames}, \code{.Label}, \code{.Names} and
  %% \code{.Tsp} as part of calls to \code{\link{structure}}, apparently
  %% for historical compatibility with S.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.
}
\seealso{
  \code{\link{.deparseOpts}} for available \code{control} settings;
  \code{\link{dput}()} and \code{\link{dump}()} for related functions using
  identical internal deparsing functionality.

  \code{\link{substitute}},
  \code{\link{parse}},
  \code{\link{expression}}.

  \code{Quotes} for quoting conventions, including backticks.
}
\examples{
require(stats); require(graphics)

deparse(args(lm))
deparse(args(lm), width.cutoff = 500)

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

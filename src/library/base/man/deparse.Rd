\name{deparse}
\alias{deparse}
\title{Expression Deparsing}
\description{
  Turn unevaluated expressions into character strings.
}
\usage{
deparse(expr, width.cutoff = 60,
        backtick = mode(expr) \%in\% c("call", "expression", "("),
        forDisplay = TRUE, useSource = FALSE)
}
\arguments{
  \item{expr}{any \R expression.}
  \item{width.cutoff}{integer in \eqn{[20, 500]} determining the cutoff
    at which line-breaking is tried.}
  \item{backtick}{logical indicating whether symbolic names should be
    enclosed in backticks if they don't follow the standard syntax.}
  \item{forDisplay}{logical indicating that the intention is to display the results
  rather than to re-parse them}
  \item{useSource}{logical indicating whether to use the saved source
  when deparsing a function}
}
\details{
  This function turns unevaluated expressions (where \dQuote{expression}
  is taken in a wider sense than the strict concept of a vector of mode
  \code{"expression"} used in \code{\link{expression}}) into character
  strings (a kind of inverse \code{\link{parse}}).

  A typical use of this is to create informative labels for data sets
  and plots.  The example shows a simple use of this facility.  It uses
  the functions \code{deparse} and \code{substitute} to create labels
  for a plot which are character string versions of the actual arguments
  to the function \code{myplot}.

  The default for the \code{backtick} option is not to quote single
  symbols but only composite expressions. This is a compromise to
  avoid breaking existing code.

  With \code{forDisplay = TRUE}, the deparsing is intended to be readable,
  but perhaps incomplete:  for example, attributes are not generally shown.
  
  The \code{forDisplay = FALSE} option requests a more accurate
  but less readable deparsing of complex objects, bringing \code{deparse()}
  closer to being an inverse of \code{parse()}.  However, not all 
  objects are deparseable even with this option.  A warning will be
  issued if the function recognizes that it is being asked to do the impossible.
    
  To display saved source rather than deparsing the internal representation 
  use \code{useSource = TRUE}.  \R currently saves source only for
  function definitions.
}
\note{
  To avoid the risk of a source attribute out of sync with the actual
  function definition, the source attribute of a function will never 
  be deparsed as an attribute.
}   
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole.
}
\seealso{
  \code{\link{substitute}},
  \code{\link{parse}},
  \code{\link{expression}}.
}
\examples{
require(stats)
deparse(args(lm))
deparse(args(lm), width = 500)
myplot <-
function(x, y) {
    plot(x, y, xlab=deparse(substitute(x)),
        ylab=deparse(substitute(y)))
}
e <- quote(`foo bar`)
deparse(e)
deparse(e, backtick=TRUE)
e <- quote(`foo bar`+1)
deparse(e)
deparse(e, forDisplay = FALSE)
}
\keyword{programming}
\keyword{manip}
\keyword{data}

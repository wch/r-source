\name{deparse}
\alias{deparse}
\alias{.deparseOpts}
\title{Expression Deparsing}
\description{
  Turn unevaluated expressions into character strings.
}
\usage{
deparse(expr, width.cutoff = 60,
        backtick = mode(expr) \%in\% c("call", "expression", "("),
        control = "showAttributes")
}
\arguments{
  \item{expr}{any \R expression.}
  \item{width.cutoff}{integer in \eqn{[20, 500]} determining the cutoff
    at which line-breaking is tried.}
  \item{backtick}{logical indicating whether symbolic names should be
    enclosed in backticks if they don't follow the standard syntax.}
  \item{control}{character vector of deparsing options.  See the details
  below.}
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

  The \code{control} argument is a vector containing some of the
  following strings.  Partial string matching is used:
  \describe{
      \item{\code{keepInteger}}{Surround integer vectors by \code{as.integer()},
      	    so they are not converted to floating point when re-parsed.}
      \item{\code{quoteExpressions}}{Surround expressions with \code{quote()},
            so they are not evaluated when re-parsed.}
      \item{\code{showAttributes}}{If the object has attributes (other than
            a \code{source} attribute), use \code{structure()} to display them as 
            well as the object value.}
      \item{\code{useSource}}{If the object has a \code{source} attribute,
            display that instead of deparsing the object.  Currently
            only applies to function definitions.}
      \item{\code{warnIncomplete}}{Some exotic objects such
            as \link[base:delay]{promises}, \link[base:environment]{environments}, 
            external pointers, etc. can not be deparsed properly.  This option causes
            a warning to be issued if any of those were present.}
      \item{\code{all}}{An abbreviated way to specify all of the options 
      	    listed above.  May not be used with other options.}
 }
  
  For the most readable (but perhaps incomplete) display, use \code{control = NULL}.
  This displays the object's value, but not its attributes.  The default
  is to display the attributes as well, but not to use any of the other
  options to make the result parseable.
  
  Using \code{control = "all"} comes closest
  to making \code{deparse()} an inverse of \code{parse()}.  However, not all 
  objects are deparseable even with this option.  A warning will be
  issued if the function recognizes that it is being asked to do the impossible.
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
deparse(e, control = "all")
}
\keyword{programming}
\keyword{manip}
\keyword{data}

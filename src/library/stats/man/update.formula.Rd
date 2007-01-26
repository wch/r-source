\name{update.formula}
\alias{update.formula}
\title{Model Updating}
\usage{
\method{update}{formula}(old, new, \dots)
}
\arguments{
  \item{old}{a model formula to be updated.}
  \item{new}{a formula giving a template which specifies how to update.}
  \item{\dots}{further arguments passed to or from other methods.}
}
\description{
  \code{update.formula} is used to update model formulae.
  This typically involves adding or dropping terms,
  but updates can be more general.
}
\details{
  Either or both of \code{old} and \code{new} can be objects such as
  length-one character vectors which can be coerced to a formula via
  \code{\link{as.formula}}.
  
  The function works by first identifying the \emph{left-hand side}
  and \emph{right-hand side} of the \code{old} formula.
  It then examines the \code{new} formula and substitutes
  the \emph{lhs} of the \code{old} formula for any occurrence
  of \sQuote{.} on the left of \code{new}, and substitutes
  the \emph{rhs} of the \code{old} formula for any occurrence
  of \sQuote{.} on the right of \code{new}.  The result is then
  simplified \emph{via} \code{\link{terms.formula}(simplify = TRUE)}.
}
\value{
  The updated formula is returned.  The environment of the result will
  that of \code{old}.
}
\seealso{
  \code{\link{terms}}, \code{\link{model.matrix}}.
}
\examples{
update(y ~ x,    ~ . + x2) #> y ~ x + x2
update(y ~ x, log(.) ~ . ) #> log(y) ~ x
}
\keyword{models}

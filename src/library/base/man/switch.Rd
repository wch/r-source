\name{switch}
\title{Select One of a List of Alternatives}
\usage{
switch(EXPR, \dots)
}
\alias{switch}
\arguments{
  \item{EXPR}{an expression that evaluates to a number or a character
    string}
  \item{\dots}{the list of alternatives, given explicitly}
}
\description{
  \code{switch} evaluates \code{EXPR}.  If the value is an integer
  between 1 and \code{nargs()-1} then the corresponding element of
  \code{\dots} is evaluated and the result returned.  If \code{EXPR}
  returns a character string then that string is used to match the names
  of the elements in \code{\dots}.  If there is an exact match then that
  element is evaluated and the result returned.
}
\examples{
centre <- function(x, type) {
switch(type,
       mean = mean(x),
       median = median(x),
       trimmed = mean(x, trim = .1))
}
x <- rcauchy(10)
centre(x, "mean")
centre(x, "median")
centre(x, "trimmed")
}
\keyword{programming}

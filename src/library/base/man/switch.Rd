\name{switch}
\title{Select One of a List of Alternatives}
\usage{
switch(EXPR, \dots)
}
\alias{switch}
\arguments{
  \item{EXPR}{an expression evaluating to a number or a character string.}
  \item{\dots}{the list of alternatives, given explicitly.}
}
\description{
  \code{switch} evaluates \code{EXPR}.  If the value is an integer
  between 1 and \code{nargs()-1} then the corresponding element of
  \code{\dots} is evaluated and the result returned.

  If \code{EXPR} returns a character string then that string is used to
  match the names of the elements in \code{\dots}.  If there is an exact
  match then that element is evaluated and the result returned.  In the
  case of no match, if there's a further argument in \code{switch(..)}
  that one is returned, otherwise \code{NULL}.
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

ccc <- c("b","QQ","a","A","b")
for(ch in ccc) cat(ch,":",switch(ch, a=1, b=2:3),		"\n")
for(ch in ccc) cat(ch,":",switch(ch, a=1, b=2:3, "Otherwise: last arg."),"\n")

## Numeric EXPR don't allow an `otherwise':
for(i in c(-1:3,9))  print(switch(i, 1,2,3,4))
}
\keyword{programming}

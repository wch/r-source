\name{switch}
\alias{switch}
\title{Select One of a List of Alternatives}
\description{
  \code{switch} evaluates \code{EXPR} and accordingly chooses one of the
  further arguments (in \code{\dots}).
}
\usage{
switch(EXPR, \dots)
}
\arguments{
  \item{EXPR}{an expression evaluating to a number or a character
    string.}
  \item{\dots}{the list of alternatives, given explicitly.}
}
\details{
  If the value of \code{EXPR} is an integer between 1 and
  \code{nargs()-1} then the corresponding element of \code{\dots} is
  evaluated and the result returned.

  If \code{EXPR} returns a character string then that string is used to
  match the names of the elements in \code{\dots}.  If there is an exact
  match then that element is evaluated and returned if there is one,
   otherwise the next element is chosen, e.g.,
  \code{switch("cc", a=1, cc=, d=2)} evaluates to \code{2}.

  In the case of no match, if there's a further argument in \code{switch}
  that one is returned, otherwise \code{NULL}.
}
\section{Warning}{
  Beware of partial matching: an alternative \code{E = foo} will match
  the first argument \code{EXPR} unless that is named. See the examples
  for good practice in naming the first argument.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole.
}
\examples{
require(stats)
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

ccc <- c("b","QQ","a","A","bb")
for(ch in ccc)
    cat(ch,":",switch(EXPR = ch, a=1,     b=2:3), "\n")
for(ch in ccc)
    cat(ch,":",switch(EXPR = ch, a=, A=1, b=2:3, "Otherwise: last"),"\n")

## Numeric EXPR don't allow an 'otherwise':
for(i in c(-1:3,9))  print(switch(i, 1,2,3,4))
}
\keyword{programming}

\name{match.arg}
\title{Argument Verification Using Partial Matching}
\usage{
match.arg(arg)
match.arg(arg, choices)
}
\alias{match.arg}
\arguments{
  \item{arg}{a character string}
  \item{choices}{a character vector of candidate values}
}
\description{
  \code{match.arg} matches \code{arg} against a table of candidate
  values as specified by \code{choices}.
}
\details{
  In the one-argument form
  \code{match.arg(arg)}, the choices are obtained from a default setting
  for the formal argument \code{arg} of the function from which
  \code{match.arg} was called.

  Matching is done using \code{\link{pmatch}}, so \code{arg} may be
  abbreviated.
}
\value{
  The unabbreviated version of the unique partial match if there is one;
  otherwise, an error is signalled.
}
\seealso{
  \code{\link{pmatch}}, \code{\link{match.fun}}, \code{\link{match.call}}.
}
\examples{
## Extends the example for `switch'
center <- function(x, type = c("mean", "median", "trimmed")) {
  type <- match.arg(type)
  switch(type,
         mean = mean(x),
         median = median(x),
         trimmed = mean(x, trim = .1))
}
x <- rcauchy(10)
center(x, "t")       # Works
center(x, "med")     # Works
\dontrun{center(x, "m")       # Error
}
}
\keyword{programming}

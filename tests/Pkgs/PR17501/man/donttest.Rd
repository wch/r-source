\name{donttest}
\alias{donttest}
\title{donttest}
\description{Syntax error in \code{\\donttest} snipppet.}
\examples{
## the snippet below will be commented by .createExdotR()
## and then also removed by massageExamples(), which seems obsolete
\donttest{
R.Version(   # missing closing parenthesis
}
\donttest{
## another donttest block
}
}

\name{contrasts}
\title{Get and Set Contrast Matrices}
\usage{
contrasts(x)
contrasts(x) <- ctr
}
\alias{contrasts}
\alias{contrasts<-}
\arguments{
\item{x}{a factor.}
\item{ctr}{either a matrix whose columns give coefficients
for contrasts in the levels of \code{x}, or the (quoted)
name of a function which computes such matrices.}
}
\value{
These functions are used to set and view the
contrasts associated with a factor.
If contrasts are not set for a factor the default
functions named the system options are used.
}
\seealso{
\code{\link{contr.helmert}},
\code{\link{contr.poly}},
\code{\link{contr.sum}},
\code{\link{contr.treatment}},
\code{\link{glm}},
\code{\link{lm}}.
}
\keyword{design}
\keyword{regression}

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
functions from \code{\link{options}("contrasts")} are used.
}
\seealso{
    \code{\link{C}}, \code{\link{contr.helmert}}, \code{\link{contr.poly}},
    \code{\link{contr.sum}}, \code{\link{contr.treatment}},
    \code{\link{glm}}, \code{\link{aov}}, \code{\link{lm}}.
}
\examples{
example(factor)
(fff <- factor(ff))
contrasts(fff) # treatment
contrasts(C(fff, sum))
}
\keyword{design}
\keyword{regression}

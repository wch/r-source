\name{.Alias}
\title{Create ``Alias'' (Pointer) to \R Object}
\usage{
new <- .Alias(expr)
}
\alias{.Alias}
\arguments{
  \item{expr}{}
  \item{new}{new name by which \code{expr} can be accessed.}
}
\description{
 ~~ A precise description of what the function does. ~~
}
\value{
  \emph{an identical copy} of \code{expr}.
}
\references{ ~put references to the literature/web site here ~ }
\section{WARNING}{
}
\seealso{\code{\link{<-}} for usual assignments.}
\examples{
mop <- options()
Op <- .Alias(mop)
## Any change to mop is reflected in Op and vice versa!
mop $ newslot <- pi
 Op $ newslot
mop $ digits <- "Wow!"
 Op $ another <- 2
mop $ another; Op $ digits
all(names(mop) == names(Op))# TRUE -- Op and mop  *ARE* the same thing
}
\keyword{programming}

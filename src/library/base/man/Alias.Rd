\name{.Alias}
\title{Create ``Alias'' (Pointer) to \R Object}
\usage{
new <- .Alias(expr)
}
\alias{.Alias}
\arguments{
  \item{expr}{an \R expression; typically a name.}
  \item{new}{new name by which \code{expr} can be accessed.}
}
\description{
  \code{.Alias} creates an \bold{alias} to another (part of) an \R
  object which is more (memory-) efficient than usual assignment.
}
\value{
  \emph{an identical copy} of \code{expr}.
}
\references{ ~put references to the literature/web site here ~ }
\section{WARNING}{
  This has a \bold{dangerous} semantic, and consequences can be
  unexpected (it can be used to defeat the call-by-value illusion).
  Know what you are doing \emph{before} using \code{.Alias}!
}
\seealso{\code{\link{<-}} for usual assignments.}
\examples{
mop <- options()
Op <- .Alias(mop)
## Any change to mop is reflected in Op and vice versa!
mop $ newslot <- pi
 Op $ newslot # 3.1415..
mop $ digits <- "Wow!"
 Op $ another <- 2
mop $ another; Op $ digits
all(names(mop) == names(Op))# TRUE -- Op and mop  *ARE* the same thing
}
\keyword{programming}

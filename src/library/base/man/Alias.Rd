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
\section{WARNING}{
  This has a \bold{dangerous} semantic, and consequences can be
  unexpected (it can be used to defeat the call-by-value illusion).
  Know what you are doing \emph{before} using \code{.Alias}!
}
\seealso{\code{\link{<-}} for usual assignments.}
\examples{
mop <- options()
Op <- .Alias(mop)
## A change to mop is reflected in Op and vice versa
## -- ONLY if no new slots are created ...
mop $ digits <- "Wow!"
Op  $ browser <- "Web broser"
mop $ browser; Op $ digits
all(names(mop) == names(Op) &
    sapply(seq(mop), function(i) all(Op[[i]] == mop[[i]])))
##> TRUE -- Op and mop  *ARE* the same thing !

mop $ newslot <- pi #--->> 'newslot' ==> (shallow) COPY of 'mop'
 Op $ newslot# R (>= 0.63): still the old one, i.e. NULL
all(names(mop) == names(Op))# no longer TRUE [R >= 0.63]
}
\keyword{programming}

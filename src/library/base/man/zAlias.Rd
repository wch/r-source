\name{.Alias-deprecated}
\alias{.Alias-deprecated}
\title{Create Alias (Pointer) to R Object}
\description{
  \code{.Alias} creates an \bold{alias} to another (part of) an \R
  object which is more (memory-) efficient than usual assignment.
}
\usage{
.Alias(expr)
}
\arguments{
  \item{expr}{an \R expression; typically a name.}
}
\details{
  Use as \code{new <- .Alias(expr)}, where \code{new} is a new name by
  which \code{expr} can be accessed.
}
\value{
  \emph{an identical copy} of \code{expr}.
}
\section{Warning}{
  This has a \bold{dangerous} semantic, and consequences can be
  unexpected (it can be used to defeat the call-by-value illusion).
  Know what you are doing \emph{before} using \code{.Alias}!
}
\seealso{\code{\link{<-}} for usual assignments.}
\examples{
mop <- options()
mop$browser <- "a browser"   # not set on all platforms
Op <- .Alias(mop)
## A change to mop is reflected in Op and vice versa
## -- ONLY if no new slots are created ...
mop$digits <- "Wow!"
Op$browser <- "another one"
mop$browser; Op$digits
all(names(mop) == names(Op) &
    sapply(seq(mop), function(i) all(Op[[i]] == mop[[i]])))
##> TRUE -- Op and mop ARE the same thing !

mop$newslot <- pi #--->> 'newslot' ==> (shallow) COPY of 'mop'
Op$newslot # R: still the old one, i.e. NULL
all(names(mop) == names(Op))# no longer TRUE

## Feel the power: `call by reference', a function modifying its argument:
tst.Al <- function(x) {
   y <- .Alias(x) ; attributes(y) <- NULL ; invisible()
}
(x0 <- structure(1:5, my.att = "Y"))
tst.Al(x0) # *changes* x0 :
x0
stopifnot(is.null(attributes(x0)))
}
\keyword{programming}

\name{mode}
\alias{mode}
\alias{mode<-}
\alias{storage.mode}
\alias{storage.mode<-}
\title{The (Storage) Mode of an Object}
\description{
  Get or set the type or storage mode of an object.
}
\usage{
mode(x)
mode(x) <- value
storage.mode(x)
storage.mode(x) <- value
}
\arguments{
  \item{x}{any \R object.}
  \item{value}{a character string giving the desired (storage) mode of
    the object.}
}
\details{
  Both \code{mode} and \code{storage.mode} return a character string
  giving the (storage) mode of the object --- often the same --- both
  relying on the output of \code{\link{typeof}(x)}, see the example
  below.

  The two assignment versions are currently identical.  Both
  \code{mode(x) <- "newmode"} and \code{storage.mode(x) <- "newmode"} change
  the \code{mode} or \code{storage.mode} of object \code{x} to
  \code{newmode}.  This is only supported if there is an appropriate
  \code{as.newmode} function, for example
  \code{"logical"}, \code{"integer"}, \code{"double"}, \code{"complex"},
  \code{"raw"}, \code{"character"}, \code{"list"}, \code{"expression"},
  \code{"name"}, \code{"symbol"} and \code{"function"}.

  As storage mode \code{"single"} is only a pseudo-mode in \R, it will
  not be reported by \code{mode} or \code{storage.mode}: use
  \code{attr(object, "Csingle")} to examine this. However, the
  assignment versions can be used to set the mode to \code{"single"},
  which sets the real mode to \code{"double"} and the \code{"Csingle"}
  attribute to \code{TRUE}. Setting any other mode will remove this
  attribute.

  Note (in the examples below) that some \code{\link{call}}s have mode
  \code{"("} which is S compatible.
}
\section{Mode names}{
  Modes have the same set of names as types (see \code{\link{typeof}})
  except that
  \itemize{
    \item types \code{"integer"} and \code{"double"} are
    returned as \code{"numeric"}.
    \item types \code{"special"} and \code{"builtin"} are returned as
    \code{"function"}.
    \item type \code{"symbol"} is called mode \code{"name"}.
    \item type \code{"language"} is returned as \code{"("} or \code{"call"}.
  }
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole.
}
\seealso{
  \code{\link{typeof}} for the R-internal \dQuote{mode},
  \code{\link{attributes}}.
}
\examples{
sapply(options(),mode)

cex3 <- c("NULL","1","1:1","1i","list(1)","data.frame(x=1)", "pairlist(pi)",
  "c", "lm", "formals(lm)[[1]]",  "formals(lm)[[2]]",
  "y~x","expression((1))[[1]]", "(y~x)[[1]]", "expression(x <- pi)[[1]][[1]]")
lex3 <- sapply(cex3, function(x) eval(parse(text=x)))
mex3 <- t(sapply(lex3, function(x) c(typeof(x), storage.mode(x), mode(x))))
dimnames(mex3) <- list(cex3, c("typeof(.)","storage.mode(.)","mode(.)"))
mex3

## This also makes a local copy of 'pi':
storage.mode(pi) <- "complex"
storage.mode(pi)
rm(pi)
}
\keyword{attribute}

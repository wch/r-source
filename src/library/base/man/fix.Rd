\name{fix}
\title{Fix an Object}
\usage{
fix(x)
}
\alias{fix}
\arguments{
  \item{x}{An \R object}
}
\description{
    \code{fix} invokes the editor specified in
    \code{\link{options}("editor")} on \code{x} and then assigns the new
    (edited) version of \code{x} in the global environment.
}
\examples{
\dontrun{
 ## Assume  `my.fun' is a user defined function :
 fix(my.fun)
 ## now my.fun is changed
}
\testonly{
 if(machine() != "Macintosh") {
  oo <- options(editor="touch") # not really changing anything
  fix(pi)
  if(!is.numeric(pi) || length(pi)!=1 ||
     !is.null(attributes(pi)) || abs(pi - 3.1415) > 1e-4)
	stop("OOPS:  fix() is broken ...")
  rm(pi); options(oo)
 }
}
}
\keyword{utilities}

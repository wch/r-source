\name{fix}
\alias{fix}
\title{Fix an Object}
\description{
  \code{fix} invokes the editor specified in
  \code{\link{options}("editor")} on \code{x} and then assigns the new
  (edited) version of \code{x} in the user's workspace.
}
\usage{
fix(x, \dots)
}
\arguments{
  \item{x}{an \R object.}
  \item{\dots}{arguments to pass to editor.}
}
\seealso{
  \code{\link{edit}},
  \code{\link{edit.data.frame}}
}
\examples{
\dontrun{
 ## Assume  `my.fun' is a user defined function :
 fix(my.fun)
 ## now my.fun is changed
 ## Also,
 fix(my.data.frame) # calls up data editor
 fix(my.data.frame, factor.mode="char") # use of ...
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

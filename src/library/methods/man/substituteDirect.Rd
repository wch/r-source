\name{substituteDirect}
\alias{substituteDirect}
\title{SubstituteDirect}
\usage{
substituteDirect(object, frame, cleanFunction=T)
}
\description{
  Substitute for the variables named in the second argument the corresponding
objects, substituting into \code{object}.  The argument \code{frame}
is a named list; if omitted, the environment of the caller is used.

This function differs from the ordinary \code{substitute} in that it treats its first argument
in the standard S way, by evaluating it.  In contrast, \code{substitute} does
not evaluate its first argument.

}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--         or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (object, frame = sys.frame(-1), cleanFunction = T) 
{
    value <- .Call("do_substitute_direct", object, frame)
    if (cleanFunction && is.function(value)) {
        environment(value) <- .GlobalEnv
    }
    value
  }
}
\keyword{}

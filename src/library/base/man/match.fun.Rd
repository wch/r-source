\name{match.fun}
\title{Function Verification for ``Function Variables''}
\usage{
match.fun(FUN, descend=TRUE)
}
\alias{match.fun}
\arguments{
 \item{FUN}{item to match as function.}
 \item{descend}{logical; control whether to search past non-function
     objects.}
}

\description{
    This function is called inside functions that take a function as
argument. It's purpose is to extract the desired function object while
avoiding undesired matching to objects of other types. \code{match.fun}
is not intended to be used at the top level since it will perform
matching in the \emph{parent} of the caller.
    
It is assumed that \code{FUN} is an argument passed to the caller and is
a function or a character string containing the name of a function.

Returns an error if no matching function is found. } \details{ If
\code{FUN} is a function, it is returned. If it is a symbol or a
character vector of length one, it will be looked up using \code{get} in
the environment of the parent of the caller. If it is of any other mode,
it is attempted first to get the argument to the caller as a symbol
(using \code{substitute} twice), and if that fails, an error is
declared.

If \code{descend=TRUE}, \code{match.fun} will look past
non-function objects with the given name; otherwise if \code{FUN}
points to a non-function object then an error is generated.

This is now used in base functions such as
\code{\link{apply}}, \code{\link{lapply}}, \code{\link{outer}},
and \code{\link{sweep}}.
}
\value{A function matching \code{FUN} or an error is generated.
}

\author{
    Peter Dalgaard and Robert Gentleman, based on an earlier version
    by Jonathan Rougier}

\section{Bugs}{
    The \code{descend} argument is a bit of misnomer and probably not
    actually needed by anything. It may go away in the future.

    It is impossible to fully foolproof this. If one \code{attach}es a
    list or data frame containing a character object with the same name
    of a system function, it will be used.}

\seealso{\code{\link{match.arg}}, \code{\link{get}}}

\examples{
# Same as get("*"):
match.fun("*")
# Overwrite outer with a vector
outer <- 1:5
\dontrun{
match.fun(outer, descend = FALSE) #-> Error:  not a function
}
match.fun(outer) # finds it anyway
is.function(match.fun("outer")) # as well
}
\keyword{programming}

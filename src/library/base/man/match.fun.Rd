\name{match.fun}
\title{Function Verification for ``Function Variables''}
\usage{
match.fun(FUN, descend=TRUE)
}
\alias{match.fun}
\arguments{
 \item{FUN}{function, e.g. as passed to \code{\link{outer}}.}
 \item{descend}{logical; control whether to search under non-function
     objects.}
}
\description{
Matches \code{FUN} to a function, where \code{FUN} can be a
character (string), e.g., \code{"*"} or \code{"fred"},
or a function such as \code{pmax}.

Returns an error if no matching function is found.
}
\details{
If \code{descend=TRUE}, \code{match.fun} will look underneath
non-function objects with the given name; otherwise if \code{FUN}
points to a non-function object then an error is generated.

This is now used in base functions such as
\code{\link{apply}}, \code{\link{lapply}}, \code{\link{outer}},
\code{\link{sweep}}, and the like.
}
\value{A function matching \code{FUN} or an error is generated.
}
\author{Jonathan Rougier}

\section{Bug/Feature}{
A character argument to \code{FUN} is always matched to a
function, even if there is a non-function object higher up the
search list with the same name.}

\seealso{\code{\link{match.arg}}}

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

\name{make.names}
\alias{make.names}
\title{Make Syntactically Valid Names}
\description{
  Make syntactically valid names out of character vectors.
}
\usage{
make.names(names, unique = FALSE)
}
\arguments{
 \item{names}{character vector to be coerced to syntactically valid
   names. This is coerced to character if necessary.}
 \item{unique}{logical; if \code{TRUE}, the resulting elements are
     unique.  This may be desired for, e.g., column names.}
}
\value{
  A character vector of same length as \code{names} with each changed to
  a syntactically valid name.
}
\details{
  A syntactically valid name consists of letters, numbers, and the dot
  character and starts with a letter or the dot.  Names such as
  \code{".2"} are not valid, and neither are the reserved words.

  The character \code{"X"} is prepended if necessary.
  All invalid characters are translated to \code{"."}.  A missing value
  is translated to \code{"NA"}.  Names which match \R keywords have a dot
  appended to them.  Duplicated values are altered by
  \code{\link{make.unique}}.
}
\seealso{
  \code{\link{make.unique}},
  \code{\link{names}},
  \code{\link{character}},
  \code{\link{data.frame}}.
}
\examples{
make.names(c("a and b", "a_and_b"), unique=TRUE)
# "a.and.b"  "a.and.b.1"

data(state)
state.name[make.names(state.name) != state.name] # those 10 with a space
}
\keyword{character}

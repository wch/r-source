\name{make.names}
\alias{make.names}
\title{Make Syntactically Valid Names}
\description{
  Make syntactically valid names out of character vectors.
}
\usage{
make.names(names, unique = FALSE, allow_ = TRUE)
}
\arguments{
 \item{names}{character vector to be coerced to syntactically valid
   names. This is coerced to character if necessary.}
 \item{unique}{logical; if \code{TRUE}, the resulting elements are
     unique.  This may be desired for, e.g., column names.}
 \item{allow_}{logical.  For compatibility with \R prior to 1.9.0.}
}
\value{
  A character vector of same length as \code{names} with each changed to
  a syntactically valid name.
}
\details{
  A syntactically valid name consists of letters, numbers and the dot or
  underline characters and starts with a letter or the dot not followed
  by a number.  Names such as \code{".2way"} are not valid, and neither
  are the reserved words.

  The character \code{"X"} is prepended if necessary.
  All invalid characters are translated to \code{"."}.  A missing value
  is translated to \code{"NA"}.  Names which match \R keywords have a dot
  appended to them.  Duplicated values are altered by
  \code{\link{make.unique}}.
}
\note{
  Prior to \R version 1.9.0, underscores were not valid in variable names,
  and code that relies on them being converted to dots will no longer
  work. Use \code{allow_ = FALSE} for back-compatibility.

  \code{allow_ = FALSE} is also useful when creating names for export to
  applications which do not allow underline in names (for example,
  S-PLUS and some DBMSs).
}
\seealso{
  \code{\link{make.unique}},
  \code{\link{names}},
  \code{\link{character}},
  \code{\link{data.frame}}.
}
\examples{
make.names(c("a and b", "a-and-b"), unique=TRUE)
# "a.and.b"  "a.and.b.1"
make.names(c("a and b", "a_and_b"), unique=TRUE)
# "a.and.b"  "a_and_b"
make.names(c("a and b", "a_and_b"), unique=TRUE, allow_=FALSE)
# "a.and.b"  "a.and.b.1"

state.name[make.names(state.name) != state.name] # those 10 with a space
}
\keyword{character}

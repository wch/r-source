\name{parse.dcf}
\alias{parse.dcf}
\title{Parse Debian Control File Format}
\description{
  Parses text read from a file in Debian control file format, e.g., the
  \file{DESCRIPTION} or \file{CONTENTS} of a package.
}
\usage{
parse.dcf(text = NULL, file = "", fields = NULL, versionfix = FALSE)
}
\arguments{
 \item{text}{A character vector containing one line of the file in each
   element.}
 \item{file}{Name of the file to be parsed.}
 \item{fields}{A character vector of fields to return (if other fields
   occur in the file they are ignored.}
 \item{versionfix}{Logical, if \code{TRUE} than the version field is
   truncated at the first whitespace character.}
}
\value{
  If \code{fields = NULL}, a list with one element per entry is
  returned. Each entry is a list of character vectors, one per field.
  If \code{fields} are specified, then a character matrix with one row
  per entry and one column per field is returned. 
}
\seealso{\code{\link{package.contents}}, \code{\link{library}}}
\examples{
file <- system.file("CONTENTS", pkg="mva")
parse.dcf(file=file)

## same in 2 steps
filecontent <- scan(file=file, what="", sep="\n")
parse.dcf(text=filecontent)
}
\keyword{utilities}

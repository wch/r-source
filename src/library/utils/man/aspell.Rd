\name{aspell}
\alias{aspell}
\title{Aspell Interface}
\description{
  Spell check given files via Aspell.
}
\usage{
aspell(files, filter, control = list(), encoding = "unknown")
}
\arguments{
  \item{files}{a character vector with the name of files to be checked.}
  \item{filter}{an optional filter for processing the files before spell
    checking, given as either a function (with formals \code{ifile} and
    \code{encoding}), or a character string specifying a built-in
    filter, or a list with the name of a built-in filter and additional
    arguments to be passed to it.  See \bold{Details} for available
    filters.  If missing or \code{NULL}, no filtering is performed.}
  \item{control}{a list or character vector of control options for
    Aspell.}
  \item{encoding}{the encoding of the files.  Recycled as needed.}
}
\details{
  It is assumed that the Aspell executable \code{aspell} is available in
  the system search path.  See \url{http://aspell.net} for information
  on obtaining Aspell, and available dictionaries.
  
  Currently, \code{"Rd"}, corresponding to
  \code{\link[tools]{RdTextFilter}} from package \pkg{tools} is the only
  available built-in filter.
}
\value{
  A data frame inheriting from \code{aspell} (which has a useful print
  method) with the information about possibly mis-spelled words.
}
\keyword{utilities}

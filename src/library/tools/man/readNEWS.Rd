% File src/library/tools/man/readNEWS.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2011 R Core Team
% Distributed under GPL 2 or later

\name{readNEWS}
\alias{readNEWS}
\alias{checkNEWS}
\title{Read R's NEWS file or a similar one}
\description{
  Read/check \R's \file{NEWS} file or a similarly formatted one.
  This was an experimental feature added in \R 2.4.0: as from
  \R 2.12.0 the preferred format is \file{NEWS.Rd}.
}
\usage{
readNEWS(file = file.path(R.home(), "NEWS"), trace = FALSE,
         chop = c("first", "1", "par1", "keepAll"))
checkNEWS(file = file.path(R.home(), "NEWS"))
}
\arguments{
  \item{file}{the name of the file which the data are to be read from.
    Alternatively, \code{file} can be a \code{\link{connection}}, which
    will be opened if necessary, and can also be a complete URL.  For
    more details, see the \code{file} argument of \code{\link{read.table}}.}
  \item{trace}{logical indicating if the recursive reading should be
    traced, i.e., print what it is doing.}
  \item{chop}{a character string specifying how the news entries should
    be \emph{chopped}; \code{chop = "keepAll"} saves the full entries.}
}
\details{
  \code{readNEWS()} reads a pre-\R-2.12.0-style \file{NEWS} file;
  \code{checkNEWS()} checks for common errors in formatting.  Currently it
  detects an incorrect number of spaces before the \code{"o"} item marker.

  If non-ASCII characters are needed, the \file{NEWS} file may be encoded in
  UTF-8 with a byte-order mark (BOM) at the beginning, which
  \code{readNEWS()} will recognize.  Other encodings will display
  incorrectly on some systems.  However, BOMs are discouraged on many
  systems and not all editors recognize them, so \file{NEWS} files should
  normally be written in ASCII.
}

\value{
  \code{readNEWS()} returns an (S3) object of class \code{"newsTree"};
  effectively a \code{\link{list}} of lists which is a tree of NEWS
  entries.

  \code{checkNEWS()} returns \code{TRUE} if no suspected errors are found,
  or prints a message for each suspected error and returns \code{FALSE}.

  Note that this was only ever experimental and will be removed soon,
  since the NEWS-file format is not longer supported (but there are
  examples \file{NEWS.0}, \file{NEWS.1} and perhaps in packages).
}
\keyword{file}
\keyword{documentation}


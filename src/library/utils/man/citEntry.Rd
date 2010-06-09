% File src/library/utils/man/citEntry.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2009 R Core Development Team
% Distributed under GPL 2 or later

\name{citEntry}
\title{Writing Package CITATION Files}
\alias{CITATION}
\alias{readCitationFile}
\alias{citEntry}
\alias{citHeader}
\alias{citFooter}
\description{
  The \file{CITATION} file of \R packages contains an annotated list of
  references that should be used for citing the packages.
}
\usage{
readCitationFile(file, meta = NULL)
citEntry(entry, textVersion, header = NULL, footer = NULL, ...)
citHeader(...)
citFooter(...)
}
\arguments{
  \item{entry}{a character string with a BibTeX entry type.
    See section \bold{Entry Types} in \code{\link{bibentry}} for
    details.
  }
  \item{textVersion}{a character string with a text representation of
    the reference.}
  \item{header}{a character string with optional header text.}
  \item{footer}{a character string with optional footer text.}
  \item{file}{a file name.}
  \item{\dots}{arguments of the form \code{\var{tag}=\var{value}} giving
    the fields of the entry, with \var{tag} and \var{value} the name and
    value of the field, respectively.
    See section \bold{Entry Fields} in \code{\link{bibentry}} for
    details.
  }
  \item{meta}{a list of package metadata as obtained by
    \code{\link{packageDescription}}, or \code{NULL} (the default).}
}
\details{
  The \file{CITATION} file of an R package should be placed in the
  \file{inst} subdirectory of the package source.  The file is an R
  source file and may contain arbitrary R commands including
  conditionals and computations.  Function \code{readCitationFile} (used
  in turn by \code{\link{citation}}) extracts the information in
  \file{CITATION} files).  The file is \code{source()}ed by the R
  parser in a temporary environment and all resulting bibliographic
  objects (spefically, of class \code{"\link{bibentry}"}) are collected.

  Traditionally, the \file{CITATION} file contained zero or more calls
  to \code{citHeader}, then one or more calls to \code{citEntry}, and
  finally zero or more calls to \code{citFooter}, where in fact
  \code{citHeader} and \code{citFooter} are simply wrappers to
  \code{\link{paste}}, with their \code{\dots} argument passed on to
  \code{\link{paste}} as is.  \R 2.12.0 adds a new
  \code{"\link{bibentry}"} class for improved representation and
  manipulation of bibliographic information (in fact, the old mechanism
  is implemented using the new one), and one can write \file{CITATION}
  files using the unified \code{\link{bibentry}} interface.  Note
  however that such files are not usable with version of \R prior to
  2.12.0.

  \code{readCitationFile} makes use of the \code{Encoding} element (if
  any) of \code{meta} to determine the encoding of the file.
}
% <FIXME>
% \value{
%   \code{citEntry} returns an object of class \code{"citation"}, for
%   which there are \code{print} and \code{\link{toBibtex}} methods.  This
%   is a named list of entries with attributes \code{"entry"},
%   \code{"textversion"} and optionally \code{"header"} and
%   \code{"footer"}.
  
%   \code{readCitationFile} returns an object of class
%   \code{"citationList"}, also with \code{print} and
%   \code{\link{toBibtex}} methods.  This is a list of one or more
%   elements of class \code{"citation"} with optional attributes
%   \code{"header"} and \code{"footer"}.
% }
% </FIXME>
\keyword{misc}
\examples{
basecit <- system.file("CITATION", package="base")
source(basecit, echo=TRUE)
readCitationFile(basecit)
}

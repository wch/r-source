% File src/library/utils/man/citEntry.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2009 R Core Team
% Distributed under GPL 2 or later

\name{citEntry}
\title{Bibliography Entries (Older Interface)}
\alias{citEntry}
\alias{citHeader}
\alias{citFooter}
\description{
  Functionality for specifying bibliographic information in enhanced
  BibTeX style.
}
\usage{
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
  \item{\dots}{for \code{citEntry}, arguments of the form
    \code{\var{tag}=\var{value}} giving the fields of the entry, with
    \var{tag} and \var{value} the name and value of the field,
    respectively.
    See section \bold{Entry Fields} in \code{\link{bibentry}} for
    details.

    For \code{citHeader} and \code{citFooter}, character strings.
  }
}
\value{
  \code{citEntry} produces an object of class \code{"bibentry"}.
}
\seealso{
  \code{\link{citation}} for more information about citing R and R
  packages and \file{CITATION} files;
  \code{\link{bibentry}} for the newer functionality for representing
  and manipulating bibliographic information.
}
\keyword{misc}

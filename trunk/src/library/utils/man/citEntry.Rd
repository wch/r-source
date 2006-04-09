\name{citEntry}
\title{Writing Package CITATION Files}
\alias{citEntry}
\alias{CITATION}
\alias{citHeader}
\alias{citFooter}
\alias{readCitationFile}
\description{
  The \file{CITATION} file of R packages contains an annotated list of
  references that should be used for citing the packages.
}
\usage{
citEntry(entry, textVersion, header = NULL, footer = NULL, ...)
citHeader(...)
citFooter(...)
readCitationFile(file)
}
\arguments{
  \item{entry}{a character string with a BibTeX entry type}
  \item{textVersion}{a character string with a text representation of
    the reference}
  \item{header}{a character string with optional header text}
  \item{footer}{a character string with optional footer text}
  \item{file}{a file name}
  \item{\dots}{see details below}
}
\details{
  The \file{CITATION} file of an R package should be placed in the
  \file{inst} subdirectory of the package source. The file is an R
  source file and may contain arbitrary R commands including
  conditionals and computations. The file is \code{source()}ed by the R
  parser in a temporary environment and all resulting objects of class
  \code{"citation"} (the return value of \code{citEntry}) are collected.

  Typically the file will contain zero or more calls to \code{citHeader},
  then one or more calls to \code{citEntry}, and  finally zero or more
  calls to  \code{citFooter}. \code{citHeader} and \code{citFooter} are
  simply wrappers to \code{\link{paste}}, and their \code{\dots} argument
  is passed on to \code{\link{paste}} as is.
}
\section{Entry Types}{
  \code{citEntry} creates \code{"citation"} objects, which are modeled
  after BibTeX entries. The entry should be a valid BibTeX entry type,
  e.g.,
  \describe{
    \item{article:}{An article from a journal or magazine.}
    \item{book:}{A book with an explicit publisher.}
    \item{inbook:}{A part of a book, which may be a chapter (or section
      or whatever) and/or a range of pages.}  
    \item{incollection:}{A part of a book having its own title.}
    \item{inproceedings:}{An article in a conference proceedings.}
    \item{manual:}{Technical documentation like a software manual.}
    \item{mastersthesis:}{A Master's thesis.}
    \item{misc:}{Use this type when nothing else fits.}
    \item{phdthesis:}{A PhD thesis.}
    \item{proceedings:}{The proceedings of a conference.}
    \item{techreport:}{A report published by a school or other
      institution, usually numbered within a series.}
    \item{unpublished:}{A document having an author and title, but not
      formally published.}
  }
}
\section{Entry Fields}{
  The \code{\dots} argument of \code{citEntry} can be any number of
  BibTeX fields, including
  \describe{
    \item{address:}{The address of the publisher or other type of
      institution.}

    \item{author:}{The name(s) of the author(s), either 
      as a character string in the format described in the LaTeX book,
      or a \code{\link{personList}} object.}

    \item{booktitle:}{Title of a book, part of which is being cited.}
    \item{chapter:}{A chapter (or section or whatever) number.}
    
    \item{editor:}{Name(s) of editor(s), same format as \code{author}.}

    \item{institution:}{The publishing institution of a technical report.}

    \item{journal:}{A journal name.}

    \item{note:}{Any additional information that can help the reader.
      The first word should be capitalized.}

    \item{number:}{The number of a journal, magazine, technical report,
      or of a work in a series.}

    \item{pages:}{One or more page numbers or range of numbers.}
    
    \item{publisher:}{The publisher's name.}
    
    \item{school:}{The name of the school where a thesis was written.}
    
    \item{series:}{The name of a series or set of books.}
    
    \item{title:}{The work's title.}
    
    \item{volume:}{The volume of a journal or multi-volume book.}
    
    \item{year:}{The year of publication.}
  }
}
\value{
  \code{citEntry} returns an object of class \code{"citation"},
  \code{readCitationFile} returns an object of class \code{"citationList"}.
}
\keyword{misc}
\examples{
basecit <- system.file("CITATION", package="base")
source(basecit, echo=TRUE)
readCitationFile(basecit)
}

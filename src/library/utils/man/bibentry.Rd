\name{bibentry}
\alias{bibentry}
\alias{print.bibentry}
\title{Bibliography Entries}
\description{
  Functionality for representing and manipulating bibliographic
  information in enhanced BibTeX style.
}
\usage{
bibentry(bibtype, textVersion, header = NULL, footer = NULL, key = NULL,
         ..., other = list(), mheader = NULL, mfooter = NULL)
\method{print}{bibentry}(x, style = c("text", "Bibtex", "citation"), ...)
}
\arguments{
  \item{bibtype}{a character string with a BibTeX entry type.
    See \bold{Entry Types} for details.
  }
  \item{textVersion}{a character string with a text representation of
    the reference.}
  \item{header}{a character string with optional header text.}
  \item{footer}{a character string with optional footer text.}
  \item{key}{a character string giving the citation key for the entry.}
  \item{...}{arguments of the form \code{\var{tag}=\var{value}} giving
    the fields of the entry, with \var{tag} and \var{value} the name and
    value of the field, respectively.
    See \bold{Entry Fields} for details.}
  \item{other}{a list of arguments as in \code{...} (useful in
    particular for fields named the same as formals of
    \code{bibentry}).}
  \item{mheader}{a character string with optional \dQuote{outer} header
    text.}
  \item{mfooter}{a character string with optional \dQuote{outer} footer
    text.}
  \item{x}{an object inheriting from class \code{"bibentry"}.}
  \item{style}{a character string with a unique abbreviation of
    \code{"text"}, \code{"Bibtex"} or \code{"citation"} specifying the
    print style.
  }
}
\details{
  The bibentry objects created by \code{bibentry} can represent an
  arbitrary positive number of references.  One can use \code{c()} to
  combine bibentry objects, and hence in particular build a multiple
  reference object from single reference ones.  Alternatively, one can
  use \code{bibentry} to directly create a multiple reference object by
  \dQuote{vectorizing} the given arguments, i.e., use character vectors
  instead of character strings.

  The \code{\link{print}} method for bibentry objects provides a choice
  between three different styles: plain text based on the
  \code{textVersion} elements (style \code{"text"}), BibTeX
  (\code{"Bibtex"}) and a mixture of plain text and BibTeX as
  traditionally used for citations (\code{"citation"}).  There is also a
  \code{\link{toBibtex}} method for direct conversion to BibTeX.
}
\value{
  An object of class \code{"bibentry"}.
}
\section{Entry Types}{
  \code{bibentry} creates \code{"bibentry"} objects, which are modeled
  after BibTeX entries.  The entry should be a valid BibTeX entry type,
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
      or a \code{\link{person}} object.}

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
\note{
  The bibentry functionality is still experimental.
}

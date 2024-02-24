% File src/library/utils/man/bibentry.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2023 R Core Team
% Distributed under GPL 2 or later

\name{bibentry}
\title{Bibliography Entries}
\alias{bibentry}
\alias{print.bibentry}
\alias{format.bibentry}
\alias{sort.bibentry}
\alias{print.citation}
\alias{format.citation}
\alias{toBibtex.bibentry}
\description{
  Functionality for representing and manipulating bibliographic
  information in enhanced BibTeX style.
}
\usage{
bibentry(bibtype, textVersion = NULL, header = NULL, footer = NULL,
         key = NULL, \dots, other = list(),
         mheader = NULL, mfooter = NULL)

\method{print}{bibentry}(x, style = "text", .bibstyle,
      bibtex = length(x) <= getOption("citation.bibtex.max", 1),
      \dots)

\method{format}{bibentry}(x, style = "text", .bibstyle = NULL,
       bibtex = length(x) <= 1,
       citMsg = missing(bibtex),
       sort = FALSE, macros = NULL, \dots)

\method{sort}{bibentry}(x, decreasing = FALSE, .bibstyle = NULL, drop = FALSE, \dots)

\method{print}{citation} (x, style = "citation", \dots)
\method{format}{citation}(x, style = "citation", \dots)

\method{toBibtex}{bibentry}(object, escape = FALSE, ...)
}
\arguments{
  \item{bibtype}{a character string with a BibTeX entry type.
    See \bold{Entry Types} for details.
  }
  \item{textVersion}{a character string with a text representation of
    the reference to optionally be employed for printing.  It is
    recommended to leave this unspecified if
    \code{format(x, style = "text")}
    works correctly.  Only if special LaTeX macros (e.g., math
    formatting) or special characters (e.g., with accents) are
    necessary, a \code{textVersion} should be provided.
  }
  \item{header}{a character string with optional header text.}
  \item{footer}{a character string with optional footer text.}
  \item{key}{a character string giving the citation key for the entry.}
  \item{\dots}{for \code{bibentry}: arguments of the form
    \code{\var{tag}=\var{value}} giving the fields of the entry, with
    \var{tag} and \var{value} the name and value of the field,
    respectively.  Arguments with empty values are dropped.
    Field names are case-insensitive.
    See \bold{Entry Fields} for details.

    For the \code{print()} method, extra arguments to pass to the
    renderer which typically includes the \code{format()} method.

    For the \code{citation} class methods, arguments passed to the next
    method, i.e., the corresponding \code{bibentry} one.

    For the \code{toBibtex()} method, currently not used.
  }
  \item{other}{a list of arguments as in \code{\dots} (useful in
    particular for fields named the same as formals of
    \code{bibentry}).}
  \item{mheader}{a character string with optional \dQuote{outer} header
    text.}
  \item{mfooter}{a character string with optional \dQuote{outer} footer
    text.}
  \item{x}{an object inheriting from class \code{"bibentry"}.}
  \item{style}{an optional character string specifying the print style.
    If present, must be a unique abbreviation (with case ignored) of the available
    styles, see \bold{Details}.}
  \item{decreasing}{logical, passed to \code{\link{order}} indicating
    the sort direction.}
  \item{.bibstyle}{a character string naming a bibliography style,
    see \code{\link{bibstyle}}.}
  \item{bibtex}{\code{\link{logical}} indicating if BibTeX code should be given
    additionally; currently applies only to \code{style = "citation"}.
    The default for the \code{print()} method depends on the number of
    (bib) entries and \code{\link{getOption}("citation.bibtex.max")} (which
    itself is 1 by default).  For example, to see no BibTeX at all, you can change the
    default by \code{\link{options}(citation.bibtex.max = 0)}.}
  \item{citMsg}{\code{\link{logical}} indicating if a \dQuote{message} should be
    added (to the footer) about how to get BibTeX code when \code{bibtex}
    is false \emph{and} \code{style = "citation"}.}
  \item{sort}{logical indicating if \I{bibentries} should be sorted, using
    \code{\link[tools]{bibstyle}(.bibstyle)$sortKeys(x)}.}
  \item{macros}{a character string or an object with already loaded Rd
    macros, see \bold{Details}.}
  \item{drop}{logical used as \code{x[ ..., drop=drop]} inside the
    \code{sort()} method.}
  \item{object}{an object inheriting from class \code{"bibentry"}.}
  \item{escape}{a logical indicating whether non-ASCII characters should
    be translated to LaTeX escape sequences.}
}
\details{
  The \I{bibentry} objects created by \code{bibentry} can represent an
  arbitrary positive number of references.  One can use \code{c()} to
  combine \I{bibentry} objects, and hence in particular build a multiple
  reference object from single reference ones.  Alternatively, one can
  use \code{bibentry} to directly create a multiple reference object by
  specifying the arguments as lists of character strings.

  The \code{\link{print}} method for \I{bibentry} objects is based on a
  corresponding \code{\link{format}} method and provides a choice
  between seven different styles:
  plain text (style \code{"text"}),
  BibTeX (\code{"bibtex"}),
  a mixture of plain text and BibTeX as traditionally used for citations
  (\code{"citation"}),
  HTML (\code{"html"}),
  \LaTeX (\code{"latex"}),
  R code (\code{"R"}),
  and a simple copy of the \code{textVersion} elements (style
  \code{"textVersion"}).

  The \code{"text"}, \code{"html"} and \code{"latex"} styles make use
  of the \code{.bibstyle} argument: a style defined by the
  \code{\link{bibstyle}} function for rendering the \I{bibentry} into
  (intermediate) Rd format.
  The Rd format uses markup commands documented in the \sQuote{Rd format}
  section of the \sQuote{Writing R Extensions} manual, e.g. \verb{\bold}.
  In addition, one can use the \code{macros} argument to
  provide additional (otherwise unknown, presumably LaTeX-style) Rd
  macros, either by giving the path to a file with Rd macros to be
  loaded via \code{\link[tools]{loadRdMacros}}, or an object with macros
  already loaded.
  Note that the \code{"latex"} result may contain commands from the
  \LaTeX style file \file{Rd.sty} shipped with \R;
  put \verb{\usepackage{Rd}} in the preamble of a LaTeX document
  to make these available when compiling,
  e.g. with \code{\link{texi2pdf}}.

  When printing \I{bibentry} objects in citation style, a
  \code{header}/\code{footer} for each item can be displayed as well as
  a \code{mheader}/\code{mfooter} for the whole vector of references.

  For formatting as R code,
  a choice between giving a character vector with one \code{bibentry()}
  call for each \I{bibentry} (as commonly used in \file{CITATION} files), or
  a character string with one collapsed call, obtained by combining the
  individual calls with \code{c()} if there is more than one \I{bibentry}.
  This can be controlled by passing the argument \code{collapse=FALSE}
  (default) or \code{TRUE}, respectively, to the \code{format()} method.
  (\emph{Printing} in R style always collapses to a single call.)

  It is possible to subscript \I{bibentry} objects by their keys (which are
  used for character subscripts if the names are \code{NULL}).

  There is also a \code{\link{toBibtex}} method for direct conversion to
  BibTeX.

  As of \R 4.3.0, there is also a \code{\link{transform}} method which
  allows to directly use the current fields, see the examples.
}
\value{
  \code{bibentry} produces an object of class \code{"bibentry"}.
}
\section{Entry Types}{
  \code{bibentry} creates \code{"bibentry"} objects, which are modeled
  after BibTeX entries.  The entry should be a valid BibTeX entry type,
  e.g.,
  \describe{
    \item{Article:}{An article from a journal or magazine.}
    \item{Book:}{A book with an explicit publisher.}
    \item{InBook:}{A part of a book, which may be a chapter (or section
      or whatever) and/or a range of pages.}
    \item{InCollection:}{A part of a book having its own title.}
    \item{InProceedings:}{An article in a conference proceedings.}
    \item{Manual:}{Technical documentation like a software manual.}
    \item{MastersThesis:}{A Master's thesis.}
    \item{Misc:}{Use this type when nothing else fits.}
    \item{PhdThesis:}{A PhD thesis.}
    \item{Proceedings:}{The proceedings of a conference.}
    \item{TechReport:}{A report published by a school or other
      institution, usually numbered within a series.}
    \item{Unpublished:}{A document having an author and title, but not
      formally published.}
  }
}
\section{Entry Fields}{
  The \code{\dots} argument of \code{bibentry} can be any number of
  BibTeX fields, including
  \describe{
    \item{address:}{The address of the publisher or other type of
      institution.}

    \item{author:}{The name(s) of the author(s), either as a
      \code{\link{person}} object, or as a character string which
      \code{\link{as.person}} correctly coerces to such.}

    \item{booktitle:}{Title of a book, part of which is being cited.}
    \item{chapter:}{A chapter (or section or whatever) number.}

    \item{doi:}{The \abbr{DOI}
      (\url{https://en.wikipedia.org/wiki/Digital_Object_Identifier})
      for the reference.}

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

    \item{url:}{A URL for the reference.
      (If the URL is an expanded \abbr{DOI}, we recommend to use the
      \samp{doi} field with the unexpanded \abbr{DOI} instead.)}

    \item{volume:}{The volume of a journal or multi-volume book.}

    \item{year:}{The year of publication.}
  }
}
\seealso{
  \code{\link{person}}
}
\examples{
## R reference
rref <- bibentry(
   bibtype = "Manual",
   title = "R: A Language and Environment for Statistical Computing",
   author = person("R Core Team"),
   organization = "R Foundation for Statistical Computing",
   address = "Vienna, Austria",
   year = 2014,
   url = "https://www.R-project.org/")

## Different printing styles
print(rref)
print(rref, style = "bibtex")
print(rref, style = "citation")
print(rref, style = "html")
print(rref, style = "latex")
print(rref, style = "R")

## References for boot package and associated book
bref <- c(
   bibentry(
     bibtype = "Manual",
     title = "boot: Bootstrap R (S-PLUS) Functions",
     author = c(
       person("Angelo", "Canty", role = "aut",
         comment = "S original"),
       person(c("Brian", "D."), "Ripley", role = c("aut", "trl", "cre"),
         comment = "R port, author of parallel support",
         email = "ripley@stats.ox.ac.uk")
     ),
     year = "2012",
     note = "R package version 1.3-4",
     url = "https://CRAN.R-project.org/package=boot",
     key = "boot-package"
   ),

   bibentry(
     bibtype = "Book",
     title = "Bootstrap Methods and Their Applications",
     author = as.person("Anthony C. Davison [aut], David V. Hinkley [aut]"),
     year = "1997",
     publisher = "Cambridge University Press",
     address = "Cambridge",
     isbn = "0-521-57391-2",
     url = "http://statwww.epfl.ch/davison/BMA/",
     key = "boot-book"
   )
)

## Combining and subsetting
c(rref, bref)
bref[2]
bref["boot-book"]

## Extracting fields
bref$author
bref[1]$author
bref[1]$author[2]$email

## Field names are case-insensitive
rref$Year
rref$Year <- R.version$year
stopifnot(identical(rref$year, R.version$year))

## Convert to BibTeX
toBibtex(bref)

## Transform
transform(rref, address = paste0(address, ", Europe"))

## BibTeX reminder message (in case of >= 2 refs):
print(bref, style = "citation")

## Format in R style
## One bibentry() call for each bibentry:
writeLines(paste(format(bref, "R"), collapse = "\n\n"))
## One collapsed call:
writeLines(format(bref, "R", collapse = TRUE))
}
\keyword{ utilities }
\keyword{ documentation }

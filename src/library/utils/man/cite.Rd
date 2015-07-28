% File src/library/utils/man/cite.Rd
% Part of the R package, http://www.R-project.org
% Copyright 2012-2014 R Core Team
% Distributed under GPL 2 or later

\name{cite}
\alias{cite}
\alias{citeNatbib}
\title{
Cite a bibliography entry.
}
\description{
Cite a \code{bibentry} object in text.  The \code{cite()} function
uses the \code{cite()} function from the default
\code{\link{bibstyle}} if present, or \code{citeNatbib()} if not.
\code{citeNatbib()} uses a style similar to that used by the LaTeX
package \pkg{natbib}.
}
\usage{
cite(keys, bib, ...)
citeNatbib(keys, bib, textual = FALSE, before = NULL, after = NULL,
           mode = c("authoryear", "numbers", "super"),
           abbreviate = TRUE, longnamesfirst = TRUE,
           bibpunct = c("(", ")", ";", "a", "", ","), previous)
}
\arguments{
  \item{keys}{
A character vector of keys of entries to cite.  May contain multiple keys in
a single entry, separated by commas.
}
  \item{bib}{
A \code{"\link{bibentry}"} object containing the list of documents in which
to find the keys.
}
  \item{...}{
Additional arguments to pass to the \code{cite()} function for the
default style.
}
  \item{textual}{
Produce a \dQuote{textual} style of citation, i.e.\sspace{}what \code{\\citet} would
produce in LaTeX.
}
  \item{before}{
Optional text to display before the citation.
}
  \item{after}{
Optional text to display after the citation.
}
  \item{mode}{
The \dQuote{mode} of citation.
}
  \item{abbreviate}{
Whether to abbreviate long author lists.
}
  \item{longnamesfirst}{
If \code{abbreviate == TRUE}, whether to leave the first citation long.
}
  \item{bibpunct}{
A vector of punctuation to use in the citation, as used in \pkg{natbib}.  See
the Details section.
}
  \item{previous}{
A list of keys that have been previously cited, to be used when
\code{abbreviate == TRUE} and \code{longnamesfirst == TRUE}
}
}
\details{
Argument names are chosen based on the documentation for the LaTeX \pkg{natbib}
package.  See that documentation for the interpretation of the
\code{bibpunct} entries.

The entries in \code{bibpunct} are as follows:
\enumerate{
\item The left delimiter.
\item The right delimiter.
\item The separator between references within a citation.
\item An indicator of the \dQuote{mode}:  \code{"n"} for numbers,
\code{"s"} for superscripts, anything else for author-year.
\item Punctuation to go between the author and year.
\item Punctuation to go between years when authorship is suppressed.
}
Note that if \code{mode} is specified, it overrides the
mode specification in \code{bibpunct[4]}.  Partial matching is used for
\code{mode}.

The defaults for \code{citeNatbib} have been chosen to match the JSS style, and
by default these are used in \code{cite}.  See \code{\link{bibstyle}}
for how to set a different default style.
}
\value{
A single element character string is returned, containing the citation.
}
\author{
Duncan Murdoch
}
\examples{
## R reference
rref <- bibentry(
   bibtype = "Manual",
   title = "R: A Language and Environment for Statistical Computing",
   author = person("R Core Team"),
   organization = "R Foundation for Statistical Computing",
   address = "Vienna, Austria",
   year = 2013,
   url = "http://www.R-project.org/",
   key = "R")

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
     url = "http://CRAN.R-project.org/package=boot",
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

## Combine and cite
refs <- c(rref, bref)
cite("R, boot-package", refs)

## Cite numerically
savestyle <- tools::getBibstyle()
tools::bibstyle("JSSnumbered", .init = TRUE,
         fmtPrefix = function(paper) paste0("[", paper$.index, "]"),
         cite = function(key, bib, ...)
         	citeNatbib(key, bib, mode = "numbers",
         	    bibpunct = c("[", "]", ";", "n", "", ","), ...)
         )
cite("R, boot-package", refs, textual = TRUE)
refs

## restore the old style
tools::bibstyle(savestyle, .default = TRUE)
}
\keyword{ utilities }
\keyword{ documentation }

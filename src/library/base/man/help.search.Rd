\name{help.search}
\alias{help.search}
\title{Search the Help System}
\usage{
help.search(what, fields = c("name", "title"),
            apropos, keyword, whatis, ignore.case = TRUE,
            packages = NULL, lib.loc = .lib.loc, verbose = .Options$verbose)
}
\arguments{
  \item{what}{a character string containing a regular expression to be
    matched in the specified fields.  If this is given, the arguments
    \code{apropos}, \code{keyword}, and \code{whatis} are ignored.}
  \item{fields}{a character vector specifying the fields of the help
    data bases to be searched.  The entries must be abbreviations of
    \code{"name"}, \code{"alias"}, \code{"title"}, and \code{"keyword"},
    corresponding to the help page's (file) name, the objects it
    provides documentation for, its title, and the keywords it can be
    classified to.}
  \item{apropos}{a character string containing a regular expression to
    be matched in the help page names and titles.}
  \item{keyword}{a character string containing a regular expression to
    be matched in the help page keywords.}
  \item{whatis}{a character string containing a regular expression to be
    matched in the help page names.}
  \item{ignore.case}{a logical.  If \code{TRUE}, case is ignored during
    matching; if \code{FALSE}, pattern matching is case sensitive.}
  \item{packages}{a character vector with the names of packages to search
    through, or \code{NULL} in which case \emph{all} available packages
    in the specified library trees \code{lib.loc} are searched.}
  \item{lib.loc}{a character vector describing the location of \R
    library trees to search through.}
  \item{verbose}{logical; if \code{TRUE}, the search process is traced.}
}
\description{
  Allows for searching the help system for documentation matching a
  given regular expression in its name, alias, title, or keyword entries
  (or any combination thereof).  Names and titles of the matched help
  entries are nicely displayed; currently, nothing is returned.
}
\details{
  Upon installation of a package, the Perl script \code{Rd2contents}
  creates a \file{CONTENTS} data base which contains the information on
  name, aliases, title and keywords (as well as the URL of the HTML
  version of the help file) in Debian Control Format.  This is the data
  base searched by \code{help.search()}.

  The arguments \code{apropos} and \code{whatis} play a role similar to
  the Unix commands with the same names.

  Note that currently, the aliases in the matching help files are not
  displayed.
}
\seealso{
  \code{\link{help}};
  \code{\link{help.start}} for starting the hypertext (currently HTML)
  version of \R's online documentation, which offers a similar search
  mechanism.

  \code{\link{apropos}} uses regexps and has nice examples.
}
\examples{
help.search("linear models")    # In case you forgot how to fit linear
                                # models
\dontrun{
help.search("print")            # All help pages with name or title
                                # matching `print'
help.search(apropos = "print")  # The same
help.search(keyword = "hplot")  # All help pages documenting high-level
                                # plots.
}
}
\keyword{documentation}

\name{help.search}
\alias{help.search}
\alias{print.hsearch}
\title{Search the Help System}
\description{
  Allows for searching the help system for documentation matching a
  given character string in the (file) name, alias, title, concept or
  keyword entries (or any combination thereof), using either
  \link{fuzzy matching} or \link{regular expression} matching.  Names
  and titles of the matched help entries are displayed nicely.
}
\usage{
help.search(pattern, fields = c("alias", "concept", "title"),
            apropos, keyword, whatis, ignore.case = TRUE,
            package = NULL, lib.loc = NULL,
            help.db = getOption("help.db"),
            verbose = getOption("verbose"),
            rebuild = FALSE, agrep = NULL)
}
\arguments{
  \item{pattern}{a character string to be matched in the specified
    fields.  If this is given, the arguments \code{apropos},
    \code{keyword}, and \code{whatis} are ignored.}
  \item{fields}{a character vector specifying the fields of the help
    data bases to be searched.  The entries must be abbreviations of
    \code{"name"}, \code{"title"}, \code{"alias"}, \code{"concept"}, and
    \code{"keyword"}, corresponding to the help page's (file) name, its
    title, the topics and concepts it provides documentation for, and
    the keywords it can be classified to.}
  \item{apropos}{a character string to be matched in the help page
    topics and title.}
  \item{keyword}{a character string to be matched in the help page
    \sQuote{keywords}. \sQuote{Keywords} are really categories: the
    standard categories are listed in file \file{RHOME/doc/KEYWORDS}
    (see also the example) and some package writers have defined their
    own.  If \code{keyword} is specified, \code{agrep} defaults to
    \code{FALSE}.}  \item{whatis}{a character string to be matched in
    the help page topics.}
  \item{ignore.case}{a logical.  If \code{TRUE}, case is ignored during
    matching; if \code{FALSE}, pattern matching is case sensitive.}
  \item{package}{a character vector with the names of packages to
    search through, or \code{NULL} in which case \emph{all} available
    packages in the library trees specified by \code{lib.loc} are
    searched.}
  \item{lib.loc}{a character vector describing the location of \R
    library trees to search through, or \code{NULL}.  The default value
    of \code{NULL} corresponds to all libraries currently known.}
  \item{help.db}{a character string giving the file path to a previously
    built and saved help data base, or \code{NULL}.}
  \item{verbose}{logical; if \code{TRUE}, the search process is traced.}
  \item{rebuild}{a logical indicating whether the help data base should
    be rebuilt.}
  \item{agrep}{if \code{NULL} (the default unless \code{keyword} is
    used) and the character string to
    be matched consists of alphanumeric characters, whitespace or a dash
    only, approximate (fuzzy) matching via \code{\link{agrep}} is used
    unless the string has fewer than 5 characters; otherwise, it is
    taken to contain a \link{regular expression} to be matched via
    \code{\link{grep}}.  If \code{FALSE}, approximate matching is not
    used.  Otherwise, one can give a numeric or a list specifying the
    maximal distance for the approximate match, see argument
    \code{max.distance} in the documentation for \code{\link{agrep}}.}
}
\details{
  Upon installation of a package, a contents data base which contains
  the information on name, title, aliases and keywords and, concepts
  starting with \R 1.8.0, is computed from the Rd files in the package and
  serialized as \file{Rd.rds} in the \file{Meta} subdirectory of the
  top-level package installation directory (or, prior to \R 1.7.0, as
  \file{CONTENTS} in Debian Control Format with aliases and keywords
  collapsed to character strings in the top-level package installation
  directory).  This, or a pre-built help.search index serialized as
  \file{hsearch.rds} in the \file{Meta} directory, is the data base
  searched by \code{help.search()}.

  The arguments \code{apropos} and \code{whatis} play a role similar to
  the Unix commands with the same names.

  If possible, the help data base is saved to the file \file{help.db} in
  the \file{.R} subdirectory of the user's home directory or the current
  working directory.

  Note that currently, the aliases in the matching help files are not
  displayed.
}
\value{
  The results are returned in an object of class \code{"hsearch"}, which
  has a print method for nicely displaying the results of the query.
  This mechanism is experimental, and may change in future versions of
  R.
}
\seealso{
  \code{\link{help}};
  \code{\link{help.start}} for starting the hypertext (currently HTML)
  version of \R's online documentation, which offers a similar search
  mechanism.

  \code{\link{RSiteSearch}} to access an on-line search of \R resources.
  
  \code{\link{apropos}} uses regexps and has nice examples.
}
\examples{
help.search("linear models")    # In case you forgot how to fit linear
                                # models
help.search("non-existent topic")
\dontrun{
help.search("print")            # All help pages with topics or title
                                # matching 'print'
help.search(apropos = "print")  # The same

help.search(keyword = "hplot")  # All help pages documenting high-level
                                # plots.
file.show(file.path(R.home(), "doc", "KEYWORDS"))  # show all keywords

## Help pages with documented topics starting with 'try'.
help.search("\\\\btry", fields = "alias")
## Do not use '^' or '$' when matching aliases or keywords
## (unless all packages were installed using R 1.7 or newer).
}
}
\keyword{documentation}

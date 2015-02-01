% File src/library/utils/man/help.search.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2014 R Core Team
% Distributed under GPL 2 or later

\name{help.search}
\alias{help.search}
\alias{??}
\alias{print.hsearch}
\title{Search the Help System}
\description{
  Allows for searching the help system for documentation matching a
  given character string in the (file) name, alias, title, concept or
  keyword entries (or any combination thereof), using either
  \link{fuzzy matching} or \link{regular expression} matching.  Names
  and titles of the matched help entries are displayed nicely formatted.

  Vignette names, titles and keywords and demo names and titles
  may also be searched.
}
\usage{
help.search(pattern, fields = c("alias", "concept", "title"),
            apropos, keyword, whatis, ignore.case = TRUE,
            package = NULL, lib.loc = NULL,
            help.db = getOption("help.db"),
            verbose = getOption("verbose"),
            rebuild = FALSE, agrep = NULL, use_UTF8 = FALSE,
            types = getOption("help.search.types"))
\special{??pattern}
\special{field??pattern}
}
\arguments{
  \item{pattern}{a character string to be matched in the specified
    fields.  If this is given, the arguments \code{apropos},
    \code{keyword}, and \code{whatis} are ignored.}
  \item{fields}{a character vector specifying the fields of the help
    database to be searched.  The entries must be abbreviations of
    \code{"name"}, \code{"title"}, \code{"alias"}, \code{"concept"}, and
    \code{"keyword"}, corresponding to the help page's (file) name, its
    title, the topics and concepts it provides documentation for, and
    the keywords it can be classified to.  See below for how vignettes
    and demos are searched.}
  \item{apropos}{a character string to be matched in the help page
    topics and title.}
  \item{keyword}{a character string to be matched in the help page
    \sQuote{keywords}. \sQuote{Keywords} are really categories: the
    standard categories are listed in file \file{R.home("doc")/KEYWORDS}
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
    built and saved help database, or \code{NULL}.}
  \item{verbose}{logical; if \code{TRUE}, the search process is traced.
    Integer values are also accepted, with \code{TRUE} being equivalent
    to \code{2}, and \code{1} being less verbose.  On Windows a progress
    bar is shown during rebuilding, and on Unix a heartbeat is shown for
    \code{verbose = 1} and a package-by-package list for
    \code{verbose >= 2}.}
  \item{rebuild}{a logical indicating whether the help database should
    be rebuilt.  This will be done automatically if \code{lib.loc} or
    the search path is changed, or if \code{package} is used and a value
    is not found.}
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
  \item{use_UTF8}{logical: should be results be given in UTF-8 encoding?
    Also changes the meaning of regexps in \code{agrep} to be Perl regexps.}
  \item{types}{a character vector listing the types of documentation
    to search.  The entries must be abbreviations of \code{"vignette"}
    \code{"help"} or \code{"demo"}. Results will be presented in the
    order specified.}
  \item{field}{a single value of \code{fields} to search.}
}
\details{
  Upon installation of a package, a pre-built help.search index is
  serialized as \file{hsearch.rds} in the \file{Meta} directory
  (provided the package has any help pages).  Vignettes are also
  indexed in the \file{Meta/vignette.rds} file.  These files are used to
  create the help search database.

  The arguments \code{apropos} and \code{whatis} play a role similar to
  the Unix commands with the same names.

  Searching with \code{agrep = FALSE} will be several times faster than
  the default (once the database is built).  However, approximate
  searches should be fast enough (around a second with 5000 packages
  installed).

  If possible, the help database is saved in memory for use by
  subsequent calls in the session.

  Note that currently the aliases in the matching help files are not
  displayed.

  As with \code{\link{?}}, in \code{??} the pattern may be prefixed with a
  package name followed by \code{::} or \code{:::} to limit the search
  to that package.

  Vignettes are searched as follows.  The \code{"name"} and
  \code{"alias"} are both the base of the vignette filename, and the
  \code{"concept"} entries are taken from the \code{\\VignetteKeyword}
  entries.  Vignettes are not classified using the help system
  \code{"keyword"} classifications.  Demos are handled
  similarly to vignettes, without the \code{"concept"} search.
}

\value{
  The results are returned in a list object of class \code{"hsearch"},
  which has a print method for nicely formatting the results of the
  query.  This mechanism is experimental, and may change in future
  versions of \R.

  In \code{R.app} on OS X, this will show up a browser with selectable
  items. On exiting this browser, the help pages for the selected items
  will be shown in separate help windows.

  The internal format of the class is undocumented and subject to change.
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

??utils::help  # All the topics matching "help" in the utils package

\donttest{
help.search("print")            # All help pages with topics or title
                                # matching 'print'
help.search(apropos = "print")  # The same

help.search(keyword = "hplot")  # All help pages documenting high-level
                                # plots.
file.show(file.path(R.home("doc"), "KEYWORDS"))  # show all keywords

## Help pages with documented topics starting with 'try'.
help.search("\\\\btry", fields = "alias")
}}
\keyword{documentation}

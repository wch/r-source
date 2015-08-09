% File src/library/utils/man/RSiteSearch.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2010 R Core Team
% Distributed under GPL 2 or later

\name{RSiteSearch}
\alias{RSiteSearch}
\title{
  Search for Key Words or Phrases in Documentation
}
\description{
  Search for key words or phrases in
  help pages, vignettes or task views, using the search engine
  at \url{http://search.r-project.org} and view them in a web browser.
}
\usage{
RSiteSearch(string,
            restrict = c("functions", "vignettes", "views"),
            format = c("normal", "short"),
            sortby = c("score", "date:late", "date:early",
                       "subject", "subject:descending",
                       "from", "from:descending",
                       "size", "size:descending"),
            matchesPerPage = 20)
}
\arguments{
  \item{string}{A character string specifying word(s) or a phrase to
    search.  If the words are to be searched as one entity, enclose all
    words in braces (see the first example).}
  \item{restrict}{a character vector, typically of length greater than one.
    Values can be abbreviated.
    Possible areas to search in:
    \code{functions} for help pages,
    \code{views} for task views and
    \code{vignettes} for package vignettes.}
  \item{format}{\code{normal} or \code{short} (no excerpts); can be
    abbreviated.}
  \item{sortby}{character string (can be abbreviated) indicating how to
    sort the search results:\cr
    (\code{score},
    \code{date:late} for sorting by date with latest results first,
    \code{date:early} for earliest first,
    \code{subject} for subject in alphabetical order,
    \code{subject:descending} for reverse alphabetical order,
    \code{from} or \code{from:descending} for sender (when applicable),
    \code{size} or \code{size:descending} for size.)}
  \item{matchesPerPage}{How many items to show per page.}
}
\details{
  This function is designed to work with the search site at
  \url{http://search.r-project.org}, and depends on that site
  continuing to be made available (thanks to Jonathan Baron and the
  School of Arts and Sciences of the University of Pennsylvania).

  Unique partial matches will work for all arguments.  Each new
  browser window will stay open unless you close it.
}
\value{
  (Invisibly) the complete URL passed to the browser,
  including the query string.
}
\author{Andy Liaw and Jonathan Baron}
\seealso{
  \code{\link{help.search}}, \code{\link{help.start}} for local searches.

  \code{\link{browseURL}} for how the help file is displayed.
}
\examples{\donttest{# need Internet connection
RSiteSearch("{logistic regression}") # matches exact phrase
Sys.sleep(5) # allow browser to open, take a quick look
## Search in vignettes and store the query-string:
fullquery <- RSiteSearch("lattice", restrict = "vignettes")
fullquery # a string of ~ 110 characters
%% FIXME: "/bug/ and other reg.exp.s seem to fail
}}
\keyword{utilities}
\keyword{documentation}

% File src/library/utils/man/RSiteSearch.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Development Team
% Distributed under GPL 2 or later

\name{RSiteSearch}
\alias{RSiteSearch}
\title{
  Search for Key Words or Phrases in the \R Documentation
}
\description{
  Search for key words or phrases in the help pages, using the
  search engine at \url{http://search.r-project.org} and view them in a
  web browser.
}
\usage{
RSiteSearch(string,
	    restrict = NULL,
            format = c("normal", "short"),
            
            sortby = c("score", "date:late", "date:early",
                       "subject", "subject:descending",
                       "from", "from:descending",
                       "size", "size:descending"),
            matchesPerPage = 20)
}
\arguments{
  \item{string}{word(s) or phrase to search.  If the words are to be
    searched as one entity, enclose all words in braces (see example).}
  \item{restrict}{not currently used; provided for back compatibility.}
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
\value{
  (Invisibly) the complete URL passed to the browser,
  including the query string.
}
\details{
  This function is designed to work with the search site at
  \url{http://search.r-project.org}, and depends on that site
  continuing to be made available (thanks to Jonathan Baron and the
  School of Arts and Sciences of the University of Pennslyvania).

  Unique partial matches will work for all arguments.  Each new
  browser window will stay open unless you close it.
  
  Since R 2.9.0, mailing lists are not searched.  They may be searched
  at several other sites, including \url{http://tolstoy.newcastle.edu.au/R/}, 
  and \url{http://markmail.org/search/list:r-project}.  See 
  \url{http://search.r-project.org} for an updated list.
  
}
\author{Andy Liaw and Jonathan Baron}
\seealso{
  \code{\link{help.search}}, \code{\link{help.start}} for local searches.

  \code{\link{browseURL}} for how the help file is displayed.
}
\examples{\donttest{ # need Internet connection
RSiteSearch("{logistic regression}") # matches exact phrase
}}
\keyword{utilities}
\keyword{documentation}

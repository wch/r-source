\name{RSiteSearch}
\alias{RSiteSearch}
\title{
  Search for key words or phrases in the R-help mailing list archives
  or documentation.
}
\description{
  Search for key words or phrases in the R-help mailing list
  archives, or \R manuals and help pages, using the search engine at
  \url{http://finzi.psych.upenn.edu} and view them in a web browser.
}
\usage{
RSiteSearch(string,
            restrict = c("Rhelp02a", "functions", "docs"),
            format = "normal", sortby = "score",
            matchesPerPage = 20)
}
\arguments{
  \item{string}{word(s) or phrase to search.  If the words are to be
    searched as one entity, enclose all words in braces (see example).}
  \item{restrict}{character: What areas to search in:
    \code{Rhelp02a} for R-help mailing list archive since 2002,
    \code{Rhelp01} for mailing list archive before 2002,
    \code{docs} for R manuals,
    \code{functions} for help pages.
    Use \code{c()} to specify more than one.}
  \item{format}{\code{normal} or \code{short} (no excerpts).}
  \item{sortby}{How to sort the search results (\code{score},
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
  \url{http://finzi.psych.upenn.edu}, and depends on that site
  continuing to be made available (thanks to Jon Baron).

  Unique partial matches will work for all arguments.  Each new
  browser window will stay open unless you close it.
}
\author{Andy Liaw and Jonathan Baron}
\seealso{
  \code{\link{help.search}}, \code{\link{help.start}} for local searches.

  \code{\link{browseURL}} for how the help file is displayed.
}
\examples{\dontrun{ # need Internet connection
RSiteSearch("{logistic regression}") # matches exact phrase
RSiteSearch("Baron Liaw", res = "Rhelp02")
}}
\keyword{utilities}
\keyword{documentation}

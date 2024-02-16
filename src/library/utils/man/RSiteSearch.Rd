% File src/library/utils/man/RSiteSearch.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2012 R Core Team
% Distributed under GPL 2 or later

\name{RSiteSearch}
\alias{RSiteSearch}
\title{
  Search for Key Words or Phrases in Documentation
}
\description{
  Search for key words or phrases in various documentation, such as
  R manuals, help pages of base and CRAN packages, vignettes, task views and others, using the search engine
  at \url{https://search.r-project.org} and view them in a web browser.
}
\usage{
RSiteSearch(string,
            restrict = c("functions", "descriptions", "news", "Rfunctions",
                         "Rmanuals", "READMEs", "views", "vignettes"),
            format,
            sortby = c("score", "date:late", "date:early", "subject",
                       "subject:descending", "size", "size:descending"),
            matchesPerPage = 20,
            words = c("all", "any"))
}
\arguments{
  \item{string}{A character string specifying word(s) or phrase(s) to
    search. If the words are to be searched as one entity, enclose them
    either in (escaped) quotes or in braces.}
  \item{restrict}{A character vector, typically of length greater than one.
    Values can be abbreviated.
    Possible areas to search in:
    \code{functions} for help pages of CRAN packages,
    \code{descriptions} for extended descriptions of CRAN packages,
    \code{news} for package NEWS,
    \code{Rfunctions} for help pages of R base packages,
    \code{Rmanuals} for R manuals,
    \code{READMEs} for \file{README} files of CRAN packages,
    \code{views} for task views,
    \code{vignettes} for package vignettes.}
  \item{format}{deprecated.}
  \item{sortby}{character string (can be abbreviated) indicating how to
    sort the search results:\cr
    (\code{score},
    \code{date:late} for sorting by date with latest results first,
    \code{date:early} for earliest first,
    \code{subject} for captions in alphabetical order,
    \code{subject:descending} for reverse alphabetical order,
    \code{size} or \code{size:descending} for size.)}
  \item{matchesPerPage}{How many items to show per page.}
  \item{words}{Show results matching \code{all} words/phrases (default) or \code{any} of them.}
}
\details{
  This function is designed to work with the search site at
  \url{https://search.r-project.org}.

  Unique partial matches will work for all arguments.  Each new
  browser window will stay open unless you close it.
}
\value{
  (Invisibly) the complete URL passed to the browser,
  including the query string.
}
\author{Andy Liaw and Jonathan Baron and Gennadiy Starostin}
\seealso{
  \code{\link{help.search}}, \code{\link{help.start}} for local searches.

  \code{\link{browseURL}} for how the help file is displayed.
}
\examples{
\donttest{# need Internet connection
## for phrase searching you may use (escaped) double quotes or brackets
RSiteSearch("{logistic regression} \"glm object\"")
RSiteSearch('"logistic regression"')

## Search in vignettes and help files of R base packages
## store the query string:
fullquery <- RSiteSearch("lattice", restrict = c("vignettes","Rfunctions"))
fullquery # a string of 112 characters
}
}
\keyword{utilities}
\keyword{documentation}

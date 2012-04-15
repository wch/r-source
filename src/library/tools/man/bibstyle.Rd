% File src/library/tools/man/bibstyle.Rd
% Part of the R package, http://www.R-project.org
% Copyright 2010-2 R Core Team
% Distributed under GPL 2 or later

\name{bibstyle}
\alias{bibstyle}
\title{
Select or define a bibliography style.
}
\description{
This function defines and registers styles for rendering \code{\link{bibentry}} objects
into Rd format, for later conversion to text, HTML, etc.
}
\usage{
bibstyle(style, envir, ..., .init = FALSE, .default = FALSE)
}
\arguments{
  \item{style}{
A character string naming the style.
}
  \item{envir}{
(optional) An environment holding the functions to implement the style.
}
  \item{\dots}{
Named arguments to add to the environment.
}
  \item{.init}{
Whether to initialize the environment from the default style \code{"JSS"}.
}
  \item{.default}{
Whether to set the specified style as the default style.
}
}
\details{
Rendering of \code{\link{bibentry}} objects may be done using routines
modelled after those used by BibTeX.  This function allows environments
to be created and manipulated to contain those routines.

There are two ways to create a new style environment.  The easiest
is to set \code{.init = TRUE}, in which case the environment will be
initialized with a copy of the default \code{"JSS"} environment.  (This style
is modelled after the \file{jss.bst} style used by the \emph{Journal 
of Statistical Software}.)  Alternatively, the \code{envir} argument 
can be used to specify a completely new style environment.

To simply retrieve an existing style, specify \code{style}
and no other arguments.  To modify an existing style, specify \code{style}
and some named entries via \code{...}.  (Modifying the default
\code{"JSS"} style is discouraged.)  Setting \code{style} to \code{NULL}
or leaving it missing will retrieve the default style, but modifications
will not be allowed.

At a minimum, the environment should contain routines to render each
of the 12 types of bibliographic entry supported by
\code{\link{bibentry}} as well as a routine to produce a sort key to
sort the entries.  The former must be named \code{formatArticle},
\code{formatBook}, \code{formatInbook}, \code{formatIncollection},
\code{formatInProceedings}, \code{formatManual},
\code{formatMastersthesis}, \code{formatMisc}, \code{formatPhdthesis},
\code{formatProceedings}, \code{formatTechreport} and
\code{formatUnpublished}.  Each of these takes one argument, a single
\code{\link{unclass}}'ed entry from the \code{\link{bibentry}} vector
passed to the renderer, and should produce a single element character
vector (possibly containing newlines).  The sort keys are produced
by a function named \code{sortKeys}.  It is passed the original 
\code{\link{bibentry}} vector and should produce a sortable vector of 
the same length to define the sort order.
}
\value{
The environment which has been selected or created.
}
\author{
Duncan Murdoch
}
\seealso{
\code{\link{bibentry}}
}
\examples{
\testonly{options(useFancyQuotes = FALSE)}
refs <- 
c(bibentry(bibtype = "manual",
    title = "R: A Language and Environment for Statistical Computing",
    author = person("R Core Team"),
    organization = "R Foundation for Statistical Computing",
    address = "Vienna, Austria",
    year = 2010,
    isbn = "3-900051-07-0",
    url = "http://www.R-project.org"),  
  bibentry(bibtype = "article",
    author = c(person(c("George", "E", "P"), "Box"),
               person(c("David",  "R"),      "Cox")),
    year = 1964,
    title = "An Analysis of Transformations",
    journal = "Journal of the Royal Statistical Society, Series B",
    volume = 26,
    pages = "211-252"))
bibstyle("unsorted", sortKeys = function(refs) seq_along(refs),
       .init = TRUE)
print(refs, .bibstyle = "unsorted")    
}
\keyword{ utilties }
\keyword{ documentation }

\name{make.function.html}
\alias{link.html.help}
\alias{make.function.html}
\alias{make.packages.html}
\alias{make.search.html}
\title{Update HTML documentation files}
\usage{
link.html.help()
make.function.html()
make.packages.html()
make.search.html()
}
\description{
    Functions to re-create the HTML documentation files to reflect all
    installed packages. \code{make.function.html} creates the
    alphabetical list of functions, \code{make.packages.html} creates
    the list of packages, and \code{make.search.html} creates the
    database used by the Java search engine. \code{link.html.help} runs
    all three.
}
\value{
  No value is returned. The file \code{RHOME/doc/html/function.html},
  \code{RHOME/doc/html/packages,html} or
  \code{RHOME/doc/html/search/index.txt} is (re-)created.
}
\author{Guido Masarotto and Brian Ripley}
\note{You will need write permission on the \code{RHOME/doc} tree.}
\keyword{utilities}

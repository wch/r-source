\name{link.html.help}
\alias{link.html.help}
\alias{make.packages.html}
\alias{make.search.html}
\title{Update HTML documentation files}
\usage{
link.html.help(verbose=FALSE, lib.loc=.Library)
make.packages.html(lib.loc=.lib.loc)
make.search.html(lib.loc=.Library)
}
\description{
  Functions to re-create the HTML documentation files to reflect all
  installed packages. \code{make.packages.html} creates
  the list of packages and \code{make.search.html} creates the
  database used by the Java search engine. \code{link.html.help} runs
  both
}
\arguments{
  \item{verbose}{logical. If true, print out a message. For use to
    explain a delay when called from other functions.}
  \item{lib.loc}{character. List of libraries to be included.}
}
\details{
  Only packages in the system library \code{.Library} are included
  by default, as cross-library links only work on this platform.
}
\value{
  No value is returned. The file(s)
  \file{R\_HOME\\doc\\html\\packages.html} and/or
  \file{R\_HOME\\doc\\html\\search\\index.txt} is (re-)created.
}
\author{Guido Masarotto and Brian Ripley}
\note{You will need write permission in the \file{R\_HOME\\doc\\html}
  directory.}
\keyword{utilities}

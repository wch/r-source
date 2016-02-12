% File src/library/tools/man/loadRdMacros.Rd
% Part of the R package, https://www.R-project.org
% Copyright 2016 R Core Team
% Distributed under GPL 2 or later


\name{loadRdMacros}
\alias{loadRdMacros}
\alias{loadPkgRdMacros}
\title{
Load user-defined Rd help system macros.
}
\description{
Loads macros from an \file{.Rd} file, or from several \file{.Rd}
files contained in a package.
}
\usage{
loadRdMacros(file, macros = TRUE)
loadPkgRdMacros(pkgdir, macros)
}
\arguments{
  \item{file}{
A file in Rd format containing macro definitions.
}
  \item{macros}{
\code{TRUE} or a previous set of macro definitions, in the format expected by the
\code{\link{parse_Rd}} \code{macros} argument.
}
  \item{pkgdir}{
The base directory of a source package or an installed package.
}
}
\details{
The files parsed by this function should contain only macro definitions;
a warning will be issued if anything else other than comments or white space
is found.

The \code{macros} argument may be a filename of a base set of macros,
or the result of a previous call to \code{loadRdMacros} or \code{loadPkgRdMacros}
in the same session.  These results should be assumed to be valid only within
the current session.

The \code{loadPkgMacros} function first looks for an \code{"RdMacros"} 
entry in the package \file{DESCRIPTION} file.  If present, it should contain
a comma-separated list of other package names; their macros will be loaded
before those of the current package.  It will then look in the current 
package for \file{.Rd} files in the \file{man/macros} or \file{help/macros}
subdirectories, and load those.
}
\value{
These functions each return an environment containing objects with the names
of the newly defined macros from the last file processed.  The parent
environment will be macros from the previous file, and so on.  The first
file processed will have \code{\link{emptyenv}()} as its parent.
}
\references{
See the \sQuote{Writing \R Extensions} manual for the syntax of Rd files,
or \url{http://developer.r-project.org/parseRd.pdf} for a technical discussion.
}
\author{
Duncan Murdoch
}
\seealso{
\code{\link{parse_Rd}}
}
\examples{
\donttest{
f <- tempfile()
writeLines(paste0("\\\\newcommand{\\\\logo}{\\\\if{html}{\\\\figure{Rlogo.jpg}{options: width=100}",
                  "\\\\if{latex}{\\\\figure{logo.jpg}{options: width=0.5in}}}"),
	    f)
m <- loadRdMacros(f)
ls(m)
ls(parent.env(m))
ls(parent.env(parent.env(m)))
}
}
\keyword{ utilities }
\keyword{ documentation }

% File src/library/tools/man/Rd2HTML.Rd
% Part of the R package, http://www.R-project.org
% Copyright 2008-9 R Core Development Team
% Distributed under GPL 2 or later

\name{Rd2HTML}
\Rdversion{1.1}
\alias{Rd2txt}
\alias{Rd2HTML}
\alias{Rd2ex}
\alias{Rd2latex}
\alias{checkRd}
\alias{findHTMLlinks}
\title{ Work with an Rd object }
\description{
  These experimental functions take the output of the
  \code{\link{parse_Rd}} function and check it or produce a help
  page from it.  Their interfaces (and existence!) are subject to
  change.
}
\usage{
Rd2HTML(Rd, out = "", package = "", defines = .Platform$OS.type,
        encoding = "unknown", Links = NULL, CHM = FALSE)

Rd2txt(Rd, out = "", package = "", defines = .Platform$OS.type)

Rd2latex(Rd, out = "", defines = .Platform$OS.type, encoding = "unknown")

Rd2ex(Rd, out = "", defines = .Platform$OS.type, encoding = "unknown")

checkRd(Rd, defines = .Platform$OS.type, encoding = "unknown",
        unknownOK = FALSE, listOK = TRUE)

findHTMLlinks(pkgDir = "", lib.loc = NULL)
}
\arguments{
  \item{Rd}{ the \code{Rd} object. }
  \item{out}{ a filename or connection object to which to write the output. }
  \item{package}{ the package to list in the output. }
  \item{defines}{ string(s) to use in \code{"#ifdef"} tests. }
  \item{encoding}{ encoding to use if the file does not specify one. }
  \item{Links}{\code{NULL} or a named (by topics) character vector of
    links, as returned by \code{findHTMLlinks}.}
  \item{CHM}{logical: prepare HTML suitable for Compiled HTML?} 
  \item{unknownOK}{ unrecognized macros are treated as errors if
    \code{FALSE}, otherwise warnings. }
  \item{listOK}{ unnecessary non-empty braces (e.g. around text, not
    as an argument) are treated as errors if \code{FALSE}, otherwise warnings. }
  \item{pkgDir}{The top-level directory of an installed package.}
  \item{lib.loc}{character vector describing the location of \R library trees
    to scan: the default indicates \code{\link{.libPaths}()}.}
}
\details{
  The \code{Rd2HTML} converter is still in development, and currently
  has the following known deficiencies:
  \itemize{
    \item \code{"\\enc"} is not supported.
    \item Error messages are not as informative as they could be.  Often
    using \code{parse_Rd(\dots, verbose = TRUE)} will give a better idea
    of what is wrong.
    \item Formatting is not always optimal.
  }

  \code{Rd2ex} extracts the examples in the format used by
  \code{\link{example}} and \R utilities.  it reproduces the previous
  version up to
  \itemize{
    \item The parser does not interpret markup in \R comments: this used
    to be done.
    \item Small improvements in layout.
  }

  \code{findHTMLlinks} uses in decreasing priority
  \itemize{
    \item The package in \code{pkgDir}: this is used when converting
    HTML help for that package.
    \item The packages that are loaded in a vanilla \R session.
    \item Other packages found in the library trees specified by
    \code{lib.loc} in the order of the trees and alphabetically within a
    library tree.
  }
}
\value{
  \code{Rd2*} are executed mainly for the side effect of writing the
  converted help page.  Its value is the name of that page.
  
  \code{checkRd} returns \code{TRUE} if the file has no errors; it
  generates an R error otherwise.

  \code{findHTMLlinks} returns a namesd character vector of relative
  file paths, in the same package or in a package in the same library tree.
}
\author{
  Duncan Murdoch, Brian Ripley
}
\references{ \url{http://developer.r-project.org/parseRd.pdf} }
\seealso{
  \code{\link{parse_Rd}}
}
\examples{
\donttest{
toolsRd <- Rd_db("tools")
con <- textConnection(toolsRd[[grep("Rd2HTML.Rd", names(toolsRd))]], "rt")
outfile <- paste(tempfile(), ".html", sep="")
browseURL(Rd2HTML(con, outfile, package="tools"))
close(con)
 
con <- textConnection(toolsRd[["Rd2HTML.Rd"]], "rt")
checkRd(con) # A stricter test than Rd2HTML uses
close(con)
}
}
\keyword{ documentation }

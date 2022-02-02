% File src/library/tools/man/HTMLlinks.Rd
% Part of the R package, https://www.R-project.org
% Copyright 2009 R Core Team
% Distributed under GPL 2 or later

\name{HTMLlinks}
\alias{findHTMLlinks}
\title{Collect HTML Links from Package Documentation}
\description{
  Compute relative file paths for URLs to other package's installed HTML
  documentation.
}
\usage{
findHTMLlinks(pkgDir = "", lib.loc = NULL, level = 0:2)
}
\arguments{
  \item{pkgDir}{the top-level directory of an installed package.  The
    default indicates no package.}
  \item{lib.loc}{character vector describing the location of \R library
    trees to scan: the default indicates \code{\link{.libPaths}()}.}
  \item{level}{Which level(s) to include.}
}
\details{
  \code{findHTMLlinks} tries to resolve links from one help page to
  another.  It uses in decreasing priority
  \itemize{
    \item The package in \code{pkgDir}: this is used when converting
    HTML help for that package (level 0).
    \item The base and recommended packages (level 1).
    \item Other packages found in the library trees specified by
    \code{lib.loc} in the order of the trees and alphabetically within a
    library tree (level 2).
  }
}
\value{
  A named character vector of file paths, relative to the \file{html}
  directory of an installed package.  So these are of the form
  \file{"../../\var{somepkg}/html/\var{sometopic}.html"}.
}
\author{Duncan Murdoch, Brian Ripley}
\keyword{ documentation }

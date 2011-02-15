% File src/library/tools/man/checkRdaFiles.Rd
% Part of the R package, http://www.R-project.org
% Copyright 2011 R Core Development Team
% Distributed under GPL 2 or later

\name{add_datalist}
\alias{add_datalist}
\title{
  Add a 'datalist' File to a Package
}
\description{
  The \code{\link{data}()} command with no arguemnts lists all the
  datasets available via \code{data} in attached packages, and to do a
  per-package list is stored at install time.  This can be slow for
  packages with huge datasets, and can be expedited by a supplying
  \file{data/datalist} file.
}
\usage{
add_datalist(pkgpath, force = FALSE)
}
\arguments{
  \item{pkgpath}{The path to a (source) package.}

  \item{force}{logical: should can existing \code{data/datalist} file be
    over-written.} 
}

\seealso{
  \code{\link{data}}.

  The \sQuote{Writing R Extensions} manual.
}
\keyword{ utilities }

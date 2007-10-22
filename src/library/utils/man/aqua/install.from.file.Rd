% File src/library/utils/man/aqua/install.from.file.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Development Team
% Distributed under GPL 2 or later

\name{install.from.file}
\alias{install.from.file}
\title{Install a package from local source. }
\description{
  Compile and install a package from a local source code directory or
  tarball.  Can be invoked from the Packages menu.
}
\usage{
install.from.file(pkg = file.choose(), binary=FALSE)
}
\arguments{
  \item{pkg}{File or directory with package to install.}
  \item{binary}{\code{TRUE} for a binary package archive}
}
\value{
 Used for its side effects.
}

\seealso{\code{\link{install.binaries}}, \code{\link{install.packages}}}

\keyword{file}
\keyword{utilities}

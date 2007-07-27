% File src/library/base/man/manglePackageName.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Development Team
% Distributed under GPL 2 or later

\name{manglePackageName}
\alias{manglePackageName}
\title{Mangle the Package Name }
\description{
 This function takes the package name and the package version number and
 pastes them together with a separating underscore.
}
\usage{
manglePackageName(pkgName, pkgVersion)
}
\arguments{
  \item{pkgName}{The package name, as a character string. }
  \item{pkgVersion}{The package version, as a character string. }
}
\value{
  A character string with the two inputs pasted together.
}

\examples{
  manglePackageName("foo", "1.2.3")
}
\keyword{utilities }

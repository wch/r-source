\name{install.from.file}
\alias{install.from.file}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Install a package from local source. }
\description{
Compile and install a package from a local source code directory or
tarball. Can be invoked from the Packages menu.
}
\usage{
install.from.file(pkg = file.choose(), binary=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pkg}{File or directory with package to install.}
  \item{binary}{\code{TRUE} for a binary package archive}
}
\value{
 Used for its side effects.
}

\seealso{\code{\link{install.binaries}}, \code{\link{install.packages}}}
\examples{

}
\keyword{file}% at least one, from doc/KEYWORDS
\keyword{utilities}% __ONLY ONE__ keyword per line

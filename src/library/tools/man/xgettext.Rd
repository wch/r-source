\name{xgettext}
\alias{xgettext}
\alias{xgettext2pot}
\title{Extract Messages from R Files in a Package}
\description{
  For each file in the \code{R} directory (including subdirectories) of
  a package, extract the unique arguments passed to \code{stop},
  \code{warning} and \code{\link{gettext}}.
}
\usage{
xgettext(dir, verbose = FALSE, asCall = TRUE)

xgettext2pot(dir, potFile)
}
\arguments{
  \item{dir}{the directory of a source package.}
  \item{verbose}{logical: should each file be listed as it is processed?}
  \item{asCall}{logical: if \code{TRUE} each argument is return whole,
    otherwise the strings within each argument are extracted.}

  \item{potFile}{name of \code{po} template file to be
    produced. Defaults to \code{R-pkg.pot} where \code{pkg} is the
    basename of \code{dir}.}
}
\value{
  For \code{xgettext}, an object of class \code{"xgettext"} which is
  a list of elements per source file of class \code{"gettext"}
  (which has a \code{"print"} method).
}
\examples{\dontrun{## in a source-directory build of R:
xgettext(file.path(R.home(), "src", "library", "splines"))
}}
\keyword{ utilities }

\name{collater}
\alias{collater}
\title{A function to collate the R code files into a single file}
\description{
  Will collate the files in a package's \code{R} directory into a single
  file with a filename that is the same as the name of the package.  The
  order of collation can be specified by the package's DESCRIPTION file.
}
\usage{
collater(pkgDir, pkg = basename(pkgDir))
}
\arguments{
  \item{pkgDir}{The directory that the package is contained in}
  \item{pkg}{The name of the package}
}
\details{
  The \code{pkg} parameter defaults to being the basneame of the
  \code{pkgDir} parameter, but exists in case these are not the same.

  This function will read the package's DESCRIPTION file to detect if
  there exists a DCF field \code{Collate} and/or \code{Collate.XXX}
  where XXX is the result of .Platform$OS.type.  If neither of these
  fields are provided, the collation is performed by concatenating the R
  files together in alphabetical order by file name.  If the \code{Collate.XXX}
  is provided, then that is the order used.  If \code{Collate.XXX} is not
  specified but \code{Collate} is then the \code{Collate} field defines
  the order of collation.  The various \code{Collate} fields will
  contain a comma separated list of file names.

  Note that if \code{Collate} is used then every file in the package's
  \code{R} directory *must* be named, and likewise every named file
  *must* exist in the \code{R} directory.  Furthermore, if a
  \code{Collate.XXX} field is specified and used, and there exists a
  subdirectory of the same name as \code{XXX} in the \code{R} directory,
  then those files must also exist in the collation field.

  Note that the files listed in the Collate fields are listed as a
  relative path from the \code{R} directory.  Thus the file
  \code{R/foo.R} would be listed as simply \code{foo.R}.  Likewise, the
  file \code{R/unix/foo.R} would be listed as \code{unix/foo.R}.
}
\value{
  TRUE if the function completes successfully.
}
\author{Jeff Gentry}
\examples{
  ## temp
}
\keyword{utilities}

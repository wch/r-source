\name{collater}
\alias{collater}
\title{A function to collate R code files into a single file}
\description{
  This function collates the files in a package's \code{R} directory
  into a single file with the same name as the package.  The
  order of collation can be specified by the package DESCRIPTION file.
}
\usage{
collater(pkgDir, pkg = basename(pkgDir), lccollate="C") )
}
\arguments{
  \item{pkgDir}{The directory that the package is contained in.}
  \item{pkg}{The name of the package.}
  \item{lccollate} {The flag for controlling sorting of names.}
}
\details{
  Package installation concatenates all files in the package's \code{R}
  directory into a single file. This function provides a facility to
  arrange the collation order. Different collations are possible on
  different operating systems.

  Entries of the form \code{Collate} and/or \code{Collate.OStype}
  where \code{OStype} is one of the known operating systems in the
  DESCRIPTION file for a package control collation.

  If there is no entry then collation is controlled by the \code{lccollate}
  parameter. For any package and operating system the most specific
  directive is used. For example, if there is an field named
  \code{Collate} and one named \code{Collate.windows} then the latter is
  used on Windows platforms and the former on all other platforms.

  A Collate field is used to control collation all files present in the R
  directory (after processing for the specific OS) must be
  explicitly named and the order that they appear in the field is used
  for collation. The Collate fields must contain comma separated, case
  sensitive file names.
 
  Files named in the Collate fields are listed as a
  relative paths from the \code{R} directory.  Thus the file
  \code{R/foo.R} would be listed as simply \code{foo.R}.  Likewise, the
  file \code{R/unix/foo.R} would be listed as \code{unix/foo.R}.
}
\value{
  \code{TRUE} if the function completes successfully.
}
\author{Jeff Gentry}
\examples{
}
\keyword{utilities}

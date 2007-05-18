\name{vignette}
\alias{vignette}
\alias{edit.vignette}
\alias{print.vignette}
\title{View or List Vignettes}
\description{
  View a specified vignette, or list the available ones.
}
\usage{
vignette(topic, package = NULL, lib.loc = NULL, all = TRUE)

\S3method{print}{vignette}(x, \ldots)
\S3method{edit}{vignette}(name, \ldots)
}
\arguments{
  \item{topic}{a character string giving the (base) name of the vignette
    to view. If omitted, all vignettes from all installed packages are listed.}
  \item{package}{a character vector with the names of packages to
    search through, or \code{NULL} in which "all" packages (as defined
    by argument \code{all}) are searched.}
  \item{lib.loc}{a character vector of directory names of \R libraries,
    or \code{NULL}.  The default value of \code{NULL} corresponds to all
    libraries currently known.}
  \item{all}{logical; if \code{TRUE} search
    all available packages in the library trees specified by \code{lib.loc}, 
    and if \code{FALSE}, search only attached packages.}
  \item{x, name}{Object of class \code{vignette}.}
  \item{\ldots}{Ignored by the \code{print} method, passed on to
    \code{\link{file.edit}} by the \code{edit} method.}
}
\details{
  Function \code{vignette} returns an object of the same class, the
  print method opens a viewer for it.
  Currently, only PDF versions of vignettes can be viewed.
#ifdef unix
  The program specified by the \code{pdfviewer} option is used for this.
#endif
  If several vignettes have PDF versions with base name identical to
  \code{topic}, the first one found is used.
  
  If no topics are given, all available vignettes are listed.  The
  corresponding information is returned in an object of class
  \code{"packageIQR"}.

  The \code{edit} method
  extracts the \R code from the vignette to a temporary file and
  opens the file in an editor (see \code{\link{edit}}). This makes it
  very easy to execute the commands line by line, modify them in any way
  you want to help you test variants, etc.. An alternative way of
  extracting the \R code from the vignette is to run
  \code{\link{Stangle}} on the source code of the vignette,
  see the examples below.
}
\examples{
## List vignettes from all *attached* packages
vignette(all = FALSE)

## List vignettes from all *installed* packages (can take a long time!):
vignette(all = TRUE)

\dontrun{
## Open the grid intro vignette
vignette("grid")

## The same
v1 <- vignette("grid")
print(v1)

## Now let us have a closer look at the code
edit(v1)

## An alternative way of extracting the code,
## R file is written to current working directory
Stangle(v1$file)

## A package can have more than one vignette (package grid has several):
vignette(package="grid")
vignette("rotated")
## The same, but without searching for it:
vignette("rotated", package="grid")
}
}
\keyword{documentation}



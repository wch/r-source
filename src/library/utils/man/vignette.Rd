% File src/library/utils/man/vignette.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2026 R Core Team
% Distributed under GPL 2 or later

\name{vignette}
\title{View or List Package Vignettes or Edit their R Code Files}
\alias{vignette}
\alias{edit.vignette}
\alias{print.vignette}
\alias{vignettes} % to link to from e.g. grid-package.Rd
%% \alias{getRcode}
%% \alias{getRcode.vignette}%hidden
\description{
  View a specific package vignette or its \R code file,
  or list the available ones.
}
\usage{
vignette(topic, package = NULL, lib.loc = NULL, all = TRUE)

\S3method{print}{vignette}(x, \dots)
\S3method{edit}{vignette}(name, \dots)
%% \S3method{getRcode}{vignette}(x, strict=TRUE, \dots)
}
\arguments{
  \item{topic}{a character string giving the (base) name of the vignette
    to view, or an (unevaluated) expression of the form \code{package::topic}.
    If omitted, all vignettes from all installed packages are listed.}
  \item{package}{a character vector with the names of packages to
    search through, or \code{NULL} in which case \sQuote{all} packages (as
    defined by argument \code{all}) are searched.}
  \item{lib.loc}{a character vector of directory names of \R libraries,
    or \code{NULL}.  The default value of \code{NULL} corresponds to all
    libraries currently known.}
  \item{all}{logical; if \code{TRUE} search all available packages in
    the library trees specified by \code{lib.loc}, and if \code{FALSE},
    search only attached packages.}
  \item{x, name}{object of class \code{vignette}.}
  \item{\dots}{ignored by the \code{print} method, passed on to
    \code{\link{file.edit}} by the \code{edit} method.}
}
\details{
  Function \code{vignette} returns an object of the same class, the
  print method opens a viewer for it.

  On Unix-alikes,
  the program specified by the \code{pdfviewer} option is used for
  viewing PDF versions of vignettes.

  If several vignettes have PDF/HTML versions with base name identical
  to \code{topic}, the first one found is used.

  If no topics are given, all available vignettes are listed.  The
  corresponding information is returned in an object of class
  \code{"packageIQR"}.

  %% \code{getRcode(x)} gets the (full path) file name of the \R code
  %% extracted from the vignette, and the \code{edit} method that  \R code
  %% to a temporary file and opens the file in an editor (see
  %% \code{\link{edit}}). This makes it very easy to execute the commands
  %% line by line, modify them in any way you want to help you test
  %% variants, etc.
}
\seealso{
  \code{\link{browseVignettes}} for an HTML-based vignette browser;
  \code{\link{RShowDoc}("\var{basename}", package = "\var{pkgname}")} displays a
  \dQuote{rendered} vignette (pdf or html).
}
\examples{
## List vignettes from all *attached* packages
vignette(all = FALSE)

## List vignettes from all *installed* packages (can take a long time!)
vignette(all = TRUE)

## List all vignettes of a specific package (package 'grid' has several)
vignette(package = "grid")

## Open one of the 'grid' vignettes
if(interactive()) {
  ## vignette("rotated", package = "grid")
  ## Or, the same:
  vignette(grid::rotated) # calling the print() method
}

## Now open the 'grid' intro vignette -- without specifying the package
\dontrun{vignette("grid")}
## OK, but warns as this topic is ambiguous: both {grid} and {lattice} have it.
## => Specify the 'package' argument or use the <package>::<topic> syntax
##    (also accelerates vignette retrieval, esp. with many installed packages):
v1 <- vignette("grid", package = "grid") # not "printed" (opened) yet
if(inherits(v1, "vignette")) { # it was found installed
  \dontrun{print(v1) # open it}
  str(v1)
  ## To open the associated R code in an editor:
  \dontrun{edit(v1) # e.g., to send lines ...}
%% print(rfilepath <- utils:::getRcode(v1))
%% if(interactive()) file.show(rfilepath)
}
}
\keyword{documentation}

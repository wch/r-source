% File src/library/utils/man/vignette.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2015 R Core Team
% Distributed under GPL 2 or later

\name{vignette}
\title{View, List or Get R Source of Package Vignettes}
\alias{vignette}
\alias{edit.vignette}
\alias{print.vignette}
\alias{vignettes} % to link to from e.g. grid-package.Rd
%% \alias{getSource}
%% \alias{getSource.vignette}%hidden
\description{
  View a specified package vignette, or list the available ones;
  display it rendered in a viewer, and get or edit its \R source file.
}
\usage{
vignette(topic, package = NULL, lib.loc = NULL, all = TRUE)

\S3method{print}{vignette}(x, \dots)
\S3method{edit}{vignette}(name, \dots)
%% \S3method{getSource}{vignette}(x, strict=TRUE, \dots)
}
\arguments{
  \item{topic}{a character string giving the (base) name of the vignette
    to view.  If omitted, all vignettes from all installed packages are
    listed.}
  \item{package}{a character vector with the names of packages to
    search through, or \code{NULL} in which \sQuote{all} packages (as
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
#ifdef unix
  The program specified by the \code{pdfviewer} option is used for
  viewing PDF versions of vignettes.
#endif
  If several vignettes have PDF/HTML versions with base name identical
  to \code{topic}, the first one found is used.

  If no topics are given, all available vignettes are listed.  The
  corresponding information is returned in an object of class
  \code{"packageIQR"}.

  %% \code{getSource(x)} gets the (full path) file name of the \R code
  %% extracted from the vignette, and the \code{edit} method that  \R code
  %% to a temporary file and opens the file in an editor (see
  %% \code{\link{edit}}). This makes it very easy to execute the commands
  %% line by line, modify them in any way you want to help you test
  %% variants, etc.
}
\seealso{
  \code{\link{browseVignettes}} for an HTML-based vignette browser.
}
\examples{
## List vignettes from all *attached* packages
vignette(all = FALSE)

## List vignettes from all *installed* packages (can take a long time!):
vignette(all = TRUE)

## The grid intro vignette -- open it
\dontrun{vignette("grid") # calling print()}
## The same (conditional on existence of the vignettte).
## Note that 'package = *' is much faster in the case of many installed packages:
if(!is.null(v1 <- vignette("grid", package="grid"))) {
\dontrun{v1 # calling print(.)}
  str(v1)
  ## Now let us have a closer look at the code
  %% print(utils:::getSource(v1)) # the R file name
  %% if(interactive()) # "display" it
  %%   file.show(utils:::getSource(v1))
  %% ## or even more interactively:
\dontrun{edit(v1) # e.g., to send lines ...}
}# if( has vignette "installed")
## A package can have more than one vignette (package grid has several):
vignette(package = "grid")
if(interactive()) {
   ## vignette("rotated")
   ## The same, but without searching for it:
   vignette("rotated", package = "grid")
}
}
\keyword{documentation}

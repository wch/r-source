% File src/library/utils/man/browseVignettes.Rd
% Part of the R package, http://www.R-project.org
% Copyright 2007 R Core Team
% Distributed under GPL 2 or later

\name{browseVignettes}
\alias{browseVignettes}
\alias{print.browseVignettes}
\title{List Vignettes in an HTML Browser}
\description{
  List available vignettes in an HTML browser with links to PDF,
  LaTeX/noweb source, and (tangled) R code (if available).
}
\usage{
browseVignettes(package = NULL, lib.loc = NULL, all = TRUE)

\S3method{print}{browseVignettes}(x, \dots)
}
\arguments{
  \item{package}{a character vector with the names of packages to
    search through, or \code{NULL} in which "all" packages (as defined
    by argument \code{all}) are searched.}
  \item{lib.loc}{a character vector of directory names of \R libraries,
    or \code{NULL}.  The default value of \code{NULL} corresponds to all
    libraries currently known.}
  \item{all}{logical; if \code{TRUE} search
    all available packages in the library trees specified by \code{lib.loc},
    and if \code{FALSE}, search only attached packages.}
  \item{x}{Object of class \code{browseVignettes}.}
  \item{\dots}{Further arguments, ignored by the \code{print} method. }
}
\details{
  Function \code{browseVignettes} returns an object of the same class;
  the print method displays it as an HTML page in a browser (using
  \code{\link{browseURL}}).
}
\seealso{
  \code{\link{browseURL}}, \code{\link{vignette}}
}
\examples{\donttest{
## List vignettes from all *attached* packages
browseVignettes(all = FALSE)

## List vignettes from a specific package
browseVignettes("grid")
}}
\keyword{documentation}



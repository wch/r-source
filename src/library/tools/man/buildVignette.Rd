% File src/library/tools/man/buildVignette.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2018 R Core Team
% Distributed under GPL 2 or later

\name{buildVignette}
\alias{buildVignette}
\title{Build One Vignette}
\description{
  Run \code{\link{Sweave}} (or other custom weave function)
  \code{\link{texi2dvi}}, and/or \code{\link{Stangle}} 
  (or other custom tangle function) on one vignette.

  This is the workhorse of \command{R CMD Sweave}.
}
\usage{
buildVignette(file, dir = ".", weave = TRUE, latex = TRUE, tangle = TRUE, 
              quiet = TRUE, clean = TRUE, keep = character(),
              engine = NULL, buildPkg = NULL, encoding, ...)
}
\arguments{
  \item{file}{character; the vignette source file.}
  \item{dir}{character; the working directory in which the intermediate and 
                 output files will be produced.}
  \item{weave}{logical; should weave be run?}
  \item{latex}{logical; \link{texi2pdf} be run if weaving produces a \file{.tex} file?}
  \item{tangle}{logical; should tangle be run?}
  \item{quiet}{logical; run in quiet mode?}
  \item{clean}{logical; whether to remove some newly created, often
    intermediate, files.  See details below.}
  \item{keep}{a list of file names to keep in any case when cleaning.  Note
    that \dQuote{target} files are kept anyway.}
  \item{engine}{\code{NULL} or character; name of vignette engine to
    use.  Overrides any \code{\\VignetteEngine\{\}} markup in the vignette.}
  \item{buildPkg}{\code{NULL} or a character vector; optional packages
    in which to find the vignette engine.}
  \item{encoding}{the encoding to assume for the file.  If not
    specified, it will be read if possible from the file's contents.
    Note that if the vignette is part of a package,
    \code{\link{buildVignettes}} reads the package's encoding from the
    \file{DESCRIPTION} file but this function does not.} 
  \item{...}{Additional arguments passed to weave and tangle.}

}
\details{
  This function determines the vignette engine for the vignette (default
  \code{utils::Sweave}), then weaves and/or tangles the vignette using
  that engine.  Finally, if \code{clean} is \code{TRUE}, newly created
  intermediate files (non \dQuote{targets}, where these depend on the engine,
  etc, and not any in \code{keep}) will be deleted.  If \code{clean} is
  \code{NA}, and \code{weave} is true, newly created intermediate output
  files (e.g., \file{.tex}) will not be deleted even if a \file{.pdf}
  file has been produced from them.  

  If \code{buildPkg} is specified, those packages will be loaded before
  the vignette is processed and will be used as the default packages in
  the search for a vignette engine, but an explicitly specified package
  in the vignette source (e.g., using
  \code{\\VignetteEngine\{utils::Sweave\}} to specify the \code{Sweave}
  engine in the \pkg{utils} package) will override it.  In contrast, if
  the \code{engine} argument is given, it will override the vignette
  source.
}
\value{
  A character vector naming the files that have been produced.
}
\author{
  Henrik Bengtsson and Duncan Murdoch
}
\seealso{
  \code{\link{buildVignettes}} for building all vignettes in a package.
}
\keyword{documentation}

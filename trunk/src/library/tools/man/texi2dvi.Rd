\name{texi2dvi}
\alias{texi2dvi}
\title{Compile LaTeX Files}
\description{Run latex and bibtex until all cross-references are
  resolved and create either a dvi or PDF file.}
\usage{
texi2dvi(file, pdf = FALSE, clean = FALSE, quiet = TRUE,
         texi2dvi = getOption("texi2dvi"))
}
\arguments{
  \item{file}{character.  Name of TeX source file.}
  \item{pdf}{logical.  If \code{TRUE}, a PDF file is produced instead of
    the default dvi file (texi2dvi command line option \option{--pdf}).}
  \item{clean}{logical.  If \code{TRUE}, all auxiliary files are removed
    (texi2dvi command line option \option{--clean}). Does not work on
    some platforms.}
  \item{quiet}{logical.  No output unless an error occurs.}
  \item{texi2dvi}{character (or \code{NULL}).  Script or program used to
    compile a TeX file to dvi or PDF, respectively.  If set to
    \code{NULL}, the \file{texi2dvi} script in R's \file{bin} directory
    is used (if it exists), otherwise it is assumed that
    \command{texi2dvi} is in the search path.}
}
\details{
  Some TeX installations do not have \file{texi2dvi.exe}.  If
  \file{texify.exe} is present, then it can be 
  used instead: set \code{options(texi2dvi="texify.exe")} or to the full
  path of the program.
}
\author{Achim Zeileis}
\keyword{utilities}

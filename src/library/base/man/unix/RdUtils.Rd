\name{RdUtils}
\alias{Rdconv}
\alias{Rdindex}
\alias{Rd2txt}
\alias{Rd2dvi}
\alias{Sd2Rd}
\title{Utilities for Processing Rd Files}
\description{
  Utilities for converting files in R documentation (Rd) format to other
  formats or create indices from them, and for converting documentation
  in other formats to Rd format.
}
\usage{
R CMD Rdconv [options] file
R CMD Rdindex [options] files
R CMD Rd2dvi [options] files
R CMD Rd2txt [options] file
R CMD Sd2Rd [options] file
}
\arguments{
  \item{file}{the path to a file to be processed.}
  \item{files}{a list of file names specifying the R documentation
    sources to use, by either giving the paths to the files, or the path
    to a directory with the sources of a package.}
  \item{options}{further options to control the processing, or for
    obtaining information about usage and version of the utility.}
}
\details{
  \code{R CMD Rdconv} converts Rd format to other formats.  Currently,
  plain text, HTML, LaTeX, S version 3 (Sd) format are supported.  It
  can also extract the examples for run-time testing.

  \code{R CMD Rd2dvi} and \code{R CMD Rd2txt} are user-level programs
  for producing DVI/PDF output or pretty text output from Rd sources.

  \code{R CMD Rdindex} creates an index table from Rd files.

  \code{R CMD Sd2Rd} converts S version 3 documentation format to Rd
  format.

  Use \code{R CMD foo --help} to obtained usage information on utility
  \code{foo}.
}
\seealso{
  chapter ``Processing Rd format'' in ``Writing \R Extensions'' (see the
  \file{doc/manual} subdirectory of the \R source tree).
}
\keyword{utilities}

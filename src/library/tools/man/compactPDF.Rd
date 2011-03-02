% File src/library/tools/man/compactPDF.Rd
% Part of the R package, http://www.R-project.org
% Copyright 2011 R Core Development Team
% Distributed under GPL 2 or later

\name{compactPDF}
\alias{compactPDF}
\title{
  Compact PDF Files
}
\description{
  Re-save PDF files (especially vignettes) more compactly.
  Support function for \command{R CMD build --compact-vignettes}.
}
\usage{
compactPDF(paths, qpdf = Sys.getenv("R_QPDF", "qpdf"),
           gs_cmd = Sys.getenv("R_GSCMD", ""),
           gs_quality = c("printer", "ebook", "screen"),
           gs_extras = character())
}
\arguments{
  \item{paths}{A character vector of paths to PDF files, or a length-one
    character vector naming a directory, when all \file{.pdf} files in
    that directory will be used.}
  \item{qpdf}{Character string giving the path to the \command{qpdf}
    command.}
  \item{gs_cmd}{Character string giving the path to the GhostScript
    executable, if that is to be used.  On Windows this is the path to
    \file{gswin32c.exe}.}
  \item{gs_quality}{A character string indicating the quality required:
    the options are respectively 300, 150 and 72dpi.}
  \item{gs_extras}{An optional character vector of further options to be
    passed to GhostScript.}
}
\details{
  This by default makes use of \command{qpdf}, available from
  \url{http://qpdf.sourceforge.net/}, including a Windows binary.  If
  \code{gs_cmd} is non-empty, GhostScript is used.

  \command{qpdf} or \command{gs_cmd} is run on all PDF files found, and
  those which are reduced by at least 10\% and 10Kb are replaced.
  
  The main approach of \command{qpdf} is to (losslessly) compress PDF
  streams.  Ghostscript does that and more (including downsampling
  images) and consequently is much slower and may lose quality (but can
  also produce much smaller PDF files).
}

\value{
  An object of class \code{c("compactPDF", "data.frame")} with a
  \code{format} and \code{print} method.  This has two columns, the old
  and new sizes in bytes for the files that were changed.
}

\note{
  The external tools used may change in future releases.
}

\seealso{
  \code{\link{resaveRdaFiles}}.

  Many other (and sometimes more effective) tools to compact PDF files
  are available, including Adobe Acrobat (not Reader).
  See the \sQuote{Writing R Extensions} manual.
}

\keyword{ utilities }

% File src/library/tools/man/compactPDF.Rd
% Part of the R package, http://www.R-project.org
% Copyright 2011-2013 R Core Team
% Distributed under GPL 2 or later

\name{compactPDF}
\alias{compactPDF}
\alias{format.compactPDF}
\title{
  Compact PDF Files
}
\description{
  Re-save PDF files (especially vignettes) more compactly.
  Support function for \command{R CMD build --compact-vignettes}.
}
\usage{
compactPDF(paths,
           qpdf = Sys.which(Sys.getenv("R_QPDF", "qpdf")),
           gs_cmd = Sys.getenv("R_GSCMD", ""),
           gs_quality = Sys.getenv("GS_QUALITY", "none"),
           gs_extras = character())

\method{format}{compactPDF}(x, ratio = 0.9, diff = 1e4, ...)
}
\arguments{
  \item{paths}{A character vector of paths to PDF files, or a length-one
    character vector naming a directory, when all \file{.pdf} files in
    that directory will be used.}
  \item{qpdf}{Character string giving the path to the \command{qpdf}
    command.  If empty, \command{qpdf} will not be used.}
  \item{gs_cmd}{Character string giving the path to the GhostScript
    executable, if that is to be used.  On Windows this is the path to
    \file{gswin32c.exe} or \file{gswin64c.exe}.  If \code{""} (the
    default), the function will try to find a platform-specific path to
    GhostScript where required.}
  \item{gs_quality}{A character string indicating the quality required:
    the options are \code{"none"} (so GhostScript is not used),
    \code{"printer"} (300dpi), \code{"ebook"} (150dpi) and
    \code{"screen"} (72dpi).}
  \item{gs_extras}{An optional character vector of further options to be
    passed to GhostScript.}
  \item{x}{An object of class \code{"compactPDF"}.}
  \item{ratio, diff}{Limits for reporting: files are only reported whose
    sizes are reduced both by a factor of \code{ratio} and by
    \code{diff} bytes.}
  \item{\dots}{Further arguments to be passed to or from other methods.}
}
\details{
  This by default makes use of \command{qpdf}, available from
  \url{http://qpdf.sourceforge.net/} (including as a Windows binary) and
  included with the CRAN Mac OS X distribution of \R.  If \code{gs_cmd}
  is non-empty and \code{gs_quality != "none"}, GhostScript will used
  first, then \command{qpdf} if it is available.  If \code{gs_quality !=
  "none"} and \code{gs_cmd} is \code{""}, an attempt will be made to find a
  GhostScript executable.

  \command{qpdf} and/or \command{gs_cmd} are run on all PDF files found,
  and those which are reduced in size by at least 10\% and 10Kb are
  replaced.

  The strategy of our use of \command{qpdf} is to (losslessly) compress
  both PDF streams and objects.  GhostScript compresses streams and more
  (including downsampling and compressing embedded images) and
  consequently is much slower and may lose quality (but can also produce
  much smaller PDF files).  However, quality \code{"ebook"} is perfectly
  adequate for screen viewing and printing on laser printers.

  Where PDF files are changed they will become PDF version 1.5 files:
  these have been supported by Acrobat Reader since version 6 in 2003,
  so this is very unlikely to cause difficulties.

  Stream compression is what most often has large gains: \R's
  \code{\link{pdf}} device prior to \R 2.14.0 did not use it, and
  older vignette PDFs often benefit from being re-generated.  Most PDF
  documents are generated with object compression, but this does not
  seem to be the default for MiKTeX's \command{pdflatex}.  For some PDF
  files (and especially package vignettes), using GhostScript can
  dramatically reduce the space taken by embedded images (often screenshots).

  Where both GhostScript and \command{qpdf} are selected (when
  \code{gs_quality != "none"} and both executables are found), they are
  run in that order and the size reductions apply to the total
  compression achieved.
}

\value{
  An object of class \code{c("compactPDF", "data.frame")}.
  This has two columns, the old and new sizes in bytes for the files
  that were changed.

  There are \code{format} and \code{print} methods: the latter passes
  \code{\dots} to the format method, so will accept \code{ratio} and
  \code{diff} arguments.
}

\note{
  The external tools used may change in future releases.

  Versions of GhostScript 9.06 and later give several times better
  compression than 9.05 on some vignettes in CRAN packages.
}

\seealso{
  \code{\link{resaveRdaFiles}}.

  Many other (and sometimes more effective) tools to compact PDF files
  are available, including Adobe Acrobat (not Reader).
  See the \sQuote{Writing R Extensions} manual.
}

\keyword{ utilities }

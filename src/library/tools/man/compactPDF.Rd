% File src/library/tools/man/checkRdaFiles.Rd
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
compactPDF(paths, qpdf = Sys.getenv("R_QPDF", "qpdf"))
}
\arguments{
  \item{paths}{A character vector of paths to PDF files, or a length-one
    character vector naming a directory, when all \file{.pdf} files in
    that directory will be used.}
  \item{qpdf}{Character string giving the path to the \command{qpdf} command.}
}
\details{
  This makes use of \command{qpdf}, available from
  \url{http://qpdf.sourceforge.net/}, including a Windows binary.

  \command{qpdf} is run on all PDF files found, and those which are
  reduced by at least 10\% and 10Kb are replaced.
  
  The main approach of \command{qpdf} is to compress PDF streams.
}

\note{
  The external tools used may change in future releases.
}

\seealso{
  \code{\link{resaveRdaFiles}}.

  Many other (and sometimes more effective) tools to compact PDF files
  are available, including Adobe Acrobat (not Reader).
}

\keyword{ utilities }

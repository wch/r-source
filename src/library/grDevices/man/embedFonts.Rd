% File src/library/grDevices/man/embedFonts.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Development Team
% Distributed under GPL 2 or later

\name{embedFonts}
\alias{embedFonts}
\concept{encoding}
\title{Embed Fonts in PostScript and PDF}
\description{
  Runs Ghostscript to process a PDF or PostScript file and
  embed all fonts in the file.
}
\usage{
embedFonts(file, format, outfile = file, fontpaths = "",
           options = "")
}
\arguments{
  \item{file}{a character string giving the name of the original file.}
  \item{format}{either \code{"pswrite"} or \code{"pdfwrite"}.
    If not specified, it is guessed from the suffix of \code{file}.}
  \item{outfile}{the name of the new file (with fonts embedded).}
  \item{fontpaths}{a character vector giving directories that
    Ghostscript will search for fonts.}
  \item{options}{a character string containing further options to
    Ghostscript.}
}
\details{
  This function is not necessary if you just use the standard
  default fonts for PostScript and PDF output.

  If you use a special font, this function is useful for
  embedding that font in your PostScript or PDF document
  so that it can be shared with others without them having
  to install your special font (provided the font licence allows this).

  If the special font is not installed for Ghostscript,
  you will need to tell Ghostscript where the font is, using
  something like \code{options="-sFONTPATH=path/to/font"}.

  This function relies on a suitable Ghostscript executable being in
  your path, or the environment variable \env{R_GSCMD} (the same as
  \code{\link{bitmap}}) being set as the full path to the Ghostscript
  executable.
#ifdef unix
  This defaults to \code{"gs"}.
#endif
#ifdef windows
  For Windows users, this should be the path to \code{gswin32c.exe}.
  If unset, environment variable \env{GSC} is tried and then
  \code{"gswin32c.exe"}.
#endif

  Note that Ghostscript may do font substitution, so the font embedded
  may differ from that specified in the original file.
}
\value{
  The shell command used to invoke Ghostscript is returned
  invisibly. This may be useful for debugging purposes as
  you can run the command by hand in a shell to look for problems.
}
\seealso{
  \code{\link{postscriptFonts}},
  \code{\link{Devices}}.

  Paul Murrell and Brian Ripley (2006) Non-standard fonts in PostScript
  and PDF graphics. \emph{R News}, 6(2):41--47.
  \url{http://cran.r-project.org/doc/Rnews/Rnews_2006-2.pdf}.
}
\keyword{device}

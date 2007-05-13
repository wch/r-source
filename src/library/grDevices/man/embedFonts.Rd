\name{embedFonts}
\alias{embedFonts}
\concept{encoding}
\title{Embed Fonts in PostScript and PDF}
\description{
  Runs ghostscript to process a PDF or PostScript file and
  embed all fonts in the file.
}
\usage{
embedFonts(file, format, outfile = file, fontpaths = "", options = "")
}
\arguments{
  \item{file}{a character string giving the name of the original file.}
  \item{format}{either \code{"pswrite"} or \code{"pdfwrite"}.
    If not specified, it is guessed from the suffix of \code{file}.}
  \item{outfile}{the name of the new file (with fonts embedded).}
  \item{fontpaths}{a character vector giving directories that
    ghostscript will search for fonts.}
  \item{options}{a character string containing further options to
    ghostscript.}
}
\details{
  This function is not necessary if you just use the standard
  default fonts for PostScript and PDF output.

  If you use a special font, this function is useful for
  embedding that font in your PostScript or PDF document
  so that it can be shared with others without them having
  to install your special font.

  If the special font is not installed for ghostscript,
  you will need to tell ghostscript where the font is, using
  something like \code{options="-sFONTPATH=path/to/font"}.

  This function relies on a suitable ghostscript executable being in
  your path, or the environment variable \code{R_GSCMD} (the same as
  \code{\link{bitmap}}) being set as the full path to the ghostscript
  executable.
#ifdef windows
  For Windows users, this should be the path to \code{gswin32c.exe}.
#endif
}
\value{
  The shell command used to invoke ghostscript is returned
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

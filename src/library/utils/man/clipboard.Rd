% File src/library/utils/man/clipboard.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2018 R Core Team
% Distributed under GPL 2 or later

\name{clipboard}
\Rdversion{1.1}
\alias{getClipboardFormats}
\alias{readClipboard}
\alias{writeClipboard}
\alias{clipboard}
\title{Read/Write to/from the Clipboard in MS Windows}
\description{
  Transfer text between a character vector and the Windows clipboard in
  MS Windows (only).
}
\usage{
getClipboardFormats(numeric = FALSE)
readClipboard(format = 1, raw = FALSE)
writeClipboard(str, format = 1)
}
\arguments{
  \item{numeric}{logical: should the result be in human-readable form
    (the default) or raw numbers?}
  \item{format}{an integer giving the desired format.}
  \item{raw}{should the value be returned as a raw vector rather than
    as a character vector?}
  \item{str}{a character vector or a raw vector.}
}
\details{
  The Windows clipboard offers data in a number of formats: see
  e.g.\sspace{}\url{https://docs.microsoft.com/en-gb/windows/desktop/dataxchg/clipboard-formats}.

  The standard formats include
  \tabular{lrl}{
  CF_TEXT        \tab   1  \tab Text in the machine's locale\cr
  CF_BITMAP      \tab   2  \tab \cr
  CF_METAFILEPICT \tab  3  \tab Metafile picture\cr
  CF_SYLK        \tab   4  \tab Symbolic link\cr
  CF_DIF         \tab   5  \tab Data Interchange Fornat\cr
  CF_TIFF        \tab   6  \tab Tagged-Image File Format\cr
  CF_OEMTEXT     \tab   7  \tab Text in the OEM codepage\cr
  CF_DIB         \tab   8  \tab Device-Independent Bitmap\cr
  CF_PALETTE     \tab   9  \tab \cr
  CF_PENDATA     \tab   10 \tab \cr
  CF_RIFF        \tab   11 \tab Audio data\cr
  CF_WAVE        \tab   12 \tab Audio data\cr
  CF_UNICODETEXT \tab   13 \tab Text in Unicode (UCS-2)\cr
  CF_ENHMETAFILE \tab   14 \tab Enhanced metafile\cr
  CF_HDROP       \tab   15 \tab Drag-and-drop data\cr
  CF_LOCALE      \tab   16 \tab Locale for the text on the clipboard\cr
  CF_MAX         \tab   17 \tab Shell-oriented formats\cr
  }
  Applications normally make data available in one or more of these and
  possibly additional private formats.  Use \code{raw = TRUE} to read binary
  formats, \code{raw = FALSE} (the default) for text formats.  The
  current codepage is used to convert text to Unicode text, and
  information on that is contained in the \code{CF_LOCALE} format.
  (Take care if you are running R in a different locale from Windows.)

  The \code{writeClipboard} function will write a character vector as
  text or Unicode text with standard CR-LF line terminators.  It will
  copy a raw vector directly to the clipboard without any changes.
}
\value{
  For \code{getClipboardFormats}, a character or integer vector of
  available formats, in numeric order.  If non human-readable character
  representation is known, the number is returned.

  For \code{readClipboard}, a character vector by default, a raw vector
  if \code{raw} is \code{TRUE}, or \code{NULL}, if the format is
  unavailable.

  For \code{writeClipboard} an invisible logical indicating success or
  failure.
}
\note{
  This is only available on Windows.
}
\seealso{
  \code{\link{file}} which can be used to set up a connection to a clipboard.
}
\keyword{utilities}

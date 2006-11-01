\name{clipboard}
\alias{getClipboardFormats}
\alias{readClipboard}
\alias{writeClipboard}
\alias{clipboard}
\title{Read/Write to/from the Windows Clipboard}
\description{
  Transfer text between a character vector and the Windows clipboard.
}
\usage{
getClipboardFormats()
readClipboard(format = 1, raw = FALSE)
writeClipboard(str, format = 1)
}
\arguments{
  \item{format}{an integer giving the desired format.}
  \item{raw}{should the value be returned as a raw vector rather than a character vector?}
  \item{str}{a character vector or a raw vector.}
}
\value{
  For \code{getClipboardFormats}, an integer vector of available formats.

  For \code{readClipboard}, a character vector by default, a raw vector if \code{raw} is 
  \code{TRUE}, or \code{NULL}, if the format is unavailable.

  For \code{writeClipboard} an invisible logical indicating success or failure.
}
\details{
  The Windows clipboard offers data in a number of formats.  The standard formats are
  \tabular{lr}{
  CF\_TEXT	 \tab 	1 \cr
  CF\_BITMAP	 \tab 	2 \cr
  CF\_METAFILEPICT	 \tab 	3 \cr
  CF\_SYLK	 \tab 	4 \cr
  CF\_DIF	 \tab 	5 \cr
  CF\_TIFF	 \tab 	6 \cr
  CF\_OEMTEXT	 \tab 	7 \cr
  CF\_DIB	 \tab 	8 \cr
  CF\_PALETTE	 \tab 	9 \cr
  CF\_PENDATA	 \tab 	10 \cr
  CF\_RIFF	 \tab 	11 \cr
  CF\_WAVE	 \tab 	12 \cr
  CF\_UNICODETEXT	 \tab 	13 \cr
  CF\_ENHMETAFILE	 \tab 	14 \cr
  CF\_HDROP	 \tab 	15 \cr
  CF\_LOCALE	 \tab 	16 \cr
  CF\_MAX	 \tab 	17 \cr
  CF\_OWNERDISPLAY	 \tab 	128 \cr
  CF\_DSPTEXT	 \tab 	129 \cr
  CF\_DSPBITMAP	 \tab 	130 \cr
  CF\_DSPMETAFILEPICT	 \tab 	131 \cr
  CF\_DSPENHMETAFILE	 \tab 	142 \cr
  CF\_PRIVATEFIRST	 \tab 	512 \cr
  CF\_PRIVATELAST	 \tab 	767 \cr
  CF\_GDIOBJFIRST	 \tab 	768 \cr
  CF\_GDIOBJLAST	 \tab 	1023 \cr
}  
Applications normally make data available in one or more of these and possibly additional private
formats.  Use \code{raw=TRUE} to read binary formats, \code{raw=FALSE} (the default)
for text formats.

The \code{writeClipboard} function will write a character vector as text, with standard
CR-LF line terminators.  It will copy a raw vector directly to the clipboard without any changes.
}
\seealso{
  \code{\link{file}} which can be used to set up a connection to a clipboard.
}
\keyword{utilities}

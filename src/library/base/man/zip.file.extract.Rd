\name{zip.file.extract}
\alias{zip.file.extract}
\title{Extract File from a Zip Archive}
\usage{
zip.file.extract(file, zipname="R.zip")
}
\arguments{
 \item{file}{A file name.}
 \item{zipname}{The file name of a \code{zip} archive.}
}
\description{
    This will extract the file named \code{file} from the zip archive,
    if possible, and write it in a temporary location.
}
\details{
    The file will be extracted if it is in the archive and any required
    \code{unzip} utility is available. It will probably be extracted to
    the directory used by \code{tempfile}.
}
\value{
    The name of the original or extracted file.
}
\note{The implementation differs by platform: it might do nothing.}

\section{Warning}{This function is intended for internal use only:
    it may be altered at any time, and may differ between platforms. Do
    NOT use in user code.}

\author{B. D. Ripley}

\keyword{file}

\name{zip.file.extract}
\alias{zip.file.extract}
\title{Extract File from a Zip Archive}
\usage{
zip.file.extract(file, zipname = "R.zip")
}
\arguments{
 \item{file}{A file name.}
 \item{zipname}{The file name of a \code{zip} archive, including the
   \code{".zip"} extension if required.}
}
\description{
  This will extract the file named \code{file} from the zip archive,
  if possible, and write it in a temporary location.
}
\details{
  The method used is selected by \code{options(unzip=)}.
  
  The file will be extracted if it is in the archive and any required
  \code{unzip} utility is available.  It will probably be extracted to
  the directory used by \code{tempfile}, overwriting an existing file of
  that name.
}
\value{
  The name of the original or extracted file.
}
\note{
  The implementation differs by platform: it might do nothing.
  The \code{"internal"} method is very simple, and will not create
  parent directories nor set file dates.
}

\author{B. D. Ripley}

\keyword{file}

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
  The method used is selected by \code{options(unzip=)}. All platforms
  support an \code{"internal"} unzip: this is the default under
  Windows and MacOS, and the fall-back under Unix if no \code{unzip}
  program was found during configuration and \code{R\_UNZIPCMD} is not set.
  
  The file will be extracted if it is in the archive and any required
  \code{unzip} utility is available.  It will probably be extracted to
  the directory given by \code{tempdir}, overwriting an existing file of
  that name.
}
\value{
  The name of the original or extracted file.  Success is indicated by
  returning a different name.
}
\note{
  The \code{"internal"} method is very simple, and will not set file dates.
}

\keyword{file}

\name{download.file}
\alias{download.file}
\title{Download File from the Internet}
\description{
  This function can be used to download a file from the Internet either
  using a helper application such as \code{wget} or by making a direct
  socket connection.
}
\usage{
download.file(url, destfile, method = "auto", quiet=FALSE)
}
\arguments{
  \item{url}{A character string with the URL of a file to be downloaded.}
  \item{destfile}{A character string with the name where the downloaded
    file is saved.}
  \item{method}{Tool to be used for downloading files.  Currently
    download methods \code{"auto"}, \code{"wget"}, \code{"lynx"} 
    \code{"cp"} and \code{"socket"} are available.  To use \code{"wget"}
    or \code{"lynx"} the executable must be installed on your system and
    in your path. To use \code{"socket"} the file must be on an
    \code{HTTP} server, in plain text, and your system must allow socket
    connections to be opened to that server.}
  \item{quiet}{If \code{TRUE}, suppress status messages from the download
    tool (if any).}
}
\details{
  The function \code{download.file} can be used to download a single
  file as described by \code{url} from the internet and store it in
  \code{destfile}. It makes a system call to the tool given by
  \code{method}, the respective program must be installed on your system
  and be in the search path for executables.  If method \code{"cp"} is
  used, then the \code{url} must start with \code{"file:"} and give the
  path to a local file. Method \code{"auto"} (the default) searches for
  available tools at runtime.
  The \code{url} must start with a type specification such as
  \code{"http://"}, \code{"ftp://"} or \code{"file:"}.
}
\note{
  Cacheing proxies and firewalls may well not allow method \code{"socket"}
  socket to be used.
}
\keyword{utilities}

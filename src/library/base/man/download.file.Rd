\name{download.file}
\alias{download.file}
\title{Download File from the Internet}
\description{
  This function can be used to download a file from the Internet.
}
\usage{
download.file(url, destfile, method, quiet = FALSE, mode="w")
}
\arguments{
  \item{url}{A character string with the URL of a resource to be downloaded.}
  \item{destfile}{A character string with the name where the downloaded
    file is saved.  For the \code{"internal"} method tilde-expansion is
    performed.}
  \item{method}{Tool to be used for downloading files.  Currently
    download methods \code{"internal"}, \code{"wget"}, \code{"lynx"} 
    and \code{"socket"} are available.  To use \code{"wget"}
    or \code{"lynx"} the executable must be installed on your system and
    in your path. To use \code{"socket"} the file must be on an
    \code{HTTP} server, in plain text, and your system must allow socket
    connections to be opened to that server.}
  \item{quiet}{If \code{TRUE}, suppress status messages from the download
    tool (if any).}
  \item{mode}{character.  The mode with which to write the file. Useful
    values are \code{"w"}, \code{"wb"} (binary), \code{"a"} (append) and
    \code{"ab"}.  Only used for the \code{"internal"} method.}
}
\details{
  The function \code{download.file} can be used to download a single
  file as described by \code{url} from the internet and store it in
  \code{destfile}.
  The \code{url} must start with a type specification such as
  \code{"http://"}, \code{"ftp://"} or \code{"file://"}.

  For the \code{"internal"} method proxies can be specified via
  environment variables.  Setting \code{"no_proxy"} stops any proxy
  being tried.  Otherwise the setting of \code{"http_proxy"} or
  \code{"ftp_proxy"} (or failing that, the all upper-case version) is
  consulted and if non-empty used as a proxy site.
  For FTP transfers, the username and password on the proxy can be
  specified by \code{"ftp_proxy_user"} and \code{"ftp_proxy_password"}.

  For methods \code{"wget"} and \code{"lynx"} it makes a system call to
  the tool given by \code{method}, and the respective program must be
  installed on your system and be in the search path for executables.
}
\note{
  Cacheing proxies and firewalls may well not allow method \code{"socket"}
  socket to be used.
}
\keyword{utilities}

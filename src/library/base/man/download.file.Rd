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
  \item{method}{Method to be used for downloading files.  Currently
    download methods \code{"internal"}, \code{"wget"}, \code{"lynx"}
    and \code{"socket"} are available.  The default is to choose the
    first of these which is available, on almost all systems
    \code{"internal"}.}
  \item{quiet}{If \code{TRUE}, suppress status messages (if any).}
  \item{mode}{character.  The mode with which to write the file. Useful
    values are \code{"w"}, \code{"wb"} (binary), \code{"a"} (append) and
    \code{"ab"}.  Only used for the \code{"internal"} method.}
}
\details{
  The function \code{download.file} can be used to download a single
  file as described by \code{url} from the internet and store it in
  \code{destfile}.
  The \code{url} must start with a scheme such as
  \code{"http://"}, \code{"ftp://"} or \code{"file://"}.

  The remaining details apply to method \code{"internal"} only.

  The timeout for many parts of the transder can be set by the option
  \code{timeout} which defaults to 60 seconds.

  The level of detail provided during transfer can be set by the
  \code{quiet} argument and the \code{internet.info} option.  The
  details depend on the platform and scheme, but setting
  \code{internet.info} to 0 gives all available details, including
  all server responses. Using 2 (the default) gives only serious
  messages, and 3 or more suppresses all messages.

#ifdef windows
  A progress bar tracks the transfer. If the file length is known, the
  full width of the bar is the known length.  Otherwise the initial
  width represents 100Kbytes and is doubled whenever the current width
  is exceeded.

#endif
  Proxies can be specified via environment variables.
  Setting \code{"no_proxy"} stops any proxy being tried.
  Otherwise the setting of \code{"http_proxy"} or \code{"ftp_proxy"}
  (or failing that, the all upper-case version) is consulted and if
  non-empty used as a proxy site.  For FTP transfers, the username
  and password on the proxy can be specified by \code{"ftp_proxy_user"}
  and \code{"ftp_proxy_password"}.
#ifdef windows
  
  Alternatively, if \code{modules\\internet2.dll} is used to replace
  \code{modules\\internet.dll}, 
  the `Internet Options' of the system are used to choose proxies and so
  on; these are set in the Control Panel and are those used for Internet
  Explorer.
#endif
}
\note{
  Methods \code{"wget"}, \code{"lynx"} and \code{"socket"} are for
  historical compatibility.  They will block any other activity on the
  \R process.

  For methods \code{"wget"} and \code{"lynx"} a system call is made to
  the tool given by \code{method}, and the respective program must be
  installed on your system and be in the search path for executables.

  Method \code{"socket"} is deprecated.  To use it the file must be on
  an \code{HTTP} server, in plain text, and your system must allow
  socket connections to be opened directly to that server (and not
  via a proxy or firewall).
}
\seealso{
  \code{\link{options}} to set the \code{timeout} and
  \code{internet.info} options.

  \code{\link{url}} for a finer-grained way to read data from URLs.

  \code{\link{url.show}}, \code{\link{CRAN.packages}},
  \code{\link{download.packages}} for applications
}
\keyword{utilities}

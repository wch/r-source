\name{download.file}
\alias{download.file}
\title{Download File from the Internet}
\description{
  This function can be used to download a file from the Internet.
}
\usage{
download.file(url, destfile, method, quiet = FALSE, mode="w",
              cacheOK = TRUE)
}
\arguments{
  \item{url}{A character string naming the URL of a resource to be
    downloaded.}

  \item{destfile}{A character string with the name where the downloaded
    file is saved.  Tilde-expansion is performed.}

  \item{method}{Method to be used for downloading files.  Currently
    download methods \code{"internal"}, \code{"wget"} and \code{"lynx"}
    are available.  The default is to choose the first of these which
    will be \code{"internal"}. The method can also be set through the
    option \code{"download.file.method"}: see \code{\link{options}()}.} 

  \item{quiet}{If \code{TRUE}, suppress status messages (if any).}

  \item{mode}{character.  The mode with which to write the file. Useful
    values are \code{"w"}, \code{"wb"} (binary), \code{"a"} (append) and
    \code{"ab"}.  Only used for the \code{"internal"} method.}

  \item{cacheOK}{logical. Is a server-side cached value acceptable?
    Implemented for the \code{"internal"} and \code{"wget"} methods.}
}
\details{
  The function \code{download.file} can be used to download a single
  file as described by \code{url} from the internet and store it in
  \code{destfile}.
  The \code{url} must start with a scheme such as
  \code{"http://"}, \code{"ftp://"} or \code{"file://"}.

  \code{cacheOK = FALSE} is useful for \code{"http://"} URLs, and will
  attempt to get a copy directly from the site rather than from an
  intermediate cache.  (Not all platforms support it.)
  It is used by \code{\link{CRAN.packages}}.

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

  There is an alternative method if you have Internet Explorer 4 or
  later installed.   You can use the flag \code{--internet2}, when
  the `Internet Options' of the system are used to choose proxies and so
  on; these are set in the Control Panel and are those used for Internet
  Explorer.  This version does not support \code{cacheOK = FALSE}.

#endif
  Method \code{"wget"} can be used with proxy firewalls which require
  user/password authentication if proper values are stored in the
  configuration file for \code{wget}.
}
\note{
  Methods \code{"wget"} and \code{"lynx"} are for historical
  compatibility. They will block all other activity on the \R process.

  For methods \code{"wget"} and \code{"lynx"} a system call is made to
  the tool given by \code{method}, and the respective program must be
  installed on your system and be in the search path for executables.
}
\section{Setting Proxies}{
  This applies to the internal code only.

  Proxies can be specified via environment variables.
  Setting \code{"no_proxy"} stops any proxy being tried.
  Otherwise the setting of \code{"http_proxy"} or \code{"ftp_proxy"}
  (or failing that, the all upper-case version) is consulted and if
  non-empty used as a proxy site.  For FTP transfers, the username
  and password on the proxy can be specified by \code{"ftp_proxy_user"}
  and \code{"ftp_proxy_password"}.  The form of \code{"http_proxy"}
  should be \code{"http://proxy.dom.com/"} or
  \code{"http://proxy.dom.com:8080/"} where the port defaults to
  \code{80} and the trailing slash may be omitted. For
  \code{"ftp_proxy"} use the form \code{"ftp://proxy.dom.com:3128/"}
  where the default port is \code{21}.  These environment variables
  must be set before the download code is first used: they cannot be
  altered later by calling \code{Sys.putenv}.  
}
\seealso{
  \code{\link{options}} to set the \code{timeout} and
  \code{internet.info} options.

  \code{\link{url}} for a finer-grained way to read data from URLs.

  \code{\link{url.show}}, \code{\link{CRAN.packages}},
  \code{\link{download.packages}} for applications
}
\keyword{utilities}

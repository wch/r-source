% File src/library/utils/man/download.file.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2016 R Core Team
% Distributed under GPL 2 or later

\name{download.file}
\alias{download.file}
\concept{proxy}
\concept{ftp}
\concept{http}
\title{Download File from the Internet}
\description{
  This function can be used to download a file from the Internet.
}
\usage{
download.file(url, destfile, method, quiet = FALSE, mode = "w",
              cacheOK = TRUE,
              extra = getOption("download.file.extra"))
}
\arguments{
  \item{url}{A character string naming the URL of a resource to be
    downloaded.}

  \item{destfile}{A character string with the name where the downloaded
    file is saved.  Tilde-expansion is performed.}

  \item{method}{Method to be used for downloading files.  Current
    download methods are \code{"internal"}, \code{"wininet"} (Windows
    only) \code{"libcurl"}, \code{"wget"} and \code{"curl"}, and there
    is a value \code{"auto"}: see \sQuote{Details} and \sQuote{Note}.

    The method can also be set through the option
    \code{"download.file.method"}: see \code{\link{options}()}.
  }

  \item{quiet}{If \code{TRUE}, suppress status messages (if any), and
    the progress bar.}

  \item{mode}{character.  The mode with which to write the file.  Useful
    values are \code{"w"}, \code{"wb"} (binary), \code{"a"} (append) and
    \code{"ab"}.  Only used for the \code{"internal"} method.
#ifdef windows
    (See also \sQuote{Details}.)
#endif
  }
  \item{cacheOK}{logical.  Is a server-side cached value acceptable?}

  \item{extra}{character vector of additional command-line arguments for
    the \code{"wget"} and \code{"curl"} methods.}
}
\details{
  The function \code{download.file} can be used to download a single
  file as described by \code{url} from the internet and store it in
  \code{destfile}.
  The \code{url} must start with a scheme such as
  \samp{http://}, \samp{https://}, \samp{ftp://} or \samp{file://}.

#ifdef unix
  If \code{method = "auto"} is chosen (the default), on a Unix-alike
  method \code{"libcurl"} is chosen for \samp{https://} and
  \samp{ftps://} URLs and the \code{"internal"} method is chosen for
  other schemes.
#endif
#ifdef windows
  If \code{method = "auto"} is chosen (the default), on Windows the
  \code{"wininet"} method is used apart from for \samp{ftps://} URLs
  where \code{"libcurl"} is tried.  The \code{"wininet"} method uses the
  WinINet functions (part of the OS).
#endif
% https://msdn.microsoft.com/en-us/library/windows/desktop/aa383630%28v=vs.85%29.aspx
  
#ifdef unix
  Method \code{"libcurl"} uses the library of that name
  (\url{http://curl.haxx.se/libcurl/}).
  It provides
#endif
#ifdef windows
  Support for method \code{"libcurl"} is optional on Windows: use
  \code{\link{capabilities}("libcurl")} to see if it is supported on
  your build.  It uses an external library of that name
  (\url{http://curl.haxx.se/libcurl/}) against which \R can be compiled.
  If supported it will provide
#endif
  (non-blocking) access to \samp{https://} and (usually) \samp{ftps://}
  URLs.  There is support for simultaneous downloads, so \code{url} and
  \code{destfile} can be character vectors of the same length greater
  than one.  For a single URL and \code{quiet = FALSE} a progress
  bar is shown in interactive use.
  
  For methods \code{"wget"} and \code{"curl"} a system call is made to
  the tool given by \code{method}, and the respective program must be
  installed on your system and be in the search path for executables.
  They will block all other activity on the \R process until they
  complete: this may make a GUI unresponsive.

  \code{cacheOK = FALSE} is useful for \samp{http://} and
  \samp{https://} URLs: it will attempt to get a copy directly from the
  site rather than from an intermediate cache.  It is used by
  \code{\link{available.packages}}.

  The \code{"libcurl"} and \code{"wget"} methods follow \samp{http://}
  and \samp{https://} redirections: the \code{"internal"} method does not.
  (For method \code{"curl"} use argument \code{extra = "-L"}. To disable
  redirection in \command{wget}, use \code{extra = "--max-redirect=0"}.)
  (The \code{"wininet"} method supports some redirections but not all.)
  
  Note that \samp{https://} URLs are not supported by the
  \code{"internal"} method but are supported by the \code{"libcurl"}
  method and the \code{"wininet"} method on Windows.

  See \code{\link{url}} for how \samp{file://} URLs are interpreted,
  especially on Windows.  The \code{"internal"} and \code{"wininet"}
  methods do not percent-decode \samp{file://} URLs, but the
  \code{"libcurl"} and \code{"curl"} methods do: method \code{"wget"}
  does not support them.

  Most methods do not percent-encode special characters such as spaces
  in URLs (see \code{\link{URLencode}}), but it seems the
  \code{"wininet"} method does.

  The remaining details apply to the \code{"internal"}, \code{"wininet"}
  and \code{"libcurl"} methods only.
  
  The timeout for many parts of the transfer can be set by the option
  \code{timeout} which defaults to 60 seconds.

  The level of detail provided during transfer can be set by the
  \code{quiet} argument and the \code{internet.info} option: the details
  depend on the platform and scheme.  For the \code{"internal"} method
  setting option \code{internet.info} to 0 gives all available details,
  including all server responses.  Using 2 (the default) gives only
  serious messages, and 3 or more suppresses all messages.  For the
  \code{"libcurl"} method values of the option less than 2 give verbose
  output.

#ifdef windows
  A progress bar tracks the transfer.  If the file length is known, the
  full width of the bar is the known length.  Otherwise the initial
  width represents 100 Kbytes and is doubled whenever the current width
  is exceeded.  (In non-interactive use this uses a text version. If the
  file length is known, an equals sign represents 2\% of the transfer
  completed: otherwise a dot represents 10Kb.)
  

  If \code{mode} is not supplied and \code{url} ends in one of
  \code{.gz}, \code{.bz2}, \code{.xz}, \code{.tgz}, \code{.zip},
  \code{.rda} or \code{.RData} a binary transfer is done.  Since Windows
  (unlike Unix-alikes) does distinguish between text and binary files,
  care is needed that other binary file types are transferred with
  \code{mode = "wb"}.
#endif
#ifdef unix
  A progress bar tracks the transfer.  If the file length is known, an
  equals sign represents 2\% of the transfer completed: otherwise a dot
  represents 10Kb.

  Code written to download binary files must use \code{mode = "wb"}, but
  the problems incurred by a text transfer will only be seen on Windows.
#endif
}
\note{
   Files of more than 2GB are supported on 64-bit builds of \R; they
  may be truncated on some 32-bit builds.
  
  Methods \code{"wget"} and \code{"curl"} are mainly for historical
  compatibility but provide may provide capabilities not supported by
  the \code{"libcurl"} or \code{"wininet"} methods.

  Method \code{"wget"} can be used with proxy firewalls which require
  user/password authentication if proper values are stored in the
  configuration file for \code{wget}.

  \command{wget} (\url{http://www.gnu.org/software/wget/}) is commonly
  installed on Unix-alikes (but not OS X).  Windows binaries are
  available from Cygwin, gnuwin32 and elsewhere.

  \command{curl} (\url{http://curl.haxx.se/}) is installed on OS X and
  commonly on Unix-alikes.  Windows binaries are available at that URL.
}
\section{Setting Proxies}{
#ifdef windows
  For the Windows-only method \code{"wininet"}, the \sQuote{Internet
  Options} of the system are used to choose proxies and so on; these are
  set in the Control Panel and are those used for Internet Explorer.
#endif
  
  The next two paragraphs apply to the internal code only.

  Proxies can be specified via environment variables.
  Setting \env{no_proxy} to \code{*} stops any proxy being tried.
  Otherwise the setting of \env{http_proxy} or \env{ftp_proxy}
  (or failing that, the all upper-case version) is consulted and if
  non-empty used as a proxy site.  For FTP transfers, the username
  and password on the proxy can be specified by \env{ftp_proxy_user}
  and \env{ftp_proxy_password}.  The form of \env{http_proxy}
  should be \code{http://proxy.dom.com/} or
  \code{http://proxy.dom.com:8080/} where the port defaults to
  \code{80} and the trailing slash may be omitted. For
  \env{ftp_proxy} use the form \code{ftp://proxy.dom.com:3128/}
  where the default port is \code{21}.  These environment variables
  must be set before the download code is first used: they cannot be
  altered later by calling \code{\link{Sys.setenv}}.

  Usernames and passwords can be set for HTTP proxy transfers via
  environment variable \env{http_proxy_user} in the form
  \code{user:passwd}.  Alternatively, \env{http_proxy} can be of the
  form \code{http://user:pass@proxy.dom.com:8080/} for compatibility
  with \code{wget}.  Only the HTTP/1.0 basic authentication scheme is
  supported.
#ifdef windows
  Under Windows, if \env{http_proxy_user} is set to \code{ask} then
  a dialog box will come up for the user to enter the username and
  password.  \bold{NB:} you will be given only one opportunity to enter this,
  but if proxy authentication is required and fails there will be one
  further prompt per download.
#endif

  Much the same scheme is supported by \code{method = "libcurl"}, including
  \env{no_proxy}, \env{http_proxy} and \env{ftp_proxy}, and for the last
  two a contents of \code{[user:password@]machine[:port]} where the
  parts in brackets are optional.  See
  \url{http://curl.haxx.se/libcurl/c/libcurl-tutorial.html} for details.
}
\section{Secure URLs}{
  Methods which access \samp{https://} and \samp{ftps://} URLs should
  try to verify their certificates.  This is usually done using the CA
  root certificates installed by the OS (although we have seen instances
  in which these got removed rather than updated). For further information
  see \url{http://curl.haxx.se/docs/sslcerts.html}.

  This is an issue for \code{method = "libcurl"} on Windows, where the
  OS does not provide a suitable CA certificate bundle, so by default on
  Windows certificates are not verified.  To turn verification on, set
  environment variable \env{CURL_CA_BUNDLE} to the path to a certificate
  bundle file, usually named \file{ca-bundle.crt} or
  \file{curl-ca-bundle.crt}.  (This is normally done for a binary
  installation of \R, which installs
  \file{\var{R_HOME}/etc/curl-ca-bundle.crt} and sets
  \env{CURL_CA_BUNDLE} to point to it if that environment variable is not
  already set.)  For an updated certificate bundle, see
  \url{http://curl.haxx.se/docs/sslcerts.html}.
  Currently one can download a copy from
  \url{https://raw.githubusercontent.com/bagder/ca-bundle/master/ca-bundle.crt}
  and set \env{CURL_CA_BUNDLE} to the full path to the downloaded file.
  
  Note that the root certificates used by \R may or may not be the same
  as used in a browser, and indeed different browsers may use different
  certificate bundles (there is typically a build option to choose
  either their own or the system ones).
}
\section{FTP sites}{
  \samp{ftp:} URLs are accessed using the FTP protocol which has a
  number of variants.  One distinction is between \sQuote{active} and
  \sQuote{(extended) passive} modes: which is used is chosen by the
  client.  The \code{"internal"} and \code{"libcurl"} method use passive
  mode, and that is almost universally used by browsers.  Prior to \R
  3.2.3 the \code{"wininet"} method used active mode: since it first
  tries passive and then active.
}
\value{
  An (invisible) integer code, \code{0} for success and non-zero for
  failure.  For the \code{"wget"} and \code{"curl"} methods this is the
  status code returned by the external program.  The \code{"internal"}
  method can return \code{1}, but will in most cases throw an error.
}
\seealso{
  \code{\link{options}} to set the \code{HTTPUserAgent}, \code{timeout}
  and \code{internet.info} options used by some of the methods.

  \code{\link{url}} for a finer-grained way to read data from URLs.

  \code{\link{url.show}}, \code{\link{available.packages}},
  \code{\link{download.packages}} for applications.

  Contributed package \CRANpkg{RCurl} provides more comprehensive
  facilities to download from URLs.
}
\keyword{utilities}

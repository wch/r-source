% File src/library/utils/man/download.file.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2024 R Core Team
% Distributed under GPL 2 or later

\name{download.file}
\alias{download.file}
\concept{proxy}
\concept{\I{ftp}}
\concept{\I{http}}
\title{Download File from the Internet}
\description{
  This function can be used to download a file from the Internet.
}
\usage{
download.file(url, destfile, method, quiet = FALSE, mode = "w",
              cacheOK = TRUE,
              extra = getOption("download.file.extra"),
              headers = NULL, \dots)
}
\arguments{
  \item{url}{a \code{\link{character}} string (or longer vector
    for the \code{"libcurl"} method) naming the URL of a resource to be
    downloaded.}

  \item{destfile}{a character string (or vector, see the \code{url}
    argument) with the file path where the downloaded file is to be
    saved.  Tilde-expansion is performed.}

  \item{method}{Method to be used for downloading files.  Current
    download methods are \code{"internal"}, \code{"libcurl"},
    \code{"wget"}, \code{"curl"} and \code{"wininet"} (Windows
    only), and there is a value \code{"auto"}: see \sQuote{Details} and
    \sQuote{Note}.

    The method can also be set through the option
    \code{"download.file.method"}: see \code{\link{options}()}.
  }

  \item{quiet}{If \code{TRUE}, suppress status messages (if any), and
    the progress bar.}

  \item{mode}{character.  The mode with which to write the file.  Useful
    values are \code{"w"}, \code{"wb"} (binary), \code{"a"} (append) and
    \code{"ab"}.  Not used for methods \code{"wget"} and \code{"curl"}.
    See also \sQuote{Details}, notably about using \code{"wb"} for Windows.
  }
  \item{cacheOK}{logical.  Is a server-side cached value acceptable?}

  \item{extra}{character vector of additional command-line arguments for
    the \code{"wget"} and \code{"curl"} methods.}

  \item{headers}{named character vector of additional HTTP headers to
    use in HTTP[S] requests.  It is ignored for non-HTTP[S] URLs.  The
    \code{User-Agent} header taken from the \code{HTTPUserAgent} option
    (see \code{\link{options}}) is automatically used as the first header.}

  \item{\dots}{allow additional arguments to be passed, unused.}
}
\details{
  The function \code{download.file} can be used to download a single
  file as described by \code{url} from the internet and store it in
  \code{destfile}.

  The \code{url} must start with a scheme such as \samp{http://},
  \samp{https://} or \samp{file://}.  Which methods support which
  schemes varies by \R version, but \code{method = "auto"} will try to
  find a method which supports the scheme.

  For \code{method = "auto"} (the default) currently the
  \code{"internal"} method is used for \samp{file://} URLs and
  \code{"libcurl"} for all others.

  Support for method \code{"libcurl"} was optional on Windows prior to
  \R 4.2.0: use \code{\link{capabilities}("libcurl")} to see if it is
  supported on an earlier version.  It uses an external library of that
  name (\url{https://curl.se/libcurl/}) against which \R can be
  compiled.

  When method \code{"libcurl"} is used, there is support for
  simultaneous downloads, so \code{url} and \code{destfile} can be
  character vectors of the same length greater than one (but the method
  has to be specified explicitly and not \emph{via} \code{"auto"}).  For
  a single URL and \code{quiet = FALSE} a progress bar is shown in
  interactive use.

  Nowadays the \code{"internal"} method only supports the \samp{file://}
  scheme (for which it is the default).  On Windows the \code{"wininet"}
  method currently supports \samp{file://} and (but deprecated with a
  warning) \samp{http://} and \samp{https://} schemes.

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
  and \samp{https://} redirections to any scheme they support.  (For
  method \code{"curl"} use argument \code{extra = "-L"}.  To disable
  redirection in \command{wget}, use \code{extra = "--max-redirect=0"}.)
  The \code{"wininet"} method supports some redirections but not all.
  (For method \code{"libcurl"}, messages will quote the endpoint of
  redirections.)

  See \code{\link{url}} for how \samp{file://} URLs are interpreted,
  especially on Windows.  The \code{"internal"} and \code{"wininet"}
  methods do not percent-decode, but the \code{"libcurl"} and
  \code{"curl"} methods do: method \code{"wget"} does not support them.

  Most methods do not percent-encode special characters such as spaces
  in URLs (see \code{\link{URLencode}}), but it seems the
  \code{"wininet"} method does.

  The remaining details apply to the \code{"wininet"} and
  \code{"libcurl"} methods only.

  The timeout for many parts of the transfer can be set by the option
  \code{timeout} which defaults to 60 seconds.  This is often
  insufficient for downloads of large files (50MB or more) and
  so should be increased when \code{download.file} is used in packages
  to do so.  Note that the user can set the default timeout by the
  environment variable \env{R_DEFAULT_INTERNET_TIMEOUT} in recent
  versions of \R, so to ensure that this is not decreased packages should
  use something like
  \preformatted{
    options(timeout = max(300, getOption("timeout")))
  }
  (It is unrealistic to require download times of less than 1s/MB.)

  The level of detail provided during transfer can be set by the
  \code{quiet} argument and the \code{internet.info} option: the details
  depend on the platform and scheme.  For the \code{"libcurl"} method
  values of the option less than 2 give verbose output.

  A progress bar tracks the transfer platform-specifically:
  \describe{
    \item{On Windows}{If the file length is known, the
      full width of the bar is the known length.  Otherwise the initial
      width represents 100 \abbr{Kbyte}s and is doubled whenever the current width
      is exceeded.  (In non-interactive use this uses a text version.  If the
      file length is known, an equals sign represents 2\% of the transfer
      completed: otherwise a dot represents 10Kb.)}
    \item{On a Unix-alike}{If the file length is known, an
      equals sign represents 2\% of the transfer completed: otherwise a dot
      represents 10Kb.}
  }


  The choice of binary transfer (\code{mode = "wb"} or \code{"ab"}) is
  important on Windows, since unlike Unix-alikes it does distinguish
  between text and binary files and for text transfers changes \samp{\\n}
  line endings to \samp{\\r\\n} (aka \file{CRLF}).

  On Windows, if \code{mode} is not supplied (\code{\link{missing}()})
  and \code{url} ends in one of \samp{.gz}, \samp{.bz2}, \samp{.xz},
  \samp{.tgz}, \samp{.zip}, \samp{.jar}, \samp{.rda}, \samp{.rds},
  \samp{.RData} or \samp{.pdf}, \code{mode = "wb"} is set so that a binary
  transfer is done to help unwary users.

  Code written to download binary files must use \code{mode = "wb"} (or
  \code{"ab"}), but the problems incurred by a text transfer will only
  be seen on Windows.
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

  \command{wget} (\url{https://www.gnu.org/software/wget/}) is commonly
  installed on Unix-alikes (but not macOS).  Windows binaries are
  available from \abbr{MSYS2} and elsewhere.

  \command{curl} (\url{https://curl.se/}) is installed on macOS and
  increasingly commonly on Unix-alikes.  Windows binaries are available
  at that URL.
}
\section{Setting Proxies}{
  For the Windows-only method \code{"wininet"}, the \sQuote{Internet
  Options} of the system are used to choose proxies and so on; these are
  set in the Control Panel and are those used for system browsers.

  For the \code{"libcurl"} and \code{"curl"} methods, proxies can be set
  \emph{via} the environment variables \env{http_proxy} or
  \env{ftp_proxy}.  See
  \url{https://curl.se/libcurl/c/libcurl-tutorial.html} for further
  details.
}
\section{Secure URLs}{
  Methods which access \samp{https://} and (where
  supported) \samp{ftps://} URLs should try to verify the site
  certificates.  This is usually done using the CA root certificates
  installed by the OS (although we have seen instances in which these
  got removed rather than updated). For further information see
  \url{https://curl.se/docs/sslcerts.html}.

  On Windows with \code{method = "libcurl"}, the CA root certificates
  are provided by the OS when \R was linked with \code{libcurl} with
  \code{Schannel} enabled, which is the current default in \I{Rtools}.  This
  can be verified by checking that \code{libcurlVersion()} returns a
  version string containing \samp{"Schannel"}.  If it does not, for
  verification to be on the environment variable \env{CURL_CA_BUNDLE}
  must be set to a path to a certificate bundle file, usually named
  \file{ca-bundle.crt} or \file{curl-ca-bundle.crt}.  (This is normally
  done automatically for a binary installation of \R, which installs
  \file{\var{R_HOME}/etc/curl-ca-bundle.crt} and sets
  \env{CURL_CA_BUNDLE} to point to it if that environment variable is
  not already set.) For an updated certificate bundle, see
  \url{https://curl.se/docs/sslcerts.html}.  Currently one can download
  a copy from
  \url{https://raw.githubusercontent.com/bagder/ca-bundle/master/ca-bundle.crt}
  and set \env{CURL_CA_BUNDLE} to the full path to the downloaded file.

  On Windows with \code{method = "libcurl"}, when \R was linked with
  \code{libcurl} with \code{Schannel} enabled, the connection fails if it
  cannot be established that the certificate has not been revoked.  Some
  \abbr{MITM} proxies present particularly in corporate environments do not work
  with this behavior. It can be changed by setting environment variable
  \env{R_LIBCURL_SSL_REVOKE_BEST_EFFORT} to \code{TRUE}, with the
  consequence of reducing security.

  Note that the root certificates used by \R may or may not be the same
  as used in a browser, and indeed different browsers may use different
  certificate bundles (there is typically a build option to choose
  either their own or the system ones).
}
\section{Good practice}{
  Setting the \code{method} should be left to the end user.  Neither of
  the \command{wget} nor \command{curl} commands is widely available:
  you can check if one is available \emph{via} \code{\link{Sys.which}},
  and should do so in a package or script.

  If you use \code{download.file} in a package or script, you must check
  the return value, since it is possible that the download will fail
  with a non-zero status but not an \R error.

  The supported \code{method}s do change: method \code{libcurl} was
  introduced in \R 3.2.0 and was optional on Windows until \R 4.2.0 --
  use \code{\link{capabilities}("libcurl")} in a program to see if it is
  available.
}

\section{\samp{ftp://} URLs}{
  Most modern browsers do not support such URLs, and \samp{https://}
  ones are much preferred for use in \R. \samp{ftps://} URLs have always
  been rare, and are nowadays even less supported.

  It is intended that \R will continue to allow such URLs for as long as
  \code{libcurl} does, but as they become rarer this is increasingly
  untested.   What \sQuote{protocols} the version of \code{libcurl}
  being used supports can be seen by calling \code{\link{libcurlVersion}()}.

  These URLs are accessed using the FTP protocol which has a
  number of variants.  One distinction is between \sQuote{active} and
  \sQuote{(extended) passive} modes: which is used is chosen by the
  client.  The \code{"libcurl"} method uses passive mode which was
  almost universally used by browsers before they dropped support
  altogether.
}

\value{
  An (invisible) integer code, \code{0} for success and non-zero for
  failure.  For the \code{"wget"} and \code{"curl"} methods this is the
  status code returned by the external program.  The \code{"internal"}
  method can return \code{1}, but will in most cases throw an error.  When
  simultaneously downloading two or more files (see the \code{url} argument)
  and download of at least one file succeeds, \code{0} is returned with
  attribute \code{retvals}, which provides an integer vector of the same
  length as \code{url} with a result code for each file (\code{0} for
  success and non-zero for failure).

  What happens to the destination file(s) in the case of error depends
  on the method and \R{} version. Currently the \code{"internal"},
  \code{"wininet"} and \code{"libcurl"} methods will remove the file if
  the URL is unavailable except when \code{mode} specifies
  appending when the file should be unchanged.
}
\seealso{
  \code{\link{options}} to set the \code{HTTPUserAgent}, \code{timeout}
  and \code{internet.info} options used by some of the methods.

  \code{\link{url}} for a finer-grained way to read data from URLs.

  \code{\link{url.show}}, \code{\link{available.packages}},
  \code{\link{download.packages}} for applications.

  Contributed packages \CRANpkg{RCurl} and \CRANpkg{curl} provide more
  comprehensive facilities to download from URLs.
}
\keyword{utilities}

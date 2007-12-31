% File src/library/base/man/EnvVar.Rd
% Part of the R package, http://www.R-project.org
% Copyright 2007 R Core Development Team
% Distributed under GPL 2 or later

\name{EnvVar}
\alias{environment variables}

\title{Environment Variables}
\description{
  Details of some of the environment variables which affect an \R session.
}
\details{
  It is impossible to list all the environment variables which can
  affect an \R session: some affect the OS system functions which \R
  uses, and others will affect add-on packages.  But here are notes on
  some of the more important ones.
  \describe{
    \item{\code{HOME}:}{The user's \sQuote{home} directory.}
    \item{\code{LANGUAGE}:}{Optional.  The language(s) to be used for
      message translations.}
    \item{\code{LC_ALL}:}{(etc) Optional.  Use to set various aspects of
      the locale -- see \code{\link{Sys.getlocale}}.}
    \item{\code{R_BATCH}:}{Optional -- set in a batch session.}
    \item{\code{R_COMPLETION}:}{Optional.  If set to \code{FALSE},
      comand-line completion is not used.  (Not used by Mac OS GUI.)}
    \item{\code{R_DEFAULT_PACKAGES}:}{A comma-separated list of packages
      which are to be loaded in every session.  See \code{\link{options}}.}
    \item{\code{R_ENVIRON}:}{Optional.  The path to the site environment
      file: see \link{Startup}.}
    \item{\code{R_GSCMD}:}{Optional.  The path to GhostScript.}
    \item{\code{R_HISTFILE}:}{Optional.  The path of the history file:
      see \link{Startup}.}
    \item{\code{R_HISTSIZE}:}{Optional.  The maximum size of the history
      file, in lines.}
    \item{\code{R_HOME}:}{The top-level directory of the \R
      installation.  Set by \R.}
    \item{\code{R_LIBS}:}{Optional.  Setting for \code{\link{.libPaths}}.}
    \item{\code{R_LIBS_SITE}:}{Optional.  Setting for \code{\link{.libPaths}}.}
    \item{\code{R_LIBS_USER}:}{Optional.  Setting for \code{\link{.libPaths}}.}
    \item{\code{R_PAPERSIZE}:}{Optional.  The default papersize,
      e.g. for \code{\link{pdf}} and \code{\link{postscript}}.}
    \item{\code{R_PROFILE}:}{Optional.  The path to the site profile
      file: see \link{Startup}.}
    \item{\code{R_RD4DVI}:}{Options for \code{latex} processing of
      \code{Rd} files.}
    \item{\code{R_RD4PDF}:}{Options for \code{pdflatex} processing of
      \code{Rd} files.}
    \item{\code{TMPDIR}, \code{TMP}, \code{TEMP}:}{Consulted (in that
      order) when setting the temporary directory.  See
      \code{\link{tempdir}}.}
    \item{\code{TZ}:}{Optional. The current timezone.  See
      \code{\link{as.POSIXlt}} for the system-specific formats.}
  }
}

#ifdef unix
\section{Unix-specific}{
  Some variables set on Unix-alikes, and not (in general) on Windows.
  \describe{
    \item{\code{DISPLAY}:}{Optional: used by \code{\link{X11}}, Tk (in
      package \pkg{tcltk}), the data editor and various packages.}
    \item{\code{EDITOR}:}{The path to default editor.}
    \item{\code{PAGER}:}{The path to default pager.}
    \item{\code{R_BROWSER}:}{The path to default browser.}
    \item{\code{R_DOC_DIR}:}{The location of the \R \code{doc} directory.}
    \item{\code{R_DVIPSCMD}:}{The path to \code{dvips}.}
    \item{\code{R_INCLUDE_DIR}:}{The location of the \R \code{include}
      directory.}
    \item{\code{R_LATEXCMD}:}{The path to \code{latex}.}
    \item{\code{R_MAKEINDEXCMD}:}{The path to \code{makeindex}.}
    \item{\code{R_PDFVIEWER}:}{The path to default PDF viewer.}
    \item{\code{R_PLATFORM}:}{The platform -- a string of the form
    \code{\var{cpu}-\var{vendor}-\var{os}}, see \code{\link{R.Version}}.}
    \item{\code{R_PRINTCMD}:}{Print command to be used by
      \code{\link{postscript}}.}
    \item{\code{R_SHARE_DIR}:}{The location of the \R \code{share} directory.}
    \item{\code{R_UNZIPCMD}:}{The path to \code{unzip}.}
    \item{\code{R_ZIPCMD}:}{The path to \code{zip}.}
    \item{\code{\code{no_proxy}, \code{http_proxy}, \code{ftp_proxy}}:}{
      (and more).  Optional.  Settings for \code{\link{download.file}}.}
  }
}
#endif

#ifdef windows
\section{Windows-specific}{
  Some Windows-specific variables are
  \describe{
    \item{\code{GSC}:}{Optional: the path to GhostScript, used if
      \env{R\_GSCMD} is not set.}
    \item{\code{R_USER}:}{The user's \sQuote{home} directory.  Set by \R.}
    \item{\code{TZDIR}:}{Optional. The top-level directory of the
      timezone database.  See \code{\link{as.POSIXlt}}.}
  }
}
#endif

\seealso{
  \code{\link{Sys.getenv}} and \code{\link{Sys.setenv}} to read and set
  environmental variables in an \R session.
}
\keyword{utilities}

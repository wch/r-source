% File src/library/base/man/EnvVar.Rd
% Part of the R package, http://www.R-project.org
% Copyright 2007-2014 R Core Team
% Distributed under GPL 2 or later

\name{EnvVar}
\alias{environment variables}
\alias{HOME}
\alias{LANGUAGE}
\alias{R_BATCH}
\alias{R_BROWSER}
\alias{R_COMPLETION}
\alias{R_DOC_DIR}
\alias{R_GSCMD}
\alias{R_HISTFILE}
\alias{R_HISTSIZE}
\alias{R_INCLUDE_DIR}
\alias{MAKEINDEX}
\alias{R_PAPERSIZE}
\alias{R_PDFVIEWER}
\alias{R_PLATFORM}
\alias{R_RD4PDF}
\alias{R_SHARE_DIR}
\alias{R_TEXI2DVICMD}
\alias{R_UNZIPCMD}
\alias{R_ZIPCMD}
\alias{TMPDIR}

% R_PROFILE, R_ENVIRON are in ?Startup
% R_LIBS* are in ?.libPaths
% TZ, TZDIR are in ?Sys.timezone
% R_HOME is in ?R.home.

#ifdef unix
\alias{DISPLAY}
\alias{EDITOR}
\alias{PAGER}
\alias{R_PRINTCMD}
#endif

#ifdef windows
\alias{GSC}
\alias{R_USER}
#endif


\title{Environment Variables}
\description{
  Details of some of the environment variables which affect an \R session.
}
\details{
  It is impossible to list all the environment variables which can
  affect an \R session: some affect the OS system functions which \R
  uses, and others will affect add-on packages.  But here are notes on
  some of the more important ones.  Those that set the defaults for
  options are consulted only at startup (as are some of the others).
  \describe{
    \item{\env{HOME}:}{The user's \sQuote{home} directory.}
    \item{\env{LANGUAGE}:}{Optional.  The language(s) to be used for
      message translations.  This is consulted when needed.}
    \item{\env{LC_ALL}:}{(etc) Optional.  Use to set various aspects of
      the locale -- see \code{\link{Sys.getlocale}}.  Consulted at startup.}
    \item{\env{MAKEINDEX}:}{The path to \command{makeindex}.
      If unset to a value determined when \R was built.
      Used by the emulation mode of \code{\link{texi2dvi}} and
      \code{\link{texi2pdf}}.}
    \item{\env{R_BATCH}:}{Optional -- set in a batch session, that is
      one started by \command{R CMD \link{BATCH}}.  Most often set to
      \code{""}, so test by something like
      \code{!is.na(\link{Sys.getenv}("R_BATCH", NA))}.}
    \item{\env{R_BROWSER}:}{The path to the default browser.  Used to
      set the default value of \code{\link{options}("browser")}.}
    \item{\env{R_COMPLETION}:}{Optional.  If set to \code{FALSE},
      command-line completion is not used.  (Not used by OS X GUI.)}
    \item{\env{R_DEFAULT_PACKAGES}:}{A comma-separated list of packages
      which are to be attached in every session.  See \code{\link{options}}.}
    \item{\env{R_DOC_DIR}:}{The location of the \R \file{doc}
      directory.  Set by \R.}
    \item{\env{R_ENVIRON}:}{Optional.  The path to the site environment
      file: see \link{Startup}.  Consulted at startup.}
    \item{\env{R_GSCMD}:}{Optional.  The path to Ghostscript, used by
      \code{\link{dev2bitmap}}, \code{\link{bitmap}} and
      \code{\link{embedFonts}}.  Consulted when those functions are
      invoked.  Since it will be treated as if passed to
      \code{\link{system}}, spaces and shell metacharacters should be escaped.}
    \item{\env{R_HISTFILE}:}{Optional.  The path of the history file:
      see \link{Startup}.  Consulted at startup and when the history is
      saved.}
    \item{\env{R_HISTSIZE}:}{Optional.  The maximum size of the history
      file, in lines.  Exactly how this is used depends on the
      interface.
#ifdef unix
      For the \code{readline} command-line interface it takes effect
      when the history is saved (by \code{\link{savehistory}} or at the
      end of a session).
#endif
#ifdef windows
      For \command{Rgui} it controls the number of lines saved to the
      history file: the size of the history used in the session is
      controlled by the console customization: see \code{\link{Rconsole}}.
#endif
    }
    \item{\env{R_HOME}:}{The top-level directory of the \R
      installation: see \code{\link{R.home}}.  Set by \R.}
    \item{\env{R_INCLUDE_DIR}:}{The location of the \R \file{include}
      directory.  Set by \R.}
    \item{\env{R_LIBS}:}{Optional.  Used for initial setting of
      \code{\link{.libPaths}}.}
    \item{\env{R_LIBS_SITE}:}{Optional.  Used for initial setting of
      \code{\link{.libPaths}}.}
    \item{\env{R_LIBS_USER}:}{Optional.  Used for initial setting of
      \code{\link{.libPaths}}.}
    \item{\env{R_PAPERSIZE}:}{Optional.  Used to set the default for
      \code{\link{options}("papersize")}, e.g.\sspace{}used by
      \code{\link{pdf}} and \code{\link{postscript}}.}
    \item{\env{R_PDFVIEWER}:}{The path to the default PDF viewer.  Used
      by \command{R CMD Rd2pdf}.}
    \item{\env{R_PLATFORM}:}{The platform -- a string of the form
      \code{\var{cpu}-\var{vendor}-\var{os}}, see \code{\link{R.Version}}.}
    \item{\env{R_PROFILE}:}{Optional.  The path to the site profile
      file: see \link{Startup}.  Consulted at startup.}
    \item{\env{R_RD4PDF}:}{Options for \command{pdflatex} processing of
      \code{Rd} files.  Used by \command{R CMD Rd2pdf}.}
    \item{\env{R_SHARE_DIR}:}{The location of the \R \file{share}
      directory.  Set by \R.}
    \item{\env{R_TEXI2DVICMD}:}{The path to \command{texi2dvi}.
      Defaults to the value of \env{TEXI2DVI}, and if that is unset to a
      value determined when \R was built.
#ifdef unix
      Consulted at startup to set the default for
      \code{\link{options}("texi2dvi")}, used by
      \code{\link{texi2dvi}} and \code{\link{texi2pdf}} in package \pkg{tools}.
#endif
    }
    \item{\env{R_UNZIPCMD}:}{The path to \command{unzip}.  Sets the
      initial value for \code{\link{options}("unzip")} on a Unix-alike
      when namespace \pkg{utils} is loaded.}
    \item{\env{R_ZIPCMD}:}{The path to \command{zip}. Used by
      \code{\link{zip}} and by \command{R CMD INSTALL --build} on Windows.}
    \item{\env{TMPDIR}, \env{TMP}, \env{TEMP}:}{Consulted (in that
      order) when setting the temporary directory for the session: see
      \code{\link{tempdir}}.  \env{TMPDIR} is also used by some of the
      utilities see the help for \code{\link{build}}.}
    \item{\env{TZ}:}{Optional.  The current time zone.  See
      \code{\link{Sys.timezone}} for the system-specific
      formats.  Consulted as needed.}
    \item{\env{no_proxy}, \env{http_proxy}, \env{ftp_proxy}:}{
      (and more).  Optional.  Settings for \code{\link{download.file}}:
      see its help for further details.}
  }
}

#ifdef unix
\section{Unix-specific}{
  Some variables set on Unix-alikes, and not (in general) on Windows.
  \describe{
    \item{\env{DISPLAY}:}{Optional: used by \code{\link{X11}}, Tk (in
      package \pkg{tcltk}), the data editor and various packages.}
    \item{\env{EDITOR}:}{The path to the default editor: sets the
      default for \code{\link{options}("editor")} when namespace
      \pkg{utils} is loaded.}
    \item{\env{PAGER}:}{The path to the pager with the default setting of
      \code{\link{options}("pager")}. The default value is chosen at
      configuration, usually as the path to \command{less}.}
    \item{\env{R_PRINTCMD}:}{Sets the default for
      \code{\link{options}("printcmd")}, which sets the default print
      command to be used by \code{\link{postscript}}.}
  }
}
#endif

#ifdef windows
\section{Windows-specific}{
  Some Windows-specific variables are
  \describe{
    \item{\env{GSC}:}{Optional: the path to Ghostscript, used if
      \env{R_GSCMD} is not set.}
    \item{\env{R_USER}:}{The user's \sQuote{home} directory.  Set by
      \R.  (\env{HOME} will be set to the same value if not already set.)}
    \item{\env{TZDIR}:}{Optional.  The top-level directory of the
      time-zone database.  See \code{\link{Sys.timezone}}.}
  }
}
#endif

\seealso{
  \code{\link{Sys.getenv}} and \code{\link{Sys.setenv}} to read and set
  environmental variables in an \R session.

  \code{\link{gctorture}} for environment variables controlling garbage
  collection.
}
\keyword{utilities}

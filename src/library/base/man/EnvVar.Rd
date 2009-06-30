% File src/library/base/man/EnvVar.Rd
% Part of the R package, http://www.R-project.org
% Copyright 2007-2009 R Core Development Team
% Distributed under GPL 2 or later

\name{EnvVar}
\alias{environment variables}
\alias{HOME}
\alias{LANGUAGE}
\alias{R_BATCH}
\alias{R_BROWSER}
\alias{R_COMPLETION}
\alias{R_DOC_DIR}
\alias{R_DVIPSCMD}
\alias{R_GSCMD}
\alias{R_HISTFILE}
\alias{R_HISTSIZE}
\alias{R_INCLUDE_DIR}
\alias{R_LATEXCMD}
\alias{R_MAKEINDEXCMD}
\alias{R_PAPERSIZE}
\alias{R_PDFLATEXCMD}
\alias{R_PDFVIEWER}
\alias{R_PLATFORM}
\alias{R_RD4DVI}
\alias{R_RD4PDF}
\alias{R_SHARE_DIR}
\alias{R_TEXI2DVICMD}
\alias{R_UNZIPCMD}
\alias{R_ZIPCMD}

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
    \item{\code{HOME}:}{The user's \sQuote{home} directory.}
    \item{\code{LANGUAGE}:}{Optional.  The language(s) to be used for
      message translations.  This is consulted when needed.}
    \item{\code{LC_ALL}:}{(etc) Optional.  Use to set various aspects of
      the locale -- see \code{\link{Sys.getlocale}}.  Consulted at startup.}
    \item{\code{R_BATCH}:}{Optional -- set in a batch session.}
    \item{\code{R_BROWSER}:}{The path to the default browser.  Used to
      set the default value of \code{\link{options}("browser")}.}
    \item{\code{R_COMPLETION}:}{Optional.  If set to \code{FALSE},
      command-line completion is not used.  (Not used by Mac OS GUI.)}
    \item{\code{R_DEFAULT_PACKAGES}:}{A comma-separated list of packages
      which are to be loaded in every session.  See \code{\link{options}}.}
    \item{\code{R_DOC_DIR}:}{The location of the \R \file{doc}
      directory.  Set by \R.}
    \item{\code{R_DVIPSCMD}:}{The path to \code{dvips}.  Defaults to
      the value of \env{DVIPS}, and if that is unset to a value
      determined when \R was built.
#ifdef unix
      Used by \code{R CMD Rd2dvi}, and at startup to set the default for
      \code{\link{options}("latexcmd")}, used by
      \code{\link{help}(offline = TRUE}.
#endif
#ifdef windows
      Used by \code{R CMD Rd2dvi}.
#endif
    }
    \item{\code{R_ENVIRON}:}{Optional.  The path to the site environment
      file: see \link{Startup}.  Consulted at startup.}
    \item{\code{R_GSCMD}:}{Optional.  The path to GhostScript, used by
      \code{\link{dev2bitmap}}.}
    \item{\code{R_HISTFILE}:}{Optional.  The path of the history file:
      see \link{Startup}.  Consulted at startup and when the history is
      saved.}
    \item{\code{R_HISTSIZE}:}{Optional.  The maximum size of the history
      file, in lines.  Exactly how this is used depends on the
      interface.
#ifdef unix
      For the \code{readline} command-line interface it takes effect
      when the history is saved (by \code{\link{savehistory}} or at the
      end of a session).
#endif
#ifdef windows
      For \code{Rgui} it controls the number of lines saved to the
      history file: the size of the history when in use is controlled by
      the console customization: see \code{\link[utils]{Rconsole}}.
#endif
    }
    \item{\code{R_HOME}:}{The top-level directory of the \R
      installation: see \code{\link{R.home}}.  Set by \R.}
    \item{\code{R_INCLUDE_DIR}:}{The location of the \R \file{include}
      directory.  Set by \R.}
    \item{\code{R_LATEXCMD}:}{The path to \code{latex}. Defaults to
      the value of \env{LATEX}, and if that is unset to a value
      determined when \R was built.
#ifdef unix
      Used by \code{R CMD Rd2dvi}, and at startup to set the default for
      \code{\link{options}("latexcmd")}, used by
      \code{\link{help}(offline = TRUE}.
#endif
#ifdef windows
      Used by \code{R CMD Rd2dvi}.
#endif
      }
    \item{\code{R_LIBS}:}{Optional.  Used for initial setting of
      \code{\link{.libPaths}}.}
    \item{\code{R_LIBS_SITE}:}{Optional.  Used for initial setting of
      \code{\link{.libPaths}}.}
    \item{\code{R_LIBS_USER}:}{Optional.  Used for initial setting of
      \code{\link{.libPaths}}.}
    \item{\code{R_MAKEINDEXCMD}:}{The path to \code{makeindex}.  Defaults
      at startup to the value of \env{MAKEINDEX}, and if that is unset
      to a value determined when \R was built.  Used by \code{R CMD Rd2dvi}.}
    \item{\code{R_PAPERSIZE}:}{Optional.  Use to set the default for
      \code{\link{options}("papersize")}, e.g. used by
      \code{\link{pdf}}, \code{\link{postscript}} and
      \code{\link{help}(offline = TRUE)}.}
    \item{\code{R_PDFLATEXCMD}:}{The path to \code{pdflatex}.  Defaults
      at startup to the value of \code{PDFLATEX}, and if that is unset
      to a value determined when \R was built.  Used by \code{R CMD Rd2dvi}.}
    \item{\code{R_PDFVIEWER}:}{The path to the default PDF viewer.  Used
      by \code{R CMD Rd2dvi}.}
    \item{\code{R_PLATFORM}:}{The platform -- a string of the form
      \code{\var{cpu}-\var{vendor}-\var{os}}, see \code{\link{R.Version}}.}
    \item{\code{R_PROFILE}:}{Optional.  The path to the site profile
      file: see \link{Startup}.  Consulted at startup.}
    \item{\code{R_RD4DVI}:}{Options for \code{latex} processing of
      \code{Rd} files.  Used by \code{R CMD Rd2dvi} and
      \code{\link{help}(offline = TRUE)}.}
    \item{\code{R_RD4PDF}:}{Options for \code{pdflatex} processing of
      \code{Rd} files.  Used by \code{R CMD Rd2dvi}.}
    \item{\code{R_SHARE_DIR}:}{The location of the \R \file{share}
      directory.  Set by \R.} 
    \item{\code{R_TEXI2DVICMD}:}{The path to \code{texi2dvi}. Defaults
      to the value of \env{TEXI2DVI}, and if that is unset to a value
      determined when \R was built.
#ifdef unix
      Consulted at startup to set the default for
      \code{\link{options}("texi2dvi")}, used by
      \code{\link{texi2dvi}} in package \pkg{tools}.
#endif
    }
    \item{\code{R_UNZIPCMD}:}{The path to \code{unzip}.  Sets the default
      value for \code{\link{options}("unzip")}.}
    \item{\code{R_ZIPCMD}:}{The path to \code{zip}.}
    \item{\code{TMPDIR}, \code{TMP}, \code{TEMP}:}{Consulted (in that
      order) when setting the temporary directory.  See
      \code{\link{tempdir}}.}
    \item{\code{TZ}:}{Optional. The current timezone.  See
      \code{\link{Sys.timezone}} for the system-specific
      formats.  Consulted as needed.}
    \item{\code{no_proxy}, \code{http_proxy}, \code{ftp_proxy}:}{
      (and more).  Optional.  Settings for \code{\link{download.file}}:
      see its help for further details.}
  }
}

#ifdef unix
\section{Unix-specific}{
  Some variables set on Unix-alikes, and not (in general) on Windows.
  \describe{
    \item{\code{DISPLAY}:}{Optional: used by \code{\link{X11}}, Tk (in
      package \pkg{tcltk}), the data editor and various packages.}
    \item{\code{EDITOR}:}{The path to the default editor.}
    \item{\code{PAGER}:}{The path to the default pager.}
    \item{\code{R_PRINTCMD}:}{Sets the default for
      \code{\link{options}("printcmd")}, which sets the default print
      command to be used by \code{\link{postscript}}.}
  }
}
#endif

#ifdef windows
\section{Windows-specific}{
  Some Windows-specific variables are
  \describe{
    \item{\code{GSC}:}{Optional: the path to GhostScript, used if
      \env{R_GSCMD} is not set.}
    \item{\code{R_USER}:}{The user's \sQuote{home} directory.  Set by
      \R.  (\env{HOME} will be set to the same value if not already set.)}
    \item{\code{TZDIR}:}{Optional. The top-level directory of the
      timezone database.  See \code{\link{Sys.timezone}}.}
  }
}
#endif

\seealso{
  \code{\link{Sys.getenv}} and \code{\link{Sys.setenv}} to read and set
  environmental variables in an \R session.
}
\keyword{utilities}

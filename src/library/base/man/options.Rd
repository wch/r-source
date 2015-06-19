% File src/library/base/man/options.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2013 R Core Team
% Distributed under GPL 2 or later

\name{options}
\alias{options}
\alias{.Options}
\alias{getOption}
\alias{option} % so help files can do 'set \link{} \code{"foo"}'
\title{Options Settings}
\description{
  Allow the user to set and examine a variety of global \emph{options}
  which affect the way in which \R computes and displays its results.
}
\usage{
options(\dots)

getOption(x, default = NULL)

.Options
}
%%-- NOTE : Keep the list up-to-date,
%%-- ====   Consider also the leading comments in ../../../main/options.c
\arguments{
  \item{\dots}{any options can be defined, using \code{name = value}.
    However, only the ones below are used in base \R.
    
    Options can also be passed by giving a single unnamed argument which
    is a named list.
  }
  \item{x}{a character string holding an option name.}
  
  \item{default}{if the specified option is not set in the options list,
    this value is returned.  This facilitates retrieving an option and
    checking whether it is set and setting it separately if not.}
}
\details{
  Invoking \code{options()} with no arguments returns a list with the
  current values of the options.  Note that not all options listed below
  are set initially.  To access the value of a single option, one should
  use, e.g., \code{getOption("width")} rather than
  \code{options("width")} which is a \emph{list} of length one.
}

\note{
  For compatibility with S there is a visible object \code{.Options} whose
  value is a pairlist containing  the current \code{options()} (in no
  particular order).  Assigning to it will make a local copy and not
  change the original.
}
\value{
  For \code{getOption}, the current value set for option \code{x}, or
  \code{NULL} if the option is unset.

  For \code{options()}, a list of all set options sorted by name.  For
  \code{options(name)}, a list of length one containing the set value,
  or \code{NULL} if it is unset.  For uses setting one or more options,
  a list with the previous values of the options changed (returned
  invisibly).
}
\section{Options used in base \R}{
  \describe{
    \item{\code{add.smooth}:}{typically logical, defaulting to
      \code{TRUE}.  Could also be set to an integer for specifying how
      many (simulated) smooths should be added.  This is currently only
      used by \code{\link{plot.lm}}.}

    \item{\code{browserNLdisabled}:}{logical: whether newline is
      disabled as a synonym for \code{"n"} is the browser.}

    \item{\code{checkPackageLicense}:}{logical, not set by default.  If
      true, \code{\link{library}} asks a user to accept any non-standard
      license at first use.
    }

    \item{\code{check.bounds}:}{logical, defaulting to \code{FALSE}.  If
      true, a \link{warning} is produced whenever a
      vector (atomic or \code{\link{list}}) is extended, by something
      like \code{x <- 1:3; x[5] <- 6}.}

    \item{\code{CBoundsCheck}:}{logical, controlling whether
      \code{\link{.C}} and \code{\link{.Fortran}} make copies to check for
      array over-runs on the atomic vector arguments.

      Initially set from value of the environment variable
      \env{R_C_BOUNDS_CHECK} (set to \code{yes} to enable).}

    \item{\code{continue}:}{a non-empty string setting the prompt used
      for lines which continue over one line.}

    %%  default is set in ../../profile/Common.R :
    \item{\code{defaultPackages}:}{the packages that are attached by
      default when \R starts up.  Initially set from value of the
      environment variable \env{R_DEFAULT_PACKAGES}, or if that is unset
      to \code{c("datasets", "utils", "grDevices", "graphics", "stats",
        "methods")}.  (Set \env{R_DEFAULT_PACKAGES} to \code{NULL} or
      a comma-separated list of package names.)   A call to
      \code{options} should be in your \file{.Rprofile} file to ensure
      that the change takes effect before the base package is
      initialized (see \code{\link{Startup}}).}

    \item{\code{deparse.cutoff}:}{integer value controlling the
      printing of language constructs which are \code{\link{deparse}}d.
      Default \code{60}.
    }

    \item{\code{deparse.max.lines}:}{controls the number of lines used
      when deparsing in \code{\link{traceback}}, \code{\link{browser}},
      and upon entry to a function whose debugging flag is set.
      Initially unset, and only used if set to a positive integer.}

    \item{\code{digits}:}{controls the number of digits to print when
      printing numeric values.  It is a suggestion only.  Valid values
      are 1\dots{}22 with default 7.  See the note in
      \code{\link{print.default}} about values greater than 15.}

    \item{\code{digits.secs}:}{controls the maximum number of digits to
      print when formatting time values in seconds.  Valid values
      are 0\dots6 with default 0.  See \code{\link{strftime}}.}

    \item{\code{download.file.extra}:}{Extra command-line argument(s) for
      non-default methods: see \code{\link{download.file}}.}

    \item{\code{download.file.method}:}{Method to be used for
      \code{download.file}.  Currently download methods
      \code{"internal"}, \code{"wget"} and \code{"lynx"} are available.
      There is no default for this option, when \code{method = "auto"}
      is chosen: see \code{\link{download.file}}.}

    \item{\code{echo}:}{logical.  Only used in non-interactive mode,
      when it controls whether input is echoed.  Command-line option
      \option{--slave} sets this to \code{FALSE}, but otherwise
      it starts the session as \code{TRUE}.}

    \item{\code{encoding}:}{The name of an encoding, default
      \code{"native.enc"}.  See \code{\link{connections}}.}

    \item{\code{error}:}{either a function or an expression governing
      the handling of non-catastrophic errors such as those generated by
      \code{\link{stop}} as well as by signals and internally detected
      errors.  If the option is a function, a call to that function,
      with no arguments, is generated as the expression.  The default
      value is \code{NULL}: see \code{\link{stop}} for the behaviour in
      that case.  The functions \code{\link{dump.frames}} and
      \code{\link{recover}} provide alternatives that allow post-mortem
      debugging.  Note that these need to specified as e.g.
      \code{options(error = utils::recover)} in startup files such as
      \file{\link{.Rprofile}}.}

    \item{\code{expressions}:}{sets a limit on the number of nested
      expressions that will be evaluated.  Valid values are
      25\dots500000 with default 5000.  If you increase it, you may
      also want to start \R with a larger protection stack;
      see \option{--max-ppsize} in \code{\link{Memory}}.  Note too that
      you may cause a segfault from overflow of the C stack, and on OSes
      where it is possible you may want to increase that.  Once the
      limit is reached an error is thrown.  The current number under
      evaluation can be found by calling \code{\link{Cstack_info}}.}

    \item{\code{keep.source}:}{When \code{TRUE}, the source code for
      functions (newly defined or loaded) is stored internally
      allowing comments to be kept in the right places.  Retrieve the
      source by printing or using \code{deparse(fn, control =
      "useSource")}.

      The default is \code{\link{interactive}()}, i.e., \code{TRUE} for
      interactive use.}

    %% used by tools::makeLazyLoading
    \item{\code{keep.source.pkgs}:}{As for \code{keep.source}, used only
    when packages are installed.  Defaults to \code{FALSE} unless the
    environment variable \env{R_KEEP_PKG_SOURCE} is set to \code{yes}.}

    \item{\code{max.print}:}{integer, defaulting to \code{99999}.
      \code{\link{print}} or \code{\link{show}} methods can make use of
      this option, to limit the amount of information that is printed,
      to something in the order of (and typically slightly less than)
      \code{max.print} \emph{entries}.}

    \item{\code{OutDec}:}{character string containing a single-byte
      character.  The character to be used as the decimal point in output
      conversions, that is in printing, plotting and \code{as.character}
      but not deparsing.}

    \item{\code{pager}:}{the command used for displaying text files by
      \code{\link{file.show}}.
#ifdef unix
      Defaults to \file{\var{\link{R_HOME}}/bin/pager}, which is a shell
      script running the command-line specified by the environment
      variable \env{PAGER} whose default is set at configuration,
      usually to \command{less}.
#endif
#ifdef windows
      Defaults to \code{"internal"}, which uses a pager similar to the
      GUI console.  Another possibility is \code{"console"} to use the
      console itself.
#endif
      Can be a character string or an \R function, in which case it
      needs to accept the arguments \code{(files, header,
	title, delete.file)} corresponding to the first four arguments of
      \code{\link{file.show}}.}

    \item{\code{papersize}:}{the default paper format used by
      \code{\link{postscript}};  set by environment variable
      \env{R_PAPERSIZE} when \R is started: if that is unset or invalid
      it defaults to
#ifdef unix
      a value derived from the locale category \code{LC_PAPER}, or
      if that is unavailable to a default set when \R was built.
#endif
#ifdef windows
      \code{"a4"}, or \code{"letter"} in US and Canadian locales.
#endif
    }

    \item{\code{pdfviewer}:}{default PDF viewer.
      The default is set from the environment variable \env{R_PDFVIEWER},
#ifdef unix
      the default value of which is set when \R is configured.
#endif
#ifdef windows
      which defaults to the full path to \command{open.exe}, a utility
      supplied with \R.
#endif
    }

    \item{\code{printcmd}:}{the command used by \code{\link{postscript}}
      for printing; set by environment variable \env{R_PRINTCMD} when
      \R is started.  This should be a command that expects either input
      to be piped to \file{stdin} or to be given a single filename
      argument.  Usually set to \code{"lpr"} on a Unix-alike.}

    \item{\code{prompt}:}{a non-empty string to be used for \R's prompt;
      should usually end in a blank (\code{" "}).}

      % verbatim, for checking " \t\n\"\\'`><=%;,|&{()}"
#ifdef unix
    \item{\code{rl_word_breaks}:}{Used for the readline-based terminal
      interface.  Default value \code{" \\t\\n\\"\\\\'`><=\%;,|&\{()\}"}.%"
      This is the set of characters use to break the input line up into
      tokens for object- and file-name completion.  Those who do not use
      spaces around operators may prefer\cr
      \code{" \\t\\n\\"\\\\'`><=+-*\%;,|&\{()\}"}%"
      which was the default in \R 2.5.0.  (The default in pre-2.5.0
      versions of \R was\cr
      \code{" \\t\\n\\"\\\\'`@$><=;|&\{("}.)}%"
#endif

    \item{\code{save.defaults}, \code{save.image.defaults}:}{
      see \code{\link{save}}.}

    \item{\code{scipen}:}{integer.  A penalty to be applied
      when deciding to print numeric values in fixed or exponential
      notation.  Positive values bias towards fixed and negative towards
      scientific notation: fixed notation will be preferred unless it is
      more than \code{scipen} digits wider.}

    \item{\code{showWarnCalls}, \code{showErrorCalls}:}{a logical.
      Should warning and error messages show a summary of the call
      stack?  By default error calls are shown in non-interactive
      sessions.}

    \item{\code{showNCalls}:}{integer.  Controls how long the sequence
      of calls must be (in bytes) before ellipses are used.  Defaults to
      40 and should be at least 30 and no more than 500.}

    \item{\code{show.error.locations}:}{Should source locations of
      errors be printed?  If set to \code{TRUE} or \code{"top"}, the
      source location that is highest on the stack (the most recent
      call) will be printed.  \code{"bottom"} will print the location
      of the earliest call found on the stack.

      Integer values can select other entries.  The value \code{0}
      corresponds to \code{"top"} and positive values count down the
      stack from there.  The value \code{-1} corresponds to
      \code{"bottom"} and negative values count up from there.
      }

    \item{\code{show.error.messages}:}{a logical.  Should error messages
      be printed?  Intended for use with \code{\link{try}} or a
      user-installed error handler.}

    \item{\code{stringsAsFactors}:}{The default setting for arguments of
      \code{\link{data.frame}} and \code{\link{read.table}}.}

    \item{\code{texi2dvi}:}{used by functions
      \code{\link{texi2dvi}} and \code{\link{texi2pdf}} in package \pkg{tools}.
#ifdef unix
      Set at startup from the environment variable \env{R_TEXI2DVICMD}.
#endif
    }

    \item{\code{timeout}:}{integer.  The timeout for some Internet
      operations, in seconds.  Default 60 seconds.  See
      \code{\link{download.file}} and \code{\link{connections}}.}

    \item{\code{topLevelEnvironment}:}{see \code{\link{topenv}} and
      \code{\link{sys.source}}.}

    \item{\code{useFancyQuotes}:}{controls the use of
      directional quotes in \code{\link{sQuote}}, \code{dQuote} and in
      rendering text help (see \code{\link{Rd2txt}} in package
      \pkg{tools}).  Can be \code{TRUE}, \code{FALSE}, \code{"TeX"} or
      \code{"UTF-8"}.}

    \item{\code{verbose}:}{logical.  Should \R report extra information
      on progress?  Set to \code{TRUE} by the command-line option
      \option{--verbose}.}

    \item{\code{warn}:}{sets the handling of warning messages.  If
      \code{warn} is negative all warnings are ignored.  If \code{warn}
      is zero (the default) warnings are stored until the top--level
      function returns.  If 10 or fewer warnings were signalled they
      will be printed otherwise a message saying how many were
      signalled.  An object called \code{last.warning} is
      created and can be printed through the function
      \code{\link{warnings}}.  If \code{warn} is one, warnings are
      printed as they occur.  If \code{warn} is two or larger all
      warnings are turned into errors.}

    \item{\code{warnPartialMatchArgs}:}{logical.  If true, warns if
      partial matching is used in argument matching.}

    \item{\code{warnPartialMatchAttr}:}{logical.  If true, warns if
      partial matching is used in extracting attributes via
      \code{\link{attr}}.}

    \item{\code{warnPartialMatchDollar}:}{logical.  If true, warns if
      partial matching is used for extraction by \code{$}.}

    \item{\code{warning.expression}:}{an \R code expression to be called
      if a warning is generated, replacing the standard message.  If
      non-null it is called irrespective of the value of option
      \code{warn}.}

    \item{\code{warning.length}:}{sets the truncation limit for error
      and warning messages.  A non-negative integer, with allowed values
      100\dots8170, default 1000.}

    \item{\code{nwarnings}:}{the limit for the number of warnings kept
      when \code{warn = 0}, default 50.  This will discard messages if
      called whilst they are being collected.}

    \item{\code{width}:}{controls the maximum number of columns on a
      line used in printing vectors, matrices and arrays, and when
      filling by \code{\link{cat}}.

      Columns are normally the same as characters except in CJK languages.

      You may want to change this if you re-size the window that \R is
      running in.  Valid values are 10\dots10000 with default normally
      80.  (The limits on valid values are in file \file{Print.h} and can be
      changed by re-compiling \R.)  Some \R consoles automatically change
      the value when they are resized.

      See the examples on \link{Startup} for one way to set this
      automatically from the terminal width when \R is started.
    }
 }

  The \sQuote{factory-fresh} default settings of some of these options are
  \tabular{ll}{
    \code{add.smooth} \tab \code{TRUE}\cr
    \code{check.bounds} \tab \code{FALSE}\cr
    \code{continue} \tab \code{"+ "}\cr
    \code{digits} \tab \code{7}\cr
    \code{echo} \tab \code{TRUE}\cr
    \code{encoding} \tab \code{"native.enc"}\cr
    \code{error} \tab \code{NULL}\cr
    \code{expressions} \tab \code{5000}\cr
    \code{keep.source} \tab \code{interactive()}\cr
    \code{keep.source.pkgs} \tab \code{FALSE}\cr
    \code{max.print} \tab \code{99999}\cr
    \code{OutDec} \tab \code{"."}\cr
    \code{prompt} \tab \code{"> "}\cr
    \code{scipen} \tab \code{0} \cr
    \code{show.error.messages} \tab \code{TRUE}\cr
    \code{timeout} \tab \code{60}\cr
    \code{verbose} \tab \code{FALSE}\cr
    \code{warn} \tab \code{0}\cr
    \code{warning.length} \tab \code{1000}\cr
    \code{width}  \tab \code{80}\cr
  }
  Others are set from environment variables or are platform-dependent.
}
\section{Options set in package grDevices}{
  These will be set when package \pkg{grDevices} (or its namespace)
  is loaded if not already set.
  \describe{
#ifdef unix
    \item{\code{bitmapType}:}{(Unix-only) character.  The default type for the
      bitmap devices such as \code{\link{png}}.  Defaults to
      \code{"cairo"} on systems where that is available, or to
      \code{"quartz"} on OS X where that is available.}
#endif

    \item{\code{device}:}{a character string giving
      the name of a function, or the function object itself,
      which when called creates a new graphics device of the default
      type for that session.  The value of this option defaults to the
      normal screen device (e.g., \code{X11}, \code{windows} or
      \code{quartz}) for an interactive session, and \code{pdf}
      in batch use or if a screen is not available.  If set to the name
      of a device, the device is looked for first from the global
      environment (that is down the usual search path) and then in the
      \pkg{grDevices} namespace.

      The default values in interactive and non-interactive sessions are
      configurable via environment variables
      \env{R_INTERACTIVE_DEVICE} and \env{R_DEFAULT_DEVICE}
      respectively.
    }

    \item{\code{device.ask.default}:}{logical.  The default for
      \code{\link{devAskNewPage}("ask")} when a device is opened.}

    \item{\code{locatorBell}:}{logical.  Should selection in \code{locator}
      and \code{identify} be confirmed by a bell?  Default \code{TRUE}.
      Honoured at least on \code{X11} and \code{windows} devices.}

#ifdef windows
    \item{\code{windowsTimeout}:}{(Windows-only) integer vector of length 2
      representing two times in milliseconds.  These control the
      double-buffering of \code{\link{windows}} devices when that is
      enabled: the first is the delay after plotting finishes
      (default 100) and the second is the update interval during
      continuous plotting (default 500).  The values at the time the
      device is opened are used.}
#endif
  }
}
\section{Other options used by package graphics}{
  \describe{
    \item{\code{max.contour.segments}:}{positive integer, defaulting to
      \code{25000} if not set.  A limit on the number of
      segments in a single contour line in \code{\link{contour}} or
      \code{\link{contourLines}}.}
  }
}
\section{Options set in package stats}{
  These will be set when package \pkg{stats} (or its namespace)
  is loaded if not already set.
  \describe{
    \item{\code{contrasts}:}{the default \code{\link{contrasts}} used in
      model fitting such as with \code{\link{aov}} or \code{\link{lm}}.
      A character vector of length two, the first giving the function to
      be used with unordered factors and the second the function to be
      used with ordered factors.  By default the elements are named
      \code{c("unordered", "ordered")}, but the names are unused.}

    \item{\code{na.action}:}{the name of a function for treating missing
      values (\code{\link{NA}}'s) for certain situations.}

    \item{\code{show.coef.Pvalues}:}{logical, affecting whether P
      values are printed in summary tables of coefficients.  See
      \code{\link{printCoefmat}}.}

    \item{\code{show.nls.convergence}:}{logical, should \code{\link{nls}}
      convergence messages be printed for successful fits?}

    \item{\code{show.signif.stars}:}{logical, should stars be printed on
      summary tables of coefficients?  See \code{\link{printCoefmat}}.}

    \item{\code{ts.eps}:}{the relative tolerance for certain time series
      (\code{\link{ts}}) computations.  Default \code{1e-05}.}

    \item{\code{ts.S.compat}:}{logical.  Used to select S compatibility
      for plotting time-series spectra.  See the description of argument
      \code{log} in \code{\link{plot.spec}}.}
  }
}
\section{Options set in package utils}{
  These will be set when package \pkg{utils} (or its namespace)
  is loaded if not already set.
  \describe{
    \item{\code{BioC_mirror}:}{The URL of a Bioconductor mirror
      for use by \code{\link{setRepositories}},
      e.g. the default \samp{"http://www.bioconductor.org"}
      or the European mirror
      \samp{"http://bioconductor.statistik.tu-dortmund.de"}.  Can be set
      by \code{\link{chooseBioCmirror}}.}

    \item{\code{browser}:}{default HTML browser used by
      \code{\link{help.start}()} and \code{\link{browseURL}} on UNIX, or
      a non-default browser on Windows.  Alternatively, an \R function that
      is called with a URL as its argument.}

    \item{\code{ccaddress}:}{default Cc: address used by
      \code{\link{create.post}} (and hence\code{\link{bug.report}} and
      \code{\link{help.request}}).  Can be \code{FALSE} or \code{""}.}

    \item{\code{citation.bibtex.max}:}{default 1; the maximal number of
      bibentries (\code{\link{bibentry}}) in a \code{\link{citation}} for
      which the bibtex version is printed in addition to the text one.}

    \item{\code{de.cellwidth}:}{integer: the cell widths (number of
      characters) to be used in the data editor \code{\link{dataentry}}.
      If this is unset (the default), 0, negative or \code{NA}, variable
      cell widths are used.}

    \item{\code{demo.ask}:}{default for the \code{ask} argument of
      \code{\link{demo}}.}

    \item{\code{editor}:}{a non-empty string, or a function that is
      called with a file path as argument.  Sets the default text
      editor, e.g., for \code{\link{edit}}.  Set from the environment
      variable \env{EDITOR} on UNIX, or if unset \env{VISUAL} or
      \command{vi}.}

    \item{\code{example.ask}:}{default for the \code{ask} argument of
      \code{\link{example}}.}

    \item{\code{help.ports}:}{optional integer vector for setting ports
      of the internal HTTP server, see \code{\link{startDynamicHelp}}.}

    \item{\code{help.search.types}:}{default types of documentation
      to be searched by \code{\link{help.search}} and \code{\link{??}}.}

    \item{\code{help.try.all.packages}:}{default for an argument of
      \code{\link{help}}.}

    \item{\code{help_type}:}{default for an argument of
    \code{\link{help}}, used also as the help type by \code{\link{?}}.}

    \item{\code{HTTPUserAgent}:}{string used as the user agent in HTTP
      requests.  If \code{NULL}, HTTP requests will be made without a
      user agent header.  The default is \code{R (<version> <platform>
	<arch> <os>)}}

    \item{\code{install.lock}:}{logical: should per-directory package
      locking be used by \code{\link{install.packages}}?  Most useful
      for binary installs on OS X and Windows, but can be used in a
      startup file for source installs \emph{via}
      \command{R CMD \link{INSTALL}}.  For binary installs, can also be
      the character string \code{"pkgloack"}.}

    \item{\code{internet.info}:}{The minimum level of information to be
      printed on URL downloads etc.  Default is 2, for failure causes.
      Set to 1 or 0 to get more information.}

    \item{\code{mailer}:}{default emailing method used by
      \code{\link{create.post}} and hence \code{\link{bug.report}} and
      \code{\link{help.request}}.}

    \item{\code{menu.graphics}:}{Logical: should graphical menus be used
      if available?.  Defaults to \code{TRUE}.  Currently applies to
      \code{\link{select.list}}, \code{\link{chooseCRANmirror}},
      \code{\link{setRepositories}} and to select from multiple (text)
      help files in \code{\link{help}}.}

    \item{\code{pkgType}:}{The default type of packages to be downloaded
      and installed -- see \code{\link{install.packages}}.
#ifdef windows
      Possible values are \code{"win.binary"} (the default) and
      \code{"source"}.  Some OS X builds use \code{"mac.binary"}.
#endif
#ifdef unix
      Possible values are \code{"source"} (the default except under the
      CRAN OS X build) and \code{"mac.binary."}.  Windows
      uses  \code{"win.binary"}.  (\code{"mac.binary.leopard"} and
      \code{"mac.binary.universal"} are no longer in use.)
#endif
    }

    \item{\code{repos}:}{URLs of the repositories for use by
      \code{\link{update.packages}}.  Defaults to
      \code{c(CRAN="@CRAN@")}, a value that causes some utilities to
      prompt for a CRAN mirror.  To avoid this do set the CRAN mirror,
      by something like
      \code{local({r <- getOption("repos"); r["CRAN"] <- "http://my.local.cran";
                   options(repos = r)})}.

      Note that you can add more repositories (Bioconductor and
      Omegahat, notably) using \code{\link{setRepositories}()}.
    }
    \item{\code{SweaveHooks}, \code{SweaveSyntax}:}{see \code{\link{Sweave}}.}

    \item{\code{unzip}:}{a character string, the path of the command
      used for unzipping help files, or \code{"internal"}.
#ifdef unix
      Defaults to the value of \env{R_UNZIPCMD}, which is set in
      \file{etc/Renviron} if an \code{unzip} command was found during
      configuration.
#endif
#ifdef windows
      Defaults to \code{"internal"} when the internal unzip code is
      used.
#endif
    }
  }
}
\section{Options set in package parallel}{
  These will be set when package \pkg{parallel} (or its namespace)
  is loaded if not already set.
  \describe{
    \item{\code{mc.cores}:}{a integer giving the maximum allowed number
      of \emph{additional} \R processes allowed to be run in parallel to
      the current \R process.  Defaults to the setting of the
      environment variable \env{MC_CORES} if set.  Most applications
      which use this assume a limit of \code{2} if it is unset.
    }
  }
}
\section{Options used on Unix only}{
  \describe{
    \item{\code{dvipscmd}:}{character string giving a command to be used in
    the (deprecated) off-line printing of help pages \emph{via}
    PostScript.  Defaults to \command{"dvips"}.}
  }
}
\section{Options used on Windows only}{
  \describe{
    \item{\code{warn.FPU}:}{logical, by default undefined.  If true,
      a \link{warning} is produced whenever \link{dyn.load} repairs the
      control word damaged by a buggy DLL.}
  }
}

\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.
}
\examples{
op <- options(); utils::str(op) # op is a named list

getOption("width") == options()$width # the latter needs more memory
options(digits = 15)
pi

# set the editor, and save previous value
old.o <- options(editor = "nedit")
old.o

options(check.bounds = TRUE, warn = 1)
x <- NULL; x[4] <- "yes" # gives a warning

options(digits = 5)
print(1e5)
options(scipen = 3); print(1e5)

options(op)     # reset (all) initial options
options("digits")

\dontrun{## set contrast handling to be like S
options(contrasts = c("contr.helmert", "contr.poly"))
}

\dontrun{## on error, terminate the R session with error status 66
options(error = quote(q("no", status = 66, runLast = FALSE)))
stop("test it")
}

\dontrun{## Set error actions for debugging:
## enter browser on error, see ?recover:
options(error = recover)
## allows to call debugger() afterwards, see ?debugger:
options(error = dump.frames)
## A possible setting for non-interactive sessions
options(error = quote({dump.frames(to.file = TRUE); q()}))
}

  # Compare the two ways to get an option and use it
  # acconting for the possibility it might not be set.
if(as.logical(getOption("performCleanp", TRUE)))
   cat("do cleanup\n")

\dontrun{
  # a clumsier way of expressing the above w/o the default.
tmp <- getOption("performCleanup")
if(is.null(tmp))
  tmp <- TRUE
if(tmp)
   cat("do cleanup\n")
}

}
\keyword{environment}
\keyword{error}
\keyword{print}

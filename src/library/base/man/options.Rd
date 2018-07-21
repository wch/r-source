% File src/library/base/man/options.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2018 R Core Team
% Distributed under GPL 2 or later

\name{options}
\alias{options}
\alias{.Options}
\alias{getOption}
\alias{option} % so help files can do 'set \link{} \code{"foo"}'

\alias{MC_CORES}
\alias{R_C_BOUNDS_CHECK}
\alias{R_DEFAULT_DEVICE}
\alias{R_KEEP_PKG_SOURCE}
\alias{R_INTERACTIVE_DEVICE}

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
  change the original. (\emph{Using} it however is faster than calling
  \code{options()}).

  An option set to \code{NULL} is indistinguishable from a non existing
  option.
}
\value{
  For \code{getOption}, the current value set for option \code{x}, or
  \code{default} (which defaults to \code{NULL}) if the option is unset.

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

    \item{\code{askYesNo}:}{a function (typically set by a front-end)
      to ask the user binary response functions in a consistent way,
      or a vector of strings used by \code{\link{askYesNo}} to use
      as default responses for such questions.}

    \item{\code{browserNLdisabled}:}{logical: whether newline is
      disabled as a synonym for \code{"n"} in the browser.}

    \item{\code{checkPackageLicense}:}{logical, not set by default.  If
      true, \code{\link{loadNamespace}} asks a user to accept any
      non-standard license at first load of the package.
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
      a comma-separated list of package names.)  It will not work to set
      this in a \file{.Rprofile} file, as its value is consulted before
      that file is read.}

    \item{\code{deparse.cutoff}:}{integer value controlling the
      printing of language constructs which are \code{\link{deparse}}d.
      Default \code{60}.
    }

    \item{\code{deparse.max.lines}:}{controls the number of lines used
      when deparsing in \code{\link{traceback}}, \code{\link{browser}},
      and upon entry to a function whose debugging flag is set.
      Initially unset, and only used if set to a positive integer.}

    \item{\code{digits}:}{controls the number of \emph{significant} (see \code{\link{signif}}) digits to
      print when printing numeric values.  It is a suggestion only.
      Valid values are 1\dots{}22 with default 7.  See the note in
      \code{\link{print.default}} about values greater than 15.}

    \item{\code{digits.secs}:}{controls the maximum number of digits to
      print when formatting time values in seconds.  Valid values
      are 0\dots6 with default 0.  See \code{\link{strftime}}.}

    \item{\code{download.file.extra}:}{Extra command-line argument(s) for
      non-default methods: see \code{\link{download.file}}.}

    \item{\code{download.file.method}:}{Method to be used for
      \code{download.file}.  Currently download methods
      \code{"internal"}, \code{"wininet"} (Windows only),
      \code{"libcurl"}, \code{"wget"} and \code{"curl"} are available.
      If not set, \code{method = "auto"}
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
      debugging.  Note that these need to specified as
      e.g.\sspace{}\code{options(error = utils::recover)} in startup
      files such as \file{\link{.Rprofile}}.}

    \item{\code{expressions}:}{sets a limit on the number of nested
      expressions that will be evaluated.  Valid values are
      25\dots500000 with default 5000.  If you increase it, you may
      also want to start \R with a larger protection stack;
      see \option{--max-ppsize} in \code{\link{Memory}}.  Note too that
      you may cause a segfault from overflow of the C stack, and on OSes
      where it is possible you may want to increase that.  Once the
      limit is reached an error is thrown.  The current number under
      evaluation can be found by calling \code{\link{Cstack_info}}.}

    \item{\code{interrupt}:}{a function taking no arguments to be called
      on a user interrupt if the interrupt condition is not otherwise
      handled.}

    \item{\code{keep.parse.data}:}{When internally storing source code
      (\code{keep.source} is TRUE), also store parse data.  Parse data can
      then be retrieved with \code{\link{getParseData}()} and used e.g.  for
      spell checking of string constants or syntax highlighting.  The value
      has effect only when internally storing source code (see
      \code{keep.source}).  The default is \code{TRUE}.}

    \item{\code{keep.parse.data.pkgs}:}{As for \code{keep.parse.data}, used
      only when packages are installed.  Defaults to \code{FALSE} unless the
      environment variable \env{R_KEEP_PKG_PARSE_DATA} is set to \code{yes}.
      The space overhead of parse data can be substantial even after
      compression and it causes performance overhead when loading packages.}

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

    \item{\code{matprod}:}{a string selecting the implementation of
      the matrix products \code{\link{\%*\%}}, \code{\link{crossprod}}, and
      \code{\link{tcrossprod}} for double and complex vectors:
      \describe{
	\item{\code{"internal"}}{uses an unoptimized 3-loop algorithm
	  which correctly propagates \code{\link{NaN}} and
	  \code{\link{Inf}} values and is consistent in precision with
	  other summation algorithms inside \R like \code{\link{sum}} or
	  \code{\link{colSums}} (which now means that it uses a \code{long
	    double} accumulator for summation if available and enabled,
	  see \code{\link{capabilities}}).}
	\item{\code{"default"}}{uses BLAS to speed up computation, but
	  to ensure correct propagation of \code{NaN} and \code{Inf}
	  values it uses an unoptimized 3-loop algorithm for inputs that may
	  contain \code{NaN} or \code{Inf} values.  When deemed
	  beneficial for performance, \code{"default"} may call the
	  3-loop algorithm unconditionally, i.e., without checking the
	  input for NaN/Inf values.  The 3-loop algorithm uses (only) a
	  \code{double} accumulator for summation, which is consistent
	  with the reference BLAS implementation.}
	\item{\code{"blas"}}{uses BLAS unconditionally without any
	  checks and should be used with extreme caution.  BLAS
	  libraries do not propagate \code{\link{NaN}} or
	  \code{\link{Inf}} values correctly and for inputs with
	  \code{NaN}/\code{Inf} values the results may be undefined.}
	\item{\code{"default.simd"}}{is experimental and will likely be
	  removed in future versions of \R.  It provides the same behavior
	  as \code{"default"}, but the check whether the input contains
	  NaN/Inf values is faster on some SIMD hardware.  On older
	  systems it will run correctly, but may be much slower than
	  \code{"default"}.}
      }
    }

    \item{\code{max.print}:}{integer, defaulting to \code{99999}.
      \code{\link{print}} or \code{\link{show}} methods can make use of
      this option, to limit the amount of information that is printed,
      to something in the order of (and typically slightly less than)
      \code{max.print} \emph{entries}.}

    \item{\code{OutDec}:}{character string containing a single
      character.  The preferred character to be used as the decimal
      point in output conversions, that is in printing, plotting,
      \code{\link{format}} and \code{\link{as.character}} but not when
      deparsing nor by \code{\link{sprintf}} nor \code{\link{formatC}}
      (which are sometimes used prior to printing.)
    }

    \item{\code{pager}:}{the command used for displaying text files by
      \code{\link{file.show}}, details depending on the platform:
      \describe{
	\item{On a unix-alike}{defaults to \file{\var{\link{R_HOME}}/bin/pager}, which is a shell
	  script running the command-line specified by the environment
	  variable \env{PAGER} whose default is set at configuration,
	  usually to \command{less}.}
	\item{On Windows}{defaults to \code{"internal"}, which uses a pager similar to the
	  GUI console.  Another possibility is \code{"console"} to use the
	  console itself.}
      }
      Can be a character string or an \R function, in which case it
      needs to accept the arguments \code{(files, header,
	title, delete.file)} corresponding to the first four arguments of
      \code{\link{file.show}}.}

    \item{\code{papersize}:}{the default paper format used by
      \code{\link{postscript}};  set by environment variable
      \env{R_PAPERSIZE} when \R is started: if that is unset or invalid
      it defaults platform dependently
      \describe{
	\item{on a unix-alike}{to a value derived from the locale category
	  \code{LC_PAPER}, or if that is unavailable to a default set
	  when \R was built.}
	\item{on Windows}{to \code{"a4"}, or \code{"letter"} in US and
	  Canadian locales.}
      }}

    \item{\code{PCRE_limit_recursion}:}{Logical: should
      \code{\link{grep}(perl = TRUE)} and similar limit the maximal
      recursion allowed when matching?  PCRE can be built not to use a
      recursion stack (see \code{\link{pcre_config}}, but it is by
      default with a recursion limit of 10000000 which potentially needs
      a very large C stack: see the discussion at
      \url{http://www.pcre.org/original/doc/html/pcrestack.html}.  If
      true, the limit is reduced using \R's estimate of the C stack size
      available (if known), otherwise 10000.  If \code{NA}, the limit is
      imposed only if any input string has 1000 or more bytes.}

     \item{\code{PCRE_study}:}{Logical or integer: should
      \code{\link{grep}(perl = TRUE)} and similar \sQuote{study} the
      patterns?  Either logical or a numerical threshold for the minimum
      number of strings to be matched for the pattern to be studied (the
      default is \code{10})).  Missing values and negative numbers are
      treated as false.}

    \item{\code{PCRE_use_JIT}:}{Logical: should \code{\link{grep}(perl =
	TRUE)}, \code{\link{strsplit}(perl = TRUE)} and similar make use
      of PCRE's Just-In-Time compiler for studied patterns, if
      available?  Missing values are treated as false.}

    \item{\code{pdfviewer}:}{default PDF viewer.
      The default is set from the environment variable \env{R_PDFVIEWER},
      the default value of which
      \describe{
	\item{on a unix-alike}{is set when \R is configured, and}
	\item{on Windows}{is the full path to \command{open.exe}, a utility
	  supplied with \R.}
      }}

    \item{\code{printcmd}:}{the command used by \code{\link{postscript}}
      for printing; set by environment variable \env{R_PRINTCMD} when
      \R is started.  This should be a command that expects either input
      to be piped to \file{stdin} or to be given a single filename
      argument.  Usually set to \code{"lpr"} on a Unix-alike.}

    \item{\code{prompt}:}{a non-empty string to be used for \R's prompt;
      should usually end in a blank (\code{" "}).}

      % verbatim, for checking " \t\n\"\\'`><=%;,|&{()}"
    \item{\code{rl_word_breaks}:}{(Unix only:) Used for the readline-based terminal
      interface.  Default value \code{" \\t\\n\\"\\\\'`><=\%;,|&{()}"}.%"

      This is the set of characters use to break the input line into
      tokens for object- and file-name completion.  Those who do not use
      spaces around operators may prefer\cr
      \code{" \\t\\n\\"\\\\'`><=+-*\%;,|&{()}"}%"
      }

    \item{\code{save.defaults}, \code{save.image.defaults}:}{
      see \code{\link{save}}.}

    \item{\code{scipen}:}{integer.  A penalty to be applied
      when deciding to print numeric values in fixed or exponential
      notation.  Positive values bias towards fixed and negative towards
      scientific notation: fixed notation will be preferred unless it is
      more than \code{scipen} digits wider.}

    \item{\code{setWidthOnResize}:}{a logical. If set and \code{TRUE}, \R
      run in a terminal using a recent \code{readline} library will set
      the \code{width} option when the terminal is resized.}

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
      \describe{
	\item{unix-alike only:}{
      Set at startup from the environment variable \env{R_TEXI2DVICMD},
      which defaults first to the value of environment variable
      \env{TEXI2DVI}, and then to a value set when \R was installed (the
      full path to a \command{texi2dvi} script if one was found).  If
      necessary, that environment variable can be set to
      \code{"emulation"}.
      } }
    }

    \item{\code{timeout}:}{integer.  The timeout for some Internet
      operations, in seconds.  Default 60 seconds.  See
      \code{\link{download.file}} and \code{\link{connections}}.}

    \item{\code{topLevelEnvironment}:}{see \code{\link{topenv}} and
      \code{\link{sys.source}}.}

    \item{\code{url.method}:}{character string: the default method for
      \code{\link{url}}.  Normally unset, which is equivalent to
      \code{"default"}, which is \code{"internal"} except on Windows.}

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
      called whilst they are being collected.  If you increase this
      limit, be aware that the current implementation pre-allocates
      the equivalent of a named list for them, i.e., do not increase it to
      more than say a million.}

    \item{\code{width}:}{controls the maximum number of columns on a
      line used in printing vectors, matrices and arrays, and when
      filling by \code{\link{cat}}.

      Columns are normally the same as characters except in East Asian
      languages.

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
    \item{\code{bitmapType}:}{(Unix only, incl. macOS) character.  The
      default type for the
      bitmap devices such as \code{\link{png}}.  Defaults to
      \code{"cairo"} on systems where that is available, or to
      \code{"quartz"} on macOS where that is available.}

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

      The search logic for \sQuote{the normal screen device} is that
      this is \code{windows} on Windows, and \code{quartz} if available
      on macOS (running at the console, and compiled into the build).
      Otherwise \code{X11} is used if environment variable \env{DISPLAY}
      is set.
    }

    \item{\code{device.ask.default}:}{logical.  The default for
      \code{\link{devAskNewPage}("ask")} when a device is opened.}

    \item{\code{locatorBell}:}{logical.  Should selection in \code{locator}
      and \code{identify} be confirmed by a bell?  Default \code{TRUE}.
      Honoured at least on \code{X11} and \code{windows} devices.}

    \item{\code{windowsTimeout}:}{(Windows-only) integer vector of length 2
      representing two times in milliseconds.  These control the
      double-buffering of \code{\link{windows}} devices when that is
      enabled: the first is the delay after plotting finishes
      (default 100) and the second is the update interval during
      continuous plotting (default 500).  The values at the time the
      device is opened are used.}
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
      values (\code{\link{NA}}'s) for certain situations, see
      \code{\link{na.action}} and \code{\link{na.pass}}.}

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
\section{Options set (or used) in package utils}{
  These will be set (apart from \code{Ncpus}) when package \pkg{utils}
  (or its namespace) is loaded if not already set.
  \describe{
    \item{\code{BioC_mirror}:}{The URL of a Bioconductor mirror
      for use by \code{\link{setRepositories}},
      e.g.\sspace{}the default \samp{"https://bioconductor.org"}
      or the European mirror
      \samp{"https://bioconductor.statistik.tu-dortmund.de"}.  Can be set
      by \code{\link{chooseBioCmirror}}.}

    \item{\code{browser}:}{The HTML browser to be used by
      \code{\link{browseURL}}.  This sets the default browser on UNIX or
      a non-default browser on Windows.  Alternatively, an \R function
      that is called with a URL as its argument.  See
      \code{\link{browseURL}} for further details.}

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

    \item{\code{editor}:}{a non-empty string or a function that sets the
      default text editor, e.g., for \code{\link{edit}}.  Set from the
      environment variable \env{EDITOR} on UNIX, or if unset
      \env{VISUAL} or \command{vi}.}

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

    \item{\code{HTTPUserAgent}:}{string used as the user agent in HTTP(S)
      requests.  If \code{NULL}, requests will be made without a
      user agent header.
      The default is \code{R (<version> <platform> <arch> <os>)} .}

    \item{\code{install.lock}:}{logical: should per-directory package
      locking be used by \code{\link{install.packages}}?  Most useful
      for binary installs on macOS and Windows, but can be used in a
      startup file for source installs \emph{via}
      \command{R CMD \link{INSTALL}}.  For binary installs, can also be
      the character string \code{"pkglock"}.}

    \item{\code{internet.info}:}{The minimum level of information to be
      printed on URL downloads etc, using the \code{"internal"} and
      \code{"libcurl"} methods.
      Default is 2, for failure causes.  Set to 1 or 0 to get more
      detailed information (for the \code{"internal"} method 0 provides
      more information than 1).
    }

    \item{\code{install.packages.check.source}:}{Used by
      \code{\link{install.packages}} (and indirectly
      \code{\link{update.packages}}) on platforms which support binary
      packages.  Possible values \code{"yes"} and \code{"no"}, with
      unset being equivalent to \code{"yes"}.}

    \item{\code{install.packages.compile.from.source}:}{Used by
      \code{\link{install.packages}(type = "both")} (and indirectly
      \code{\link{update.packages}}) on platforms which
      support binary packages.  Possible values are \code{"never"},
      \code{"interactive"} (which means ask in interactive use and
      \code{"never"} in batch use) and \code{"always"}.  The default is
      taken from environment variable
      \env{R_COMPILE_AND_INSTALL_PACKAGES}, with default
      \code{"interactive"} if unset.  However, \code{install.packages}
      uses \code{"never"} unless a \command{make} program is found,
      consulting the environment variable \env{MAKE}.}

    \item{\code{mailer}:}{default emailing method used by
      \code{\link{create.post}} and hence \code{\link{bug.report}} and
      \code{\link{help.request}}.}

    \item{\code{menu.graphics}:}{Logical: should graphical menus be used
      if available?.  Defaults to \code{TRUE}.  Currently applies to
      \code{\link{select.list}}, \code{\link{chooseCRANmirror}},
      \code{\link{setRepositories}} and to select from multiple (text)
      help files in \code{\link{help}}.}

    \item{\code{Ncpus}:}{an integer \eqn{n \ge 1}{n >= 1}, used in
      \code{\link{install.packages}} as default for the number of cpus
      to use in a potentially parallel installation, as
      \code{Ncpus = getOption("Ncpus", 1L)}, i.e., when unset is
      equivalent to a setting of 1.}

    \item{\code{pkgType}:}{The default type of packages to be downloaded
      and installed -- see \code{\link{install.packages}}.
      Possible values are platform dependently
      \describe{
	\item{on Windows}{
	  \code{"win.binary"}, \code{"source"} and
	  \code{"both"} (the default).}
	\item{on unix-alikes}{\code{"source"} (the default except under a
	  CRAN macOS build), \code{"mac.binary.el-capitan"} and
	  \code{"both"} (the default for CRAN macOS builds).
	  (\code{"mac.binary.mavericks"}, \code{"mac.binary.leopard"},
	  \code{"mac.binary.universal"} are no longer in use.)}
      }
      Value \code{"binary"} is a synonym for the native binary type (if
      there is one); \code{"both"} is used by
      \code{\link{install.packages}} to choose between source and binary
      installs.
    }

    \item{\code{repos}:}{URLs of the repositories for use by
      \code{\link{update.packages}}.  Defaults to
      \code{c(CRAN="@CRAN@")}, a value that causes some utilities to
      prompt for a CRAN mirror.  To avoid this do set the CRAN mirror,
      by something like
      \code{local({r <- getOption("repos"); r["CRAN"] <- "http://my.local.cran";
                   options(repos = r)})}.

      Note that you can add more repositories (Bioconductor,
      R-Forge, Rforge.net \dots)
      using \code{\link{setRepositories}}.
    }
    \item{\code{SweaveHooks}, \code{SweaveSyntax}:}{
      see \code{\link{Sweave}}.
    }

    \item{\code{unzip}:}{a character string used by \code{\link{unzip}}:
      the path of the external program \command{unzip} or \code{"internal"}.
      Defaults (platform dependently)
      \describe{
	\item{on unix-alikes}{to the value of \env{R_UNZIPCMD}, which is set in
	  \file{etc/Renviron} to the path of the \code{unzip} command found
	  during configuration and otherwise to \code{""}.}
	\item{on Windows}{to \code{"internal"} when the internal unzip
	  code is used.}
      }
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

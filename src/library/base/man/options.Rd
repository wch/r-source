\name{options}
\alias{options}
\alias{.Options}
\alias{getOption}
\title{Options Settings}
\description{
  Allow the user to set and examine a variety of global \dQuote{options}
  which affect the way in which \R computes and displays its results.
}
\usage{
options(\dots)
getOption(x)
.Options
}
%%-- NOTE : Keep the list up-to-date,
%%-- ====   Consider also the leading comments in ../../../main/options.c
\arguments{
  \item{\dots}{any options can be defined, using \code{name = value} or
    by passing a list of such tagged values.  However, only the ones
    below are used in \dQuote{base \R}.
    Further, \code{options('name') == options()['name']}, see the example.
  }
  \item{x}{a character string holding an option name.}
}
\details{
  Invoking \code{options()} with no arguments returns a list with the
  current values of the options.  Note that not all options listed below
  are set initially.  To access the value of a single option, one should
  use \code{getOption("width")}, e.g., rather than
  \code{options("width")} which is a \emph{list} of length one.

  \code{.Options} also always contains the \code{options()} list, for
  S compatibility.  You must use it \dQuote{read only} however.
}
\value{
  For \code{options}, a list (in any case) with the previous values of
  the options changed, or all options when no arguments were given.
}
\section{Options used in base \R}{
  \describe{
    \item{\code{prompt}:}{a string, used for \R's prompt; should usually
      end in a blank (\code{" "}).}

    \item{\code{continue}:}{a string setting the prompt used for lines
      which continue over one line.}

    \item{\code{width}:}{controls the number of characters on a line.
      You may want to change this if you re-size the window that \R is
      running in.  Valid values are 10\dots10000 with default normally
      80.  (The valid values are in file \file{Print.h} and can be
      changed by re-compiling \R.)}

    \item{\code{digits}:}{controls the number of digits to print when
      printing numeric values.  It is a suggestion only.  Valid values
      are 1\dots22 with default 7.  See \code{\link{print.default}}.}

    \item{\code{editor}:}{sets the default text editor, e.g., for
      \code{\link{edit}}.  Set from the environment variable
      \code{VISUAL} on UNIX.}

    \item{\code{pager}:}{the (stand-alone) program used for displaying
      ASCII files on \R's console, also used by \code{\link{file.show}} and
      sometimes \code{\link{help}}.
#ifdef unix
      Defaults to \file{\$R\_HOME/bin/pager}.
#endif
#ifdef windows
      Defaults to \code{"internal"}, which uses a pager similar to the
      GUI console.  Another possibility is \code{"console"} to use the
      console itself.
#endif
    }

    \item{\code{browser}:}{default HTML browser used by
      \code{\link{help.start}()} on UNIX, or a non-default browser on
      Windows.}

#ifdef unix
    \item{\code{pdfviewer}:}{default PDF viewer.  Set from the
      environment variable \env{R\_PDFVIEWER}.}
#endif

    \item{\code{mailer}:}{default mailer used by
      \code{\link{bug.report}()}.  Can be \code{"none"}.}

    \item{\code{contrasts}:}{the default \code{\link{contrasts}} used in
      model fitting such as with \code{\link{aov}} or \code{\link{lm}}.
      A character vector of length two, the first giving the function to
      be used with unordered factors and the second the function to be
      used with ordered factors.}

    \item{\code{defaultPackages}:}{the packages that are attached by
      default when \R starts up.  Initially set from value of the
      environment variables \env{R\_DefaultPackages}, or if that is unset
      %%  default is set in ../../profile/Common.R :
      to \code{c("utils", "stats", "graphics", "methods")}.  (Set
      \env{R\_DEFAULT\_PACKAGES} to \code{NULL} or a comma-separated list
      of package names.)  A call to \code{options} should be in your
      \file{.Rprofile} file to ensure that the change takes effect
      before the base package is initialized (see \code{\link{Startup}}).}

    \item{\code{expressions}:}{sets a limit on the number of nested
      expressions that will be evaluated.
      Valid values are 25\dots100000 with default 500.}

    \item{\code{keep.source}:}{When \code{TRUE}, the source code for
      functions (newly defined or loaded) is stored in their
      \code{"source"} attribute (see \code{\link{attr}}) allowing
      comments to be kept in the right places.

      The default is \code{\link{interactive}()}, i.e., \code{TRUE} for
      interactive use.}

    \item{\code{keep.source.pkgs}:}{As for \code{keep.source}, for
      functions in packages loaded by \code{\link{library}} or
      \code{\link{require}}.  Defaults to \code{FALSE} unless the
      environment variable \env{R\_KEEP\_PKG\_SOURCE} is set to
      \code{yes}.}

    \item{\code{na.action}:}{the name of a function for treating missing
      values (\code{\link{NA}}'s) for certain situations.}

    \item{\code{papersize}:}{the default paper format used by
      \code{\link{postscript}};  set by environment variable
      \env{R\_PAPERSIZE} when \R is started and defaulting to
      \code{"a4"} if that is unset or invalid.}

    \item{\code{printcmd}:}{the command used by \code{\link{postscript}}
      for printing; set by environment variable \env{R\_PRINTCMD} when
      \R is started.  This should be a command that expects either input
      to be piped to \file{stdin} or to be given a single filename
      argument.
#ifdef windows
      See \code{\link{postscript}} for ways to set this up.
#endif
    }

#ifdef unix
    \item{\code{latexcmd, dvipscmd}:}{character strings giving commands
      to be used in off-line printing of help pages.}
#endif

    \item{\code{show.signif.stars, show.coef.Pvalues}:}{logical,
      affecting P value printing, see \code{\link{printCoefmat}}.}

    \item{\code{ts.eps}:}{the relative tolerance for certain time series
      (\code{\link{ts}}) computations.}

    \item{\code{error}:}{either a function or an expression governing
      the handling of non-catastrophic errors such as those generated by
      \code{\link{stop}} as well as by signals and internally detected
      errors.  If the option is a function, a call to that function,
      with no arguments, is generated as the expression.  The default
      value is \code{NULL}: see \code{\link{stop}} for the behaviour in
      that case.  The function \code{\link{dump.frames}} provides one
      alternative that allows post-mortem debugging.}

    \item{\code{show.error.messages}:}{a logical.  Should error messages
      be printed?  Intended for use with \code{\link{try}} or a
      user-installed error handler.}

    \item{\code{warn}:}{sets the handling of warning messages.  If
      \code{warn} is negative all warnings are ignored.  If \code{warn}
      is zero (the default) warnings are stored until the top--level
      function returns.  If fewer than 10 warnings were signalled they
      will be printed otherwise a message saying how many (max 50) were
      signalled.  A top--level variable called \code{last.warning} is
      created and can be viewed through the function
      \code{\link{warnings}}.  If \code{warn} is one, warnings are
      printed as they occur.  If \code{warn} is two or larger all
      warnings are turned into errors.}

    \item{\code{warning.length}:}{sets the truncation limit for error
      and warning messages.  A non-negative integer, with allowed values
      100--8192, default 1000.}

    \item{\code{warning.expression}:}{an \R code expression to be called
      if a warning is generated, replacing the standard message.  If
      non-null is called irrespective of the value of option
      \code{warn}.}
#ifdef windows

	\item{\code{warn.FPU}:}{logical, by default undefined.  If true,
	  a \link{warning} is produced whenever \link{dyn.load} repairs the
	  control word damaged by a buggy DLL.}
#endif

    \item{\code{check.bounds}:}{logical, defaulting to \code{FALSE}.  If
      true, a \link{warning} is produced whenever a \dQuote{generalized
      vector} (atomic or \code{\link{list}}) is extended, by something
      like \code{x <- 1:3; x[5] <- 6}.}

    \item{\code{echo}:}{logical.  Only used in non-interactive mode,
      when it controls whether input is echoed.  Command-line option
      \option{--slave} sets this initially to \code{FALSE}.}

    \item{\code{verbose}:}{logical.  Should \R report extra information
      on progress?  Set to \code{TRUE} by the command-line option
      \option{--verbose}.}

    \item{\code{device}:}{a character string giving the default device
      for that session.  This defaults to the normal screen device
      (e.g., \code{x11}, \code{windows} or \code{gtk}) for an
      interactive session, and \code{postscript} in batch use or if a
      screen is not available.}

#ifdef unix
    \item{\code{X11colortype}:}{The default colour type for
      \code{\link{X11}} devices.}
#endif

    \item{\code{CRAN}:}{The URL of the preferred CRAN node for use by
      \code{\link{update.packages}}.  Defaults to
      \url{http://cran.r-project.org}.}

    \item{\code{repositories}:}{A function that when evaluated returns a
      named vector of URLs which can be used to specify the set and
      order of any online repositories the user would like used in
      functions such as \code{\link{pkgDepends}}.  The default
      return value for the function is a vector with two components, one the
      value of \code{contrib.url(getOption("CRAN"))} and the second
      \code{contrib.url(getOption("BIOC"))}.}

    \item{\code{download.file.method}:}{Method to be used for
      \code{download.file}.  Currently download methods
      \code{"internal"}, \code{"wget"} and \code{"lynx"} are available.
      There is no default for this option, when \code{method = "auto"}
      is chosen: see \code{\link{download.file}}.}

    \item{\code{unzip}:}{the command used for unzipping help files.
#ifdef unix
      Defaults to the value of \env{R\_UNZIPCMD}, which is set in
      \file{etc/Renviron} if an \code{unzip} command was found during
      configuration.}
#endif
#ifdef windows
      Defaults to \code{"internal"} when the internal unzip code is
      used.}
#endif

    \item{\code{de.cellwidth}:}{integer: the cell widths (number of
      characters) to be used in the data editor \code{\link{dataentry}}.
      If this is unset, 0, negative or \code{NA}, variable cell widths
      are used.}

    \item{\code{encoding}:}{An integer vector of length 256 holding an
      input encoding.  Defaults to \code{native.enc} (= \code{0:255}).
      See \code{\link{connections}}.}

    \item{\code{timeout}:}{integer.  The timeout for some Internet
      operations, in seconds.  Default 60 seconds.
      See \code{\link{download.file}} and \code{\link{connections}}.}

    \item{\code{internet.info}:}{The minimum level of information to be
      printed on URL downloads etc.  Default is 2, for failure causes.
      Set to 1 or 0 to get more information.}

    \item{\code{scipen}:}{integer.  A penalty to be applied
      when deciding to print numeric values in fixed or exponential
      notation.  Positive values bias towards fixed and negative towards
      scientific notation: fixed notation will be preferred unless it is
      more than \code{scipen} digits wider.}

    \item{\code{locatorBell}:}{logical.  Should selection in \code{locator}
      and \code{identify} be confirmed by a bell.  Default \code{TRUE}.
      Honoured at least on \code{X11} and \code{windows} devices.}
#ifdef unix
    \item{\code{X11fonts}:}{character vector of length 2.
      See \code{\link{X11}}.}
#endif
  }

  The default settings of some of these options are
  \tabular{llll}{
    \code{prompt} \tab \code{"> "} \tab
    \code{continue} \tab \code{"+ "}\cr
    \code{width}  \tab \code{80} \tab
    \code{digits} \tab \code{7}\cr
    \code{expressions} \tab \code{500} \tab
    \code{keep.source} \tab \code{TRUE}\cr
    \code{show.signif.stars} \tab \code{TRUE} \tab
    \code{show.coef.Pvalues} \tab \code{TRUE}\cr
    \code{na.action} \tab \code{na.omit} \tab
    \code{ts.eps} \tab \code{1e-5}\cr
    \code{error} \tab \code{NULL} \tab
    \code{show.error.messages} \tab \code{TRUE} \cr
    \code{warn} \tab \code{0}\tab
    \code{warning.length} \tab \code{1000}\cr
    \code{echo} \tab \code{TRUE} \tab
    \code{verbose} \tab \code{FALSE} \cr
    \code{scipen} \tab \code{0} \tab \code{locatorBell} \tab \code{TRUE}
  }
  Others are set from environment variables or are platform-dependent.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole.
}
\examples{
options() # printing all current options
op <- options(); str(op) # nicer printing

# .Options is the same:
all(sapply(1:length(op), function(i) if(is.atomic(op[[i]]))
                         {all(.Options[[i]] == op[[i]])} else TRUE))

options('width')[[1]] == options()$width # the latter needs more memory
options(digits=20)
pi

# set the editor, and save previous value
old.o <- options(editor="nedit")
old.o

options(check.bounds = TRUE)
x <- NULL; x[4] <- "yes" # gives a warning

options(digits=5)
print(1e5)
options(scipen=3); print(1e5)

options(op)     # reset (all) initial options
options('digits')

\dontrun{## set contrast handling to be like S
options(contrasts=c("contr.helmert", "contr.poly"))
}
\dontrun{## on error, terminate the R session with error status 66
options(error=quote(q("no", status=66, runLast=FALSE)))
stop("test it")
}
\dontrun{## set an error action for debugging: see ?debugger.
options(error=dump.frames)
## A possible setting for non-interactive sessions
options(error=quote({dump.frames(to.file=TRUE); q()}))
}}
\keyword{environment}
\keyword{error}
\keyword{print}

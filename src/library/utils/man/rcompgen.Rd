% File src/library/utils/man/rcompgen.Rd
% Part of the R package, http://www.R-project.org
% Copyright 2008-2014 R Core Team
% Distributed under GPL 2 or later

\name{rcompgen}
\alias{rcompgen}
\alias{rc.settings}
\alias{rc.status}
\alias{rc.getOption}
\alias{rc.options}
\alias{.DollarNames}
\alias{.DollarNames.default}
\alias{.DollarNames.list}
\alias{.DollarNames.environment}
\alias{completion}

\title{A Completion Generator for R}

\description{
  This page documents a mechanism to generate relevant completions
  from a partially completed command line.  It is not intended to be
  useful by itself, but rather in conjunction with other mechanisms that
  use it as a backend.  The functions listed in the usage section
  provide a simple control and query mechanism.  The actual interface
  consists of a few unexported functions described further down.
}

\usage{

rc.settings(ops, ns, args, func, ipck, S3, data, help,
            argdb, fuzzy, quotes, files)

rc.status()
rc.getOption(name)
rc.options(\dots)

.DollarNames(x, pattern)

\S3method{.DollarNames}{default}(x, pattern = "")
\S3method{.DollarNames}{list}(x, pattern = "")
\S3method{.DollarNames}{environment}(x, pattern = "")

}

\arguments{
  \item{ops, ns, args, func, ipck, S3, data, help, argdb, fuzzy, quotes, files}{

    logical, turning some optional completion features on and off.

    \describe{
      \item{\code{ops}:}{ Activates completion after the \code{$} and
        \code{@} operators. }

      \item{\code{ns}:}{ Controls namespace related completions. }

      \item{\code{args}:}{ Enables completion of function arguments. }

      \item{\code{func}:}{ Enables detection of functions.  If enabled,
        a customizable extension (\code{"("} by default) is appended to
        function names.  The process of determining whether a potential
        completion is a function requires evaluation, including for lazy
        loaded symbols.  This is undesirable for large objects, because
        of potentially wasteful use of memory in addition to the time
        overhead associated with loading.  For this reason, this feature
        is disabled by default. }

      \item{\code{S3}:}{ When \code{args = TRUE}, activates completion on
        arguments of all S3 methods (otherwise just the generic, which
        usually has very few arguments). }

      \item{\code{ipck}:}{ Enables completion of installed package names
        inside \code{\link{library}} and \code{\link{require}}. }

      \item{\code{data}:}{ Enables completion of data sets (including
        those already visible) inside \code{\link{data}}. }

      \item{\code{help}:}{ Enables completion of help requests starting
        with a question mark, by looking inside help index files. }

      \item{\code{argdb}:}{ When \code{args = TRUE}, completion is
        attempted on function arguments.  Generally, the list of valid
        arguments is determined by dynamic calls to \code{\link{args}}.
        While this gives results that are technically correct, the use
        of the \code{\dots} argument often hides some useful arguments.
        To give more flexibility in this regard, an optional table of
        valid arguments names for specific functions is retained
        internally.  Setting \code{argdb = TRUE} enables preferential
        lookup in this internal data base for functions with an entry in
        it.  Of course, this is useful only when the data base contains
        information about the function of interest.  Some functions are
        already included, and more can be added by the user through the
        unexported function \code{.addFunctionInfo} (see below).  }

      \item{\code{fuzzy}:}{ Enables fuzzy matching, where close but
	non-exact matches (e.g., with different case) are considered if
	no exact matches are found.  This feature is experimental and
	the details can change.  }

      \item{\code{quotes}:}{ Enables completion in \R code when inside
        quotes.  This normally leads to filename completion, but can be
        otherwise depending on context (for example, when the open quote
        is preceded by \code{?}, help completion is invoked.  Setting
        this to \code{FALSE} relegates completion to the underlying
        completion front-end, which may do its own processing (for
        example, \code{readline} on Unix-alikes will do filename
        completion.  }

      \item{\code{files}:}{ Deprecated.  Use \code{quotes} instead.  }

    }

    All settings are turned on by default except \code{ipck},
    \code{func}, and \code{fuzzy}.  Turn more off if your CPU cycles are
    valuable; you will still retain basic completion on names of objects
    in the search list.  See below for additional details.

  }
  \item{name, \dots}{ user-settable options.  Currently valid names are
    \describe{
      \item{\code{function.suffix}:}{ default \code{"("} }
      \item{\code{funarg.suffix}:}{ default \code{" = "} }
      \item{\code{package.suffix}}{ default \code{"::"} }
    }
    Usage is similar to that of \code{\link{options}}.
  }

  \item{x}{ An R object for which valid names after \code{"$"}
    are computed and returned.
  }

  \item{pattern}{ A regular expression.  Only matching names are
    returned.
  }
}

\details{
  There are several types of completion, some of which can be disabled
  using \code{rc.settings}.  The most basic level, which can not be
  turned off once the completion functionality is activated, provides
  completion on names visible on the search path, along with a few
  special keywords (e.g., \code{TRUE}).  This type of completion is not
  attempted if the partial \sQuote{word} (a.k.a. token) being completed
  is empty (since there would be too many completions).  The more
  advanced types of completion are described below.

  \describe{

    \item{\bold{Completion after extractors \code{$} and \code{@}}:}{
      When the \code{ops} setting is turned on, completion after
      \code{$} and \code{@} is attempted.  This requires the prefix to
      be evaluated, which is attempted unless it involves an explicit
      function call (implicit function calls involving the use of
      \code{[}, \code{$}, etc \emph{do not} inhibit evaluation).

      Valid completions after the \code{$} extractor are determined by
      the generic function \code{.DollarNames}.  Some basic methods are
      provided, and more can be written for custom classes.
    }

    \item{\bold{Completion inside namespaces}:}{
      When the \code{ns} setting is turned on, completion inside
      namespaces is attempted when a token is preceded by the \code{::}
      or \code{:::} operators.  Additionally, the basic completion
      mechanism is extended to include all loaded namespaces, i.e.,
      \code{foopkg::} becomes a valid completion of \code{foo} if
      \code{"foopkg"} is a loaded namespace.

      The completion of package namespaces applies only to already
      loaded namespaces, i.e.\sspace{}if \code{MASS} is not loaded,
      \code{MAS} will not complete to \code{MASS::}.  However, attempted
      completion \emph{inside} an apparent namespace will attempt to
      load the namespace if it is not already loaded,
      e.g.\sspace{}trying to complete on \code{MASS::fr} will load
      \code{MASS} if it is not already loaded.
    }

    \item{\bold{Completion for help items}:}{
      When the \code{help} setting is turned on, completion on help
      topics is attempted when a token is preceded by \code{?}.
      Prefixes (such as \code{class}, \code{method}) are supported, as
      well as quoted help topics containing special characters.
    }

    \item{\bold{Completion of function arguments}:}{
      When the \code{args} setting is turned on, completion on function
      arguments is attempted whenever deemed appropriate.  The mechanism
      used will currently fail if the relevant function (at the point
      where completion is requested) was entered on a previous prompt
      (which implies in particular that the current line is being typed
      in response to a continuation prompt, usually \code{+}).  Note
      that separation by newlines is fine.

      The list of possible argument completions that is generated can be
      misleading.  There is no problem for non-generic functions (except
      that \code{\dots} is listed as a completion; this is intentional
      as it signals the fact that the function can accept further
      arguments).  However, for generic functions, it is practically
      impossible to give a reliable argument list without evaluating
      arguments (and not even then, in some cases), which is risky (in
      addition to being difficult to code, which is the real reason it
      hasn't even been tried), especially when that argument is itself
      an inline function call.  Our compromise is to consider arguments
      of \emph{all} currently available methods of that generic.  This
      has two drawbacks.  First, not all listed completions may be
      appropriate in the call currently being constructed.  Second, for
      generics with many methods (like \code{print} and \code{plot}),
      many matches will need to be considered, which may take a
      noticeable amount of time.  Despite these drawbacks, we believe
      this behaviour to be more useful than the only other practical
      alternative, which is to list arguments of the generic only.

      Only S3 methods are currently supported in this fashion, and that
      can be turned off using the \code{S3} setting.

      Since arguments can be unnamed in \R function calls, other types
      of completion are also appropriate whenever argument completion
      is.  Since there are usually many many more visible objects than
      formal arguments of any particular function, possible argument
      completions are often buried in a bunch of other possibilities.
      However, recall that basic completion is suppressed for blank
      tokens.  This can be useful to list possible arguments of a
      function.  For example, trying to complete \code{seq([TAB]} and
      \code{seq(from = 1, [TAB])} will both list only the arguments of
      \code{seq} (or any of its methods), whereas trying to complete
      \code{seq(length[TAB]} will list both the \code{length.out}
      argument and the \code{length(} function as possible completions.
      Note that no attempt is made to remove arguments already supplied,
      as that would incur a further speed penalty.
    }

    \item{\bold{Special functions}:}{
      For a few special functions (\code{\link{library}},
      \code{\link{data}}, etc), the first argument is treated specially,
      in the sense that normal completion is suppressed, and some
      function specific completions are enabled if so requested by the
      settings.  The \code{ipck} setting, which controls whether
      \code{\link{library}} and \code{\link{require}} will complete on
      \emph{installed packages}, is disabled by default because the
      first call to \code{\link{installed.packages}} is potentially time
      consuming (e.g., when packages are installed on a remote network
      file server).  Note, however, that the results of a call to
      \code{\link{installed.packages}} is cached, so subsequent calls
      are usually fast, so turning this option on is not particularly
      onerous even in such situations.
    }

  }
}

\value{
  If \code{rc.settings} is called without any arguments, it returns the
  current settings as a named logical vector.  Otherwise, it returns
  \code{NULL} invisibly. 
  
  \code{rc.status} returns, as a list, the contents of an internal
  (unexported) environment that is used to record the results of the
  last completion attempt.  This can be useful for debugging.  For such
  use, one must resist the temptation to use completion when typing the
  call to \code{rc.status} itself, as that then becomes the last attempt
  by the time the call is executed.

  The items of primary interest in the returned list are:

  \item{comps}{ The possible completions generated by the last
    call to \code{.completeToken}, as a character vector. }
  \item{token}{ The token that was (or, is to be) completed, as
    set by the last call to \code{.assignToken} (possibly inside a call
    to \code{.guessTokenFromLine}). }
  \item{linebuffer}{ The full line, as set by the last call to
    \code{.assignLinebuffer}. }
  \item{start}{ The start position of the token in the line
    buffer, as set by the last call to \code{.assignStart}. }
  \item{end}{ The end position of the token in the line
    buffer, as set by the last call to \code{.assignEnd}. }
  \item{fileName}{ Logical, indicating whether the cursor is
    currently inside quotes. }
  \item{fguess}{ The name of the function the cursor is currently inside. }
  \item{isFirstArg}{ Logical.  If cursor is inside a function, is it the
    first argument? }

  In addition, the components \code{settings} and \code{options} give
  the current values of settings and options respectively.

  \code{rc.getOption} and \code{rc.options} behave much like
  \code{\link{getOption}} and \code{\link{options}} respectively.
}


\section{Unexported API}{

  There are several unexported functions in the package.  Of these,
  a few are special because they provide the API through which other
  mechanisms can make use of the facilities provided by this package
  (they are unexported because they are not meant to be called directly
  by users).  The usage of these functions are:

\preformatted{    .assignToken(text)
    .assignLinebuffer(line)
    .assignStart(start)
    .assignEnd(end)

    .completeToken()
    .retrieveCompletions()
    .getFileComp()

    .guessTokenFromLine()
    .win32consoleCompletion(linebuffer, cursorPosition,
                            check.repeat = TRUE,
                            minlength = -1)

    .addFunctionInfo(...)
}

  The first four functions set up a completion attempt by specifying the
  token to be completed (\code{text}), and indicating where
  (\code{start} and \code{end}, which should be integers) the token is
  placed within the complete line typed so far (\code{line}).

  Potential completions of the token are generated by
  \code{.completeToken}, and the completions can be retrieved as an \R
  character vector using \code{.retrieveCompletions}. It is possible for
  the user to specify a replacement for this function by setting
  \code{rc.options("custom.completer")}; if not \code{NULL}, this
  function is called to compute potential completions.  This facility is
  meant to help in situations where completing as R code is not
  appropriate. See source code for more details.

  If the cursor is inside quotes, completion may be suppressed.  The
  function \code{.getFileComp} can be used after a call to
  \code{.completeToken} to determine if this is the case (returns
  \code{TRUE}), and alternative completions generated as deemed useful.
  In most cases, filename completion is a reasonable fallback.

  The \code{.guessTokenFromLine} function is provided for use with
  backends that do not already break a line into tokens.  It requires
  the linebuffer and endpoint (cursor position) to be already set, and
  itself sets the token and the start position.  It returns the token as
  a character string.

  The \code{.win32consoleCompletion} is similar in spirit, but is more
  geared towards the Windows GUI (or rather, any front-end that has no
  completion facilities of its own).  It requires the linebuffer
  and cursor position as arguments, and returns a list with three
  components, \code{addition}, \code{possible} and \code{comps}.  If
  there is an unambiguous extension at the current position,
  \code{addition} contains the additional text that should be inserted
  at the cursor.  If there is more than one possibility, these are
  available either as a character vector of preformatted strings in
  \code{possible}, or as a single string in \code{comps}.
  \code{possible} consists of lines formatted using the current
  \code{width} option, so that printing them on the console one line at
  a time will be a reasonable way to list them.  \code{comps} is a space
  separated (collapsed) list of the same completions, in case the
  front-end wishes to display it in some other fashion.

  The \code{minlength} argument can be used to suppress completion when
  the token is too short (which can be useful if the front-end is set up
  to try completion on every keypress).  If \code{check.repeat} is
  \code{TRUE}, it is detected if the same completion is being requested
  more than once in a row, and ambiguous completions are returned only
  in that case.  This is an attempt to emulate GNU Readline behaviour,
  where a single TAB completes up to any unambiguous part, and multiple
  possibilities are reported only on two consecutive TABs.

  As the various front-end interfaces evolve, the details of these
  functions are likely to change as well.

  The function \code{.addFunctionInfo} can be used to add information
  about the permitted argument names for specific functions.  Multiple
  named arguments are allowed in calls to it, where the tags are names
  of functions and values are character vectors representing valid
  arguments.  When the \code{argdb} setting is \code{TRUE}, these are
  used as a source of valid argument names for the relevant functions.
}

\note{
  If you are uncomfortable with unsolicited evaluation of pieces of
  code, you should set \code{ops = FALSE}.  Otherwise, trying to
  complete \code{foo@ba} will evaluate \code{foo}, trying to complete
  \code{foo[i, 1:10]$ba} will evaluate \code{foo[i, 1:10]}, etc.  This
  should not be too bad, as explicit function calls (involving
  parentheses) are not evaluated in this manner.  However, this
  \emph{will} affect promises and lazy loaded symbols.
}

\author{ Deepayan Sarkar, \email{deepayan.sarkar@r-project.org} }

\keyword{IO}
\keyword{utilities}

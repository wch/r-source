% -*- coding: utf-8 -*-
\newcommand{\Rlogo}{\if{html}{\figure{../../html/logo.jpg}}\if{latex}{\figure{logo.jpg}{options: width=0.5in}}}

\name{NEWS}
\title{R News}
\encoding{UTF-8}

\section{\Rlogo CHANGES IN R-devel}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item \code{anyNA()} gains a \code{recursive} argument.

      \item When \code{x} is missing and \code{names} is not false
      (including the default value), \code{Sys.getenv(x, names)} returns
      an object of class \code{"Dlist"} and hence prints tidily.

      \item (Windows.)  \code{shell()} no longer consults the
      environment variable \env{SHELL}: too many systems have been
      encountered where it was set incorrectly (usually to a path where
      software was compiled, not where it was installed).
      \env{R_SHELL}, the preferred way to select a non-default shell,
      can be used instead.

      \item \code{embedFonts()} now defaults to \code{format =
      "ps2write"} for \file{.ps} and \file{.eps} files.  This is
      available in Ghostscript 9.x (since 2010) whereas the previous
      default, \code{format = "pswrite"}, was removed in 9.10.

      \item Some unusual arguments to \code{embedFonts()} can now be
      specified as character vectors, and the defaults have changed
      accordingly.

      \item Functions in the \code{Summary} group duplicate less.
      (\PR{15798})

      \item (Unix-alikes.) \code{system(cmd, input = )} now uses
      \sQuote{shell-execution-environment} redirection, which will be
      more natural if \code{cmd} is not a single command (but requires a
      POSIX-compliant shell). (\PR{15508})

      \item \code{read.fwf()} and \code{read.DIF()} gain a
      \code{fileEncoding} argument, for convenience.

      \item Graphics devices can add attributes to their description in
      \code{.Device} and \code{.Devices}.  Several of those included with
      \R use a \code{"filepath"} attribute.

      \item \code{pmatch()} uses hashing in more cases and so is faster
      at the expense of using more memory. (\PR{15697})

      \item \code{pairs()} gains new arguments to select sets of
      variables to be plotted against each other.

      \item \code{file.info(, extra_cols = FALSE)} allows a minimal set
      of columns to be computed: on some systems this can be
      significantly faster with large file lists.

      \item New function \code{dir.exists()} in \pkg{base} to test
      efficiently whether one or more paths exist and are directories.

      \item \code{dput()} and friends gain new controls
      \samp{"hexNumeric"} and \samp{"digits17"} which output double and
      complex quantities as, respectively, binary fractions (exactly,
      see \code{sprintf("\%a")}) and as decimals with up to 17
      significant digits.

      \item \code{save()}, \code{saveRDS()} and \code{serialize()} now
      support \code{ascii = NA} which writes ASCII files using the
      \code{sprintf("\%a")}) for double/complex quantities.  This is
      read-compatible with \code{ascii = TRUE} but avoids
      binary->decimal->binary conversions with potential loss of
      precision.  Unfortunately Windows' lack of C99 compliance means
      that the format can be written but not read correctly there in \R
      before 3.2.0.

      \item The default for \code{formatC(decimal.mark =)} has been
      changed to be \code{getOption("OutDec")}; this makes it more
      consistent with \code{format()} and suitable for use in print
      methods.

      \item The print methods for classes \code{"density"},
      \code{"ecdf"}, \code{"kmeans"}, \code{"stepfun"} and
      \code{"summary.lm"} now consult \code{getOption("OutDec")} even
      where \code{formatC()} or \code{sprintf()} is used.

      Similarly when \code{cut()}, \code{dendrogram()}, \code{plot.ts()}
      and \code{quantile()} construct labels, and for the report from
      \code{legend(trace = TRUE)}.

      (In part, wish of \PR{15819}.)

      \item \code{printNum()} (and hence \code{format()} and
      \code{formatC()} give a warning if \code{big.mark} and
      \code{decimal.mark} are set to the same value (period and comma
      are not uncommonly used for each, and this is a check that
      conventions have not got mixed).
      
      \item \code{merge()} can create a result which uses long vectors
      on 64-bit platforms.
      
      \item \code{dget()} gains a new argument \code{keep.source} which
      defaults to \code{FALSE} for speed (\code{dput()} and
      \code{dget()} are most often used for data objects where this can
      make \code{dget()} many times faster).
      
      \item Packages may now use a file of common macro definitions in their
      help files, and may import definitions from other packages.  
      
      \item A number of macros have been added to the new \file{share/Rd} 
      directory for use in package overview help
      pages, and \code{promptPackage()} now makes use of them. 
    }
  }
  
  \subsection{INSTALLATION and INCLUDED SOFTWARE}{
    \itemize{
      \item When building \R as a shared library, the
      \option{-L${R_HOME}/lib${R_ARCH}} flag is placed earlier in the
      link commands used during installation and when packages are
      installed: this helps ensure that the current build has priority if
      an \R shared library has already been installed by e.g.
      \command{install-libR} in a library mentioned in \env{LDFLAGS}
      (and not in \sQuote{your system's library directory} as
      documented). (Wish of \PR{15790}.)

      \item LaTeX package \pkg{upquote} is no longer required for \R's
      use of \pkg{inconsolata}.

      \item The \code{\\donttest} sections of \R's help files can be
      tested by\cr
      \command{make check TEST_DONTTEST=TRUE}.
    }
  }

  \subsection{UTILITIES}{
    \itemize{
      \item There is some support for testing the \code{\\dontrun} and
      \code{\\donttest} parts of examples in packages.

      \code{tools::testInstalledPackage()} accepts new arguments
      \code{commentDontrun = FALSE} and \code{commentDonttest = FALSE}.

      \command{R CMD check} gains \option{--run-dontrun} and
      \option{--run-donttest} options.
    }
  }
  
  \subsection{C-LEVEL FACILITIES}{
    \itemize{
      \item New C API utility \code{logspace_sum(logx[], n)}.
    }
  }

  \subsection{BUG FIXES}{
    \itemize{
      \item For consistency with \code{[dpq]lnorm()},
      \code{rlnorm(sdlog = 0)} is now an error and so returns \code{NaN}.
    }
  }
}

\section{\Rlogo CHANGES IN R 3.1.0 patched}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item When \code{attach()} reports conflicts, it does so
      compatibly with \code{library()} by using \code{message()}.

      \item \command{R CMD Sweave} no longer cleans any files by
      default, compatibly with versions of \R prior to 3.1.0.  There are
      new options \option{--clean}, \option{--clean=default} and
      \option{--clean=keepOuts}.

      \item \code{tools::buildVignette()} and \code{tools::buildVignettes()}
      with \code{clean = FALSE} no longer remove any created files.
      \code{buildvignette()} gains a \code{keep} argument for more
      cleaning customization.

      \item The Bioconductor \sQuote{version} used by
      \code{setRepositories()} can now be set by environment variable
      \env{R_BIOC_VERSION} at runtime, not just when \R is installed.
      (It has been stated that Bioconductor will switch from
      \sQuote{version} 2.14 to \sQuote{version} 3.0 during the lifetime
      of the \R 3.1 series.)

      \item Error messages from bugs in embedded \samp{Sexpr} code
      in Sweave documents now report the source location.

      \item \code{type.convert()}, \code{read.table()} and similar
      \code{read.*()} functions get a new \code{numerals} argument,
      specifying how numeric input is converted when its conversion to
      double precision loses accuracy.  The default value,
      \code{"allow.loss"} allows accuracy loss, as in \R versions before
      3.1.0.

      \item For some compilers, integer addition could overflow without
      a warning.  \R's internal code for both integer addition and
      subtraction is more robust now.  (\PR{15774})

      \item The function determining the default number of knots
      for \code{smooth.spline()} is now exported, as \code{.nknots.smspl}.

      \item \code{dbeta(, a,b)}, \code{pbeta()}, \code{qbeta()} and
      \code{rbeta()} are now defined also for \eqn{a = 0}, \eqn{b = 0},
      or infinite \eqn{a} and \eqn{b} (where they typically returned
      \code{NaN} before).

      \item Many package authors report that the RStudio graphics device
      does not work correctly with their package's use of
      \code{dev.new()}.  The new option \code{dev.new(noRStudioGD =
      TRUE)} replaces the RStudio override by the default device as
      selected by \R itself, still respecting environment variables
      \env{R_INTERACTIVE_DEVICE} and \env{R_DEFAULT_DEVICE}.

      \item \code{readRDS()} now returns visibly.

      \item Modifying internal logical scalar constants now results in
      an error instead of a warning.
      
      \item \code{install.packages(repos = NULL)} now accepts
      \code{http://} or \code{ftp://} URLs of package archives as well
      as file paths, and will download as required.  In most cases
      \code{repos = NULL} can be deduced from the extension of the URL.
      
      \item The warning when using partial matching with the \code{$}
      operator on data frames is now only given when
      \code{options("warnPartialMatchDollar")} is \code{TRUE}.
      
      \item Package help requests like \code{package?foo} now 
      try the package \code{foo} whether loaded or not.
      
      \item General help requests now default to trying all loaded 
      packages, not just those on the search path.
    }
  }

  \subsection{INSTALLATION and INCLUDED SOFTWARE}{
    \itemize{
      \item \command{configure} option \option{--with-internal-tzcode}
      can now be used with variable \code{rsharedir}.

      \item The included version of PCRE has been updated to 8.35.

      \item There is a new target \command{make uninstall-libR} to
      remove an installed shared/static \file{libR}.

       \command{make install-libR} now works if a sub-architecture is
       used, although the user will need to specify \code{libdir}
       differently for different sub-architectures.

       \item There is more extensive advice on which LaTeX packages are
       required to install \R or to make package manuals (as done by
       \command{R CMD check}) in the \sQuote{Writing R Extensions}
       manual.

       \item Compilers were handling the visibility control in
       \file{src/extra/xz} inconsistently, so it has been simplified.
       (\PR{15327})
     }
  }

  \subsection{BUG FIXES}{
    \itemize{
      \item \code{dbinom(x,n)}, \code{pbinom()}, \code{dpois()}, etc,
      are slightly less restrictive in checking if \code{n} is
      integer-valued.  (Wish of \PR{15734}.)

      \item \code{pchisq(x, df, ncp, log.p = TRUE)} is more accurate and
      no longer underflows for small \code{x} and \code{ncp < 80}, e.g,
      for \code{pchisq(1e-5, df = 100, ncp = 1, log = TRUE)}.
      (Based on \PR{15635} and a suggestion by Roby Joehanes.)

      \item The \code{s} (\dQuote{step into}) command in the debugger
      would cause \R to step into expressions evaluated there,
      not just into functions being debugged.  (\PR{15770})

      \item The third-party code used by \code{strptime()} rejected
      time-zone offsets of more than \code{+1200} (\code{+1245},
      \code{+1300} and \code{+1400} can occur).  (\PR{15768})

      \item (Windows only.)
      \code{png(type = "cairo", antialias = "gray")}
      was not accepted.  (\PR{15760})

      \item Use of \code{save(..., envir=)} with named objects could
      fail.  (\PR{15758})

      \item \code{Sweave()} mis-parsed \samp{Sexpr} expressions that
      contained backslashes.  (\PR{15779})

      \item The return value from \code{options(foo = NULL)} was not
      the previous value of the option.  (\PR{15781})

      \item \code{enc2utf8()} and \code{enc2native()} did not always
      mark the encoding of the return values when it was known.

      \item \code{dnbinom(x, size = <large>, mu, log = TRUE)} no longer
      underflows to -Inf for large \code{mu}, thanks to a suggestion
      from Alessandro Mammana (MPI MolGen, Berlin).

      \item \code{pbeta(x, a, b, log = TRUE)} no longer behaves
      discontinuously (in a small x-region) because of denormalized
      numbers.  Also, \code{pbeta(1-1e-12, 1e30, 1.001, log=TRUE)} now
      terminates \dQuote{in real time}.

      \item The \code{"CRAN"} filter (see \code{available.packages()})
      no longer removes duplicates other than of packages on CRAN, and
      does not fail if there is no CRAN repository in
      \code{getOption("repos")}.

      \item The device listing from \code{dev2bitmap()} and
      \code{bitmap()} was truncated to 1000 characters: modern versions
      of GhostScript on most platforms have many more devices.

      \item (Windows.)  Commands such as \code{Sys.which()} and
      \code{pipe()} which needed to find the full path to a command
      could segfault if the \sQuote{long} path name was much longer than
      the \sQuote{short} path name (which \code{Sys.which()} returns),
      as the behaviour of the Windows API call had changed.

      \item \command{R CMD build} will fail with an error if one of the
      packages specified in the \samp{VignetteBuilder} field is not
      installed.  (Without loading those packages it cannot be
      ascertained which files are intended to be vignettes. This means
      that the \samp{VignetteBuilder} packages have to be installed for
      package checking too.)
      (Wish of \PR{15775}.)

      \item Misguided attempts to use \code{chull()} with non-finite
      points now give an error (related to \PR{15777}).

      \item For a formula with exactly 32 variables the 32nd variable
      was aliased to the intercept in some C-level computations of
      terms, so that for example attempting to remove it would remove
      the intercept instead (and leave a corrupt internal structure).
      (\PR{15735})

      \item \code{anyDuplicated()} silently returned wrong values when
      the first duplicate was at an index which was too large to be
      stored in an integer vector (although a lot of RAM and patience
      would have been needed to encounter this).

      \item \code{tools::Rd2ex(commentDontrun = FALSE)} failed if the
      block had only one line.

      \item Hexadecimal constants such as \code{0x110p-5L} which were
      incorrectly qualified by \code{L} were parsed incorrectly since \R
      3.0.0, with a slightly garbled warning. (\PR{15753})

      \item \code{system()} returned success on some platforms even if
      the system was unable to launch a process. (\PR{15796})

      \item (Windows \command{Rgui} console.)  Unbuffered output was
      sometimes not output immediately if the prompt was not on the last
      line of the console.

      \item The built-in help server did not declare the encoding for
      the \file{DESCRIPTION} or other text files to be the package
      encoding, so non-ASCII characters could be displayed incorrectly.

      \item \R{} is now trying harder to not cleanup child processes
      that were not spawned by \code{mcparallel()} on platforms that
      provide information about the source process of the \code{SIGCHLD}
      signal. This allows 3rd party libraries to manage the exit status
      of children that they spawn without \R{} interfering.

      \item \code{mcmapply()} was only parallelizing if the number of
      jobs was bigger than the number of cores. It now parallelizes if
      the number of jobs is more than one.

      \item Auto-printing would re-evaluate its argument when trying to
      dispatch to a print method.  This is now avoided when possible.

      \item Unserializing (including \code{load()} and \code{readRDS()})
      could silently return incorrect numeric values from ASCII saves if
      there was a read error.

      \item \code{getParseData()} could return incorrect values for
      the parents of some elements. (Reported by Andrew Redd.)
      
      \item Attempting to use data frames of 2^31 or more rows with
      \code{merge()} or to create a merged data frame of that size now
      gives a clearer error message.

      \item \code{parse()} did not check its \code{file} argument was a
      connection if it was not a character string, so
      e.g. \code{parse(FALSE)} attempted to read from \code{stdin}.

      Nor did \code{dump()} and \code{dput()}.
      
      \item The \code{"help.try.all.packages"} option was ignored when
      the shortcut syntax for help was used, e.g. \code{?foo}.  
      
      \item A potential segfault in string allocation has been fixed.
      (Found by Radford Neal.)
      
      \item A potential memory protection error in \code{sort()} has
      been fixed. (Found by Radford Neal.)      
    }
  }
}

\section{\Rlogo CHANGES IN R 3.1.0}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item \code{type.convert()} (and hence by default
      \code{read.table()}) returns a character vector or factor when
      representing a numeric input as a double would lose accuracy.
      Similarly for complex inputs.

      If a file contains numeric data with unrepresentable numbers of
      decimal places that are intended to be read as numeric, specify
      \code{colClasses} in \code{read.table()} to be \code{"numeric"}.

      \item \code{tools::Rdiff(useDiff = FALSE)} is closer to the POSIX
      definition of \command{diff -b} (as distinct from the description
      in the \command{man} pages of most systems).

      \item New function \code{anyNA()}, a version of
      \code{any(is.na(.))} which is fast for atomic vectors, based on a
      proposal by Tim Hesterberg. (Wish of \PR{15239}.)

      \item \code{arrayInd(*, useNames = TRUE)} and, analogously,
      \code{which(*, arr.ind = TRUE)} now make use of
      \code{names(.dimnames)} when available.

      \item \code{is.unsorted()} now also works for \code{raw} vectors.

      \item The \code{"table"} method for \code{as.data.frame()} (also
      useful as \code{as.data.frame.table()}) now passes \code{sep} and
      \code{base} arguments to \code{provideDimnames()}.

      \item \code{uniroot()} gets new optional arguments, notably
      \code{extendInt}, allowing to auto-extend the search interval when
      needed.  The return value has an extra component, \code{init.it}.

      % advertize at all?
      % \item New \code{.lm.fit()} for benchmark lovers but not Joe
      % Average.

      %% TODO: change in ../src/library/compiler/noweb/compiler.nw + ..../R/cmp.R
      \item \code{switch(f, ...)} now warns when \code{f} is a factor,
      as this typically happens accidentally where the useR meant to
      pass a character string, but \code{f} is treated as integer (as
      always documented).

      \item The parser has been modified to use less memory.

      \item The way the unary operators (\code{+ - !}) handle attributes
      is now more consistent.  If there is no coercion, all attributes
      (including class) are copied from the input to the result:
      otherwise only names, dims and dimnames are.

      \item \code{colorRamp()} and \code{colorRampPalette()} now allow
      non-opaque colours and a ramp in opacity via the new argument
      \code{alpha = TRUE}.  (Suggested by Alberto Krone-Martins, but
      optionally as there are existing uses which expect only RGB values.)
      % https://stat.ethz.ch/pipermail/r-devel/2013-July/067046.html

      \item \code{grid.show.layout()} and \code{grid.show.viewport()}
      get an optional \code{vp.ex} argument.

      \item There is a new function \code{find_gs_cmd()} in the
      \pkg{tools} package to locate a GhostScript executable.  (This is
      an enhanced version of a previously internal function there.)

      \item \code{object.size()} gains a \code{format()} method.

      \item There is a new family, \code{"ArialMT"}, for the
      \code{pdf()} and \code{postscript()} devices.  This will only be
      rendered correctly on viewers which have access to Monotype
      TrueType fonts (which are sometimes requested by journals).

      \item The text and PDF news files, including \file{NEWS} and
      \file{NEWS.2}, have been moved to the \file{doc} directory.

      \item \code{combn(x, simplify = TRUE)} now gives a factor result
      for factor input \code{x} (previously user error).
      (Related to \PR{15442}.)

      \item Added \code{utils::fileSnapshot()} and
      \code{utils::changedFiles()} functions to allow snapshots and
      comparison of directories of files.

      \item \code{make.names(names, unique=TRUE)} now tries to preserve
      existing names. (Suggestion of \PR{15452}.)

      \item New functions \code{cospi(x)}, \code{sinpi(x)}, and
      \code{tanpi(x)}, for more accurate computation of
      \code{cos(pi*x)}, etc, both in \R{} and the C API.  Using these
      gains accuracy in some cases, e.g., inside \code{lgamma()} or
      \code{besselI()}.  (Suggested by Morten Welinder in \PR{15529}.)

      \item \code{print.table(x, zero.print = ".")} now also has an
      effect when \code{x} is not integer-valued.

      \item There is more support to explore the system's idea of
      time-zone names.  \code{Sys.timezone()} tries to give the current
      system setting by name (and succeeds at least on Linux, OS X,
      Solaris and Windows), and \code{OlsonNames()} lists the names in
      the system's Olson database. \code{Sys.timezone(location = FALSE)}
      gives the previous behaviour.

      \item Platforms with a 64-bit \code{time_t} type are allowed to
      handle conversions between the \code{"POSIXct"} and
      \code{"POSIXlt"} classes for date-times outside the 32-bit range
      (before 1902 or after 2037): the existing workarounds are used on
      other platforms.  (Note that time-zone information for post-2037
      is speculative at best, and the OS services are tested for known
      errors and so not used on OS X.)

      Currently \code{time_t} is usually \code{long} and hence 64-bit on
      Unix-alike 64-bit platforms: however in several cases the
      time-zone database is 32-bit.  For \R for Windows it is 64-bit
      (for both architectures as from this version).

      \item The \code{"save.defaults"} option can include a value for
      \code{compression_level}.  (Wish of \PR{15579}.)

      \item \code{colSums()} and friends now have support for arrays and
      data-frame columns with \eqn{2^{31}}{2^31} or more elements.

      \item \code{as.factor()} is faster when \code{f} is an unclassed
      integer vector (for example, when called from \code{tapply()}).

      \item \code{fft()} now works with longer inputs, from the 12
      million previously supported up to 2 billion.  (\PR{15593})

      \item Complex \code{svd()} now uses LAPACK subroutine
      \code{ZGESDD}, the complex analogue of the routine used for the
      real case.

      \item Sweave now outputs \file{.tex} files in UTF-8 if the
      input encoding is declared to be UTF-8, regardless of the
      local encoding.  The UTF-8 encoding may now be declared using
      a LaTeX comment containing the string \code{\%\\SweaveUTF8}
      on a line by itself.

      \item \code{file.copy()} gains a \code{copy.date} argument.

      \item Printing of date-times will make use of the time-zone
      abbreviation in use at the time, if known.  For example, for Paris
      pre-1940 this could be \samp{LMT}, \samp{PMT}, \samp{WET} or
      \samp{WEST}.  To enable this, the \code{"POSIXlt"} class has an
      optional component \code{"zone"} recording the abbreviation for
      each element.

      For platforms which support it, there is also a component
      \code{"gmtoff"} recording the offset from GMT where known.

      \item (On Windows, by default on OS X and optionally elsewhere.)
      The system C function \code{strftime} has been replaced by a more
      comprehensive version with closer conformance to the POSIX 2008
      standard.

      \item \code{dnorm(x, log = FALSE)} is more accurate (but somewhat
      slower) for |x| > 5; as suggested in \PR{15620}.

      \item Some versions of the \code{tiff()} device have further
      compression options.

      \item \code{read.table()}, \code{readLines()} and \code{scan()}
      have a new argument to influence the treatment of embedded nuls.

      \item Avoid duplicating the right hand side values in complex
      assignments when possible.  This reduces copying of replacement
      values in expressions such as \code{Z$a <- a0} and
      \code{ans[[i]] <- tmp}: some package code has relied on there
      being copies.

      Also, a number of other changes to reduce copying of objects; all
      contributed by or based on suggestions by Michael Lawrence.

      \item The \code{fast} argument of \code{KalmanLike()},
      \code{KalmanRun()} and \code{KalmanForecast()} has been replaced
      by \code{update}, which instead of updating \code{mod} in place,
      optionally returns the updated model in an attribute \code{"mod"}
      of the return value.

      \item \code{arima()} and \code{makeARIMA()} get a new optional
      argument \code{SSinit}, allowing the choice of a different
      \bold{s}tate \bold{s}pace initialization which has been observed
      to be more reliable close to non-stationarity: see \PR{14682}.

      \item \code{warning()} has a new argument \code{noBreaks.}, to
      simplify post-processing of output with \code{options(warn = 1)}.

      \item \code{pushBack()} gains an argument \code{encoding}, to
      support reading of UTF-8 characters using \code{scan()},
      \code{read.table()} and related functions in a non-UTF-8 locale.

      \item \code{all.equal.list()} gets a new argument \code{use.names}
      which by default labels differing components by names (if they
      match) rather than by integer index.  Saved \R output in packages may
      need to be updated.

      \item The methods for \code{all.equal()} and
      \code{attr.all.equal()} now have argument \code{check.attributes}
      after \code{\dots} so it cannot be partially nor positionally
      matched (as it has been, unintentionally).

      A side effect is that some previously undetected errors of passing
      empty arguments (no object between commas) to \code{all.equal()}
      are detected and reported.

      There are explicit checks that \code{check.attributes} is logical,
      \code{tolerance} is numeric and \code{scale} is \code{NULL} or
      numeric.  This catches some unintended positional matching.

      The message for \code{all.equal.numeric()} reports a
      \code{"scaled difference"} only for \code{scale != 1}.

      \item \code{all.equal()} now has a \code{"POSIXt"} method
      replacing the \code{"POSIXct"} method.

      \item The \code{"Date"} and \code{"POSIXt"} methods of
      \code{seq()} allows \code{by = "quarter"} for completeness
      (\code{by = "3 months"} always worked).

      \item \code{file.path()} removes any trailing separator on
      Windows, where they are invalid (although sometimes accepted).
      This is intended to enhance the portability of code written by
      those using POSIX file systems (where a trailing \code{/} can be
      used to confine path matching to directories).

      \item New function \code{agrepl()} which like \code{grepl()}
      returns a logical vector.

      \item \code{fifo()} is now supported on Windows. (\PR{15600})

      \item \code{sort.list(method = "radix")} now allows negative
      integers (wish of \PR{15644}).

      \item Some functionality of \code{print.ts()} is now available in
      \code{.preformat.ts()} for more modularity.% and format.ts() might follow

      \item \code{mcparallel()} gains an option \code{detach = TRUE}
      which allows execution of code independently of the current
      session.  It is based on a new \code{estranged = TRUE} argument to
      \code{mcfork()} which forks child processes such that they become
      independent of the parent process.

      \item The \code{pdf()} device omits circles and text at extremely
      small sizes, since some viewers were failing on such files.

      \item The rightmost break for the \code{"months"},
      \code{"quarters"} and \code{"years"} cases of
      \code{hist.POSIXlt()} has been increased by a day.  (Inter alia,
      fixes \PR{15717}.)

      \item The handling of \code{DF[i,] <- a} where \code{i} is of
      length 0 is improved.  (Inter alia, fixes \PR{15718}.)

      \item \code{hclust()} gains a new method \code{"ward.D2"} which
      implements Ward's method correctly.  The previous \code{"ward"}
      method is \code{"ward.D"} now, with the old name still working.
      Thanks to research and proposals by Pierre Legendre.

      \item The \code{sunspot.month} dataset has been amended and
      updated from the official source, whereas the \code{sunspots} and
      \code{sunspot.year} datasets will remain immutable. The
      documentation and source links have been updated correspondingly.

      \item The \code{summary()} method for \code{"lm"} fits warns if
      the fit is essentially perfect, as most of the summary may be
      computed inaccurately (and with platform-dependent values).

      Programmers who use \code{summary()} in order to extract just
      a component which will be reliable (e.g. \code{$cov.unscaled})
      should wrap their calls in \code{suppressWarnings()}.
    }
  }

  \subsection{INSTALLATION and INCLUDED SOFTWARE}{
    \itemize{
      \item The included version of LAPACK has been updated to 3.5.0.

      \item There is some support for parallel testing of an
      installation, by setting \env{TEST_MC_CORES} to an integer
      greater than one to indicate the maximum number of cores to be
      used in parallel. (It is worth specifying at least 8 cores if
      available.)  Most of these require a \command{make} program (such
      as GNU \command{make} and \command{dmake}) which supports the
      \command{${MAKE} -j nproc} syntax.

      Except on Windows: the tests of standard package examples in
      \command{make check} are done in parallel.  This also applies to
      running \code{tools::testInstalledPackages()}.

      The more time-consuming regression tests are done in parallel.

      The package checks in \command{make check-devel} and \command{make
	check-recommended} are done in parallel.

      \item More of \command{make check} will work if recommended packages
      are not installed: but recommended packages remain needed for
      thorough checking of an \R build.

      \item The version of \samp{tzcode} included in
      \file{src/extra/tzone} has been updated.  (Formerly used only on
      Windows.)

      \item The included (64-bit) time-zone conversion code and Olson
      time-zone database can be used instead of the system version: use
      \command{configure} option \option{--with-internal-tzcode}.  This
      is the default on Windows and OS X.  (Note that this does not
      currently work if a non-default \code{rsharedir}
      \command{configure} variable is used.)

      (It might be necessary to set environment variable \env{TZ} on
      OSes where this is not already set, although the system timezone is
      deduced correctly on at least Linux, OS X and Windows.)

      This option also switches to the version of \code{strftime}
      included in directory \file{src/extra/tzone}.

      \item \command{configure} now tests for a C++11-compliant compiler
      by testing some basic features.  This by default tries flags for
      the compiler specified by \samp{CXX}, but an alternative compiler,
      options and standard can be specified by variables \samp{CXX1X},
      \samp{CXX1XFLAGS} and \samp{CXX1XSTD} (e.g. \samp{-std=gnu++11}).

      \item \R{} can now optionally be compiled to use reference
      counting instead of the \code{NAMED} mechanism by defining
      \code{SWITCH_TO_REFCNT} in \file{Rinternals.h}. This may become
      the default in the future.

      \item There is a new option \option{--use-system-tre} to use a
      suitable system \pkg{tre} library: at present this means a version
      from their \command{git} repository, after corrections.
      (Wish of \PR{15660}.)
    }
  }

  \subsection{PACKAGE INSTALLATION}{
    \itemize{
      \item The \code{CRANextra} repository is no longer a default
      repository on Windows: all the binary versions of packages from
      CRAN are now on CRAN, although \code{CRANextra} contains packages
      from Omegahat and elsewhere used by CRAN packages.

      \item Only vignettes sources in directory \file{vignettes} are
      considered to be vignettes and hence indexed as such.

      \item In the \file{DESCRIPTION} file, \preformatted{    License: X11}
      is no longer recognized as valid.  Use \samp{MIT} or
      \samp{BSD_2_clause} instead, both of which need \samp{+ file LICENSE}.

      \item For consistency, entries in \file{.Rinstignore} are now matched
      case-insensitively on all platforms.

      \item Help for S4 methods with very long signatures now tries
      harder to split the description in the \samp{Usage} field to no
      more than 80 characters per line (some packages had over 120
      characters).

      \item \command{R CMD INSTALL --build} (not Windows) now defaults to
      the internal \code{tar()} unless \env{R_INSTALL_TAR} is set.

      \item There is support for compiling C++11 code in packages on
      suitable platforms: see \sQuote{Writing R Extensions}.

      \item Fake installs now install the contents of directory
      \file{inst}: some packages use this to install e.g. C++ headers
      for use by other packages that are independent of the package
      itself.  Option \option{--no-inst} can be used to get the previous
      behaviour.
    }
  }

  \subsection{DEBUGGING}{
    \itemize{
      \item The behaviour of the code browser has been made more
      consistent, in part following the suggestions in \PR{14985}.

      \item Calls to \code{browser()} are now consistent with calls
      to the browser triggered by \code{debug()}, in that \kbd{Enter}
      will default to \code{n} rather than \code{c}.

      \item A new browser command \code{s} has been added, to
      \dQuote{step into} function calls.

      \item A new browser command \code{f} has been added, to
      \dQuote{finish} the current loop or function.

      \item Within the browser, the command \code{help} will
      display a short list of available commands.
    }
  }

  \subsection{UTILITIES}{
    \itemize{
      \item Only vignettes sources in directory \file{vignettes} are
      considered to be vignettes by \command{R CMD check}.  That has
      been the preferred location since \R 2.14.0 and is now obligatory.

      \item For consistency, \command{R CMD build} now matches entries
      in \file{.Rbuildignore}  and \file{vignettes/.install_extras}
      case-insensitively on all platforms (not just on Windows).

      \item \code{checkFF()} (called by \command{R CMD check} by
      default) can optionally check foreign function calls for
      consistency with the registered type and argument count.  This is
      the default for \command{R CMD check --as-cran} or can be enabled
      by setting environment variable \env{_R_CHECK_FF_CALLS_} to
      \samp{registration} (but is in any case suppressed by
      \option{--install=no}).   Because this checks calls in which
      \code{.NAME} is an \R object and not just a literal character
      string, some other problems are detected for such calls.

      Functions \code{suppressForeignCheck()} and \code{dontCheck()}
      have been added to allow package authors to suppress false
      positive reports.

      \item \command{R CMD check --as-cran} warns about a false value of
      the \file{DESCRIPTION} field \samp{BuildVignettes} for Open Source
      packages, and ignores it.  (An Open Source package needs to have
      complete sources for its vignettes which should be usable on a
      suitably well-equipped system).

      \item \command{R CMD check --no-rebuild-vignettes} is defunct:\cr
      \command{R CMD check --no-build-vignettes} has been preferred since
      \R 3.0.0.

      \item \command{R CMD build --no-vignettes} is defunct:\cr
      \command{R CMD build --no-build-vignettes} has been preferred since
      \R 3.0.0.

      \item \command{R CMD Sweave} and \command{R CMD Stangle} now
      process both Sweave and non-Sweave vignettes.  The
      \code{tools::buildVignette()} function has been added to do the
      same tasks from within \R.

      \item The flags returned by \command{R CMD config --ldflags} and
      (where installed) \command{pkg-config --libs libR} are now those
      needed to link a front-end against the (shared or static) \R
      library.

      \item \file{Sweave.sty} has a new option \samp{[inconsolata]}.

      \item \command{R CMD check} customizations such as
      \env{_R_CHECK_DEPENDS_ONLY_} make available packages only in
      \samp{LinkingTo} only for installation, and not for
      loading/runtime tests.

      \item \command{tools::checkFF()} reports on \code{.C} and
      \code{.Fortran} calls with \code{DUP = FALSE} if argument
      \code{check_DUP} is true.  This is selected by
      \command{R CMD check} by default.

      \item \command{R CMD check --use-gct} can be tuned to
      garbage-collect less frequently using \code{gctorture2()}
      \emph{via} the setting of environment variable
      \env{_R_CHECK_GCT_N_}.

      \item Where supported, \code{tools::texi2dvi()} limits the number
      of passes tried to 20.
    }
  }

  \subsection{C-LEVEL FACILITIES}{
    \itemize{
      \item (Windows only) A function \code{R_WaitEvent()} has been
      added (with declaration in header\file{R.h}) to block execution
      until the next event is received by \R.

      \item Remapping in the \file{Rmath.h} header can be suppressed by
      defining \samp{R_NO_REMAP_RMATH}.

      \item The remapping of \code{rround()} in header \file{Rmath.h}
      has been removed: use \code{fround()} instead.

      \item \code{ftrunc()} in header \file{Rmath.h} is now a wrapper
      for the C99 function \code{trunc()}, which might as well be used
      in C code: \code{ftrunc()} is still needed for portable C++ code.

      \item The never-documented remapping of \code{prec()} to
      \code{fprec()} in header \file{Rmath.h} has been removed.

      \item The included LAPACK subset now contains \code{ZGESDD} and
      \code{ZGELSD}.

      \item The function \code{LENGTH()} now checks that it is only
      applied to vector arguments.  However, in packages \code{length()}
      should be used. (In \R{} itself \code{LENGTH()} is a macro without
      the function overhead of \code{length()}.)

      \item Calls to \code{SET_VECTOR_ELT()} and \code{SET_STRING_ELT()}
      are now checked for indices which are in-range: several packages
      were writing one element beyond the allocated length.

      \item \code{allocVector3} has been added which allows custom
      allocators to be used for individual vector allocations.
    }
  }

  \subsection{DEPRECATED AND DEFUNCT}{
    \itemize{
      \item \code{chol(pivot = TRUE, LINPACK = TRUE)} is defunct.

      Arguments \code{EISPACK} for \code{eigen()} and \code{LINPACK} for
      \code{chol()}, \code{chol2inv()}, \code{solve()} and \code{svd()}
      are ignored: LAPACK is always used.

      \item \code{.find.package()} and \code{.path.package()} are
      defunct: only the versions without the initial dot introduced in
      \R 2.13.0 have ever been in the API.

      \item Partial matching when using the \code{$} operator \emph{on
	data frames} now throws a warning and may become defunct in the
      future. If partial matching is intended, replace \code{foo$bar}
      by \code{foo[["bar", exact = FALSE]]}.

      \item The long-deprecated use of \code{\\synopsis} in the
      \samp{Usage} section of \file{.Rd} files has been removed: such
      sections are now ignored (with a warning).

      \item \code{package.skeleton()}'s deprecated argument
      \code{namespace} has been removed.

      \item Many methods are no longer exported by package \pkg{stats}.
      They are all registered on their generic, which should be called
      rather than calling a method directly.

      \item Functions \code{readNEWS()} and \code{checkNEWS()} in
      package \pkg{tools} are defunct.

      \item \code{download.file(method = "lynx")} is deprecated.

      \item \code{.C(DUP = FALSE)} and \code{.Fortran(DUP = FALSE)} are
      now deprecated, and may be disabled in future versions of \R.  As
      their help has long said, \code{.Call()} is much preferred.

      \command{R CMD check} notes such usages (by default).

      \item The workaround of setting \env{R_OSX_VALGRIND} has been
      removed: it is not needed in current valgrind.
    }
  }

  \subsection{BUG FIXES}{
    \itemize{
      \item Calling \code{lm.wfit()} with no non-zero weights gave an
      array-overrun in the Fortran code and a not very sensible answer.
      It is now special-cased with a simpler answer (no \code{qr}
      component).

      \item Error messages involving non-syntactic names (e.g. as produced
      by \code{`\\r`} when that object does not exist) now encode
      the control characters.  (Reported by Hadley Wickham.)

      \item \code{getGraphicsEvent()} caused 100\% usage of one CPU in
      Windows. (PR#15500)

      \item \code{nls()} with no \code{start} argument may now work
      inside another function (scoping issue).

      \item \code{pbeta()} and similar work better for very large
      (billions) \code{ncp}.

      \item Where time zones have changed abbreviations over the years,
      the software tries to more consistently use the abbreviation
      appropriate to the time or if that is unknown, the current
      abbreviation.  On some platforms where the C function
      \code{localtime} changed the \code{tzname} variables the reported
      abbreviation could have been that of the last time converted.

      \item \code{all.equal(list(1), identity)} now works.

      \item Bug fix for pushing viewports in \pkg{grid} (reported by
      JJ Allaire and Kevin Ushey).

      NOTE for anyone poking around within the graphics engine display
      list (despite the warnings not to) that this changes what
      is recorded by \pkg{grid} on the graphics engine display list.

      \item Extra checks have been added for unit resolution and
      conversion in \pkg{grid}, to catch instances of division-by-zero.
      This may introduce error messages in existing code and/or produce
      a different result in existing code (but only where a non-finite
      location or dimension may now become zero).

      \item Some bugs in TRE have been corrected by updating from the
      \command{git} repository.  This allows \R to be installed on some
      platforms for which this was a blocker (\PR{15087} suggests Linux
      on ARM and HP-UX).

      \item \code{?} applied to a call to an S4 generic failed in
      several cases. (\PR{15680})

      \item The implicit S4 generics for primitives with \code{\dots} in
      their argument list were incorrect. (\PR{15690})

      \item Bug fixes to \code{methods::callGeneric()}. (\PR{15691})

      \item The bug fix to \code{aggregrate()} in \PR{15004} introduced
      a new bug in the case of no grouping variables. (\PR{15699})

      \item In rare cases printing deeply nested lists overran a buffer
      by one byte and on a few platforms segfaulted. (\PR{15679})

      \item The dendrogram method of \code{as.dendrogram()} was hidden
      accidentally, (\PR{15703}), and \code{order.dendrogram(d)} gave
      too much for a leaf \code{d}. (\PR{15702})

      \item \R would try to kill processes on exit that have pids ever
      used by a child process spawned by \code{mcparallel} even though
      the current process with that pid was not actually its child.

      \item \code{cophenetic()} applied to a \code{"dendrogram"} object
      sometimes incorrectly returned a \code{"Labels"} attribute with
      dimensions.  (\PR{15706})

      \item \code{printCoefmat()} called from quite a few \code{print()}
      methods now obeys small \code{getOption("width")} settings,
      line wrapping the \samp{"signif. codes"} legend appropriately.
      (\PR{15708})

      \item \code{model.matrix()} assumed that the stored dimnames for a
      matrix was \code{NULL} or length 2, but length 1 occurred.

      \item The clipping region for a device was sometimes used in base
      graphics before it was set.
    }
  }
}

\section{\Rlogo CHANGES IN R 3.0.3}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item On Windows there is support for making \file{.texi} manuals
      using \command{texinfo} 5.0 or later: the setting is in file
      \file{src/gnuwin32/MkRules.dist}.

      A packaging of the Perl script and modules for \command{texinfo}
      5.2 has been made available at
      \url{http://www.stats.ox.ac.uk/pub/Rtools/}.

      \item \code{write.table()} now handles matrices of
      \eqn{2^{31}}{2^31} or more elements, for those with large amounts
      of patience and disc space.

      \item There is a new function, \code{La_version()}, to report the
      version of LAPACK in use.

      \item The HTML version of \sQuote{An Introduction to R} now has
      links to PNG versions of the figures.

      \item There is some support to produce manuals in ebook
      formats. (See \file{doc/manual/Makefile}.  Suggested by Mauro
      Cavalcanti.)

      \item On a Unix-alike \code{Sys.timezone()} returns \code{NA} if
      the environment variable \env{TZ} is unset, to distinguish it from
      an empty string which on some OSes means the \samp{UTC} time zone.

      \item The backtick may now be escaped in strings, to allow names
      containing them to be constructed, e.g. \code{`\\``}.  (\PR{15621})

      \item \code{read.table()}, \code{readLines()} and \code{scan()}
      now warn when an embedded nul is found in the input.  (Related to
      \PR{15625} which was puzzled by the behaviour in this unsupported
      case.)

      \item (Windows only.)  \code{file.symlink()} works around the
      undocumented restriction of the Windows system call to
      backslashes.  (Wish of \PR{15631}.)

      \item \code{KalmanForecast(fast = FALSE)} is now the default, and
      the help contains an example of how \code{fast = TRUE} can be used
      in this version.  (The usage will change in 3.1.0.)

      \item \code{strptime()} now checks the locale only when
      locale-specific formats are used and caches the locale in use:
      this can halve the time taken on OSes with slow system
      functions (e.g. OS X).

      \item \code{strptime()} and the \code{format()} methods for
      classes \code{"POSIXct"}, \code{"POSIXlt"} and \code{"Date"}
      recognize strings with marked encodings: this allows, for example,
      UTF-8 French month names to be read on (French) Windows.

      \item \code{iconv(to = "utf8")} is now accepted on all platforms
      (some implementations did already, but GNU \pkg{libiconv} did not:
      however converted strings were not marked as being in UTF-8).  The
      official name, \code{"UTF-8"} is still preferred.

      \item \code{available.packages()} is better protected against
      corrupt metadata files.  (A recurring problem with Debian package
      \pkg{shogun-r}: \PR{14713}.)

      \item Finalizers are marked to be run at garbage collection, but
      run only at a somewhat safer later time (when interrupts are
      checked).  This circumvents some problems with finalizers running
      arbitrary code during garbage collection (the known instances being
      running \code{options()} and (C-level) \code{path.expand()}
      re-entrantly).
    }
  }

  \subsection{INSTALLATION and INCLUDED SOFTWARE}{
    \itemize{
      \item The included version of PCRE has been updated to 8.34.  This
      fixes bugs and makes the behaviour closer to Perl 5.18.  In
      particular, the concept of \sQuote{space} includes \samp{VT} and
      hence agrees with POSIX's.
    }
  }

  \subsection{PACKAGE INSTALLATION}{
    \itemize{
      \item The new field \samp{SysDataCompression} in the
      \file{DESCRIPTION} file allows user control over the compression
      used for \file{sysdata.rda} objects in the lazy-load database.

      \item \code{install.packages(dependencies = value)} for \code{value =
	NA} (the default) or \code{value = TRUE} omits packages only in
      \code{LinkingTo} for binary package installs.
    }
  }

  \subsection{C-LEVEL FACILITIES}{
    \itemize{
      \item The long undocumented remapping of \code{rround()} to
      \code{Rf_fround()} in header \file{Rmath.h} is now formally
      deprecated: use \code{fround()} directly.

      \item Remapping of \code{prec()} and \code{trunc()} in the
      \file{Rmath.h} header has been disabled in C++ code (it has caused
      breakage with \code{libc++} headers).
    }
  }

  \subsection{BUG FIXES}{
    \itemize{
      \item \code{getParseData()} truncated the imaginary part of
      complex number constants.  (Reported by Yihui Xie.)

      \item \code{dbeta(x, a, b)} with \code{a} or \code{b} within a
      factor of 2 of the largest representable number could
      infinite-loop.  (Reported by Ioannis Kosmidis.)

      \item \code{provideDimnames()} failed for arrays with a 0
      dimension. (\PR{15465})

      \item \code{rbind()} and \code{cbind()} did not handle
      list objects correctly. (\PR{15468})

      \item \code{replayPlot()} now checks if it is replaying a plot
      from the same session.

      \item \code{rasterImage()} and \code{grid.raster()} now give
      error on an empty (zero-length) raster.  (Reported by Ben North.)

      \item \code{plot.lm()} would sometimes scramble the labels
      in plot type 5.  (\PR{15458} and \PR{14837})

      \item \code{min()} did not handle \code{NA_character_} values
      properly.  (Reported by Magnus Thor Torfason.)

      \item (Windows only.)  \code{readRegistry()} would duplicate
      default values for keys.  (\PR{15455})

      \item \code{str(..., strict.width = "cut")} did not handle
      it properly when more than one line needed to be cut.  (Reported
      by Gerrit Eichner.)

      \item Removing subclass back-references when S4 classes were
      removed or their namespace unloaded had several bugs (e.g., \PR{15481}).

      \item \code{aggregate()} could fail when there were too many
      levels present in the \code{by} argument.  (\PR{15004})

      \item \code{namespaceImportFrom()} needed to detect primitive
      functions when checking for duplicated imports (reported by
      Karl Forner).

      \item \code{getGraphicsEvent()} did not exit when a user closed
      the graphics window.  (\PR{15208})

      \item Errors in vignettes were not always captured and displayed
      properly.  (\PR{15495})

      \item \code{contour()} could fail when dealing with extremely
      small z values.  (\PR{15454})

      \item Several functions did not handle zero-length vectors properly,
      including \code{browseEnv()}, \code{format()}, \code{gl()},
      \code{relist()} and \code{summary.data.frame()}.  (E.g., \PR{15499})

      \item \code{Sweave()} did not restore the \R{} output to the
      console if it was interrupted by a user in the middle of evaluating
      a code chunk.  (Reported by Michael Sumner.)

      \item Fake installs of packages with vignettes work again.

      \item Illegal characters in the input caused \code{parse()}
      (and thus \code{source()}) to segfault.  (\PR{15518})

      \item The nonsensical use of \code{nmax = 1} in
      \code{duplicated()} or \code{unique()} is now silently ignored.

      \item \code{qcauchy(p, *)} is now fully accurate even when p is
      very close to 1. (\PR{15521})

      \item The \code{validmu()} and \code{valideta()} functions in the
      standard \code{glm()} families now also report non-finite values,
      rather than failing.

      \item Saved vignette results (in a \file{.Rout.save} file) were
      not being compared to the new ones during \command{R CMD check}.

      \item Double-clicking outside of the list box (e.g. on the scrollbar)
      of a Tk listbox widget generated by \code{tk_select.list()} no
      longer causes the window to close. (\PR{15407})

      \item Improved handling of edge cases in
      \code{parallel::splitindices()}. (\PR{15552})

      \item HTML display of results from \code{help.search()} and
      \code{??} sometimes contained badly constructed links.

      \item \code{c()} and related functions such as \code{unlist()}
      converted raw vectors to invalid logical vectors. (\PR{15535})

      \item (Windows only) When a call to \code{system2()} specified
      one of \code{stdin}, \code{stdout} or \code{stderr} to be a file,
      but the command was not found (e.g. it contained its arguments,
      or the program was not on the \env{PATH}), it left the file open
      and unusable until \R terminated.  (Reported by Mathew McLean.)

      \item The \code{bmp()} device was not recording \code{res = NA}
      correctly: it is now recorded as 72 ppi.

      \item Several potential problems with compiler-specific behaviour
      have been identified using the \sQuote{Undefined Behaviour
	Sanitizer} in conjunction with the \command{clang} compiler.

      \item \code{hcl()} now honours \code{NA} inputs (previously they
      were mapped to black).

      \item Some translations in base packages were being looked up in
      the main catalog rather than that for the package.

      \item As a result of the 3.0.2 change about \sQuote{the last
	second before the epoch}, most conversions which should have given
      \code{NA} returned that time.  (The platforms affected include
      Linux and OS X, but not Windows nor Solaris.)

      \item \code{rowsum()} has more support for matrices and dataframes
      with \eqn{2^{31}}{2^31} or more elements. (\PR{15587})

      \item \code{predict(<lm object>, interval = "confidence", scale =
	<something>)} now works.  (\PR{15564})

      \item The bug fix in 3.0.2 for \PR{15411} was too aggressive,
      and sometimes removed spaces that should not have been removed.
      (\PR{15583})

      \item Running \R code in a \pkg{tcltk} callback failed to set the
      busy flag, which will be needed to tell OS X not to \sQuote{App Nap}.

      \item The code for date-times before 1902 assumed that the offset
      from GMT in 1902 was a whole number of minutes: that was not true
      of Paris (as recorded on some platforms).

      \item Using \code{Sys.setlocale} to set \code{LC_NUMERIC} to
      \code{"C"} (to restore the sane behavior) no longer gives a
      warning.

      \item \code{deparse()} now deparses complex vectors in a way that
      re-parses to the original values.  (\PR{15534}, patch based on code
      submitted by Alex Bertram.)

      \item In some extreme cases (more than \eqn{10^{15}}{10^15})
      integer inputs to \code{dpqrxxx()} functions might have been
      rounded up by one (with a warning about being non-integer).
      (\PR{15624})

      \item Plotting symbol \code{pch = 14} had the triangle upside down
      on some devices (typically screen devices). The triangle is
      supposed to be point up.  (Reported by Bill Venables.)

      \item \code{getSrcref()} did not work on method definitions if
      \code{rematchDefinition()} had been used.

      \item \code{KalmanForecast(fast = FALSE)} reported a (harmless)
      stack imbalance.

      \item The count of observations used by \code{KalmanRun()} did not
      take missing values into account.

      \item In locales where the abbreviated name of one month is a
      partial match for the full name of a later one, the \code{\%B}
      format in \code{strptime()} could fail.  An example was French on
      OS X, where \samp{juin} is abbreviated to \samp{jui} and partially
      matches \code{juillet}.  Similarly for weekday names.

      \item \code{pbeta(x, a, b, log.p = TRUE)} sometimes underflowed to
      zero for very small and very differently sized \code{a}, \code{b}.
      (\PR{15641})

      \item \code{approx()} and \code{approxfun()} now handle infinite
      values with the \code{"constant"} method. (\PR{15655})

      \item \code{stripchart()} again respects reversed limits in
      \code{xlim} and \code{ylim}.  (\PR{15664})
    }
  }
}

\section{\Rlogo CHANGES IN R 3.0.2}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item The \file{NEWS} files have been re-organized.

      This file contains news for \R >= 3.0.0: news for the 0.x.y, 1.x.y
      and 2.x.y releases is in files \file{NEWS.0}, \file{NEWS.1} and
      \file{NEWS.2}.  The latter files are now installed when \R is
      installed.  An HTML version of news from 2.10.0 to 2.15.3 is
      available as \file{doc/html/NEWS.2.html}.

      \item \code{sum()} for integer arguments now uses an integer
      accumulator of at least 64 bits and so will be more accurate in
      the very rare case that a cumulative sum exceeds
      \eqn{2^{53}}{2^53} (necessarily summing more than 4 million
      elements).

      \item The \code{example()} and \code{tools::Rd2ex()} functions now
      have parameters to allow them to ignore \code{\\dontrun} markup in
      examples.  (Suggested by Peter Solymos.)

      \item \code{str(x)} is considerably faster for very large lists,
      or factors with 100,000 levels, the latter as in \PR{15337}.

      \item \code{col2rgb()} now converts factors to character strings
      not integer codes (suggested by Bryan Hanson).

      \item \code{tail(warnings())} now works, via the new \code{`[`}
      method.

      \item There is now support for the LaTeX style file \file{zi4.sty}
      which has in some distributions replaced \file{inconsolata.sty}.

      \item \code{unlist(x)} now typically returns all non-list
      \code{x}s unchanged, not just the \dQuote{vector} ones.
      Consequently, \code{format(lst)} now also works when the list
      \code{lst} has non-vector elements.

      \item The \code{tools::getVignetteInfo()} function has been added
      to give information about installed vignettes.

      \item New \code{assertCondition()}, etc. utilities in \pkg{tools}, useful
      for testing.

      \item Profiling now records non-inlined calls from byte-compiled
      code to \code{BUILTIN} functions.

      \item Various functions in \pkg{stats} and elsewhere that use
      non-standard evaluation are now more careful to follow the
      namespace scoping rules.  E.g. \code{stats::lm()} can now find
      \code{stats::model.frame()} even if \pkg{stats} is not on the
      search path or if some package defines a function of that name.

      \item If an invalid/corrupt \code{.Random.seed} object is
      encountered in the workspace it is ignored with a warning rather
      than giving an error.  (This allows \R itself to rely on a working
      RNG, e.g. to choose a random port.)

      \item \code{seq()} and \code{seq.int()} give more explicit error
      messages if called with invalid (e.g. \code{NaN}) inputs.

      \item When \code{parse()} finds a syntax error, it now makes
      partial parse information available up to the location of the
      error.  (Request of Reijo Sund.)

      \item Methods invoked by \code{NextMethod()} had a different
      dynamic parent to the generic. This was causing trouble where S3
      methods invoked via lazy evaluation could lose track of their
      generic.  (\PR{15267})

      \item Code for the negative binomial distribution now treats the case
      \code{size == 0} as a one-point distribution at zero.

      \item \code{abbreviate()} handles without warning non-ASCII input
      strings which require no abbreviation.

      \item \code{read.dcf()} no longer has a limit of 8191 bytes per
      line. (Wish of \PR{15250}.)

      \item \code{formatC(x)} no longer copies the class of \code{x} to
      the result, to avoid misuse creating invalid objects as in
      \PR{15303}.  A warning is given if a class is discarded.

      \item Dataset \code{npk} has been copied from \CRANpkg{MASS} to
      allow more tests to be run without recommended packages being
      installed.

      \item The initialization of the regression coefficients for
      non-degenerate differenced models in \code{arima()} has been
      changed and in some examples avoids a local maximum.  (\PR{15396})

      \item \code{termplot()} now has an argument \code{transform.x}
      to control the display of individual terms in the plot.
      (\PR{15329})

      \item \code{format()} now supports \code{digits = 0}, to
      display \code{nsmall} decimal places.

      \item There is a new read-only \code{par()} parameter called
      \code{"page"}, which returns a logical value indicating whether
      the next \code{plot.new()} call will start a new page.

      \item Processing Sweave and Rd documents to PDF now renders
      backticks and single quotes better in several instances, including
      in \samp{\\code} and \samp{\\samp} expressions.

      \item \code{utils::modifyList()} gets a new argument \code{keep.null}
      allowing \code{NULL} components in the replacement to be retained,
      instead of causing corresponding components to be deleted.

      \item \code{tools::pkgVignettes()} gains argument \code{check};
      if set to \code{TRUE}, it will warn when it appears a vignette requests
      a non-existent vignette engine.
    }
  }

  \subsection{UTILITIES}{
    \itemize{
      \item \command{R CMD check --as-cran} checks the line widths in
      usage and examples sections of the package Rd files.

      \item \command{R CMD check --as-cran} now implies \option{--timings}.

      \item \command{R CMD check} looks for command \command{gfile} if a
      suitable \command{file} is not found.  (Although \command{file} is
      not from GNU, OpenCSW on Solaris installs it as \command{gfile}.)

      \item \command{R CMD build} (with the internal \code{tar}) checks
      the permissions of \file{configure} and \file{cleanup} files and
      adds execute permission to the recorded permissions for these
      files if needed, with a warning.  This is useful on OSes and file
      systems which do not support execute permissions (notably, on
      Windows).

      \item \command{R CMD build} now weaves and tangles all vignettes,
      so suggested packages are not required during package installation
      if the source tarball was prepared with current
      \command{R CMD build}.

      \item \code{checkFF()} (used by \command{R CMD check}) does a
      better job of detecting calls from other packages, including not
      reporting those where a function has been copied from another
      namespace (e.g. as a default method).  It now reports calls where
      \code{.NAME} is a symbol registered in another package.

      \item On Unix-alike systems, \command{R CMD INSTALL} now installs packages
      group writably whenever the library (\code{lib.loc}) is group
      writable.  Hence, \code{update.packages()} works for other group
      members (suggested originally and from a patch by Dirk Eddelbuettel).

      \item \command{R CMD javareconf} now supports the use of symbolic
      links for \env{JAVA_HOME} on platforms which have
      \command{realpath}.  So it is now possible to
      use \preformatted{R CMD javareconf JAVA_HOME=/usr/lib/jvm/java-1.7.0}
      on a Linux system and record that value rather than the
      frequently-changing full path such as
      \file{/usr/lib/jvm/java-1.7.0-openjdk-1.7.0.25.x86_64}.

      \item (Windows only.) \command{Rscript -e} requires a non-empty
      argument for consistency with Unix versions of \R.  (Also
      \command{Rterm -e} and \command{R -e}.)

      \item \command{R CMD check} does more thorough checking of
      declared packages and namespaces.  It reports
      \itemize{
	\item packages declared in more than one of the
	\samp{Depends}, \samp{Imports}, \samp{Suggests} and
	\samp{Enhances} fields of the \file{DESCRIPTION} file.

	\item namespaces declared in \samp{Imports} but not
	imported from, neither in the \file{NAMESPACE} file nor using
	the \code{::} nor \code{:::} operators.

	\item packages which are used in \code{library()} or
	\code{requires()} calls in the \R code but were already put on
	the search path \emph{via} \samp{Depends}.

	\item packages declared in \samp{Depends} not imported
	\emph{via} the \file{NAMESPACE} file (except the standard
	packages).  Objects used from \samp{Depends} packages should be
	imported to avoid conflicts and to allow correct operation when
	the namespace is loaded but not attached.

	\item objects imported \emph{via} \code{:::} calls where
	\code{::} would do.

	\item objects imported by \code{::} which are not exported.

	\item objects imported by \code{:::} calls which do not exist.
      }
      See \sQuote{Writing R Extensions} for good practice.

      \item \command{R CMD check} optionally checks for non-standard
      top-level files and directories (which are often mistakes): this
      is enabled for \option{--as-cran}.

      \item LaTeX style file \code{upquote.sty} is no longer included
      (the version was several years old): it is no longer used in \R.
      A much later version is commonly included in LaTeX distributions
      but does not play well with the \code{ae} fonts which are the
      default for Sweave vignettes.

      \item \command{R CMD build} makes more use of the \file{build}
      sub-directory of package sources, for example to record
      information about the vignettes.

      \item \command{R CMD check} analyses \code{:::} calls.
    }
  }

  \subsection{INSTALLATION and INCLUDED SOFTWARE}{
    \itemize{
      \item The macros used for the texinfo manuals have been changed to
      work better with the incompatible changes made in \command{texinfo
      5.x}.

      \item The minimum version for a system \code{xz} library is now
      5.0.3 (was 4.999).  This is in part to avoid 5.0.2, which can
      compress in ways other versions cannot decompress.

      \item The included version of PCRE has been updated to 8.33.

      \item The included version of \code{zlib} has been updated to
      1.2.8, a bug-fix release.

      \item The included version of xz utils's \code{liblzma} has been
      updated to 5.0.5.

      \item Since \command{javareconf} (see above) is used when \R is
      installed, a stable link for \env{JAVA_HOME} can be supplied then.

      \item Configuring with \option{--disable-byte-compilation} will
      override the \file{DESCRIPTION} files of recommended packages,
      which typically require byte-compilation.

      \item More of the installation and checking process will work even
      when \env{TMPDIR} is set to a path containing spaces, but this is
      not recommended and external software (such as
      \command{texi2dvi}) may fail.
    }
  }

  \subsection{PACKAGE INSTALLATION}{
    \itemize{
      \item Installation is aborted immediately if a \code{LinkingTo}
      package is not installed.

      \item \command{R CMD INSTALL} has a new option
      \code{--no-byte-compile} which will override a \samp{ByteCompile}
      field in the package's \file{DESCRIPTION} file.

      \item License \samp{BSD} is deprecated: use \samp{BSD_3_clause}
      or \samp{BSD_2_clause} instead.

      License \samp{X11} is deprecated: use \samp{MIT} or
      \samp{BSD_2_clause} instead.

      \item Version requirements for \code{LinkingTo} packages are now
      recognized: they are checked at installation.  (Fields with
      version requirements were previously silently ignored.)

      \item The limit of 500 \code{S3method} entries in a
      \code{NAMESPACE} file has been removed.

      \item The default \sQuote{version} of Bioconductor for its
      packages has been changed to the upcoming \samp{2.13}, but this
      can be set by the environment variable \env{R_BIOC_VERSION} when
      \R is installed.
    }
  }

  \subsection{C-LEVEL FACILITIES}{
    \itemize{
      \item \file{Rdefines.h} has been tweaked so it can be included in
      C++ code after \file{R_ext/Boolean.h} (which is included by
      \file{R.h}).

      Note that \file{Rdefines.h} is not kept up-to-date, and
      \file{Rinternals.h} is preferred for new code.

      \item \code{eval} and \code{applyClosure} are now protected
      against package code supplying an invalid \code{rho}.
    }
  }

  \subsection{DEPRECATED AND DEFUNCT}{
    \itemize{
      \item The unused \code{namespace} argument to
      \code{package.skeleton()} is now formally deprecated and will be
      removed in \R 3.1.0.

      \item \code{plclust()} is deprecated: use the \code{plot()} method
      for class \code{"hclust"} instead.

      \item Functions \code{readNEWS()} and \code{checkNEWS()} in
      package \pkg{tools} are deprecated (and they have not worked with
      current \file{NEWS} files for a long time).
    }
  }

  \subsection{DOCUMENTATION}{
    \itemize{
      \item \sQuote{An Introduction to R} has a new chapter on using \R
      as a scripting language including interacting with the OS.
    }
  }

  \subsection{BUG FIXES}{
    \itemize{
      \item \code{help.request()} could not determine the current version
      of \R on CRAN.  (\PR{15241})

      \item On Windows, \code{file.info()} failed on root directories unless
      the path was terminated with an explicit \code{"."}.  (\PR{15302})

      \item The \code{regmatches<-()} replacement function mishandled
      results coming from \code{regexpr()}.  (\PR{15311})

      \item The help for \code{setClass()} and
      \code{representation()} still suggested the deprecated argument
      \code{representation=}. (\PR{15312})

      \item \command{R CMD config} failed in an installed build of \R
      3.0.1 (only) when a sub-architecture was used.  (Reported by
      Berwin Turlach.)

      \item On Windows, the installer modified the \file{etc/Rconsole}
      and \file{etc/Rprofile.site} files even when default options were
      chosen, so the MD5 sums did not refer to the installed versions.
      (Reported by Tal Galili.)

      \item \code{plot(hclust(), cex =)} respects \code{cex} again
      (and possibly others similarly).  (Reported by Peter Langfelder.)

      \item If multiple packages were checked by \command{R CMD check},
      and one was written for a different OS, it would set
      \command{--no-install} for all following packages as well as
      itself.

      \item \code{qr.coef()} and related functions did not properly coerce
      real vectors to complex when necessary.  (\PR{15332})

      \item \code{ftable(a)} now fixes up empty \code{dimnames} such
      that the result is printable.

      \item \code{package.skeleton()} was not starting its search for
      function objects in the correct place if \code{environment} was
      supplied.  (Reported by Karl Forner.)

      \item Parsing code was changing the length field of vectors and
      confusing the memory manager.  (\PR{15345})

      \item The Fortran routine \code{ZHER2K} in the reference BLAS
      had a comment-out bug in two places.
      This caused trouble with \code{eigen()} for Hermitian matrices.
      (\PR{15345} and report from Robin Hankin)

      \item \code{vignette()} and \code{browseVignettes()} did not
      display non-Sweave vignettes properly.

      \item Two warning/error messages have been corrected:
      the (optional) warning produced by a partial name match
      with a pairlist, the error message from a zero-length argument to
      the \code{:} operator.  (Found by Radford Neal; \PR{15358},
      \PR{15356})

      \item \code{svd()} returned \code{NULL} rather than omitting
      components as documented.  (Found by Radford Neal; \PR{15360})

      \item \code{mclapply()} and \code{mcparallel()} with
      \code{silent = TRUE} could break a process that uses
      \code{stdout} output
      unguarded against broken pipes (e.g., \code{zip} will fail
      silently). To work around such issues, they now replace
      \code{stdout} with a descriptor pointed to \file{/dev/null}
      instead. For this purpose, internal \code{closeStdout} and
      \code{closeStderr} functions have gained the \code{to.null} flag.

      \item \code{log()}, \code{signif()} and \code{round()} now raise an
      error if a single named argument is not named \code{x}.  (\PR{15361})

      \item \code{deparse()} now deparses raw vectors in a form that
      is syntactically correct. (\PR{15369})

      \item The \code{jpeg} driver in Sweave created a JPEG file, but gave
      it a \file{.png} extension. (\PR{15370})

      \item Deparsing of infix operators with named arguments is
      improved. (\PR{15350})

      \item \code{mget()}, \code{seq.int()} and \code{numericDeriv()}
      did not duplicate arguments properly. (\PR{15352}, \PR{15353},
      \PR{15354})

      \item \code{kmeans(algorithm = "Hartigan-Wong")} now always stops
      iterating in the QTran stage. (\PR{15364}).

      \item \code{read.dcf()} re-allocated incorrectly and so could
      segfault when called on a file with lines of more than 100 bytes.

      \item On systems where \code{mktime()} does not set \code{errno},
      the last second before the epoch could not be converted from
      \code{POSIXlt} to \code{POSIXct}. (Reported by Bill Dunlap.)

      \item \code{add1.glm()} miscalculated F-statistics when df > 1. (Bill
      Dunlap, \PR{15386}).

      \item \code{stem()} now discards infinite inputs rather than
      hanging.  (\PR{15376})

      \item The parser now enforces C99 syntax for floating point
      hexadecimal constants (e.g. \code{0x1.1p0}), rather than returning
      unintended values for malformed constants. (\PR{15234})

      \item \code{model.matrix()} now works with very long LHS
      names (more than 500 bytes).  (\PR{15377})

      \item \code{integrate()} reverts to the pre-2.12.0 behaviour: from
      2.12.0 to 3.0.1 it sometimes failed to achieve the requested
      tolerance and reported error estimates that were exceeded.
      (\PR{15219})

      \item \code{strptime()} now handles \samp{\%W} fields with value
      0. (\PR{15915})

      \item \R is now better protected against people trying to interact
      with the console in startup code.  (\PR{15325})

      \item Subsetting 1D arrays often lost dimnames (\PR{15301}).

      \item Unary \code{+} on a logical vector did not coerce to
      integer, although unary \code{-} did.

      \item \code{na.omit()} and \code{na.exclude()} added a row
      to a zero-row data frame.  (\PR{15399})

      \item All the (where necessary cut-down) vignettes are installed
      if \R was configured with \option{--without-recommended-packages}.

      \item \code{source()} did not display filenames when reporting
      syntax errors.

      \item Syntax error reports misplaced the caret pointing out the bad
      token.

      \item (Windows only) Starting \R with \command{R} (instead of
      \command{Rterm} or \command{Rgui}) would lose any
      zero-length strings from the command line arguments. (\PR{15406})

      \item Errors in the encoding specified on the command line via
      \command{--encoding=foo} were not handled properly.  (\PR{15405})

      \item If \code{x} is a symbol, \code{is.vector(x, "name")} now
      returns \code{TRUE}, since \code{"name"} and \code{"symbol"}
      should be synonyms.  (Reported by Hervé Pagès.)

      \item \command{R CMD rtags} works on platforms (such as OS X)
      with a XSI-conformant shell command \command{echo}. (\PR{15231})

      \item \code{is.unsorted(NA)} returns false as documented (rather than
      \code{NA}).

      \item \command{R CMD LINK} did not know about sub-architectures.

      \item \code{system()} and \code{system2()} are better protected
      against users who misguidedly have spaces in the temporary
      directory path.

      \item \code{file.show()} and \code{edit()} are now more likely to
      work on file paths containing spaces.  (Where external utilities
      are used, not the norm on Windows nor in \command{R.app} which
      should previously have worked.)

      \item Packages using the \pkg{methods} package are more likely to
      work when they import it but it is not attached.  (Several parts
      of its C code were looking for its \R functions on the search path
      rather than in its namespace.)

      \item \code{lgamma(-x)} is no longer \code{NaN} for very small x.

      \item (Windows) \code{system2()} now respects specifying
      \code{stdout} and \code{stderr} as files if called from
      \command{Rgui}. (\PR{15393})

      \item Closing an \code{x11()} device whilst \code{locator()} or
      \code{identify()} is in progress no longer hangs \R.  (\PR{15253})

      \item \code{list.dirs(full.names = FALSE)} was not implemented.
      (\PR{15170})

      \item \code{format()} sometimes added unnecessary spaces.
      (\PR{15411})

      \item \code{all.equal(check.names = FALSE)} would ignore the request
      to ignore the names and would check them as attributes.

      \item The symbol set by \code{tools::Rd2txt_options(itemBullet=)}
      was not respected in some locales. (\PR{15435})

      \item \code{mcMap()} was not exported by package
      \pkg{parallel}. (\PR{15439})

      \item \code{plot()} for \code{TukeyHSD} objects did not balance
      \code{dev.hold()} and \code{dev.flush()} calls on multi-page plots.
      (\PR{15449})
    }
  }
}

\section{\Rlogo CHANGES IN R 3.0.1}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item \code{chooseCRANmirror()} and \code{chooseBioCmirror()} gain
      an \code{ind} argument (like \code{setRepositories()}).

      \item \code{mcparallel} has a new argument \code{mc.interactive}
      which can modify the interactive flag in the child process. The
      new default is \code{FALSE} which makes child processes
      non-interactive by default (this prevents lock-ups due to children
      waiting for interactive input).

      \item \code{scan()} now warns when end-of-file occurs within
      a quoted string.

      \item \code{count.fields()} is now consistent with \code{scan()}
      in its handling of newlines in quoted strings.  Instead of
      triggering an error, this results in the current line receiving
      \code{NA} as the field count, with the next line getting the total
      count of the two lines.

      \item The default method of \code{image()} will plot axes of the
      class of \code{xlim} and \code{ylim} (and hence of \code{x} and
      \code{y} if there is a suitable \code{range()} method).  Based on
      a suggestion of Michael Sumner.

      \item \code{load()} now has a \code{verbose} argument for
      debugging support, to print the names of objects just before
      loading them.

      \item When loading a serialized object encounters a reference to a
      namespace which cannot be loaded, this is replaced by a reference
      to the global environment, with a warning.

      \item \code{pairs()} gains a \code{line.main} option for title placement.

      \item The remaining instances in which serialization to a raw
      vector was limited to 2GB have been unlimited on a 64-bit
      platform, and in most cases serialization to a vector of more than
      1GB will be substantially faster.
    }
  }

  \subsection{UTILITIES}{
    \itemize{
      \item \command{R CMD config} now make use of personal
      \file{Makevars} files under \file{~/.R} and a site file
      \file{Makevars.site}, in the same way as \command{R CMD SHLIB} and
      \command{R CMD INSTALL}.  This makes the utility more useful in
      package \command{configure} scripts.

      On Windows finding the personal files may require the environment
      variable \env{HOME} set.

      The old behaviour can be obtained with the new options
      \option{--no-user-files} and \option{--no-site-files}.
    }
  }

  \subsection{PACKAGE INSTALLATION}{
    \itemize{
      \item Alternatives to the site and user customization files
      \file{Makevars.site} and \file{~/.R/Makevars} can be specified
      \emph{via} the environment variables \env{R_MAKEVARS_SITE} and
      \env{R_MAKEVARS_USER} respectively.  These can be used to suppress
      the use of the default files by setting an empty value (where
      possible) or a non-existent path.
    }
  }

  \subsection{BUG FIXES}{
    \itemize{
      \item \code{sys.source()} did not report error locations when
      \code{keep.source = TRUE}.

      \item \code{as.POSIXct.numeric} was coercing \code{origin} using
      the \code{tz} argument and not \code{"GMT"} as documented
      (\PR{14973}).

      \item The active binding to assign fields in reference classes
      has been cleaned up to reduce dependence on the class' package
      environment, also fixing bug in initializing read-only fields
      (inspired by a report from Hadley Wickham).

      \item \code{str(d)} no longer gives an error when \code{names(d)}
      contain illegal multibyte strings (\PR{15247}).

      \item Profiling of built-in functions with \code{line.profiling=
      TRUE} did not record the line from which they were called.

      \item \code{citation(pkg)} dropped the header and footer specified
      in the \file{CITATION} file (\PR{15257}).

      \item Quotes were handled differently when reading the first line
      and reading the rest, so \code{read.table()} misread some files
      that contained quote characters (\PR{15245}).

      \item \code{cat()} with \code{sep} a character vector of length
      greater than one and more than one argument was using separators
      inconsistently (\PR{15261}).

      \item On Windows in \R 3.0.0, \code{savePlot()} failed because of
      an incorrect check on the argument count.

      \item \code{unzip(list = TRUE)} returned \code{Names} as a factor
      and not a character vector (as documented) for the internal method.
      (Noticed by Sean O'Riordain.)

      \item \code{contourLines()} now checks more comprehensively for
      conformance of its \code{x}, \code{y} and \code{z} arguments (it
      was used incorrectly in package \CRANpkg{R2G2}).

      \item Saved graphics display lists are \R version-specific.
      Attempting to load workspaces containing them (or some other
      version-specific objects) aborted the load in \R 3.0.0 and
      earlier; now it does a partial load and generates a warning
      instead.

      \item In \R 3.0.0, \code{identify()} and \code{locator()} did
      not record information correctly, so replaying a graph (e.g. by
      copying it to another device) would fail. (\PR{15271})

      \item Calling \code{file.copy()} or \code{dirname()} with the
      invalid input \code{""} (which was being used in packages, despite
      not being a file path) could have caused a segfault.

      \code{dirname("")} is now \code{""} rather than \code{"."} (unless
      it segfaulted).

      \item \code{supsmu()} could read/write outside its input vectors
      for very short inputs (seen in package \CRANpkg{rms} for \code{n = 4}).

      \item \code{as.dendrogram()}'s \code{hclust} method uses less
      memory and hence gets considerably faster for large (n ~ 1000)
      clusterings, thanks to Daniel Müllner. (\PR{15174})

      \item The return value when all workers failed from
      \code{parallel::mclapply(mc.preschedule = TRUE)} was a list of
      strings and not of error objects.  (Spotted by Karl Forner and
      Bernd Bischl.)

      \item In \R 3.0.0, when \code{help()} found multiple pages with
      the same alias, the HTML display of all the selections was not
      produced.  (\PR{15282})

      \item \code{splinefun(method="monoH.FC")} now produces a
      function with first argument named \code{x} and allows
      \code{deriv=3}, as documented. (\PR{15273})

      \item \code{summaryRprof()} would only read the first
      \code{chunksize} lines of an \code{Rprof} file produced with
      \code{line.profiling=TRUE}. By default, this is the first 100
      seconds.  (\PR{15288})

      \item \code{lsfit()} produced an incorrect error message when
      argument \code{x} had more columns than rows or \code{x} had a
      different number of rows than \code{y}. (Spotted by Renaud Gaujoux.)

      \item Binary operations on equal length vectors copied the
      class name from the second operand when the first had no
      class name, but did not set the object bit. (\PR{15299})

      \item The \code{trace()} method for reference generator objects
      failed after those objects became function definitions.

      \item \code{write.table()} did not check that factors were
      constructed correctly, and so caused a segment fault when
      writing bad ones. (\PR{15300})

      \item The internal HTTP server no longer chokes on POST requests
      without body. It will also pass-through other request types for
      custom handlers (with the method stored in Request-Method header)
      instead of failing.
    }
  }
}

\section{\Rlogo CHANGES IN R 3.0.0}{
  \subsection{SIGNIFICANT USER-VISIBLE CHANGES}{
    \itemize{
      \item Packages need to be (re-)installed under this version
      (3.0.0) of \R.

      \item There is a subtle change in behaviour for numeric index
      values \eqn{2^{31}}{2^31} and larger.  These never used to be
      legitimate and so were treated as \code{NA}, sometimes with a
      warning.  They are now legal for long vectors so there is no
      longer a warning, and \code{x[2^31] <- y} will now extend the
      vector on a 64-bit platform and give an error on a 32-bit one.

      \item It is now possible for 64-bit builds to allocate amounts of
      memory limited only by the OS.  It may be wise to use OS
      facilities (e.g. \command{ulimit} in a \command{bash} shell,
      \command{limit} in \command{csh}), to set limits on overall memory
      consumption of an \R process, particularly in a multi-user
      environment.  A number of packages need a limit of at least 4GB of
      virtual memory to load.

      64-bit Windows builds of \R are by default limited in memory usage
      to the amount of RAM installed: this limit can be changed by
      command-line option \option{--max-mem-size} or setting environment
      variable \env{R_MAX_MEM_SIZE}.

      \item Negative numbers for colours are consistently an error:
      previously they were sometimes taken as transparent, sometimes
      mapped into the current palette and sometimes an error.
    }
  }

  \subsection{NEW FEATURES}{
    \itemize{
      \item \code{identical()} has a new argument,
      \code{ignore.environment}, used when comparing functions (with
      default \code{FALSE} as before).

      \item There is a new option, \code{options(CBoundsCheck=)}, which
      controls how \code{.C()} and \code{.Fortran()} pass arguments to
      compiled code.  If true (which can be enabled by setting the
      environment variable \env{R_C_BOUNDS_CHECK} to \samp{yes}), raw,
      integer, double and complex arguments are always copied, and
      checked for writing off either end of the array on return from the
      compiled code (when a second copy is made).  This also checks
      individual elements of character vectors passed to \code{.C()}.

      This is not intended for routine use, but can be very helpful in
      finding segfaults in package code.

      \item In \code{layout()}, the limits on the grid size have been
      raised (again).

      \item New simple \code{provideDimnames()} utility function.

      \item Where methods for \code{length()} return a double value
      which is representable as an integer (as often happens for package
      \CRANpkg{Matrix}), this is converted to an integer.

      \item Matrix indexing of dataframes by two-column numeric indices
      is now supported for replacement as well as extraction.

      \item \code{setNames()} now has a default for its \code{object}
      argument, useful for a character result.

      \item \code{StructTS()} has a revised additive constant in the
      \code{loglik} component of the result: the previous definition is
      returned as the \code{loglik0} component.  However, the help page has
      always warned of a lack of comparability of log-likelihoods for
      non-stationary models.  (Suggested by Jouni Helske.)

      \item The logic in \code{aggregate.formula()} has been revised.
      It is now possible to use a formula stored in a variable;
      previously, it had to be given explicitly in the function call.

      \item \code{install.packages()} has a new argument \code{quiet} to
      reduce the amount of output shown.

      \item Setting an element of the graphics argument \code{lwd} to a
      negative or infinite value is now an error.  Lines corresponding
      to elements with values \code{NA} or \code{NaN} are silently
      omitted.

      Previously the behaviour was device-dependent.

      \item Setting graphical parameters \code{cex}, \code{col},
      \code{lty}, \code{lwd} and \code{pch} in \code{par()} now requires a
      length-one argument.   Previously some silently took the first
      element of a longer vector, but not always when documented to do so.

      \item \code{Sys.which()} when used with inputs which would be
      unsafe in a shell (e.g. absolute paths containing spaces) now uses
      appropriate quoting.

      \item \code{as.tclObj()} has been extended to handle raw vectors.
      Previously, it only worked in the other direction.
      (Contributed by Charlie Friedemann, \PR{14939}.)

      \item New functions \code{cite()} and \code{citeNatbib()} have
      been added, to allow generation of in-text citations from
      \code{"bibentry"} objects.  A \code{cite()} function may be added
      to \code{bibstyle()} environments.

      \item A \code{sort()} method has been added for \code{"bibentry"}
      objects.

      \item The \code{bibstyle()} function now defaults to setting the
      default bibliography style. The \code{getBibstyle()} function
      has been added to report the name of the current default style.

      \item \code{scatter.smooth()} now has an argument \code{lpars} to
      pass arguments to \code{lines()}.

      \item \code{pairs()} has a new \code{log} argument, to allow some
      or all variables to be plotted on logarithmic scale.
      (In part, wish of \PR{14919}.)

      \item \code{split()} gains a \code{sep} argument.

      \item \code{termplot()} does a better job when given a model with
      interactions (and no longer attempts to plot interaction terms).

      \item The parser now incorporates code from Romain Francois'
      \CRANpkg{parser} package, to support more detailed computation on
      the code, such as syntax highlighting, comment-based
      documentation, etc.  Functions \code{getParseData()} and
      \code{getParseText()} access the data.

      \item There is a new function \code{rep_len()} analogous to
      \code{rep.int()} for when speed is required (and names are not).

      \item The undocumented use \code{rep(NULL, length.out = n)} for
      \code{n > 0} (which returns \code{NULL}) now gives a warning.

      \item \code{demo()} gains an \code{encoding} argument for those
      packages with non-ASCII demos: it defaults to the package encoding
      where there is one.

      \item \code{strwrap()} converts inputs with a marked encoding to
      the current locale: previously it made some attempt to pass
      through as bytes inputs invalid in the current locale.

      \item Specifying both \code{rate} and \code{scale} to
      \code{[dpqr]gamma} is a warning (if they are essentially the same
      value) or an error.

      \item \code{merge()} works in more cases where the data frames
      include matrices.  (Wish of \PR{14974}.)

      \item \code{optimize()} and \code{uniroot()} no longer use a
      shared parameter object across calls.  (\code{nlm()},
      \code{nlminb()} and \code{optim()} with numerical derivatives
      still do, as documented.)

      \item The \code{all.equal()} method for date-times is now
      documented: times are regarded as equal (by default) if they
      differ by up to 1 msec.

      \item \code{duplicated()} and \code{unique()} gain a \code{nmax}
      argument which can be used to make them much more efficient when
      it is known that there are only a small number of unique entries.
      This is done automatically for factors.

      \item Functions \code{rbinom()}, \code{rgeom()}, \code{rhyper()},
      \code{rpois()}, \code{rnbinom(),} \code{rsignrank()} and
      \code{rwilcox()} now return integer (not double) vectors.  This
      halves the storage requirements for large simulations.

      \item \code{sort()}, \code{sort.int()} and \code{sort.list()} now
      use radix sorting for factors of less than 100,000 levels when
      \code{method} is not supplied.  So does \code{order()} if called
      with a single factor, unless \code{na.last = NA}.

      \item \code{diag()} as used to generate a diagonal matrix has been
      re-written in C for speed and less memory usage.  It now forces
      the result to be numeric in the case \code{diag(x)} since it is
      said to have \sQuote{zero off-diagonal entries}.

      \item \code{backsolve()} (and \code{forwardsolve()}) are now
      internal functions, for speed and support for large matrices.

      \item More matrix algebra functions (e.g. \code{chol()} and
      \code{solve()}) accept logical matrices (and coerce to numeric).

      \item \code{sample.int()} has some support for  \eqn{n \ge
	2^{31}}{n >= 2^31}: see its help for the limitations.

      A different algorithm is used for \code{(n, size, replace = FALSE,
	prob = NULL)} for \code{n > 1e7} and \code{size <= n/2}.  This
      is much faster and uses less memory, but does give different results.

      \item \code{approxfun()} and \code{splinefun()} now return a
      wrapper to an internal function in the \pkg{stats} namespace
      rather than a \code{.C()} or \code{.Call()} call.  This is more
      likely to work if the function is saved and used in a different
      session.

      \item The functions \code{.C()}, \code{.Call()},
      \code{.External()} and \code{.Fortran()} now give an error (rather
      than a warning) if called with a named first argument.

      \item \code{Sweave()} by default now reports the locations in
      the source file(s) of each chunk.

      \item \code{clearPushBack()} is now a documented interface to a
      long-existing internal call.

      \item \code{aspell()} gains filters for \R code, Debian Control
      Format and message catalog files, and support for \R level
      dictionaries.  In addition, package \pkg{utils} now provides
      functions \code{aspell_package_R_files()} and
      \code{aspell_package_C_files()} for spell checking \R and C level
      message strings in packages.

      \item \code{bibentry()} gains some support for \dQuote{incomplete}
      entries with a \samp{crossref} field.

      \item \code{gray()} and \code{gray.colors()} finally allow
      \code{alpha} to be specified.

      \item \code{monthplot()} gains parameters to control the look of
      the reference lines.  (Suggestion of Ian McLeod.)

      \item Added support for new \code{\%~\%} relation
      (\dQuote{is distributed as}) in plotmath.

      \item \code{domain = NA} is accepted by \code{gettext()} and
      \code{ngettext()}, analogously to \code{stop()} etc.

      \item \code{termplot()} gains a new argument \code{plot = FALSE}
      which returns information to allow the plots to be modified for
      use as part of other plots, but does not plot them.
      (Contributed by Terry Therneau, \PR{15076}.)

      \item \code{quartz.save()}, formerly an undocumented part of
      \command{R.app}, is now available to copy a device to a
      \code{quartz()} device.  \code{dev.copy2pdf()} optionally does
      this for PDF output: \code{quartz.save()} defaults to PNG.

      \item The default method of \code{pairs()} now allows
      \code{text.panel = NULL} and the use of \code{<foo>.panel = NULL}
      is now documented.

      \item \code{setRefClass()} and \code{getRefClass()} now return
      class generator functions, similar to \code{setClass()}, but
      still with the reference fields and methods as before
      (suggestion of Romain Francois).

      \item New functions \code{bitwNot()}, \code{bitwAnd()},
      \code{bitwOr()} and \code{bitwXor()}, using the internal
      interfaces previously used for classes \code{"octmode"} and
      \code{"hexmode"}.

      Also \code{bitwShiftL()} and \code{bitwShiftR()} for shifting bits
      in elements of integer vectors.

      \item New option \code{"deparse.cutoff"} to control the deparsing
      of language objects such as calls and formulae when printing.
      (Suggested by a comment of Sarah Goslee.)

      \item \code{colors()} gains an argument \code{distinct}.

      \item New \code{demo(colors)} and \code{demo(hclColors)}, with
      utility functions.

      \item \code{list.files()} (aka \code{dir()}) gains a new optional
      argument \code{no..} which allows to exclude \code{"."} and
      \code{".."} from listings.

      \item Multiple time series are also of class \code{"matrix"};
      consequently, \code{head()}, e.g., is more useful.

      \item \code{encodeString()} preserves UTF-8 marked encodings.
      Thus if factor levels are marked as UTF-8 an attempt is made to
      print them in UTF-8 in \command{RGui} on Windows.

      \item \code{readLines()} and \code{scan()} (and hence
      \code{read.table()}) in a UTF-8 locale now discard a UTF-8
      byte-order-mark (BOM).  Such BOMs are allowed but not recommended
      by the Unicode Standard: however Microsoft applications can
      produce them and so they are sometimes found on websites.

      The encoding name \code{"UTF-8-BOM"} for a connection will
      ensure that a UTF-8 BOM is discarded.

      \item \code{mapply(FUN, a1, ..)} now also works when \code{a1} (or
      a further such argument) needs a \code{length()} method (which the
      documented arguments never do).  (Requested by Hervé Pagès; with a
      patch.)

      \item \code{.onDetach()} is supported as an alternative to
      \code{.Last.lib}.  Unlike \code{.Last.lib}, this does not need to
      be exported from the package's namespace.

      \item The \code{srcfile} argument to \code{parse()} may now be a
      character string, to be used in error messages.

      \item The \code{format()} method for \code{ftable} objects gains
      a \code{method} argument, propagated to \code{write.ftable()} and
      \code{print()}, allowing more compact output, notably for LaTeX
      formatting, thanks to Marius Hofert.

      \item The \code{utils::process.events()} function has been added
      to trigger immediate event handling.

      \item \code{Sys.which()} now returns \code{NA} (not \code{""}) for
      \code{NA} inputs (related to \PR{15147}).

      \item The \code{print()} method for class \code{"htest"} gives
      fewer trailing spaces (wish of \PR{15124}).

      Also print output from \code{HoltWinters()}, \code{nls()} and others.

      \item \code{loadNamespace()} allows a version specification to be
      given, and this is used to check version specifications given in
      the \samp{Imports} field when a namespace is loaded.

      \item \code{setClass()} has a new argument, \code{slots}, clearer
      and less ambiguous than \code{representation}.  It is recommended
      for future code, but should be back-compatible.  At the same time,
      the allowed slot specification is slightly more general.  See the
      documentation for details.

      \item \code{mget()} now has a default for \code{envir} (the frame
      from which it is called), for consistency with \code{get()} and
      \code{assign()}.

      \item \code{close()} now returns an integer status where available,
      invisibly. (Wish of \PR{15088}.)

      \item The internal method of \code{tar()} can now store paths too
      long for the \samp{ustar} format, using the (widely supported) GNU
      extension.  It can also store long link names, but these are much
      less widely supported.  There is support for larger files, up to
      the \samp{ustar} limit of 8GB.

      \item Local reference classes have been added to package
      \pkg{methods}.  These are a technique for avoiding unneeded
      copying of large components of objects while retaining standard \R
      functional behavior.  See \code{?LocalReferenceClasses}.

      \item \code{untar()} has a new argument \code{restore_times} which
      if false (not the default) discards the times in the tarball.
      This is useful if they are incorrect (some tarballs submitted to
      \acronym{CRAN} have times in a local time zone or many years in the
      past even though the standard required them to be in UTC).

      \item \code{replayplot()} cannot (and will not attempt to) replay
      plots recorded under \R < 3.0.0.  It may crash the \R session if
      an attempt is made to replay plots created in a different build of
      \R >= 3.0.0.

      \item Palette changes get recorded on the display list, so
      replaying plots (including when resizing screen devices and using
      \code{dev.copy()}) will work better when the palette is changed
      during a plot.

      \item \code{chol(pivot = TRUE)} now defaults to LAPACK, not LINPACK.

      \item The \code{parse()} function has a new parameter
      \code{keep.source}, which defaults to \code{options("keep.source")}.

      \item Profiling via \code{Rprof()} now optionally records information
      at the statement level, not just the function level.

      \item The \code{Rprof()} function now quotes function names in
      in its output file on Windows, to be consistent with the quoting
      in Unix.

      \item Profiling via \code{Rprof()} now optionally records
      information about time spent in GC.

      \item The HTML help page for a package now displays non-vignette
      documentation files in a more accessible format.

      \item To support \code{options(stringsAsFactors = FALSE)},
      \code{model.frame()}, \code{model.matrix()} and
      \code{replications()} now automatically convert character
      vectors to factors without a warning.

      \item The \code{print} method for objects of class \code{"table"}
      now detects tables with 0-extents and prints the results as, e.g.,
      \samp{< table of extent 0 x 1 x 2 >}. (Wish of \PR{15198}.)

      \item Deparsing involving calls to anonymous functions has been
      made closer to reversible by the addition of extra parentheses.

      \item The function \code{utils::packageName()} has been added as
      a lightweight version of \code{methods::getPackageName()}.

      \item \code{find.package(lib.loc = NULL)} now treats loaded
      namespaces preferentially in the same way as attached packages
      have been for a long time.

      \item In Windows, the Change Directory dialog now defaults to
      the current working directory, rather than to the last directory
      chosen in that dialog.

      \item \code{available.packages()} gains a
      \code{"license/restricts_use"} filter which retains only packages
      for which installation can proceed solely based on packages which
      are guaranteed not to restrict use.

      \item New \code{check_packages_in_dir()} function in package
      \pkg{tools} for conveniently checking source packages along with
      their reverse dependencies.

      \item R's completion mechanism has been improved to handle help
      requests (starting with a question mark).  In particular, help
      prefixes are now supported, as well as quoted help topics.  To
      support this, completion inside quotes are now handled by R by
      default on all platforms.

      \item The memory manager now allows the strategy used to balance
      garbage collection and memory growth to be controlled by setting
      the environment variable \env{R_GC_MEM_GROW}. See \code{?Memory}
      for more details.

      \item (\sQuote{For experts only}, as the introductory manual
      says.)  The use of environment variables \env{R_NSIZE} and
      \env{R_VSIZE} to control the initial (= minimum) garbage
      collection trigger for number of cons cels and size of heap has
      been restored: they can be overridden by the command-line options
      \code{--min-nsize} and \code{--min-vsize}; see \code{?Memory}.

      \item On Windows, the device name for bitmap devices as reported
      by \code{.Device} and \code{.Devices} no longer includes the file
      name.   This is for consistency with other platforms and was
      requested by the \CRANpkg{lattice} maintainer.

      \code{win.metafile()} still uses the file name: the exact form is
      used by package \CRANpkg{tkrplot}.

      \item \code{set.seed(NULL)} re-initializes \code{.Random.seed} as
      done at the beginning of the session if not already set.
      (Suggestion of Bill Dunlap.)

      \item The \code{breaks} argument in \code{hist.default()} can now be
      a function that returns the breakpoints to be used (previously it
      could only return the suggested number of breakpoints).

      \item File \file{share/licenses/licenses.db} has some
      clarifications, especially as to which variants of \sQuote{BSD}
      and \sQuote{MIT} is intended and how to apply them to packages.
      The problematic licence \sQuote{Artistic-1.0} has been removed.
    }
  }

  \subsection{LONG VECTORS}{
    This section applies only to 64-bit platforms.
    \itemize{
      \item There is support for vectors longer than \eqn{2^{31}-1}{2^31
      - 1} elements.  This applies to raw, logical, integer, double,
      complex and character vectors, as well as lists.  (Elements of
      character vectors remain limited to \eqn{2^{31}-1}{2^31 - 1}
      bytes.)

      \item Most operations which can sensibly be done with long vectors
      work: others may return the error \sQuote{long vectors not
      supported yet}.  Most of these are because they explicitly work
      with integer indices (e.g. \code{anyDuplicated()} and
      \code{match()}) or because other limits (e.g. of character strings
      or matrix dimensions) would be exceeded or the operations would be
      extremely slow.

      \item \code{length()} returns a double for long vectors, and
      lengths can be set to \eqn{2^{31}}{2^31} or more by the
      replacement function with a double value.

      \item Most aspects of indexing are available.  Generally
      double-valued indices can be used to access elements beyond
      \eqn{2^{31}-1}{2^31 - 1}.

      \item There is some support for matrices and arrays with each
      dimension less than \eqn{2^{31}}{2^31} but total number of
      elements more than that.  Only some aspects of matrix algebra work
      for such matrices, often taking a very long time.  In other cases
      the underlying Fortran code has an unstated restriction (as was
      found for complex \code{svd()}).

      \item \code{dist()} can produce dissimilarity objects for more
      than 65536 rows (but for example \code{hclust()} cannot process
      such objects).

      \item \code{serialize()} to a raw vector is unlimited in size
      (except by resources).

      \item The C-level function \code{R_alloc} can now allocate
      \eqn{2^{35}}{2^35} or more bytes.

      \item \code{agrep()} and \code{grep()} will return double vectors
      of indices for long vector inputs.

      \item Many calls to \code{.C()} have been replaced by
      \code{.Call()} to allow long vectors to be supported (now or in
      the future).  Regrettably several packages had copied the non-API
      \code{.C()} calls and so failed.

      \item \code{.C()} and \code{.Fortran()} do not accept long vector
      inputs.  This is a precaution as it is very unlikely that existing
      code will have been written to handle long vectors (and the \R
      wrappers often assume that \code{length(x)} is an integer).

      \item Most of the methods for \code{sort()} work for long vectors.

      \code{rank()}, \code{sort.list()} and \code{order()} support
      long vectors (slowly except for radix sorting).

      \item \code{sample()} can do uniform sampling from a long vector.
    }
  }

  \subsection{PERFORMANCE IMPROVEMENTS}{
    \itemize{
      \item More use has been made of \R objects representing registered
      entry points, which is more efficient as the address is provided
      by the loader once only when the package is loaded.

      This has been done for packages \code{base}, \code{methods},
      \code{splines} and \code{tcltk}: it was already in place for the
      other standard packages.

      Since these entry points are always accessed by the \R entry
      points they do not need to be in the load table which can be
      substantially smaller and hence searched faster.  This does mean
      that \code{.C} / \code{.Fortran} / \code{.Call} calls copied from
      earlier versions of \R may no longer work -- but they were never
      part of the API.

      \item Many \code{.Call()} calls in package \pkg{base} have been
      migrated to \code{.Internal()} calls.

      \item \code{solve()} makes fewer copies, especially when \code{b}
      is a vector rather than a matrix.

      \item \code{eigen()} makes fewer copies if the input has dimnames.

      \item Most of the linear algebra functions make fewer copies when
      the input(s) are not double (e.g. integer or logical).

      \item A foreign function call (\code{.C()} etc) in a package
      without a \code{PACKAGE} argument will only look in the first DLL
      specified in the \file{NAMESPACE} file of the package rather than
      searching all loaded DLLs.  A few packages needed \code{PACKAGE}
      arguments added.

      \item The \code{@<-} operator is now implemented as a primitive,
      which should reduce some copying of objects when used.  Note that
      the operator object must now be in package \pkg{base}: do not try
      to import it explicitly from package \pkg{methods}.
    }
  }


  \subsection{PACKAGE INSTALLATION}{
    \itemize{
      \item The transitional support for installing packages without
      namespaces (required since \R 2.14.0) has been removed.
      \command{R CMD build} will still add a namespace, but a
      \code{.First.lib()} function will need to be converted.

      \command{R CMD INSTALL} no longer adds a namespace (so
      installation will fail), and a \code{.First.lib()} function in a
      package will be ignored (with an installation warning for now).

      As an exception, packages without a \file{R} directory and no
      \file{NAMESPACE} file can still be installed.

      \item Packages can specify in their \file{DESCRIPTION file} a line
      like \preformatted{     Biarch: yes
}
      to be installed on Windows with \option{--force-biarch}.

      \item Package vignettes can now be processed by other engines
      besides \code{Sweave}; see \sQuote{Writing R Extensions} and the
      \code{tools::vignetteEngine} help topic for details.

      \item The \file{*.R} tangled source code for vignettes is now
      included in tarballs when \command{R CMD build} is used to produce
      them.  In \R 3.0.0, \file{*.R} files not in the sources will be
      produced at install time, but eventually this will be dropped.

      \item The package type \code{"mac.binary"} now looks in a path in
      the repository without any Mac subtype (which used to be
      \samp{universal} or \samp{leopard}): it looks in
      \file{bin/macosx/contrib/3.0} rather than
      \file{bin/macosx/leopard/contrib/2.15}).  This is the type used
      for the \acronym{CRAN} binary distribution for OS X as from \R
      3.0.0.

      \item File \file{etc/Makeconf} makes more use of the macros
      \code{$(CC)}, \code{$(CXX)}, \code{$(F77)} and \code{$(FC)}, so
      the compiler in use can be changed by setting just these (and if
      necessary the corresponding flags and \code{FLIBS}) in file
      \file{~/.R/Makevars}.

      This is convenient for those working with binary distributions of
      \R, e.g. on OS X.
    }
  }

  \subsection{UTILITIES}{
    \itemize{
      \item \command{R CMD check} now gives a warning rather than a
      note if it finds calls to \code{abort}, \code{assert} or
      \code{exit} in compiled code, and has been able to find the
      \file{.o} file in which the calls occur.

      Such calls can terminate the \R process which loads the package.

      \item The location of the build and check environment files can
      now be specified by the environment variables
      \env{R_BUILD_ENVIRON} and \env{R_CHECK_ENVIRON}, respectively.

      \item \command{R CMD Sweave} gains a \option{--compact} option
      to control possibly reducing the size of the PDF file it creates
      when \option{--pdf} is given.

      \item \command{R CMD build} now omits Eclipse's \file{.metadata}
      directories, and \command{R CMD check} warns if it finds them.

      \item \command{R CMD check} now does some checks on functions
      defined within reference classes, including of \code{.Call()} etc
      calls.

      \item \command{R CMD check --as-cran} notes assignments to the
      global environment, calls to \code{data()} which load into the
      global environment, and calls to \code{attach()}.

      \item \command{R CMD build} by default uses the internal method of
      \code{tar()} to prepare the tarball.  This is more likely to
      produce a tarball compatible with \command{R CMD INSTALL} and
      \command{R CMD check}: an external \command{tar} program,
      including options, can be specified \emph{via} the environment
      variable \env{R_BUILD_TAR}.

      \item \code{tools::massageExamples()} is better protected against
      packages which re-define base functions such as \code{cat()} and
      \code{get()} and so can cause \command{R CMD check} to fail when
      checking examples.

      \item \command{R CMD javareconf} has been enhanced to be more
      similar to the code used by \command{configure}.

      There is now a test that a JNI program can be compiled (like
      \command{configure} did) and only working settings are used.

      It makes use of custom settings from configuration recorded in
      \file{etc/javaconf}.

      \item The \option{--no-vignettes} argument of \command{R CMD
	build} has been renamed to the more accurate
      \option{--no-build-vignettes}: its action has always been to
      (re)build vignettes and never omitted them.

      \command{R CMD check} accepts \option{--no-build-vignettes} as a
      preferred synonym for \option{--no-rebuild-vignettes}.
    }
  }

  \subsection{DEPRECATED AND DEFUNCT}{
    \itemize{
      \item The \code{ENCODING} argument to \code{.C()} is defunct.
      Use \code{iconv()} instead.

      \item The \code{.Internal(eval.with.vis)} non-API function has
      been removed.

      \item Support for the converters for use with \code{.C()} has been
      removed, including the oft misused non-API header
      \file{R_ext/RConverters.h}.

      \item The previously deprecated uses of \code{array()} with a
      0-length \code{dim} argument and \code{tapply()} with a 0-length
      \code{INDEX} list are now errors.

      \item \samp{Translation} packages are defunct.

      \item Calling \code{rep()} or \code{rep.int()} on a pairlist or
      other non-vector object is now an error.

      \item Several non-API entry points have been transferred to
      packages (e.g. \code{R_zeroin2}) or replaced by different non-API
      entry points (e.g. \code{R_tabulate}).

      \item The \sQuote{internal} graphics device invoked by
      \code{.Call("R_GD_nullDevice", package = "grDevices")} has
      been removed: use \code{pdf(file = NULL)} instead.

      \item The \code{.Fortran()} entry point \code{"dqrls"} which has
      not been used by \R since version 2.15.1 is no longer available.

      \item Functions \code{traceOn()} and \code{traceOff()} in package
      \pkg{methods} are now defunct.

      \item Function \code{CRAN.packages()} is finally defunct.

      \item Use of \code{col2rgb(0)} is defunct: use \code{par("bg")} or
      \code{NA} instead.

      \item The long-defunct functions \code{Rd_parse()},
      \code{anovalist.lm()}, \code{categpry()}, \code{clearNames()},
      \code{gammaCody()}, \code{glm.fit.null()}, \code{lm.fit.null()},
      \code{lm.wfit.null()}, \code{manglePackageNames()},
      \code{mauchley.test()}, \code{package.contents()},
      \code{print.coefmat()}, \code{reshapeLong()},
      \code{reshapeWide()}, \code{tkclose()}, \code{tkcmd()},
      \code{tkfile.dir()}, \code{tkfile.tail()}, \code{tkopen()},
      \code{tkputs()}, \code{tkread()}, \code{trySilent()} and
      \code{zip.file.extract()} have been removed entirely (but are
      still documented in the help system).

      \item The unused \code{dataPath} argument to
      \code{attachNamespace()} has been removed.

      \item \code{grid.prompt()} has been removed: use
      \code{devAskNewPage()} instead.

      \item The long-deprecated \code{intensities} component is no
      longer returned by \code{hist()}.

      \item \code{mean()} for data frames and \code{sd()} for data
      frames and matrices are defunct.

      \item \code{chol(pivot = FALSE, LINPACK = TRUE)},
      \code{ch2inv(LINPACK = TRUE)}, \code{eigen(EISPACK = TRUE)},
      \code{solve(LINPACK = TRUE)} and \code{svd(LINPACK = TRUE)} are
      defunct: LAPACK will be used, with a warning.

      \item The \code{keep.source} argument to \code{library()} and
      \code{require()} is defunct.  This option needs to be set
      at install time.

      \item Documentation for \code{real()}, \code{as.real()} and
      \code{is.real()} has been moved to \sQuote{defunct} and the
      functions removed.

      \item The \code{maxRasters} argument of \code{pdf()} (unused since
      \R 2.14.0) has been removed.

      \item The unused \code{fontsmooth} argument has been removed from
      the \code{quartz()} device.

      \item All the (non-API) EISPACK entry points in \R have been removed.

      \item \code{chol(pivot = TRUE, LINPACK = TRUE)} is deprecated.

      \item The long-deprecated use of \code{\\synopsis} in the
      \samp{Usage} section of \file{.Rd} files will be removed in \R
      3.1.0.

      \item \code{.find.package()} and \code{.path.package()} are
      deprecated: only the public versions without the dot have ever
      been in the API.

      \item In a package's \file{DESCRIPTION} file, \preformatted{    License: X11}
      is deprecated, since it includes
      \sQuote{Copyright (C) 1996 X Consortium} which cannot be
      appropriate for a current \R package.  Use \sQuote{MIT} or
      \sQuote{BSD_2_clause} instead.
    }
  }

  \subsection{CODE MIGRATION}{
    \itemize{
      \item The C code underlying base graphics has been migrated to the
      \pkg{graphics} package (and hence no longer uses
      \code{.Internal()} calls).

      \item Most of the \code{.Internal()} calls used in the \pkg{stats}
      package have been migrated to C code in that package.

      This means that a number of \code{.Internal()} calls which have
      been used by packages no longer exist, including
      \code{.Internal(cor)} \code{.Internal(cov)},
      \code{.Internal(optimhess)} and \code{.Internal(update.formula)}.

      \item Some \code{.External()} calls to the \code{base} package
      (really to the \R executable or shared library) have been moved to
      more appropriate packages.  Packages should not have been using
      such calls, but some did (mainly those used by \code{integrate()}).
    }
  }

  \subsection{PACKAGE parallel}{
    \itemize{
      \item There is a new function \code{mcaffinity()} which allows
      getting or setting the CPU affinity mask for the current \R process on
      systems that supports this (currently only Linux has been tested
      successfully).  It has no effect on systems which do not support
      process affinity.  Users are not expected to use this function
      directly (with the exception of fixing libraries that break
      affinity settings like OpenBLAS) -- the function is rather
      intended to support affinity control in high-level parallel
      functions.  In the future, \R may supplement lack of affinity
      control in the OS by its own bookkeeping via \code{mcaffinity()}
      related to processes and threads it spawns.

      \item \code{mcparallel()} has a new argument \code{mc.affinity}
      which attempts to set the affinity of the child process according
      to the specification contained therein.

      \item The port used by socket clusters is chosen randomly: this
      should help to avoid clashes observed when two users of a
      multi-user machine try to create a cluster at the same time.  To
      reproduce the previous behaviour set environment variable
      \env{R_PARALLEL_PORT} to \code{10187}.
    }
  }

  \subsection{C-LEVEL FACILITIES}{
    \itemize{
      \item There has been some minor re-organization of the non-API
      header files.  In particular, \file{Rinternals.h} no longer
      includes the non-API header \file{R_exts/PrtUtil.h}, and that no
      longer includes \file{R_exts/Print.h}.

      \item Passing \code{NULL} to \code{.C()} is now an error.

      \item \code{.C()} and \code{.Fortran()} now warn if
      \code{"single"} arguments are used with \code{DUP = FALSE}, as
      changes to such arguments are not returned to the caller.

      \item C entry points \code{R_qsort} and \code{R_qsort_I} now have
      \code{start} and \code{end} as \code{size_t} to allow them to work
      with longer vectors on 64-bit platforms.  Code using them should
      be recompiled.

      \item A few recently added C entry points were missing the
      remapping to \code{Rf_}, notably \code{[dpq]nbinom_mu}.

      \item Some of the interface pointers formerly available only to
      \command{R.app} are now available to front-ends on all
      Unix-alikes: one has been added for the interface to
      \code{View()}.

      \item \code{PACKAGE = ""} is now an error in \code{.C()} etc calls:
      it was always contrary to the documentation.

      \item Entry point \code{rcont2} has been migrated to package
      \pkg{stats} and so is no longer available.

      \item \code{R_SVN_REVISION} in \file{Rversion.h} is now an integer
      (rather than a string) and hence usable as e.g.
      \code{#if R_SVN_REVISION < 70000}.

      \item The entry points \code{rgb2hsv} and \code{hsv2rgb} have been
      migrated to package \pkg{grDevices} and so are no longer available.

      \item \code{R_GE_version} has been increased to \code{10} and
      \code{name2col} removed (use \code{R_GE_str2col} instead).  \R
      internal colour codes are now defined using the typedef
      \code{rcolor}.

      \item The \code{REPROTECT} macro now checks that the protect index
      is valid.

      \item Several non-API entry points no longer used by \R have been
      removed, including the Fortran entry points \code{chol},
      \code{chol2inv}, \code{cg}, \code{ch} and \code{rg}, and the C
      entry points \code{Brent_fmin}, \code{fft_factor} and \code{fft_work}.

      \item If a \code{.External} call is registered with a number of
      arguments (other than \code{-1}), the number of arguments passed
      is checked for each call (as for other foreign function calls).

      \item It is now possible to write custom connection
      implementations outside core R using \file{R_ext/Connections.h}.
      Please note that the implementation of connections is still
      considered internal and may change in the future (see the above
      file for details).
    }
  }

  \subsection{INTERNATIONALIZATION}{
    \itemize{
      \item The management of translations has been converted to \R
      code: see \code{?tools::update_pkg_po}.

      \item The translations for the \R interpreter and
      \command{RGui.exe} are now part of the \pkg{base} package (rather than
      having sources in directory \file{po} and being installed to
      \file{share/locale}).  Thus the \pkg{base} package supports three
      translation domains, \code{R-base}, \code{R} and \code{RGui}.

      \item The compiled translations which ship with \R are all
      installed to the new package \pkg{translations} for easier
      updating.  The first package of that name found on
      \code{.libPaths()} at the start of the \R session will be used.
      (It is possible messages will be used before \code{.libPaths()} is
      set up in which case the default translations will be used: set
      environment variable \env{R_TRANSLATIONS} to point to the location
      of the intended \pkg{translations} package to use this right from
      the start.)

      \item The translations form a separate group in the Windows
      installer, so can be omitted if desired.

      \item The markup for many messages has been changed to make them
      easier to translate, incorporating suggestions from Łukasz Daniel.
    }
  }

  \subsection{INSTALLATION}{
    \itemize{
      \item There is again support for building without using the C
      \sQuote{long double} type.  This is required by C99, but system
      implementations can be slow or flawed.  Use \command{configure}
      option \option{--disable-long-double}.

      \item \command{make pdf} and \command{make install-pdf} now make
      and install the full reference index (including all base and
      recommended packages).

      \item The 'reference manual' on the Windows GUI menu and included
      in the installer is now the full reference index, including all
      base and recommended packages.

      \item \R help pages and manuals have no ISBNs because ISBN
      rules no longer allow constantly changing content to be assigned
      an ISBN.

      \item The Windows installer no longer installs a Start Menu
      link to the static help pages; as most pages are generated
      dynamically, this led to a lot of broken links.

      \item Any custom settings for Java configuration are recorded in
      file \file{etc/javaconf} for subsequent use by \command{R CMD
	javareconf}.

      \item There is now support for \command{makeinfo} version 5.0
      (which requires a slightly different \file{.texi} syntax).

      \item The minimum versions for \option{--use-system-zlib} and
      \code{--use-system-pcre} are now tested as 1.2.5 and 8.10
      respectively.

      \item On Windows, the stack size is reduced to 16MB on 32-bit
      systems: misguided users were launching many threads without
      controlling the stack size.

      \item \command{configure} no longer looks for file
      \file{~/.Rconfig}: \file{~/.R/config} has long been preferred.
    }
  }

  \subsection{BUG FIXES}{
    \itemize{
      \item When \command{R CMD build} is run in an encoding other than
      the one specified in the package's \file{DESCRIPTION} file it
      tries harder to expand the \code{authors@R} field in the specified
      encoding.  (\PR{14958})

      \item If \command{R CMD INSTALL} is required to expand the
      \code{authors@R} field of the \file{DESCRIPTION} file, it tries
      harder to do so in the encoding specified for the package (rather
      than using ASCII escapes).

      \item Fix in package \pkg{grid} for pushing a viewport into a
      layout cell, where the layout is within a viewport that has zero
      physical width OR where the layout has zero total relative width
      (likewise for height).  The layout column widths (or row heights)
      in this case were being calculated with non-finite values.
      (Reported by Winston Chang.)

      \item \code{solve(A, b)} for a vector \code{b} gave the answer
      names from \code{colnames(A)} for \code{LINPACK = TRUE} but not in
      the default case.

      \item \code{La.svd()} accepts logical matrices (as documented, and
      as \code{svd()} did).

      \item \code{legend()} now accepts negative \code{pch} values, in
      the same way \code{points()} long has.

      \item Parse errors when installing files now correctly display
      the name of the file containing the bad code.

      \item In Windows, tcltk windows were not always properly constructed.
      (\PR{15150})

      \item The internal functions implementing \code{parse()},
      \code{tools::parseLatex()} and \code{tools::parse_Rd()} were not
      reentrant, leading to errors in rare circumstances such as a
      garbage collection triggering a recursive call.

      \item Field assignments in reference class objects via
      \code{$<-} were not being checked
      because the magic incantation to turn methods on for that
      primitive operator had been inadvertently omitted. %$

      \item \code{setHook(hookname, value, action="replace")} set the
      hook to be the value, rather than a list containing the value as
      documented. (\PR{15167})

      \item  If a package used a \file{NEWS.Rd} file, the main HTML
      package index page did not link to it.  (Reported by Dirk
      Eddelbuettel.)

      \item The primitive implementation of \code{@<-} was not
      checking the class of the replacement.  It now does a check,
      quicker but less general than \code{slot<-}. See the help.

      \item \code{split(x, f)} now recycles classed objects \code{x} in
      the same way as vectors.  (Reported by Martin Morgan.)

      \item \code{pbeta(.28, 1/2, 2200, lower.tail=FALSE, log.p=TRUE)}
      is no longer \code{-Inf}; ditto for corresponding \code{pt()} and
      \code{pf()} calls, such as \code{pt(45, df=5000, lower.tail=FALSE,
	log.p=TRUE)}. (\PR{15162})

      \item The Windows graphics device would crash \R{} if a user
      attempted to load the graphics history from a variable that was
      not a saved history. (\PR{15230})

      \item The workspace size for the \code{predict()}
      method for \code{loess()} could exceed the maximum integer size.
      (Reported by Hiroyuki Kawakatsu.)

      \item \code{ftable(x, row.vars, col.vars)} now also works when the
      \code{*.vars} arguments are (integer or character vectors) of
      length zero.

      \item Calling \code{cat()} on a malformed UTF-8 string could cause
      the Windows GUI to lock up. (\PR{15227})

      \item \code{removeClass(cc)} gave "node stack overflow" for some
      class definitions containing \code{"array"} or \code{"matrix"}.
    }
  }
}

\section{CHANGES in previous versions}{
  \itemize{
    \item Older news can be found in text format in files
    \ifelse{html}{\href{../NEWS.0}{NEWS.0}, \href{../NEWS.1}{NEWS.1}
      and \href{../NEWS.2}{NEWS.2}}{\file{NEWS.0}, \file{NEWS.1} and
      \file{NEWS.2}}
    in the \file{doc} directory.  News in HTML format for
    \R versions from 2.10.0 to 2.15.3 is in
    \ifelse{html}{\url{NEWS.2.html}}{\file{doc/html/NEWS.2.html}}.
  }
}

% -*- coding: utf-8 -*-
\newcommand{\PR}{\Sexpr[results=rd]{tools:::Rd_expr_PR(#1)}}
\newcommand{\Rlogo}{\if{html}{\figure{../../html/logo.jpg}}\if{latex}{\figure{logo.jpg}{options: width=0.5in}}}
\newcommand{\CRANpkg}{\href{http://CRAN.R-project.org/package=#1}{\pkg{#1}}}

\name{NEWS}
\title{ R News}
\encoding{UTF-8}

\section{\Rlogo CHANGES IN R 3.0.1 patched}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item The macros used for the texinfo manuals have been changed to
      work better with the incompatible changes made in texinfo 5.x.
    }
  }
  
  \subsection{C-LEVEL FACILITIES}{
    \itemize{
      \item \file{Rdefines.h} has been tweaked so it can be included in
      C++ code with \file{R_exts/Boolean.h} (which is included by
      \file{R.h}).  Note though that \file{Rdefines.h} is not kept
      up-to-date, and \file{Rinternals.h} is preferred for new code.
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

      \item Documentation for \code{setClass} and
      \code{representation} still suggested the deprecated argument
      \code{representation=}. (\PR{15312})
      
      \item \command{R CMD config} failed in an installed build of \R
      3.0.1 (only) when a sub-architecture was used.  (Reported by
      Berwin Turlach.)
      
      \item On Windows, the installer modified the \file{etc/Rconsole} and
      \file{etc/Rprofile.site} files even when default options were 
      chosen.  (Reported by Tal Galili.)
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
      \code{parallel::mclapply(mc.presechedule = TRUE)} was a list of
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
      \acronym{CRAN} have times in a local timezone or many years in the
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

      \item Deparsing involving calls to anonymous functions and has been
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

\section{\Rlogo CHANGES IN R VERSION 2.15.3}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item \code{lgamma(x)} for very small \code{x} (in the
      denormalized range) is no longer \code{Inf} with a warning.

      \item \code{image()} now sorts an unsorted \code{breaks} vector,
      with a warning.

      \item The internal methods for \code{tar()} and \code{untar()} do
      a slightly more general job for \sQuote{ustar}-style handling of
      paths of more than 100 bytes.

      \item Packages \pkg{compiler} and \pkg{parallel} have been added
      to the reference index (\file{refman.pdf}).

      \item \code{untar(tar = "internal")} has some support for
      \code{pax} headers as produced by e.g. \command{gnutar --posix}
      (which seems prevalent on OpenSUSE 12.2) or
      \command{bsdtar --format pax}, including long path and link names.

      \item \code{sQuote()} and \code{dQuote()} now handle 0-length
      inputs.  (Suggestion of Ben Bolker.)

      \item \code{summaryRprof()} returns zero-row data frames rather
      than throw an error if no events are recorded, for consistency.

      \item The included version of PCRE has been updated to 8.32.

      \item The \pkg{tcltk} namespace can now be re-loaded after
      unloading.

      The Tcl/Tk event loop is inhibited in a forked child from package
      \pkg{parallel} (as in e.g. \code{mclapply()}).

      \item \code{parallel::makeCluster()} recognizes the value
      \samp{random} for the environment variable \env{R_PARALLEL_PORT}:
      this chooses a random value for the port and reduces the chance of
      conflicts when multiple users start a cluster at the same time.
    }
  }

  \subsection{UTILITIES}{
    \itemize{
      \item The default for \env{TAR} on Windows for \command{R CMD
	build} has been changed to be \samp{internal} if no
      \command{tar} command is on the path.

      This enables most packages to be built \sQuote{out of the box}
      without \command{Rtools}: the main exceptions are those which need
      to be installed to re-build vignettes and need \command{Rtools}
      for installation (usually because they contain compiled code).
    }
  }

  \subsection{C-LEVEL FACILITIES}{
    \itemize{
      \item On a 64-bit Windows platform with enough RAM, \code{R_alloc}
      can now allocate up to just under 32GB like other 64-bit
      platforms.
    }
  }

  \subsection{DEPRECATED AND DEFUNCT}{
   \itemize{
      \item Use of \code{col2rgb(0)} is deprecated (see the help page
      for its limitations).

      \item The deprecated \code{intensities} component returned by
      \code{hist()} is no longer recognized by the \code{plot()} method
      and will be removed in \R 3.0.0.

      \item \code{real()}, \code{as.real()} and \code{is.real()} are now
      formally deprecated and give a warning.

      \item This is formal notice that the non-API EISPACK entry points
      in \R will be removed shortly.
    }
  }

  \subsection{INSTALLATION}{
   \itemize{
      \item The \command{configure} tests for Objective C and Objective
      C++ now work on Mac OS 10.8 with Xcode 4.5.2 (\PR{15107}).

      \item The cairo-based versions of \code{X11()} now work with
      current versions of cairographics (e.g. 1.12.10).  (\PR{15168})

      A workaround for earlier versions of \R is to use
      \code{X11.options(type = "nbcairo")}.

      \item Configuration and \command{R CMD javareconf} now come up
      with a smaller set of library paths for Java on Oracle-format JDK
      (including OpenJDK).  This helps avoid conflicts between libraries
      (such as \code{libjpeg}) supplied in the JDK and system
      libraries.  This can always be overridden if needed: see the
      \sQuote{R Installation and Administration} manual.
    }
  }

  \subsection{BUG FIXES}{
    \itemize{
      \item \code{beta(a, b)} could overflow to infinity in its
      calculations when one of \code{a} and \code{b} was less than one.
      (\PR{15075})

      \item \code{lbeta(a, b)} no longer gives \code{NaN} if \code{a} or
      \code{b} is very small (in the denormalized range).

      \item \code{bquote()} is now able to substitute default arguments in
      single-argument functions.  (\PR{15077})

      \item \code{browseEnv(html = FALSE)} would segfault if called from
      \R (not \command{R.app}) on a CRAN-style Mac OS X build of \R.

      \item \samp{[[<-} for lists (generic vectors) needed to increment
      \code{NAMED} count when RHS is used more than once. (\PR{15098})

      \item On Windows, warnings about opening a file or pipe with a
      non-ASCII description were sometimes output in UTF-8 rather than
      in the current locale's character set.

      \item The \code{call()} function did not duplicate its arguments.
      (\PR{15115})

      \item \code{TukeyHSD()} could give \code{NA} results with some
      \samp{na.action} methods such as \code{na.exclude()}.
      (Hinted at on R-help by John Fox.)

      \item The deprecated \code{svd(X, LINPACK = TRUE)} could alter
      \code{X} in \R 2.15.[12].  (Reported by Bill Dunlap.)

      \item Under Windows, \code{file.link()} and \code{file.symlink()}
      used the link name twice, so would always fail. (Reported
      by Rui Barradas/Oliver Soong).

      \item \code{summaryRprof(memory = "both")} mixed up the units of
      Vcells and Ncells: it now works in bytes.  (\PR{15138})

      \item \code{tools::Rd2HTML()} would sometimes delete text. (\PR{15134})

      \item \code{plot()} failed for \code{"table"} objects containing
      just one entry.  (\PR{15118})

      \item \code{embedFonts()} needed to quote some filepaths.
      (\PR{15149})

      \item \code{parallel::mccollect()} handled \code{NULL} returns
      incorrectly (removing the element rather than setting it to
      \code{NULL}).

      \item The full reference index (\file{fullrefman.pdf}) was missing
      packages \pkg{compiler} and \pkg{parallel}.

      \item The report for
      \code{optim(method = "L-BFGS-B", control = list(trace = 1))}
      reported the last completed and not the current iteration, unlike
      other methods and trace levels.  (\PR{15103})

      \item \code{qt(1e-12, 1.2)} no longer gives \code{NaN}.

      \item \code{dt(1e160, 1.2, log=TRUE)} no longer gives \code{-Inf}.

      \item On Windows the \code{untar()} function now quotes the
      directory name when using an external \command{tar} utility, so
      \code{R CMD check} will handle pathnames containing spaces.

      \item The version for Windows 8 and Windows Server 2012 is now
      displayed by \code{win.version()}.  (Reported by Gabor
      Grothendieck.)

      \item The custom Windows installer target \code{myR} in the
      installer \file{Makefile} did not work in 2.15.2.  (Reported
      by Erich Neuwirth.)

      \item \code{aperm(matrix(1:6, 2, dimnames=list(A={}, B={})), "A")}
      no longer segfaults.

      \item Expressions involving user defined operators were not
      always deparsed faithfully. (\PR{15179})

      \item The \code{enc2utf8()} function converted \code{NA_character_}
      to \code{"NA"} in non-UTF-8 locales. (\PR{15201})

      \item The \code{exclude} argument to \code{xtabs()} was ignored
      for \code{"factor"} arguments.

      \item On Windows, work around an event-timing problem when
      the RGui console was closed from the \sQuote{X} control and the closure
      cancelled. (This would on some 64-bit systems crash \R, typically
      those with a slow GPU relative to the CPU.)

      \item On unix \code{Rscript} will pass the \code{r_arch} setting
      it was compiled with on to the R process so that the architecture
      of \code{Rscript} and that of \R will match unless overridden.

      \item On Windows, \code{basename()}, \code{dirname()} and
      \code{file.choose()} have more support for long non-ASCII file
      names with 260 or more bytes when expressed in UTF-8.
    }
  }
}

\section{\Rlogo CHANGES IN R VERSION 2.15.2}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item The \code{X11()} window gains an icon: the
      latter may be especially useful on Ubuntu's \sQuote{Unity}
      interface.

      The \code{WM_CLASS} should be set in circumstances where the
      Window Manager failed to make use of X11 resource settings.

      (Contributed by Philip Johnson.)

      \item The \code{"Date"} and \code{"POSIXt"} methods for
      \code{cut()} will accept an unsorted \code{breaks} argument (as
      the default method does, although this was undocumented).
      (Wish of \PR{14961}.)

      \item Reference class methods (in the \pkg{methods} package) that
      use other methods in an indirect way (e.g., by \code{sapply()})
      must tell the code analysis to include that method.  They can now
      do so by invoking \code{$usingMethods()}. %$

      \item More Polish translations are available: for the
      \command{RGui} menus and for several recommended packages.

      \item Multistratum MANOVA works. In fact, it seems to have done so
      for years in spite of the help page claiming it did not.

      \item \code{qqline()} has new optional arguments
      \code{distribution}, \code{probs} and \code{qtype}, following the
      example of \CRANpkg{lattice}'s \code{panel.qqmathline()}.

      \item The handling of single quotes in the \code{en@quot}
      pseudo-language has been slightly improved.  Double quotes are no
      longer converted.

      \item New functions \code{checkPoFiles()} and \code{checkPoFile()}
      have been added to the \pkg{tools} package to check for
      consistency of format strings in translation files.

      \item \code{model.matrix(~1, ...)} now also contains the same
      rownames that less trivial formulae produce.  (Wish of \PR{14992},
      changes the output of several packages.)

      \item Misuse of \code{rep()} on undocumented types of objects
      (e.g. calls) is now reported as an error.

      \item The included LAPACK has been updated to 3.4.1, with some
      patches from the current SVN sources.  (\emph{Inter alia}, this
      resolves \PR{14692}.)

      \item \code{file.copy(recursive = TRUE)} has some additional
      checks on user error leading to attempted infinite recursion (and on
      some platforms to crashing \R).

      \item PCRE has been updated to version 8.31, a bug-fix release.

      \item The included version of \code{liblzma} has been updated to
      version 5.0.4, a minor bug-fix release.

      \item New function \code{.bincode()}, a \sQuote{bare-bones}
      version of \code{cut.default(labels = FALSE)} for use in packages
      with \code{image()} methods.

      \item The HTML manuals now use directional single quotes.

      \item \code{maintainer()} now converts embedded new lines to
      spaces.  It no longer gives a non-obvious error for non-installed
      packages.

      \item The \code{X11()} device has some protection against
      being used with forked processes \emph{via} package \pkg{parallel}.

      \item Setting the environment variable \env{R_OSX_VALGRIND} (to
      any value) allows \R to be run under \command{valgrind} on Mac OS
      10.6 and 10.7 (\command{valgrind} currently has very limited
      support for 10.8), provided \code{system()} is not used (directly
      or indirectly).  This should not be needed for \command{valgrind}
      >= 3.8.1.

      \item The \code{model.frame()} method for \code{"lm"} uses
      \code{xlevels}: this is safer if \code{data} was supplied or
      \code{model = FALSE} was used and the levels of factors used in
      the fit had been re-ordered since fitting.

      Similarly, \code{model.frame(fm, data=<data>)} copies across
      the variables used for safe prediction from the fit.

      \item Functions such as \code{parLapply()} in package
      \pkg{parallel} can make use of a default cluster if one is set.
      (Reported by Martin Morgan.)

      \item \code{chol(pivot = TRUE, LINPACK = FALSE)} is now available
      using LAPACK 3.2 subroutine \code{DPSTRF}.

      \item The functions \code{.C()}, \code{.Call()},
      \code{.External()} and \code{.Fortran()} now check that they are
      called with an unnamed first argument: the formal arguments were
      changed from \code{name=} to \code{.NAME=} in \R 2.13.0, but some
      packages were still using the old name.  This is currently a
      warning, but will be an error in future.

      \item \code{step()} no longer tries to improve a model with
      AIC of \code{-Inf} (a perfect fit).

      \item \code{spline()} and \code{splinefun()} gain a new method
      \code{"hyman"}, an implementation of Hyman's method of
      constructing monotonic interpolation splines.  (Based on
      contributions of Simon Wood and Rob Hyndman.)

      \item On Windows, the C stack size has been increased to 64MB (it
      has been 10MB since the days of 32MB RAM systems).
    }
  }

  \subsection{PERFORMANCE IMPROVEMENTS}{
    \itemize{
      \item \code{array()} is now implemented in C code (for speed) when
      \code{data} is atomic or an unclassed list (so it is known that
      \code{as.vector(data)} will have no class to be used by \code{rep()}).

      \item \code{rep()} is faster and uses less memory,
      substantially so in some common cases (e.g. if \code{times} is of
      length one or \code{length.out} is given, and \code{each = 1}).

      \item \code{findInterval()}, \code{tabulate()}, \code{cut()},
      \code{hist()} and \code{image.default()} all use \code{.Call()}
      and are more efficient.

      \item \code{duplicated()}, \code{unique()} and similar now
      support vectors of lengths above \eqn{2^{29}}{2^29} on 64-bit
      platforms.

      \item Omitting \code{PACKAGE} in \code{.C()} etc calls was
      supposed to make use of the DLL from the namespace within which
      the enclosing function was defined.  It was less successful in
      doing so than it might be, and gave no indication it had failed.

      A new search strategy is very successful and gives a warning
      when it fails.  In most cases this is because the entry point is
      not actually provided by that package (and so \code{PACKAGE}
      should be used to indicate which package is intended) but in some
      the namespace does not have a DLL specified by a
      \code{useDynLib()} directive so \code{PACKAGE} is required.
    }
  }

  \subsection{UTILITIES}{
    \itemize{
      \item \command{R CMD check} now checks if a package can be loaded
      by \code{library(pkgname, lib.loc = "somewhere")} without being on
      the library search path (unless it is already installed in
      \code{.Library}, when it always will be).

      \item \command{R CMD check --as-cran} notes \sQuote{hidden} files
      and directories (with names starting with a dot) that are not
      needed for the operation of \command{R CMD INSTALL} or \code{R CMD
	build}: such files should be excluded from the published tarball.

      \item \command{R CMD check} (if checking subdirectories) checks
      that the \R code in any demos is ASCII and can be parsed, and
      warns if not.

      \item When \code{R CMD Rd2pdf} is used with \file{inputenx.sty},
      it allows further characters (mainly for Eastern European
      languages) by including \file{ix-utf8enc.dfu} (if available).
      (Wish of \PR{14989}.)

      \item \command{R CMD build} now omits several types of hidden
      files/directories, including \file{inst/doc/.Rinstignore},
      \file{vignettes/.Rinstignore}, (\file{.Rinstignore} should be at
      top level), \file{.deps} under \file{src}, \file{.Renviron},
      \file{.Rprofile}, \file{.Rproj.user}, \file{.backups},
      \file{.cvsignore}, \file{.cproject}, \file{.directory},
      \file{.dropbox}, \file{.exrc}, \file{.gdb.history},
      \file{.gitattributes}, \file{.gitignore}, \file{.gitmodules},
      \file{.hgignore}, \file{.hgtags}, \file{.htaccess},
      \file{.latex2html-init}, \file{.project}, \file{.seed},
      \file{.settings}, \file{.tm_properties} and various leftovers.

      \item \command{R CMD check} now checks for \code{.C()},
      \code{.Call()}, \code{.External()} and \code{.Fortran()} calls in
      other packages, and gives a warning on those found from \R itself
      (which are not part of the API and change without notice: many
      will be changed for \R 3.0.0).
    }
  }

  \subsection{C-LEVEL FACILITIES}{
    \itemize{
      \item The limit for \code{R_alloc} on 64-bit platforms has been
      raised to just under 32GB (from just under 16GB).

      \item The misuse of \code{.C("name", ..., PACKAGE = foo)} where
      \code{foo} is an arbitrary \R object is now an error.

      The misuse \code{.C("name",..., PACKAGE = "")} is now warned about
      in \command{R CMD check}, and will be an error in future.
    }
  }

  \subsection{DEPRECATED AND DEFUNCT}{
   \itemize{
      \item Use of \code{array()} with a 0-length \code{dim} argument is
      deprecated with a warning (and was contrary to the documentation).

      \item Use of \code{tapply()} with a 0-length \code{INDEX} list is
      deprecated with a warning.

      \item \samp{Translation} packages are deprecated.

      \item Calling \code{rep()} or \code{rep.int()} on a pairlist is
      deprecated and will give a warning.  In any case, \code{rep()}
      converted a pairlist to a list so you may as well do that
      explicitly.

      \item Entry point \code{rcont2} is no longer part of the API, and
      will move to package \pkg{stats} in \R 3.0.0.

      \item The \sQuote{internal} graphics device invoked by
      \code{.Call("R_GD_nullDevice", package = "grDevices")} is about to
      be removed: use \code{pdf(file = NULL)} instead.

      \item \code{eigen(EISPACK = TRUE)},
      \code{chol(pivot = FALSE, LINPACK = TRUE)},
      \code{chol2inv(LINPACK = TRUE)}, \code{solve(LINPACK = TRUE)} and
      \code{svd(LINPACK = TRUE)} are deprecated and give a warning.

      They were provided for compatibility with \R 1.7.0 (Mar 2003)!

      \item The \sQuote{internal function} \code{kappa.tri()} has been
      renamed to \code{.kappa_tri()} so it is not inadvertently called
      as a method for class \code{"tri"}.

      \item Functions \code{sessionData()} and \code{browseAll()} in
      package \pkg{methods} are on a help page describing them as
      \sQuote{deprecated} and are now formally deprecated.
    }
  }

  \subsection{PACKAGE INSTALLATION}{
    \itemize{
      \item For a Windows or Mac OS X binary package install,
      \code{install.packages()} will check if a source package is
      available on the same repositories, and report if it is a later
      version or there is a source package but no binary package
      available.

      This check can be suppressed: see the help page.

      \item \code{install.packages(type = "both")} has been enhanced.
      In interactive use it will ask whether to choose the source
      version of a package if the binary version is older and contains
      compiled code, and also asks if source packages with no binary
      version should be installed).
    }
  }

  \subsection{INSTALLATION}{
     \itemize{
      \item There is a new \command{configure} option
      \option{--with-libtiff} (mainly in case the system installation
      needs to be avoided).

      \item LAPACK 3.4.1 does use some Fortran 90 features, so
      \command{g77} no longer suffices.

      \item If an external LAPACK is used, it must be version 3.2 or later.
    }
 }

  \subsection{BUG FIXES}{
    \itemize{
      \item On Windows, starting \command{Rterm} via \command{R.exe}
      caused Ctrl-C to misbehave.  (\PR{14948})

      \item The \code{tools::latexToUtf8()} function missed conversions that
      were contained within braces.

      \item Long timezone specifications (such as a file name preceded
      by \code{:}) could crash \code{as.POSIXlt}.  (\PR{14945})

      \item \code{R CMD build --resave-data} could fail if there was no
      \file{data} directory but there was an \file{R/sysdata.rda} file.
      (\PR{14947})

      \item \code{is.na()} misbehaved on a 0-column data frame.
      (\PR{14959})

      \item \code{anova.lmlist()} failed if \code{test} was
      supplied.  (\PR{14960})

      It was unable to compute Cp tests for object of class \code{"lm"}
      (it assumed class \code{"glm"}).

      \item The \code{formula} method for \code{sunflowerplot()} now
      allows \code{xlab} and \code{ylab} to be set.  (Reported by Gerrit
      Eichner.)

      \item The \code{"POSIXt"} and \code{"Date"} methods for \code{hist()}
      could fail on Windows where adjustments to the right-hand boundary
      crossed a DST transition time.

      \item On Windows, the code in \code{as.POSIXct()} to handle
      incorrectly specified \code{isdst} fields might have resulted in
      \code{NA} being returned.

      \item \code{aov()} and \code{manova()} gave spurious warning about
      singular error model in the multiresponse case.

      \item In \code{ns()} and \code{bs()}, specifying \code{knots = NULL}
      is now equivalent to omitting it, also when \code{df} is
      specified.  (\PR{14970})

      \item \code{sprintf()} did not accept numbered arguments ending
      in zero.  (\PR{14975})

      \item \code{rWishart()} could overflow the C stack and maybe crash
      the \R process for dimensions of several hundreds or more.
      (Reported by Michael Braun on R-sig-mac.)

      \item Base package vignettes (e.g. \code{vignette("Sweave")}) were
      not fully installed in builds of \R from the tarball.

      \item \code{lchoose()} and \code{choose()} could overflow the C
      stack and crash \R.

      \item When given a 0-byte file and asked to keep source
      references, \code{parse()} read input from \code{stdin()}
      instead.

      \item \code{pdf(compress = TRUE)} did not delete temporary files
      it created until the end of the \R session.  (\PR{14991})

      \item \code{logLik()} did not detect the error of applying it
      to a multiple-response linear model.  (\PR{15000})

      \item \code{file.copy(recursive = TRUE)} did not always report
      \code{FALSE} for a failure two or more directories deep.

      \item \code{qgeom()} could return \code{-1} for extremely small
      \code{q}. (\PR{14967})

      \item \code{smooth.spline()} used \code{DUP = FALSE} which allowed
      its compiled C code to change the function: this was masked by the
      default byte-compilation. (\PR{14965})

      \item In Windows, the GUI preferences for foreground color were
      not always respected.  (Reported by Benjamin Wells.)

      \item On OS X, the Quartz versions of the bitmap devices did not
      respect \code{antialias = "none"}.  (\PR{15006})

      \item \code{unique()} and similar would infinite-loop if called on
      a vector of length > \eqn{2^{29}}{2^29} (but reported that the
      vector was too long for \eqn{2^{30}}{2^30} or more).

      \item \code{parallel::stopCluster()} now works with MPI
      clusters without \CRANpkg{snow} being on the search path.

      \item \code{terms.formula()} could exhaust the stack, and the
      stack check did not always catch this before the segfault.
      (\PR{15013})

      \item \code{sort.list(method = "radix")} could give incorrect
      results on certain compilers (seen with \command{clang} on Mac OS
      10.7 and \command{Xcode 4.4.1}).

      \item \code{backsolve(T, b)} gave incorrect results when
      \code{nrows(b) > ncols(T)} and \code{b} had more than one column.

      It could segfault or give nonsense if \code{k} was specified as
      more than \code{ncols(T)}.

      \item \code{smooth.spline()} did not check that a specified
      numeric \code{spar} was of length 1, and gave corrupt results if
      it was of length 0.

      \item Protection added to \code{do_system}. (\PR{15025})

      \item Printing of vectors with names > 1000 characters now works
      correctly rather than truncating. (\PR{15028})

      \item \code{qr()} for a complex matrix did not pivot the column names.

      \item \code{--with-blas='-framework vecLib'} now also works on OS X 10.8.

      \item \command{R CMD check} no longer fails with an error if
      a \file{DESCRIPTION} file incorrectly contains a blank line.
      (Reported by Bill Dunlap.)

      \item \code{install.packages(type = "both")} could call
      \code{chooseCRANmirror()} twice.

      \item \code{lm.wfit()} could segfault in \R 2.15.1 if all the
      weights were zero. (\PR{15044})

      \item A malformed package name could cause \command{R CMD INSTALL}
      to write outside the target library.

      \item Some of the quality control functions
      (e.g. \code{tools::checkFF()}) were wrongly identifying the source
      of S4 methods in a package and so not checking them.

      \item The default type of display by \code{browseEnv()} when using
      \command{R.app} on Mac OS X has been incorrect for a long time.

      \item The implementation of \code{importMethodsFrom} in a
      \code{NAMESPACE} file could be confused and fail to find generics
      when importing from multiple packages (reported and fixed by
      Michael Lawrence).

      \item The detection of the C stack direction is better protected
      against compiler optimization. (\PR{15011})

      \item Long custom line types would sometimes segfault on the
      cairographics-based devices. (\PR{15055})

      \item \code{tools::checkPoFile()} unprotected too early in its C
      code and so segfaulted from time to time.

      \item The Fortran code underlying \code{nlminb()} could
      infinite-loop if any of the input functions returned \code{NA} or
      \code{NaN}.  This is now an error for the gradient or Hessian, and
      a warning for the function (with the value replaced by
      \code{Inf}).  (In part, \PR{15052}.)

      \item The code for creating \code{coerce()} methods could
      generate false notes about ambiguous selection; the notes have
      been suppressed for this function.

      \item \code{arima.sim()} could give too long an output in some
      corner cases (in part, \PR{15068}).

      \item \code{anova.glm()} with \code{test = "Rao"} didn't work when
      models included an offset. (Reported by Søren Feodor Nielsen.)

      \item \code{as.data.frame.matrix()} could return invalid data
      frame with no \code{row.names} attribute for 0-row matrix. (Reported
      by Hervé Pagès.)

      \item Compilation with the \code{vecLib} or \code{Accelerate}
      frameworks on OS X without using that also for LAPACK is more
      likely to be successful.
    }
  }
}

\section{\Rlogo CHANGES IN R VERSION 2.15.1}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item \code{source()} now uses \code{withVisible()} rather than
      \code{.Internal(eval.with.vis)}.  This sometimes alters tracebacks
      slightly.

      \item \code{install.packages("pkg_version.tgz")} on Mac OS X now
      has sanity checks that this is actually a binary package (as
      people have tried it with incorrectly named source packages).

      \item \code{splineDesign()} and \code{spline.des()} in package
      \pkg{splines} have a new option \code{sparse} which can be used
      for efficient construction of a sparse B-spline design matrix
      (\emph{via} \CRANpkg{Matrix}).

      \item \code{norm()} now allows \code{type = "2"} (the
      \sQuote{spectral} or 2-norm) as well, mainly for didactical
      completeness.

      \item \code{pmin()} and \code{pmax())} now also work when one of
      the inputs is of length zero and others are not, returning a
      zero-length vector, analogously to, say, \code{+}.

      \item \code{colorRamp()} (and hence \code{colorRampPalette()}) now
      also works for the boundary case of just one color when the ramp is
      flat.

      \item \code{.C()} gains some protection against the misuse of
      character vector arguments.  (An all too common error is to pass
      \code{character(N)}, which initializes the elements to \code{""},
      and then attempt to edit the strings in-place, sometimes forgetting
      to terminate them.)

      \item Calls to the new function \code{globalVariables()} in
      package \pkg{utils} declare that functions and other objects in a
      package should be treated as globally defined, so that \code{CMD
      check} will not note them.

      \item \code{print(packageDescription(*))} trims the \code{Collate}
      field by default.

      \item The included copy of \code{zlib} has been updated to version
      1.2.7.

      \item A new option \code{"show.error.locations"} has been added.
      When set to \code{TRUE}, error messages will contain the
      location of the most recent call containing source reference
      information. (Other values are supported as well; see
      \code{?options}.)

      \item The NA warning messages from e.g. \code{pchisq()} now report
      the call to the closure and not that of the \code{.Internal}.

      \item Added Polish translations by Łukasz Daniel.
    }
  }

  \subsection{PERFORMANCE IMPROVEMENTS}{
    \itemize{
      \item In package \pkg{parallel}, \code{makeForkCluster()} and the
      multicore-based functions use native byte-order for serialization
      (deferred from 2.15.0).

      \item \code{lm.fit()}, \code{lm.wfit()}, \code{glm.fit()} and
      \code{lsfit()} do less copying of objects, mainly by using
      \code{.Call()} rather than \code{.Fortran()}.

      \item \code{.C()} and \code{.Fortran()} do less copying: arguments
      which are raw, logical, integer, real or complex vectors and are
      unnamed are not copied before the call, and (named or not) are not
      copied after the call.  Lists are no longer copied (they are
      supposed to be used read-only in the C code).

      \item \code{tabulate()} makes use of \code{.C(DUP = FALSE)} and
      hence does not copy \code{bin}.  (Suggested by Tim Hesterberg.)
      It also avoids making a copy of a factor argument \code{bin}.

      \item Other functions (often or always) doing less copying include
      \code{cut()}, \code{dist()}, the complex case of \code{eigen()},
      \code{hclust()}, \code{image()}, \code{kmeans()}, \code{loess()},
      \code{stl()} and \code{svd(LINPACK = TRUE)}.

      \item There is less copying when using primitive replacement
      functions such as \code{names()}, \code{attr()} and
      \code{attributes()}.
    }
  }

  \subsection{DEPRECATED AND DEFUNCT}{
   \itemize{
      \item The converters for use with \code{.C()} (see
      \code{?getCConverterDescriptions}) are deprecated: use the
      \code{.Call()} interface instead.  There are no known examples
      (they were never fully documented).
    }
  }

  \subsection{UTILITIES}{
    \itemize{
      \item For \command{R CMD check}, a few people have reported
      problems with junctions on Windows (although they were tested on
      Windows 7, XP and Server 2008 machines and it is unknown under
      what circumstances the problems occur).  Setting the environment
      variable \env{R_WIN_NO_JUNCTIONS} to a non-empty value (e.g. in
      \file{~/.R/check.Renviron}) will force copies to be used instead.
    }
  }

  \subsection{INSTALLATION}{
    \itemize{
      \item \command{R CMD INSTALL} with \env{_R_CHECK_INSTALL_DEPENDS_}
        set to a true value (as done by \command{R CMD check --as-cran})
        now restricts the packages available when lazy-loading as well
	as when test-loading (since packages such as \CRANpkg{ETLUtils}
	and \CRANpkg{agsemisc} had top-level calls to \code{library()}
	for undeclared packages).

	This check is now also available on Windows.
    }
  }

  \subsection{C-LEVEL FACILITIES}{
    \itemize{
      \item C entry points \code{mkChar} and \code{mkCharCE} now check
      that the length of the string they are passed does not exceed
      \eqn{2^{31}-1}{2^31-1} bytes: they used to overflow with
      unpredictable consequences.

      \item C entry points \code{R_GetCurrentSrcref} and
      \code{R_GetSrcFilename} have been added to the API to allow
      debuggers access to the source references on the stack.
    }
  }

  \subsection{WINDOWS-SPECIFIC CHANGES}{
    \itemize{
      \item Windows-specific changes will now be announced in this file
      (\file{NEWS}).  Changes up and including \R 2.15.0 remain in the
      \file{CHANGES} file.

      \item There are two new environment variables which
      control the defaults for command-line options.

      If \env{R_WIN_INTERNET2} is set to a non-empty value, it is as if
      \option{--internet2} was used.

      If \env{R_MAX_MEM_SIZE} is set, it gives the default memory limit
      if \option{--max-mem-size} is not specified: invalid values being
      ignored.
    }
  }

  \subsection{BUG FIXES}{
    \itemize{
      \item \code{lsfit()} lost the names from the residuals.

      \item More cases in which \code{merge()} could create a data frame
      with duplicate column names now give warnings.  Cases where
      names specified in \code{by} match multiple columns are errors.

      \item Nonsense uses such as \code{seq(1:50, by = 5)} (from package
      \CRANpkg{plotrix}) and \code{seq.int(1:50, by = 5)} are now errors.

      \item The residuals in the 5-number summary printed by
      \code{summary()} on an \code{"lm"} object are now explicitly
      labelled as weighted residuals when non-constant weights are
      present.  (Wish of \PR{14840}.)

      \item \code{tracemem()} reported that all objects were copied
      by \code{.C()} or \code{.Fortran()} whereas only some object types
      were ever copied.

      It also reported and marked as copies \emph{some} transformations
      such as \code{rexp(n, x)}: it no longer does so.

      \item The \code{plot()} method for class \code{"stepfun"} only
      used the optional \code{xval} argument to compute \code{xlim} and
      not the points at which to plot (as documented).  (\PR{14864})

      \item Names containing characters which need to be escaped were
      not deparsed properly.  (\PR{14846})

      \item Trying to update (recommended) packages in
      \file{R_HOME/library} without write access is now dealt with more
      gracefully.  Further, such package updates may be skipped (with a
      warning), when a newer installed version is already going to be
      used from \code{.libPaths()}.  (\PR{14866})

      \item \code{hclust()} is now fast again (as up to end of 2003),
      with a different fix for the "median"/"centroid" problem.  (\PR{4195}).

      \item \code{get_all_vars()} failed when the data came entirely
      from vectors in the global environment. (\PR{14847})

      \item \command{R CMD check} with \env{_R_CHECK_NO_RECOMMENDED_}
      set to a true value (as done by the \command{--as-cran} option)
      could issue false errors if there was an indirect dependency
      on a recommended package.

      \item \code{formatC()} uses the C entry point \code{str_signif}
      which could write beyond the length allocated for the output string.

      \item Missing default argument added to implicit S4 generic for
      \code{backsolve()}. (\PR{14883})

      \item Some bugs have been fixed in handling load actions that
      could fail to export assigned items or generate spurious warnings
      in \code{CMD check} on loading.

      \item For \code{tiff(type = "windows")}, the numbering of per-page
      files except the last was off by one.

      \item On Windows, loading package \pkg{stats} (which is done for
      a default session) would switch line endings on \file{stdout}
      and \file{stderr} from CRLF to LF.  This affected
      \command{Rterm} and \command{R CMD BATCH}.

      \item On Windows, the compatibility function \code{x11()} had
      not kept up with changes to \code{windows()}, and issued
      warnings about bad parameters. (\PR{14880})

      \item On Windows, the \code{Sys.glob()} function did not handle
      UNC paths as it was designed to try to do. (\PR{14884})

      \item In package \pkg{parallel}, \code{clusterApply()} and similar
      failed to handle a (pretty pointless) length-1 argument. (\PR{14898})

      \item Quartz Cocoa display reacted asynchronously to
      \code{dev.flush()} which means that the redraw could be performed
      after the plot has been already modified by subsequent code. The
      redraw is now done synchronously in \code{dev.flush()} to allow
      animations without sleep cycles.

      \item Source locations reported in \code{traceback()} were
      incorrect when byte-compiled code was on the stack.

      \item \code{plogis(x, lower = FALSE, log.p = TRUE)} no longer
      underflows early for large x (e.g. 800).

      \item \code{?Arithmetic}'s \dQuote{\code{1 ^ y} and \code{y ^ 0}
	are \code{1}, \emph{always}} now also applies for \code{integer}
      vectors \code{y}.

      \item X11-based pixmap devices like \code{png(type = "Xlib")} were
      trying to set the cursor style, which triggered some warnings and
      hangs.

      \item Code executed by the built-in HTTP server no longer allows
      other HTTP clients to re-enter \R until the current worker
      evaluation finishes, to prevent cascades.

      \item The \code{plot()} and \code{Axis()} methods for class
      \code{"table"} now respect graphical parameters such as
      \code{cex.axis}.  (Reported by Martin Becker.)

      \item Under some circumstances \code{package.skeleton()} would
      give out progress reports that could not be translated and so were
      displayed by question marks.  Now they are always in English.
      (This was seen for CJK locales on Windows, but may have occurred
      elsewhere.)

      \item The evaluator now keeps track of source references outside
      of functions, e.g. when \code{source()} executes a script.

      \item The replacement method for \code{window()} now works
      correctly for multiple time series of class \code{"mts"}.
      (\PR{14925})

      \item \code{is.unsorted()} gave incorrect results on non-atomic
      objects such as data frames.  (Reported by Matthew Dowle.)

      \item The value returned by \code{tools::psnice()} for invalid
      \code{pid} values was not always \code{NA} as documented.

      \item Closing an \code{X11()} window while \code{locator()} was
      active could abort the \R process.

      \item \code{getMethod(f, sig)} produced an incorrect error message
      in some cases when \code{f} was not a string).

      \item Using a string as a \dQuote{call} in an error condition
      with \code{options(showErrorCalls=TRUE)} could cause a segfault.
      (\PR{14931})

      \item The string \code{"infinity"} allowed by C99 was not accepted
      as a numerical string value by e.g. \code{scan()} and
      \code{as.numeric()}.  (\PR{14933})

      \item In \code{legend()}, setting some entries of \code{lwd} to
      \code{NA} was inconsistent (depending on the graphics device) in
      whether it would suppress those lines; now it consistently does
      so.  (\PR{14926})

      \item \code{by()} failed for a zero-row data frame.  (Reported by
      Weiqiang Qian.)

      \item The Yates' correction in \code{chisq.test()} could be bigger
      than the terms it corrected, previously leading to an infinite
      test statistic in some corner cases which are now reported as
      \code{NaN}.

      \item \code{xgettext()} and related functions sometimes returned
      items that were not strings for translation. (\PR{14935})

      \item \code{plot(<lm>, which=5)} now correctly labels the factor
      level combinations for the special case where all
      \eqn{h_ii}{h[i,i]} are the same. (\PR{14837})

      \item \code{Sys.glob()} caused a segfault if the first element of
      \code{path} was \code{NA_character}. (\PR{14990})
    }
  }
}

\section{\Rlogo CHANGES IN R VERSION 2.15.0}{
  \subsection{SIGNIFICANT USER-VISIBLE CHANGES}{
    \itemize{
      \item The behaviour of \code{unlink(recursive = TRUE)} for a
      symbolic link to a directory has changed: it now removes the link
      rather than the directory contents (just as \command{rm -r} does).

      On Windows it no longer follows reparse points (including
      junctions and symbolic links).
    }
  }

  \subsection{NEW FEATURES}{
    \itemize{
      \item Environment variable \env{RD2DVI_INPUTENC} has been renamed
      to \env{RD2PDF_INPUTENC}.

      \item \code{.Deprecated()} becomes a bit more flexible, getting an
      \code{old} argument.

      \item Even data-only packages without \R code need a namespace and
      so may need to be installed under \R 2.14.0 or later.

      \item \code{assignInNamespace()} has further restrictions on use
      apart from at top-level, as its help page has warned.
      Expect it to be disabled from programmatic use in the future.

      \item \code{system()} and \code{system2()} when capturing output
      report a non-zero status in the new \code{"status"} attribute.

      \item \code{kronecker()} now has an S4 generic in package
      \pkg{methods} on which packages can set methods.  It will be
      invoked by \code{X \%x\% Y} if either \code{X} or \code{Y} is an S4
      object.

      \item \code{pdf()} accepts forms like \code{file = "|lpr"} in the
      same way as \code{postscript()}.

      \item \code{pdf()} accepts \code{file = NULL}.
      This means that the device does NOT create a PDF file
      (but it can still be queried, e.g., for font metric info).

      \item \code{format()} (and hence \code{print()}) on
      \code{"bibentry"} objects now uses \code{options("width")} to
      set the output width.

      \item \code{legend()} gains a \code{text.font} argument. (Suggested
      by Tim Paine, \PR{14719}.)

      \item \code{nchar()} and \code{nzchar()} no longer accept factors
      (as integer vectors).  (Wish of \PR{6899}.)

      \item \code{summary()} behaves slightly differently (or more
      precisely, its \code{print()} method does).  For numeric inputs, the
      number of NAs is printed as an integer and not a real.  For dates
      and datetimes, the number of NAs is included in the printed output
      (the latter being the wish of \PR{14720}).

      The \code{"data.frame"} method is more consistent with the default
      method: in particular it now applies \code{zapsmall()} to
      numeric/complex summaries.

      \item The number of items retained with \code{options(warn = 0)}
      can be set by \code{options(nwarnings=)}.

      \item There is a new function \code{assignInMyNamespace()} which
      uses the namespace of the function it is called from.

      \item \code{attach()} allows the default name for an attached file
      to be overridden.

      \item \code{bxp()}, the work horse of \code{boxplot()}, now uses a
      more sensible default \code{xlim} in the case where \code{at} is
      specified differently from \code{1:n}, see the discussion on R-devel,
      \url{https://stat.ethz.ch/pipermail/r-devel/2011-November/062586.html}.

      \item New function \code{paste0()}, an efficient version of
      \code{paste(*, sep="")}, to be used in many places for more
      concise (and slightly more efficient) code.

      \item Function \code{setClass()} in package \pkg{methods} now
      returns, invisibly, a generator function for the new class,
      slightly preferred to calling \code{new()}, as explained on the
      \code{setClass} help page.

      \item The \code{"dendrogram"} method of \code{str()} now takes
      its default for \code{last.str} from option \code{str.dendrogram.last}.

      \item New simple \code{fitted()} method for \code{"kmeans"} objects.

      \item The \code{traceback()} function can now be called with
      an integer argument, to display a current stack trace. (Wish of
      \PR{14770}.)

      \item \code{setGeneric()} calls can be simplified when creating
      a new generic function by supplying the default method as the
      \code{def} argument.  See \code{?setGeneric}.

      \item \code{serialize()} has a new option \code{xdr = FALSE} which
      will use the native byte-order for binary serializations.  In
      scenarios where only little-endian machines are involved (these
      days, close to universal) and (un)serialization takes an
      appreciable amount of time this may speed up noticeably
      transferring data between systems.

      \item The internal (un)serialization code is faster for long
      vectors, particularly with XDR on some platforms.  (Based on a
      suggested patch by Michael Spiegel.)
      % https://stat.ethz.ch/pipermail/r-devel/2011-September/062113.html

      \item For consistency, circles with zero radius are omitted by
      \code{points()} and \code{grid.circle()}.  Previously this was
      device-dependent, but they were usually invisible.

      \item \code{NROW(x)} and \code{NCOL(x)} now work whenever
      \code{dim(x)} looks appropriate, e.g., also for more generalized
      matrices.

      \item PCRE has been updated to version 8.30.

      \item The internal \code{R_Srcref} variable is now updated
      before the browser stops on entering a function.  (Suggestion of
      \PR{14818}.)

      \item There are \sQuote{bare-bones} functions \code{.colSums()},
      \code{.rowSums()}, \code{.colMeans()} and \code{.rowMeans()} for
      use in programming where ultimate speed is required.

      \item The formerly internal function
      \code{.package_dependencies()} from package \pkg{tools} for
      calculating (recursive) (reverse) dependencies on package databases
      has been renamed to \code{package_dependencies()} and is now
      exported.

      \item There is a new function \code{optimHess()} to compute the
      (approximate) Hessian for an \code{optim()} solution if
      \code{hessian = TRUE} was forgotten.

      \item \code{.filled.contour()} is a \sQuote{bare-bones} function to
      add a filled-contour rectangular plot to an already prepared plot
      region.

      \item The stepping in debugging and single-step browsing modes has
      changed slightly: now left braces at the start of the body are
      stepped over for \code{if} statements as well as for \code{for} and
      \code{while} statements.  (Wish of \PR{14814}.)

      \item \code{library()} no longer warns about a conflict with a
      function from \code{package:base} if the function has the same
      code as the base one but with a different environment.  (An
      example is \code{Matrix::det()}.)

      \item When deparsing very large language objects,
      \code{as.character()} now inserts newlines after each line of
      approximately 500 bytes, rather than truncating to the first
      line.

      \item New function \code{rWishart()} generates Wishart-distributed
      random matrices.

      \item Packages may now specify actions to be taken when the
      package is loaded (\code{setLoadActions()}).

      \item \code{options(max.print = Inf)} and similar now give an
      error (instead of warnings later).

      \item The \code{"difftime"} replacement method of \code{units}
      tries harder to preserve other attributes of the argument.  (Wish
      of \PR{14839}.)

      \item \code{poly(raw = TRUE)} no longer requires more unique
      points than the degree.  (Requested by John Fox.)
    }
  }

  \subsection{PACKAGE parallel}{
    \itemize{
      \item There is a new function \code{mcmapply()}, a parallel version
      of \code{mapply()}, and a wrapper \code{mcMap()}, a parallel
      version of \code{Map()}.

      \item A default cluster can be registered by the new function
      \code{setDefaultCluster()}: this will be used by default in
      functions such as \code{parLapply()}.

      \item \code{clusterMap()} has a new argument \code{.scheduling} to
      allow the use of load-balancing.

      \item There are new load-balancing functions \code{parLapplyLB()}
      and \code{parSapplyLB()}.

      \item \code{makePSOCKCluster()} has a new option \code{useXDR =
	FALSE} which can be used to avoid byte-shuffling for
      serialization when all the nodes are known to be little-endian (or
      all big-endian).
    }
  }

  \subsection{PACKAGE INSTALLATION}{
    \itemize{
      \item Non-ASCII vignettes without a declared encoding are no
      longer accepted.

      \item C/C++ code in packages is now compiled with \code{-NDEBUG}
      to mitigate against the C/C++ function \code{assert} being called
      in production use.  Developers can turn this off during package
      development with \code{PKG_CPPFLAGS = -UNDEBUG}.

      \item \command{R CMD INSTALL} has a new option \option{--dsym}
      which on Mac OS X (Darwin) dumps the symbols alongside the
      \file{.so} file: this is helpful when debugging with
      \command{valgrind} (and especially when installing packages into
      \file{R.framework}).  [This can also be enabled by setting the
      undocumented environment variable \env{PKG_MAKE_DSYM}, since \R
      2.12.0.]

      \item \command{R CMD INSTALL} will test loading under all installed
      sub-architectures even for packages without compiled code, unless
      the flag \option{--no-multiarch} is used.  (Pure \R packages can
      do things which are architecture-dependent: in the case which
      prompted this, looking for an icon in a Windows \R executable.)

      \item There is a new option \code{install.packages(type = "both")}
      which tries source packages if binary packages are not available,
      on those platforms where the latter is the default.

      \item The meaning of \code{install.packages(dependencies = TRUE)}
      has changed: it now means to install the essential dependencies of
      the named packages plus the \samp{Suggests}, but only the
      essential dependencies of dependencies.  To get the previous
      behaviour, specify \code{dependencies} as a character vector.

      \item \command{R CMD INSTALL --merge-multiarch} is now supported
      on OS X and other Unix-alikes using multiple sub-architectures.

      \item \command{R CMD INSTALL --libs-only} now by default does a
      test load on Unix-alikes as well as on Windows: suppress with
      \option{--no-test-load}.
    }
  }

  \subsection{UTILITIES}{
    \itemize{
       \item \command{R CMD check} now gives a warning rather than a
       note if it finds inefficiently compressed datasets.  With
       \command{bzip2} and \command{xz} compression having been
       available since \R 2.10.0, it only exceptionally makes sense to
       not use them.

       The environment variable \env{_R_CHECK_COMPACT_DATA2_} is
       no longer consulted: the check is always done if
       \env{_R_CHECK_COMPACT_DATA_} has a true value (its default).

       \item Where multiple sub-architectures are to be tested,
       \command{R CMD check} now runs the examples and tests for all the
       sub-architectures even if one fails.

       \item \command{R CMD check} can optionally report timings on
       various parts of the check: this is controlled by environment
       variable \env{_R_CHECK_TIMINGS_} documented in \sQuote{Writing R
	 Extensions}.  Timings (in the style of \command{R CMD BATCH}) are
       given at the foot of the output files from running each test and
       the \R code in each vignette.

       \item There are new options for more rigorous testing by
       \command{R CMD check} selected by environment variables -- see
       the \sQuote{Writing R Extensions} manual.

       \item \command{R CMD check} now warns (rather than notes) on
       undeclared use of other packages in examples and tests:
       increasingly people are using the metadata in the
       \file{DESCRIPTION} file to compute information about packages,
       for example reverse dependencies.

       \item The defaults for some of the options in \command{R CMD
	 check} (described in the \sQuote{R Internals} manual) have
       changed: checks for unsafe and \code{.Internal()} calls and
       for partial matching of arguments in \R function calls are now
       done by default.

       \item \command{R CMD check} has more comprehensive facilities for
       checking compiled code and so gives fewer reports on entry points
       linked into \file{.so}/\file{.dll} files from libraries
       (including C++ and Fortran runtimes).

       Checking compiled code is now done on FreeBSD (as well as the
       existing supported platforms of Linux, Mac OS X, Solaris and
       Windows).

       \item \command{R CMD build} has more options for
       \option{--compact-vignettes}: see \command{R CMD build --help}.

       \item \command{R CMD build} has a new option \option{--md5} to
       add an \file{MD5} file (as done by CRAN): this is used by \code{R
	 CMD INSTALL} to check the integrity of the distribution.

       If this option is not specified, any existing (and probably
       stale) \file{MD5} file is removed.
     }
   }

  \subsection{DEPRECATED AND DEFUNCT}{
   \itemize{
      \item \command{R CMD Rd2dvi} is now defunct: use \command{R CMD Rd2pdf}.

      \item Options such \option{--max-nsize}, \option{--max-vsize} and
      the function \code{mem.limits()} are now defunct.  (Options
      \option{--min-nsize} and \option{--min-vsize} remain available.)

      \item Use of \code{library.dynam()} without specifying all the
      first three arguments is now disallowed.

      Use of an argument \code{chname} in \code{library.dynam()}
      including the extension \file{.so} or \file{.dll} (which was never
      allowed according to the help page) is defunct.  This also applies
      to \code{library.dynam.unload()} and to \code{useDynLib}
      directives in \file{NAMESPACE} files.

      \item The internal functions \code{.readRDS()} and
      \code{.saveRDS()} are now defunct.

      \item The off-line \code{help()} types \samp{"postscript"} and
      \samp{"ps"} are defunct.

      \item \code{Sys.putenv()}, replaced and deprecated in \R 2.5.0, is
      finally removed.

      \item Some functions/objects which have been defunct for five or
      more years have been removed completely.  These include
      \code{.Alias()}, \code{La.chol()}, \code{La.chol2inv()},
      \code{La.eigen()}, \code{Machine()}, \code{Platform()},
      \code{Version}, \code{codes()}, \code{delay()},
      \code{format.char()}, \code{getenv()}, \code{httpclient()},
      \code{loadURL()}, \code{machine()}, \code{parse.dcf()},
      \code{printNoClass()}, \code{provide()}, \code{read.table.url()},
      \code{restart()}, \code{scan.url()}, \code{symbol.C()},
      \code{symbol.For()} and \code{unix()}.

      \item The \code{ENCODING} argument to \code{.C()} is deprecated.
      It was intended to smooth the transition to multi-byte character
      strings, but can be replaced by the use of \code{iconv()} in the
      rare cases where it is still needed.
    }
  }

  \subsection{INSTALLATION}{
    \itemize{
      \item Building with a positive value of
      \option{--with-valgrind-instrumentation} now also instruments logical,
      complex and raw vectors.
    }
  }

  \subsection{C-LEVEL FACILITIES}{
    \itemize{
      \item Passing \R objects other than atomic vectors, functions,
      lists and environments to \code{.C()} is now deprecated and will
      give a warning.  Most cases (especially \code{NULL}) are actually
      coding errors.  \code{NULL} will be disallowed in future.

      \code{.C()} now passes a pairlist as a \code{SEXP} to the compiled
      code.  This is as was documented, but pairlists were in reality
      handled differently as a legacy from the early days of \R.

      \item \code{call_R} and \code{call_S} are deprecated.  They still
      exist in the headers and as entry points, but are no longer
      documented and should not be used for new code.
    }
  }

  \subsection{BUG FIXES}{
    \itemize{
      \item \code{str(x, width)} now obeys its \code{width}
      argument also for function headers and other objects \code{x}
      where \code{deparse()} is applied.

      \item The convention for \code{x \%/\% 0L} for integer-mode \code{x}
      has been changed from \code{0L} to \code{NA_integer_}.
      (\PR{14754})

      \item The \code{exportMethods} directive in a \file{NAMESPACE}
      file now exports S4 generics as necessary, as the extensions
      manual said it does.  The manual has also been updated to be a
      little more informative on this point.

      It is now required that there is an S4 generic (imported or
      created in the package) when methods are to be exported.

      \item Reference methods cannot safely use non-exported entries
      in the namespace.  We now do not do so, and warn in the documentation.

      \item The namespace import code was warning when identical
      S4 generic functions were imported more than once, but should
      not (reported by Brian Ripley, then Martin Morgan).

      \item \code{merge()} is no longer allowed (in some ways) to create
      a data frame with duplicate column names (which confused
      \PR{14786}).

      \item Fixes for rendering raster images on X11 and Windows devices
      when the x-axis or y-axis scale is reversed.

      \item \code{getAnywhere()} found S3 methods as seen from the
      \pkg{utils} namespace and not from the environment from which it
      was called.

      \item \code{selectMethod(f, sig)} would not return inherited group
      methods when caching was off (as it is by default).

      \item \code{dev.copy2pdf(out.type = "cairo")} gave an error.
      (\PR{14827})

      \item Virtual classes (e.g., class unions) had a \code{NULL}
      prototype even if that was not a legal subclass.  See
      \code{?setClassUnion}.

      \item The C prototypes for \code{zdotc} and \code{zdotu} in
      \file{R_ext/BLAS.h} have been changed to the more modern style
      rather than that used by \command{f2c}.  (Patch by Berwin Turlach.)

      \item \code{isGeneric()} produced an error for primitives that
      can not have methods.

      \item \code{.C()} or \code{.Fortran()} had a lack-of-protection
      error if the registration information resulted in an argument
      being coerced to another type.

      \item \code{boxplot(x=x, at=at)} with non finite elements in \code{x}
      and non integer \code{at} could not generate a warning but failed.

      \item \code{heatmap(x, symm=TRUE, RowSideColors=*)} no longer
      draws the colors in reversed order.

      \item \code{predict(<ar>)} was incorrect in the multivariate case,
      for p >= 2.

      \item \code{print(x, max=m)} is now consistent when \code{x} is a
      \code{"Date"}; also the \dQuote{reached ... max.print ..} messages
      are now consistently using single brackets.

      \item Closed the \samp{<li>} tag in pages generated by \code{
      Rd2HTML()}. (\PR{14841})

      \item Axis tick marks could go out of range when a log scale was
      used.  (\PR{14833})

      \item Signature objects in methods were not allocated as S4
      objects (caused a problem with \code{trace()} reported by Martin Morgan).
    }
  }
}

\section{\Rlogo CHANGES IN R VERSION 2.14.2}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item The internal \code{untar()} (as used by default by
      \command{R CMD INSTALL}) now knows about some \command{pax}
      headers which \command{bsdtar} (e.g., the default \command{tar} for
      Mac OS >= 10.6) can incorrectly include in \command{tar} files,
      and will skip them with a warning.

      \item PCRE has been upgraded to version 8.21: as well as bug fixes
      and greater Perl compatibility, this adds a JIT pattern compiler,
      about which PCRE's news says
      \sQuote{large performance benefits can be had in many situations}.
      This is supported on most but not all \R platforms.

      \item Function \code{compactPDF()} in package \pkg{tools} now
      takes the default for argument \code{gs_quality} from environment
      variable \env{GS_QUALITY}: there is a new value \code{"none"}, the
      ultimate default, which prevents GhostScript being used in
      preference to \command{qpdf} just because environment variable
      \env{R_GSCMD} is set.  If \env{R_GSCMD} is unset or set to
      \code{""}, the function will try to find a suitable GhostScript
      executable.

      \item The included version of zlib has been updated to 1.2.6.

      \item For consistency with the \code{logLik()} method,
      \code{nobs()} for \code{"nls"} files now excludes observations
      with zero weight.  (Reported by Berwin Turlach.)
    }
  }

  \subsection{UTILITIES}{
    \itemize{
      \item \command{R CMD check} now reports by default on licenses not
      according to the description in \sQuote{Writing R Extensions}.

      \item \command{R CMD check} has a new option \option{--as-cran} to
      turn on most of the customizations that CRAN uses for its incoming
      checks.
    }
  }

  \subsection{PACKAGE INSTALLATION}{
    \itemize{
      \item \command{R CMD INSTALL} will now no longer install certain
      file types from \file{inst/doc}: these are almost certainly
      mistakes and for some packages are wasting a lot of space.  These
      are \file{Makefile}, files generated by running LaTeX, and unless
      the package uses a \file{vignettes} directory, PostScript and
      image bitmap files.

      Note that only PDF vignettes have ever been supported: some of
      these files come from DVI/PS output from the Sweave defaults prior
      to \R 2.13.0.
    }
  }

  \subsection{BUG FIXES}{
    \itemize{
      \item R configured with \option{--disable-openmp} would mistakenly
      set \code{HAVE_OPENMP} (internal) and \code{SUPPORT_OPENMP} (in
      \file{Rconfig.h}) even though no OpenMP flags were populated.

      \item The \code{getS3method()} implementation had an old
      computation to find an S4 default method.

      \item \code{readLines()} could overflow a buffer if the last line
      of the file was not terminated.  (\PR{14766})

      \item \command{R CMD check} could miss undocumented S4 objects in
      packages which used S4 classes but did not \samp{Depends: methods}
      in their \file{DESCRIPTION} file.

      \item The HTML Help Search page had malformed links.
      (\PR{14769})

      \item A couple of instances of lack of protection of \code{SEXP}s
      have been squashed. (\PR{14772}, \PR{14773})

      \item \code{image(x, useRaster=TRUE)} misbehaved on single-column
      \code{x}. (\PR{14774})

      \item Negative values for \code{options("max.print")} or the \code{max}
      argument to \code{print.default()} caused crashes.  Now the former
      are ignored and the latter trigger an error.  (\PR{14779})

      \item The text of a function body containing more than 4096
      bytes was not properly saved by the parser when entered
      at the console.

      \item Forgetting the \code{#endif} tag in an Rd file could cause
      the parser to go into a loop.  (Reported by Hans-Jorg Bibiko.)

      \item \code{str(*, ....., strict.width="cut")} now also obeys
      \code{list.len = n}.  (Reported by Sören Vogel.)

      \item Printing of arrays did not have enough protection (C level),
      e.g., in the context of \code{capture.output()}.  (Reported by
      Hervé Pagès and Martin Morgan.)

      \item \code{pdf(file = NULL)} would produce a spurious file named
      \file{NA}.  (\PR{14808})

      \item \code{list2env()} did not check the type of its \code{envir}
      argument. (\PR{14807})

      \item \code{svg()} could segfault if called with a non-existent
      file path.  (\PR{14790})

      \item \command{make install} can install to a path containing
      \samp{+} characters. (\PR{14798})

      \item The \code{edit()} function did not respect the
      \code{options("keep.source")} setting. (Reported by Cleridy
      Lennert.)

      \item \code{predict.lm(*, type="terms", terms=*, se.fit=TRUE)} did
      not work.  (\PR{14817})

      \item There is a partial workaround for errors in the TRE
      regular-expressions engine with named classes and repeat counts of
      at least 2 in a MBCS locale (\PR{14408}): these are avoided when
      TRE is in 8-bit mode (e.g. for \code{useBytes = TRUE} and when all
      the data are ASCII).

      \item The C function \code{R_ReplDLLdo1()} did not call top-level
      handlers.

      \item The Quartz device was unable to detect window sessions on
      Mac OS X 10.7 (Lion) and higher and thus it was not used as the
      default device on the console.  Since Lion any application can use
      window sessions, so Quartz will now be the default device if the
      user's window session is active and \R is not run via
      \command{ssh} which is at least close to the behavior in prior OS
      X versions.

      \item \code{mclapply()} would fail in code assembling the translated
      error message if some (but not all) cores encountered an error.

      \item \code{format.POSIXlt(x)} raised an arithmetic exception when
      \code{x} was an invalid object of class \code{"POSIXlt"} and parts
      were empty.

      \item \code{installed.packages()} has some more protection against
      package installs going on in parallel.

      \item \code{.Primitive()} could be mis-used to call
      \code{.Internal()} entry points.
    }
  }
}

\section{\Rlogo CHANGES IN R VERSION 2.14.1}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item \code{parallel::detectCores()} is now able to find the
      number of physical cores (rather than CPUs) on Sparc Solaris.

      It can also do so on most versions of Windows; however the default
      remains \code{detectCores(logical = TRUE)} on that platform.

      \item Reference classes now keep a record of which fields are
      locked.  \code{$lock()} with no arguments returns the names % $
      of the locked fields.

      \item \code{HoltWinters()} reports a warning rather than an error
      for some optimization failures (where the answer might be a
      reasonable one).

      \item \code{tools::dependsOnPkg()} now accepts the shorthand
      \code{dependencies = "all"}.

      \item \code{parallel::clusterExport()} now allows specification of
      an environment from which to export.

      \item The \code{quartz()} device now does tilde expansion on its
      \code{file} argument.

      \item \code{tempfile()} on a Unix-alike now takes the process ID
      into account.  This is needed with \CRANpkg{multicore} (and as part of
      \pkg{parallel}) because the parent and all the children share a
      session temporary directory, and they can share the C random
      number stream used to produce the unique part.  Further, two
      children can call \code{tempfile()} simultaneously.

      \item Option \code{print} in Sweave's \code{RweaveLatex()} driver
      now emulates auto-printing rather than printing (which can differ
      for an S4 object by calling \code{show()} rather than
      \code{print()}).

      \item \code{filled.contour()} now accepts infinite values:
      previously it might have generated invalid graphics files
      (e.g. containing NaN values).
    }
  }

  \subsection{INSTALLATION}{
    \itemize{
      \item On 64-bit Linux systems, \command{configure} now only sets
      \samp{LIBnn} to \code{lib64} if \file{/usr/lib64} exists.  This
      may obviate setting \samp{LIBnn} explicitly on Debian-derived
      systems.

      It is still necessary to set \samp{LIBnn = lib} (or
      \samp{lib32}) for 32-bit builds of \R on a 64-bit OS on those Linux
      distributions capable for supporting that concept.

      \item \command{configure} looks for \file{inconsolata.sty}, and if
      not found adjusts the default \env{R_RD4PDF} to not use it (with a
      warning, since it is needed for high-quality rendering of manuals).
    }
  }

  \subsection{PACKAGE INSTALLATION}{
    \itemize{
      \item \command{R CMD INSTALL} will now do a test load for all
      sub-architectures for which code was compiled (rather than just
      the primary sub-architecture).
    }
  }

  \subsection{UTILITIES}{
    \itemize{
      \item When checking examples under more than one sub-architecture,
      \command{R CMD check} now uses a separate directory
      \file{examples_arch} for each sub-architecture, and leaves the
      output in file \file{pkgname-Ex_arch.Rout}.  Some packages
      expect their examples to be run in a clean directory \dots{}.
     }
   }

   \subsection{BUG FIXES}{
    \itemize{
      \item \code{stack()} now gives an error if no vector column is
      selected, rather than returning a 1-column data frame (contrary to
      its documentation).

      \item \code{summary.mlm()} did not handle objects where the
      formula had been specified by an expression.  (Reported by
      Helios de Rosario Martinez).

      \item \code{tools::deparseLatex(dropBraces=TRUE)} could drop
      text as well as braces.

      \item \code{colormodel = "grey"} (new in \R 2.14.0))
      did not always work in \code{postscript()} and \code{pdf()}.

      \item \code{file.append()} could return \code{TRUE} for failures.
      (\PR{14727})

      \item \code{gzcon()} connections are no longer subject to garbage
      collection: it was possible for this to happen when unintended
      (e.g. when calling \code{load()}).

      \item \code{nobs()} does not count zero-weight observations for
      \code{glm()} fits, for consistency with \code{lm()}.  This affects
      the \code{BIC()} values reported for such \code{glm()} fits.
      (Spotted by Bill Dunlap.)

      \item \code{options(warn = 0)} failed to end a (C-level) context
      with more than 50 accumulated warnings.  (Spotted by Jeffrey
      Horner.)

      \item The internal \code{plot.default()} code did not do sanity
      checks on a \code{cex} argument, so invalid input could cause
      problems.  (Reported by Ben Bolker.)

      \item \code{anyDuplicated(<array>, MARGIN=0)} no longer fails.
      (Reported by Hervé Pagès.)

      \item \code{read.dcf()} removes trailing blanks: unfortunately
      on some platforms this included \code{\\xa0} (non-breaking space)
      which is the trailing byte of a UTF-8 character.  It now only
      considers ASCII space and tab to be \sQuote{blank}.

      \item There was a sign error in part of the calculations for the
      variance returned by \code{KalmanSmooth()}.  (\PR{14738})

      \item \code{pbinom(10, 1e6, 0.01, log.p = TRUE)} was \code{NaN}
      thanks to the buggy fix to \PR{14320} in \R 2.11.0.  (\PR{14739})

      \item \code{RweaveLatex()} now emulates auto-printing rather than
      printing, by calling \code{methods::show()} when auto-printing
      would.

      \item \code{duplicated()} ignored \code{fromLast} for a
      one-column data frame.  (\PR{14742})

      \item \code{source()} and related functions did not put the correct
      timestamp on the source references; \code{srcfilecopy()} has gained
      a new argument \code{timestamp} to support this fix. (\PR{14750})

      \item \code{srcfilecopy()} has gained a new argument \code{isFile}
      and now records the working directory, to allow debuggers to find
      the original source file.  (\PR{14826})

      \item LaTeX conversion of Rd files did not correctly handle
      preformatted backslashes.  (\PR{14751})

      \item HTML conversion of Rd files did not handle markup within
      tabular cells properly.  (\PR{14708})

      \item \code{source()} on an empty file with \code{keep.source =
	TRUE} tried to read from \code{stdin()}, in \R 2.14.0 only.
      (\PR{14753})

      \item The code to check Rd files in packages would abort if
      duplicate description sections were present.
    }
  }
}

\section{\Rlogo CHANGES IN R VERSION 2.14.0}{
  \subsection{SIGNIFICANT USER-VISIBLE CHANGES}{
    \itemize{
      \item All packages must have a namespace, and one is created on
      installation if not supplied in the sources.  This means that any
      package without a namespace must be re-installed under this
      version of \R (but previously-installed data-only packages without
      \R code can still be used).

      \item The \code{yLineBias} of the \code{X11()} and
      \code{windows()} families of devices has been changed from 0.1 to
      0.2: this changes slightly the vertical positioning of text in the
      margins (including axis annotations).  This is mainly for
      consistency with other devices such as \code{quartz()} and
      \code{pdf()}.  (Wish of \PR{14538}.)

      There is a new graphics parameter \code{"ylbias"} which
      allows the y-line bias of the graphics device to be tweaked,
      including to reproduce output from earlier versions of \R.

      \item Labeling of the p-values in various anova tables has been
      rationalized to be either \code{"Pr(>F)"} or \code{"Pr(>Chi)"}
      (i.e. the \code{"Pr(F)"}, \code{"Pr(Chi)"} and \code{"P(>|Chi|)"}
      variants have been eliminated).  Code which extracts the p value
      \emph{via} indexing by name may need adjustment.

      \item \code{::} can now be used for datasets made available for
      lazy-loading in packages with namespaces (which makes it consistent
      with its use for data-only packages without namespaces in earlier
      versions of \R).

      \item There is a new package \pkg{parallel}.

      It incorporates (slightly revised) copies of packages
      \CRANpkg{multicore} and \CRANpkg{snow} (excluding MPI, PVM and NWS
      clusters).  Code written to use the higher-level API functions in
      those packages should work unchanged (apart from changing any
      references to their namespaces to a reference to \pkg{parallel},
      and links explicitly to \CRANpkg{multicore} or \CRANpkg{snow} on help
      pages).

      It also contains support for multiple RNG streams following
      L'Ecuyer \emph{et al} (2002), with support for both
      \code{mclapply} and \CRANpkg{snow} clusters.  This replaces functions
      like \code{clusterSetupRNG()} from \CRANpkg{snow} (which are not in
      \pkg{parallel}).

      The version released for \R 2.14.0 contains base functionality:
      higher-level convenience functions are planned (and some are
      already available in the \sQuote{R-devel} version of \R).

      \item Building PDF manuals (for \R itself or packages,
      e.g. \emph{via} \command{R CMD check}) by default requires the
      LaTeX package \samp{inconsolata}: see the section on
      \sQuote{Making the manuals} in the \sQuote{R Installation and
	Administration Manual}.

      \item \code{axTicks(*, log=TRUE)} has changed in some cases to
      satisfy the documented behavior and be consistent.
    }
  }

  \subsection{NEW FEATURES}{
    \itemize{
      \item \code{txtProgressBar()} can write to an open connection
      instead of the console.

      \item Non-portable package names ending in \file{.} are no longer
      allowed.  Nor are single-character package names (\pkg{R} was
      already disallowed).

      \item \code{regexpr()} and \code{gregexpr()} with \code{perl =
	TRUE} allows Python-style named captures.  (Wish and
      contribution of \PR{14518}.)

      \item The placement of \sQuote{plotmath} text in the margins of
      plots done by base graphics now makes the same vertical adjustment
      as ordinary text, so using ordinary and plotmath text on the same
      margin line will seem better aligned (but not exactly aligned,
      since ordinary text has descenders below the baseline and plotmath
      places them on the baseline).  (Related to \PR{14537}.)

      \item \code{sunflowerplot()} now has a formula interface.
      (Wish of \PR{14541}.)

      \item \code{iconv()} has a new argument \code{toRaw} to handle
      encodings such as \code{UTF-16} with embedded nuls (as was
      possible before the \code{CHARSXP} cache was introduced).

      It will also accept as input the type of list generated with
      \code{toRaw = TRUE}.

      \item Garbage-collecting an unused input text connection no longer
      gives a warning (since it \sQuote{connects} to nothing outside
      \R).

      \item \code{read.table()} and \code{scan()} have gained a
      \code{text} argument, to allow reading data from a (possibly
      literal) character string.

      \item \code{optim(*, method = .)} now allows \code{method = "Brent"}
      as an interface to \code{optimize()}, for use in cases such as
      \code{mle()} where \code{optim()} is used internally.

      \item \code{mosaicplot()} gains a \code{border} argument.
      (Wish of \PR{14550}.)

      \item \code{smooth.spline()} gains a \code{tol} argument which
      controls how different \code{x} values need to be to be treated as
      distinct.  The default has been changed to be more reliable for
      inputs whose range is small compared to their maximum absolute
      value.  (Wish of \PR{14452}.)

      \item \code{gl()} runs faster by avoiding calling \code{factor()}.

      \item The \code{print()} method for \code{object.size()} accepts
      \samp{B} as well as \samp{b} as an abbreviation for \sQuote{bytes}.

      \item \code{unlink()} gains a \code{force} argument to work like
      \command{rm -f} and if possible override restrictive permissions.

      \item \code{pbirthday()} and \code{qbirthday()} now use exact
      calculations for \code{coincident = 2}.

      \item \code{unzip()} and \code{unz()} connections have been
      updated with support for more recent Zip64 features (including
      large file sizes and \command{bzip2} compression, but not UTF-8
      file names).

      \code{unzip()} has a new option to restore file times from those
      recorded (in an unknown timezone) in the zip file.

      \item \code{update.packages()} now accepts a character vector of
      package names for the \code{oldPkgs} argument.  (Suggestion of
      Tal Galili.)

      \item The special reference class fields \code{.self} and
      \code{.refClassDef} are now read-only to prevent corrupting
      the object.

      \item \code{decompose()} now returns the original series as part
      of its value, so it can be used (rather than reconstructed) when
      plotting.  (Suggestion of Rob Hyndman.)

      \item Rao's efficient score test has been implemented for
      \code{glm} objects.  Specifically, the \code{add1}, \code{drop1},
      and \code{anova} methods now allow \code{test = "Rao"}.

      \item If a saved workspace (e.g. \file{.RData}) contains objects
      that cannot be loaded, \R will now start with an warning message
      and an empty workspace, rather than failing to start.

      \item \code{strptime()} now accepts times such as \samp{24:00} for
      midnight at the end of the day, for although these are disallowed by
      POSIX 1003.1-2008, ISO 8601:2004 allows them.

      \item Assignment of \code{names()} to S4 objects now checks for
      a corresponding \code{"names"} slot, and generates a warning or
      an error if that slot is not defined.  See the section on slots
      in \code{?Classes}.

      \item The default methods for \code{is.finite()}, \code{is.infinite()}
      and \code{is.nan()} now signal an error if their argument is not
      an atomic vector.

      \item The formula method for \code{plot()} no longer places
      package \pkg{stats} on the search path (it loads the namespace
      instead).

      \item There now is a genuine \code{"function"} method for
      \code{plot()} rather than the generic dispatching internally to
      \code{graphics::plot.function()}.  It is now exported, so can be
      called directly as \code{plot.function()}.

      \item The one-sided \code{ks.test()} allows \code{exact = TRUE} to
      be specified in the presence of ties (but the approximate
      calculation remains the default: the \sQuote{exact} computation
      makes assumptions known to be invalid in the presence of ties).

      \item The behaviour of \code{curve(add = FALSE)} has changed: it
      now no longer takes the default x limits from the previous plot
      (if any): rather they default to \code{c(0, 1)} just as the
      \code{"function"} method for \code{plot()}.  To get the previous
      behaviour use \code{curve(add = NA)}, which also takes the default for
      log-scaling of the x-axis from the previous plot.

      \item Both \code{curve()} and the \code{plot()} method for
      functions have a new argument \code{xname} to facilitate plots
      such as \code{sin(t)} \emph{vs} \code{t}.

      \item The \code{local} argument to \code{source()} can specify an
      environment as well as \code{TRUE} (\code{parent.env()}) and
      \code{FALSE} (\code{.GlobalEnv}).  It gives better error messages
      for other values, such as \code{NA}.

      \item \code{vcov()} gains methods for classes \code{"summary.lm"} and
      \code{"summary.glm"}.

      \item The \code{plot()} method for class \code{"profile.nls"}
      gains \code{ylab} and \code{lty} arguments, and passes
      \code{\dots} on to \code{plot.default}.

      \item Character-string arguments such as the \code{mode} argument
      of \code{vector()}, \code{as.vector()} and \code{is.vector()} and
      the \code{description} argument of \code{file()} are required to
      be of length exactly one, rather than any further elements being
      silently discarded.  This helps catch incorrect usage in
      programming.

      \item The \code{length} argument of \code{vector()} and its
      wrappers such as \code{numeric()} is required to be of length
      exactly one (other values are now an error rather than giving a
      warning as previously).

      \item \code{vector(len)} and \code{length(x) <- len} no longer
      accept \code{TRUE}/\code{FALSE} for \code{len} (not that they
      were ever documented to, but there was special-casing in the C
      code).

      \item There is a new function \code{Sys.setFileTime()} to set the
      time of a file (including a directory).  See its help for exactly
      which times it sets on various OSes.

      \item The file times reported by \code{file.info()} are reported
      to sub-second resolution on systems which support it.  (Currently
      the POSIX 2008 and FreeBSD/Darwin/NetBSD methods are detected.)

      \item New function \code{getCall(m)} as an abstraction for
      \code{m$call}, enabling \code{update()}'s default method to apply % $
      more universally.  (NB: this can be masked by existing functions
      in packages.)

      \item \code{Sys.info()} gains a \code{euser} component to report
      the \sQuote{effective} user on OSes which have that concept.

      \item The result returned by \code{try()} now contains the
      original error condition object as the \code{"condition"}
      attribute.

      \item All packages with \R code are lazy-loaded irrespective of
      the \samp{LazyLoad} field in the \file{DESCRIPTION} file.  A
      warning is given if the \samp{LazyLoad} field is overridden.

      \item \code{Rd} markup has a new \samp{\figure} tag so that
      figures can be included in help pages when converted to HTML or
      LaTeX.  There are examples on the help pages for \code{par()} and
      \code{points()}.

      \item The built-in httpd server now allows access to files
      in the session temporary directory \code{tempdir()},
      addressed as the \file{/session} directory on the httpd
      server.

      \item Development versions of \R are no longer referred to by the
      number under which they might be released, e.g. in the startup
      banner, \command{R --version} and \code{sessionUtils()}.  The
      correct way to refer to a development version of R is
      \sQuote{R-devel}, preferably with the date and SVN version number.\cr
      E.g. \samp{R-devel (2011-07-04 r56266)}

      \item There is a new function \code{texi2pdf()} in package
      \pkg{tools}, currently a convenience wrapper for
      \code{texi2dvi(pdf = TRUE)}.

      \item There are two new options for typesetting PDF manuals from
      Rd files.  These are \samp{beramono} and \samp{inconsolata}, and
      used the named font for monospaced output. They are intended to
      be used in combination with \samp{times}, and
      \samp{times,inconsolata,hyper} is now the default for the
      reference manual and package manuals.  If you do not have that
      font installed, you can set \env{R_RD4PF} to one of the other
      options: see the \sQuote{R Installation and Administration Manual}.

      \item Automatic printing for reference classes is now done by
      the \code{$show()} method.  A method is defined for class % $
      \samp{envRefClass} and may be overridden for user classes (see
      the \code{?ReferenceClasses} example).  S4 \code{show()} methods
      should no longer be needed for reference classes.

      \item \code{tools::Rdiff} (by default) and \code{R CMD Rdiff}
      now ignore differences in pointer values when comparing printed
      environments, compiled byte code, etc.

      \item The \code{"source"} attribute on functions created with
      \code{keep.source=TRUE} has been replaced with a \code{"srcref"}
      attribute.  The \code{"srcref"} attribute references an in-memory
      copy of the source file using the \code{"srcfilecopy"} class or
      the new \code{"srcfilealias"} class.

      \strong{NB:} This means that functions sourced with
      \code{keep.source = TRUE} and saved (e.g., by \code{save()} or
      \code{readRDS()}) in earlier versions of \R will no longer show
      the original sources (including comments).

      \item New items \href{UserManuals.html}{User Manuals} and
      \href{http://developer.r-project.org/TechDocs}{Technical Papers}
      have been added to the HTML help main page.  These link to
      vignettes in the base and recommended packages and to a
      collection of papers about R issues, respectively.

      \item Documentation and messages have been standardized to
      use \dQuote{namespace} rather than \dQuote{name space}.

      \item \code{setGeneric()} now looks in the default packages for
      a non-generic version of a function if called from a package with a
      namespace.  (It always did for packages without a namespace.)

      \item Setting the environment variable
      \env{_R_WARN_ON_LOCKED_BINDINGS_} will give a warning if an
      attempt is made to change a locked binding.

      \item \code{\\SweaveInput} is now supported when generating
      concordances in \code{Sweave()}.

      \item \code{findLineNum()} and \code{setBreakpoint()} now allow
      the environment to be specified indirectly; the latter gains
      a \code{clear} argument to allow it to call \code{untrace()}.

      \item The body of a closure can be one of further types of \R
      objects, including environments and external pointers.

      \item The \code{Rd2HTML()} function in package \pkg{tools} now has
      a \code{stylesheet} argument, allowing pages to be displayed in
      alternate formats.

      \item New function \code{requireNamespace()} analogous to
      \code{require()}, returning a logical value after attempting to
      load a namespace.

      \item There is a new type of RNG, \code{"L'Ecuyer-CMRG"},
      implementing L'Ecuyer (1999)'s \sQuote{combined multiple-recursive
      generator} \samp{MRG32k3a}.  See the comments on \code{?RNG}.

      \item \code{help.search()} and \code{??} can now display
      vignettes and demos as well as help pages.  The new option
      \code{"help.search.types"} controls the types of documentation
      and the order of their display.

      This also applies to HTML searches, which now give results in all
      of help pages, vignettes and demos.

      \item \code{socketConnection()} now has a \code{timeout} argument.
      It is now documented that large values (package \CRANpkg{snow} used a
      year) do not work on some OSes.

      \item The initialization of the random-number generator now uses
      the process ID as well as the current time, just in case two \R
      processes are launched very rapidly on a machine with
      low-resolution wall clock (some have a resolution of a second;
      modern systems have microsecond-level resolution).

      \item New function \code{pskill()} in the \pkg{tools} package to
      send a terminate signal to one or more processes, plus constants
      such as \code{SIGTERM} to provide a portable way to refer to
      signals (since the numeric values are OS-dependent).

      \item New function \code{psnice()} in the \pkg{tools} package to
      return or change the \sQuote{niceness} of a process.  (Refers to
      the \sQuote{priority class} on Windows.)

      \item \code{list.dirs()} gains a \code{recursive} argument.

      \item An \samp{Authors@R} field in a package \file{DESCRIPTION}
      file can now be used to generate \samp{Author} and
      \samp{Maintainer} fields if needed, and to auto-generate package
      citations.

      \item New utility \code{getElement()} for accessing either a list
      component or a slot in an S4 object.

      \item \code{stars()} gains a \code{col.lines} argument, thanks to
      Dustin Sallings. (Wish of \PR{14657}.)

      \item New function \code{regmatches()} for extracting or replacing
      matched or non-matched substrings from match data obtained by
      \code{regexpr()}, \code{gregexpr()} and \code{regexec()}.

      \item \code{help(package = "pkg_name", help_type = "HTML")} now
      gives HTML help on the package rather than text help.  (This gives
      direct access to the HTML version of the package manual shown
      \emph{via} \code{help.start()}'s \sQuote{Packages} menu.)

      \item \code{agrep()} gains a \code{fixed} argument to optionally
      allow approximate regular expression matching, and a \code{costs}
      argument to specify possibly different integer match costs for
      insertions, deletions and substitutions.

      \item \code{read.dcf()} and \code{write.dcf()} gain a
      \code{keep.white} argument to indicate fields where whitespace
      should be kept as is.

      \item \code{available.packages()} now works around servers that
      fail to return an error code when \file{PACKAGES.gz} does not
      exist.  (Patch submitted by Seth Schommer.)

      \item \code{readBin()} can now read more than
      \eqn{2^{31}-1}{2^31 - 1} bytes in a single call (the previously
      documented limitation).

      \item New function \code{regexec()} for finding the positions of
      matches as well as all substrings corresponding to parenthesized
      subexpressions of the given regular expression.

      \item New function \code{adist()} in package \pkg{utils} for
      computing \sQuote{edit} (generalized Levenshtein) distances between
      strings.

      \item Class \code{"raster"} gains an \code{is.na} method to avoid
      confusion from the misuse of the matrix method (such as \PR{14618}).

      \item The \code{identical()} function gains an
      \code{ignore.bytecode} argument to control comparison of
      compiled functions.

      \item \code{pmin} and \code{pmax} now warn if an argument is
      partially recycled (wish of \PR{14638}).

      \item The default for \code{image(useRaster=)} is now taken from
      option \code{"preferRaster"}: for the small print see \code{?image}.

      \item \code{str()} now displays reference class objects and their
      fields, rather than treating them as classical S4 classes.

      \item New function \code{aregexec()} in package \pkg{utils} for
      finding the positions of approximate string matches as well as all
      substrings corresponding to parenthesized subexpressions of the
      given regular expression.

      \item \code{download.file()} has an \code{extra} argument to pass
      additional command-line options to the non-default methods using
      command-line utilities.

      \code{cacheOK = FALSE} is now supported for \code{method = "curl"}.

      \item \code{interaction.plot(*, type = .)} now also allows type
      \code{"o"} or \code{"c"}.

      \item \code{axTicks(*, log=TRUE)} did sometimes give more values
      than the ticks in the corresponding \code{graphics::axis()}.  By
      default, it now makes use of the new (\pkg{graphics}-package
      independent) \code{axisTicks()} which can make use of a new
      utility \code{.axisPars()}.  Further, it now returns a decreasing
      sequence (as for \code{log=FALSE}) when \code{usr} is decreasing.

      \item Using \code{fix()} or \code{edit()} on a \R{} object (except
      perhaps a matrix or data frame) writes its temporary file with
      extension \file{.R} so editors which select their mode based on
      the extension will select a suitable mode.
    }
  }

  \subsection{GRAPHICS DEVICES}{
    \itemize{
      \item The \code{pdf()} device makes use of Flate compression: this
      is controlled by the new logical argument \code{compress}, and is
      enabled by default.

      \item Devices \code{svg()}, \code{cairo_pdf()} and
      \code{cairo_ps()} gain a \code{family} argument.

      On a Unix-alike \code{X11()} gains a \code{family} argument.  This
      is one of the \code{x11.options()} and so can be passed as an
      argument to the \code{bmp()}, \code{jpeg()}, \code{png()} and
      \code{tiff()} devices.

      Analogous changes have been made on Windows, so all built-in \R
      graphics devices now have a \code{family} argument except
      \code{pictex()} (which has no means to change fonts).

      \item The \code{bmp()}, \code{jpeg()}, \code{png()} and \code{tiff()}
      devices now make use of the \code{antialias} argument for
      \code{type = "quartz"}.

      \item There are several new built-in font mappings for
      \code{X11(type = "Xlib")}: see the help on \code{X11Fonts()}.

      \item There is a new type \code{X11(type = "dbcairo")} which
      updates the screen less frequently: see its help page.

      \item The \code{X11()} device now makes use of cursors to
      distinguish its states.  The normal cursor is an arrow (rather
      than a crosshair); the crosshair is used when the locator is in
      use, and a watch cursor is shown when plotting computations are
      being done.  (These are the standard names for X11 cursors: how
      they are actually displayed depends on the window manager.)

      \item New functions \code{dev.hold()} and \code{dev.flush()} for
      use with graphics devices with buffering.  These are used for most
      of the high-level graphics functions such as \code{boxplot()}, so
      that the plot is only displayed when the page is complete.

      Currently implemented for \code{windows(buffered = TRUE)},
      \code{quartz()} and the cairographics-based \code{X11()} types
      with buffering (which are the default on-screen devices).

      \item New function \code{dev.capture()} for capture of bitmap
      snapshots of image-based devices (a superset of the functionality
      provided by \code{grid.cap()} in \code{grid}).

      \item The default \code{colormodel} for \code{pdf()} and
      \code{postscript()} is now called \code{"srgb"} to more accurately
      describe it.  (Instead of \code{"rgb"}, and in the case of
      \code{postscript()} it no longer switches to and from the gray
      colorspace, by default.)

      The \code{colormodel} for \code{postscript()} which does use both
      gray and sRGB colorspaces is now called \code{"srgb+gray"}.

      Plots which are known to use only black/white/transparent can
      advantageously use \code{colormodel = "gray"} (just as before, but
      there is now slightly more advantage in doing so).

      \item \code{postscript()} with values \code{colormodel = "rgb"} and
      \code{colormodel = "rgb-nogray"} give the behaviour prior to \R
      2.13.0 of uncalibrated RGB, which under some circumstances can be
      rendered much faster by a viewer.

      \code{pdf(colormodel = "rgb")} gives the behaviour prior to \R
      2.13.0 of uncalibrated RGB, which under some circumstances can be
      rendered faster by a viewer, and the files will be smaller (by
      about 9KB if compression is not used).

      \item The \code{postscript()} device only includes the definition
      of the sRGB colorspace in the output file for the colormodels
      which use it.

      \item The \code{postscript()} and \code{pdf()} devices now output
      greyscale raster images (and not RGB) when \code{colormodel = "gray"}.

      \item \code{postscript(colormodel = "gray")} now accepts non-grey
      colours and uses their luminance (as \code{pdf()} long has).

      \item \code{colormodel = "grey"} is allowed as an alternative name
      for \code{postscript()} and \code{pdf()}.

      \item \code{pdf()} in the default sRGB colorspace outputs many
      fewer changes of colorspace, which may speed up rendering in some
      viewing applications.

      \item There is a new function \code{dev.capabilities()} to query
      the capabilities of the current device.  The initial set of
      capabilities are support for semi-transparent colours, rendering
      and capturing raster images, the locator and for interactive
      events.

      \item For \code{pdf()}, \code{maxRasters} is increased as needed
      so the argument is no longer used.
    }
  }

  \subsection{SWEAVE & VIGNETTES}{
    \itemize{
      \item Options \code{keep.source = TRUE, figs.only = FALSE}
      are now the default.

      \item The way the type of user-defined options is determined has
      changed.  Previously they were all regarded as logical: now the
      type is determined by the value given at first use.

      \item The allowed values of logical options are now precisely
      those allowed for character inputs to \code{as.logical()}: this
      means that \samp{t} and \samp{f} are no longer allowed (although
      \code{T} and \code{F} still are).

      \item The preferred location for vignette sources is now the
      directory \file{vignettes} and not \file{inst/doc}: \command{R CMD
      build} will now re-build vignettes in directory \file{vignettes}
      and copy the \file{.Rnw} (etc) files and the corresponding PDFs to
      \file{inst/doc}.  Further files to be copied to \file{inst/doc}
      can be specified \emph{via} the file
      \file{vignettes/.install_extras}.

      \item \command{R CMD Sweave} now supports a \option{--driver}
      option to select the Sweave driver: the default is equivalent to
      \option{--driver=RweaveLatex}.

      \item \command{R CMD Sweave} and \command{R CMD Stangle} support
      options \option{--encoding} and \option{--options}.

      \item The \code{Rtangle()} driver allows \code{output = "stdout"}
      or \code{output = "stderr"} to select the output or message
      connection.  This is convenient for scripting using something like
\preformatted{    R CMD Stangle --options='output="stdout"' foo.Rnw > foo2.R}

      \item There is a new option \code{pdf.compress} controlling
      whether PDF figures are generated using Flate compression (they
      are by default).

      \item \command{R CMD Sweave} now has a \option{--pdf} option to
      produce a PDF version of the processed Sweave document.

      \item It is no longer allowed to have two vignettes with the
      same vignette basename (e.g. \file{vig.Rnw} and \file{vig.Snw}).
      (Previously one vignette hid the other in the \code{vignette()}
      function.)
    }
  }

  \subsection{C-LEVEL FACILITIES}{
    \itemize{
      \item Function \code{R_tmpnam2} has been added to the API to allow
      a temporary filename to include a specified extension.
    }
  }

  \subsection{PACKAGE INSTALLATION}{
    \itemize{
      \item Package \file{DESCRIPTION} file field \samp{KeepSource}
      forces the package to be installed with \code{keep.source =
	TRUE} (or \code{FALSE}).  (Suggestion of Greg Snow.  Note that
      as all packages are lazy-loaded, this is now only relevant at
      installation.)

      There are corresponding options \option{--with-keep.source} and
      \option{--without-keep.source} for \command{R CMD INSTALL}.

      \item \command{R CMD INSTALL} has a new option
      \option{--byte-compile} to byte-compile the packages during
      installation (since all packages are now lazy-loaded).  This can
      be controlled on a per-package basis by the optional field
      \samp{ByteCompile} in the \file{DESCRIPTION} file.

      \item A package \R code but without a \file{NAMESPACE} file will
      have a default one created at \command{R CMD build} or \command{R
      CMD INSTALL} time, so all packages will be installed with
      namespaces.  A consequence of this is that \code{.First.lib()}
      functions need to be copied to \code{.onLoad()} (usually) or
      \code{.onAttach()}.  For the time being, if there is an
      auto-generated \file{NAMESPACE} file and no \code{.onLoad()} nor
      \code{.onAttach()} function is found but \code{.First.lib()} is,
      it will be run as the attach hook (unless the package is one of a
      list of known exceptions, when it will be run as the load hook).

      \item A warning is given if test-loading a package changes a
      locked binding in a package other than itself.  It is likely that
      this will be disallowed in future releases.  (There are \emph{pro
	tem} some exceptions to the warning.)

      \item A dependency on SVN revision is allowed for \code{R},
      e.g. \code{R (>= r56550)}.  This should be used in conjunction with a
      version number, e.g. \code{R (>= 2.14.0), R (>= r56550)} to
      distinguish between R-patched and R-devel versions with the same
      SVN revision.

      \item \code{installed.packages()} now hashes the names of its
      cache files to avoid very rare problems with excessively long path
      names.  (\PR{14669})

      \item A top-level \file{COPYING} file in a package is no longer
      installed (file names \file{LICENSE} or \file{LICENCE} having long
      been preferred).
    }
  }

  \subsection{UTILITIES}{
    \itemize{
      \item \command{R CMD check} now gives an error if the \R code in a
      vignette fails to run, unless this is caused by a missing package.

      \item \command{R CMD check} now unpacks tarballs in the same way
	as \command{R CMD INSTALL}, including making use of the
	environment variable \env{R_INSTALL_TAR} to override the default
	behaviour.

      \item \command{R CMD check} performs additional code analysis of
      package startup functions, and notifies about incorrect argument
      lists and (incorrect) calls to functions which modify the search
      path or inappropriately generate messages.

      \item \command{R CMD check} now also checks compiled code for
      symbols corresponding to functions which might terminate \R or
      write to \file{stdout}/\file{stderr} instead of the console.

      \item \command{R CMD check} now uses a \code{pdf()} device when
      checking examples (rather than \code{postscript()}).

      \item \command{R CMD check} now checks line-endings of makefiles
      and C/C++/Fortran sources in subdirectories of \file{src} as well
      as in \file{src} itself.

      \item \command{R CMD check} now reports as a NOTE what look like
      methods documented with their full names even if there is a
      namespace and they are exported.  In almost all cases they are
      intended to be used only as methods and should use the
      \samp{\\method} markup.  In the other rare cases the recommended
      form is to use a function such as \code{coefHclust} which would
      not get confused with a method, document that and register it in
      the \file{NAMESPACE} file by \code{s3method(coef, hclust,
      coefHclust)}.

      \item The default for the environment variable
      \env{_R_CHECK_COMPACT_DATA2_} is now true: thus if using the newer
      forms of compression introduced in \R 2.10.0 would be beneficial
      is now checked (by default).

      \item Reference output for a vignette can be supplied when
      checking a package by \command{R CMD check}: see \sQuote{Writing R
	Extensions}.

      \item \command{R CMD Rd2dvi} allows the use of LaTeX package
      \samp{inputenx} rather than \samp{inputenc}: the value of the
      environment variable \env{RD2DVI_INPUTENC} is used.  (LaTeX package
      \samp{inputenx} is an optional install which provides greater
      coverage of the UTF-8 encoding.)

      \item \command{Rscript} on a Unix-alike now accepts file names
      containing spaces (provided these are escaped or quoted in the
      shell).

      \item \command{R CMD build} on a Unix-alike (only) now tries to
      preserve dates on files it copies from its input directory.  (This
      was the undocumented behaviour prior to \R 2.13.0.)
     }
   }

  \subsection{DEPRECATED AND DEFUNCT}{
   \itemize{
      \item \code{require()} no longer has a \code{save} argument.

      \item The \code{gamma} argument to \code{hsv()},
      \code{rainbow()}, and \code{rgb2hsv()} has been removed.

      \item The \option{--no-docs} option for
      \command{R CMD build --binary} is defunct: use
      \option{--install-args} instead.

      \item The option \option{--unsafe} to \command{R CMD INSTALL} is
      defunct: use the identical option \option{--no-lock} instead.

      \item The entry point \code{pythag} formerly in \file{Rmath.h} is
      defunct: use instead the C99 function \code{hypot}.

      \item \command{R CMD build --binary} is formally defunct:
      \command{R CMD INSTALL --build} has long been the preferred
      alternative.

      \item \command{zip.file.extract()} is now defunct: use
      \code{unzip()} or \code{unz()} instead.

      \item \command{R CMD Rd2dvi} without the \option{--pdf} option is
      now deprecated: only PDF output will be supported in future
      releases (since this allows the use of fonts only supported for
      PDF), and only \command{R CMD Rd2pdf} will be available.

      \item Options such as \option{--max-nsize} and the function
      \code{mem.limits()} are now deprecated: these limits are nowadays
      almost never used, and are reported by \code{gc()} when they are
      in use.

      \item Forms like \code{binomial(link = "link")} for GLM families
      deprecated since \R 2.4.0 are now defunct.

      \item The \code{declarativeOnly} argument to
      \code{loadNamespace()} (not relevant since \R 2.13.0) has been
      removed.

      \item Use of \code{library.dynam()} without specifying all the
      first three arguments is deprecated.  (It is often called from a
      namespace, and the defaults are only appropriate to a package.)

      Use of \code{chname} in \code{library.dynam()} with the extension
      \file{.so} or \code{.dll} (which is clearly not allowed according
      to the help page) is deprecated.  This also applies to
      \code{library.dynam.unload()} and \code{useDynLib} directives in
      \file{NAMESPACE} files.

      \item It is deprecated to use \code{mean(x)} and \code{sd(x)}
      directly on data frames (or also matrices, for \code{sd})
      \code{x}, instead of simply using \code{sapply}.

      In the same spirit, \code{median(x)} now gives an error for a data
      frame \code{x} (it often gave nonsensical results).

      \item The \code{keep.source} argument to \code{library()} and
      \code{require()} is deprecated: it was only used for packages
      installed without lazy-loading, and now all packages are
      lazy-loaded.

      \item Using a false value for the \file{DESCRIPTION} field
      \samp{LazyLoad} is deprecated.
    }
  }

  \subsection{INSTALLATION}{
    \itemize{
      \item The base and recommended packages are now byte-compiled
      (equivalent to \command{make bytecode} in \R 2.13.x).

      \item Configure option \option{--with-system-zlib} now only makes
      use of the basic interface of \code{zlib} and not the
      C function \samp{gzseek} which has shown erroneous behaviour in
      \code{zlib} 1.2.4 and 1.2.5.

      \item The \code{zlib} in the R sources is now version 1.2.5.
      (This is safe even on 32-bit Linux systems because only the basic
      interface is now used.)

      \item The \file{.afm} files in package \pkg{grDevices} are now
      installed as compressed files (as long done on Windows), saving ca
      2MB on the installed size.

      \item The non-screen cairo-based devices are no longer in the X11
      module and so can be installed without X11.  (We have never seen a
      Unix-alike system with cairographics installed but not X11, but a
      user might select \option{--without-x}.)

      \item Configure will try to use \option{-fobjc-exceptions} for the
      Objective-C compiler (if present) to ensure that even compilers
      that do not enable exceptions by default (such as vanilla gcc) can
      be used.  (Objective-C is currently only used on Mac OS X.)

      \item The system call \code{times} is required.

      \item The C99 functions \code{acosh}, \code{asinh}, \code{atanh},
      \code{snprintf} and \code{vsnprintf} are now required.

      \item There is no longer support for making DVI manuals \emph{via}
      \command{make dvi}, \command{make install-dvi} and similar.  Only
      PDF manuals are supported (to allow the use of fonts which are
      only available for PDF.)

      \item The \file{configure} arguments used during configuration of
      \R are included as a comment in \file{Makeconf} for informative
      purposes on Unix-alikes in a form suitable for shell execution.
      Note that those are merely command-line arguments, they do not
      include environment variables (one more reason to use configure
      variables instead) or site configuration settings.

      \item Framework installation now supports \code{DESTDIR}
      (Mac OS X only).

      \item Java detection (\code{R CMD javareconf}) works around bogus
      \code{java.library.path} property in recent Oracle Java binaries.
    }
  }

  \subsection{BUG FIXES}{
    \itemize{
      \item The locale category \samp{LC_MONETARY} was only being set on
      startup on Windows: that is now done on Unix-alikes where
      supported.

      \item Reference class utilities will detect an attempt to modify
      methods or fields in a locked class definition (e.g., in a
      namespace) and generate an error.

      \item The formula methods for \code{lines()}, \code{points()} and
      \code{text()} now work even if package \pkg{stats} is not on the
      search path.

      \item In principle, S4 classes from different packages could
      have the same name.  This has not previously worked.  Changes
      have now been installed that should allow such classes and
      permit methods to use them.  New functions \code{className()}
      and \code{multipleClasses()} are related tools for programming.

      \item Work around an issue in Linux (a system \code{select} call
      resetting \code{tv}) which prevented internet operations from
      timing out properly.

      \item Several stack trampling and overflow issues have been fixed
      in TRE, triggered by \code{agrep} and friends with long patterns.
      (\PR{14627})

      \item (\dQuote{design infelicity}) Field assignments in reference
      classes are now consistent with slots in S4 classes:  the
      assigned value must come from the declared class (if any) for the field
      or from a subclass.

      \item The methods objects constructed for \code{"coerce"} and
      \code{"coerce<-"} were lacking some essential information in the
      \code{generic}, \code{defined} and \code{target} slots;
      \code{as()} did not handle duplicate class definitions correctly.

      \item The parser no longer accepts the digit \code{8} in an octal
      character code in a string, nor does it accept unterminated
      strings in a file.  (Reported by Bill Dunlap.)

      \item The \code{print()} method for class \code{"summary.aov"} did
      not pass on argument \code{digits} when \code{summary()} was
      called on a single object, and hence used more digits than
      documented.

      \item The \code{X11()} device's cairo back-end produced incorrect
      capture snapshot images on big-endian machines.

      \item \code{loglin()} gave a spurious error when argument
      \code{margin} consisted of a single element of length one.
      (\PR{14690})

      \item \code{loess()} is better protected against misuse,
      e.g. zero-length \code{span}.  (\PR{14691})

      \item \code{HoltWinters()} checks that the optimization
      succeeded. (\PR{14694})

      \item The (undocumented) inclusion of superclass objects in
      default initializing of reference classes overwrote explicit
      field arguments. The bug is fixed, the feature documented and a
      test added.

      \item \code{round(x, -Inf)} now does something sensible (return
      zero rather than \code{NA}).

      \item \code{signif(x, -Inf)} now behaves as documented
      (\code{signif(x, 1)}) rather than giving zero.

      \item The \code{"table"} method for \code{Axis()} hardcoded
      \code{side = 1}, hence calls to \code{plot(<vector>, <table>)}
      labelled the wrong axis. (\PR{14699})

      \item Creating a connection might fail under \code{gctorture(TRUE)}.

      \item \code{stack()} and \code{unstack()} converted character
      columns to factors.

      \code{unstack()} sometimes produced incorrect results (a list or a
      vector) if the factor on which to un-split had only one level.

      \item On some systems \code{help(".C", help_type = "pdf")} and
      similar generated file names that TeX was unable to handle.

      \item Non-blocking listening socket connections continued to
      report \code{isIncomplete()} as true even when the peer had closed
      down and all available input had been read.

      \item The revised HTML search system now generates better
      hyperlinks to help topics found: previously it gave problems with
      help pages with names containing e.g. spaces and slashes.

      \item A late change in \R 2.13.2 broke \samp{\Sexpr} expressions
      in Rd files.

      \item The creation of ticks on log axes (including by
      \code{axTicks()}) sometimes incorrectly omitted a tick at one end
      of the range by rounding error in a platform-dependent way.  This
      could be seen in the examples for \code{axTicks()}, where with
      axis limits \code{c(0.2, 88)} the tick for \code{0.2} was
      sometimes omitted.

      \item \code{qgamma()} for small \code{shape} underflows to
      \code{0} rather than sometimes giving \code{NaN}.
      (\PR{8528}, \PR{14710})

      \item \code{mapply()} now gives an explicit error message (rather
      than an obscure one) if inputs of zero and positive length are
      mixed.

      \item Setting a Hershey font family followed by string height
      query would crash R.

      \item \code{R CMD javareconf -e} would fail for some shells due to
      a shift error. Also the resulting paths will no longer contain
      \code{$(JAVA_HOME)} as that can result in an unintended
      substitution based on \file{Makeconf} instead of the shell
      setting.
    }
  }
}

\section{\Rlogo CHANGES IN R VERSION 2.13.2}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item \code{mem.limits()} now reports values larger than the
      maximum integer (previously documented to be reported as
      \code{NA}), and allows larger values to be set, including
      \code{Inf} to remove the limit.

      \item The \code{print()} methods for classes \code{"Date"},
      \code{"POSIXct"} and \code{"POSIXlt"} respect the option
      \code{"max.print"} and so are much faster for very long datetime
      vectors.  (Suggestion of Yohan Chalabi.)

      \item \code{untar2()} now works around errors generated with
      \code{tar} files that use more than the standard 6 digits for the
      checksum. (\PR{14654})

      \item \code{install.packages()} with \code{Ncpus > 1} guards
      against simultaneous installation of indirect dependencies as well
      as direct ones.

      \item Sweave now knows about a few more Windows' encodings
      (including \code{cp1250} and \code{cp1257}) and some
      \code{inputenx} encodings such as \code{koi8-r}.

      \item \code{postscript(colormodel = "rgb-nogray")} no longer sets
      the sRGB colorspace for each colour and so some viewers may render
      its files much faster than the default \code{colormodel ="rgb"}.

      \item The default for \code{pdf(maxRasters=)} has been increased
	from 64 to 1000.

      \item \code{readBin()} now warns if \code{signed = FALSE} is used
      inappropriately (rather than being silently ignored).

      It enforces the documented limit of \eqn{2^{31}-1}{2^31-1} bytes
      in a single call.

      \item PCRE has been updated to version 8.13, a bug-fix release
      with updated Unicode tables (version 6.0.0).  An additional patch
      (r611 from PCRE 8.20-to-be) has been added to fix a collation
      symbol recognition issue.
    }
  }

  \subsection{INSTALLATION}{
    \itemize{
      \item It is possible to build in \file{src/extra/xdr} on more
      platforms.  (Needed since glibc 2.14 hides its RPC implementation.)

      \item \command{configure} will find the Sun TI-RPC implementation
      of xdr (in \file{libtirpc}) provided its header files are in the
      search path: see the \sQuote{R Installation and Administration
	Manual}.
    }
  }

  \subsection{PACKAGE INSTALLATION}{
    \itemize{
      \item Using a broad \code{exportPattern} directive in a
      \file{NAMESPACE} file is no longer allowed to export internal
      objects such as \code{.onLoad} and \code{.__S3MethodsTable__.} .

      These are also excluded from imports, along with \code{.First.lib}.
    }
  }

  \subsection{BUG FIXES}{
    \itemize{
     \item \code{fisher.test()} had a buglet: If arguments were factors
      with unused levels, levels were dropped and you would get an error
      saying that there should be at least two levels, inconsistently
      with pre-tabulated data.  (Reported by Michael Fay).

      \item \code{package.skeleton()} will no longer dump S4 objects
      supplied directly rather than in a code file.  These cannot be
      restored correctly from the dumped version.

      \item Build-time expressions in help files did not have access
      to functions in the package being built (with \command{R CMD build}).

      \item Because \code{quote()} did not mark its result as being in
      use, modification of the result could in some circumstances
      modify the original call.

      \item Plotting \code{pch = '.'} now guarantees at least a one-pixel
      dot if \code{cex > 0}.

      \item The very-rarely-used command-line option
      \option{--max-vsize} was incorrectly interpreted as a number of
      Vcells and not in bytes as documented.  (Spotted by Christophe
      Rhodes.)

      \item The HTML generated by \code{Rd2HTML()} comes closer to
      being standards compliant.

      \item \code{filter(x, recursive = TRUE)} gave incorrect results on a
      series containing \code{NA}s.  (Spotted by Bill Dunlap.)

      \item Profiling \code{stats::mle()} fits with a \code{fixed}
      parameter was not supported.  (\PR{14646})

      \item \code{retracemem()} was still using positional matching.
      (\PR{14650})

      \item The \code{quantile} method for \code{"ecdf"} objects now
      works and is documented.

      \item \code{xtabs(~ .., ..., sparse=TRUE)} now also works together
      with an \code{exclude = ..} specification.

      \item \code{decompose()} computed an incorrect seasonal component for
      time series with odd frequencies.

      \item The \code{pdf()} device only includes the definition of the
      sRGB colorspace in the output file for the \code{"rgb"} colormodel
      (and not for \code{"gray"} nor \code{"cmyk"}): this saves ca
      9KB in the output file.

      \item \code{.hasSlot()} wrongly gave \code{FALSE} in some cases.

      \item \code{Sweave()} with \code{keep.source=TRUE} could generate
      spurious \code{NA} lines when a chunk reference appeared last in
      a code chunk.

      \item \samp{\Sexpr[results=rd]} in an \file{.Rd} file now first tries
      \code{parse_Rd(fragment=FALSE)} to allow Rd section-level macros
      to be inserted.

      \item The \code{print()} method for class \code{"summary.aov"} did
      not pass on arguments such as \code{signif.stars} when
      \code{summary()} was called on a single object.  (\PR{14684})

      \item In rare cases \code{ks.test()} could return a p-value
      very slightly less than 0 by rounding error.  (\PR{14671})

      \item If \code{trunc()} was called on a \code{"POSIXlt"} vector
      and the result was subsetted, all but the first element was
      converted to \code{NA}.  (\PR{14679})

      \item \code{cbind()} and \code{rbind()} could cause memory
      corruption when used on a combination of raw and logical/integer
      vectors.
    }
  }
}

\section{\Rlogo CHANGES IN R VERSION 2.13.1}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item \code{iconv()} no longer translates \code{NA} strings as
      \code{"NA"}.

      \item \code{persp(box = TRUE)} now warns if the surface extends
      outside the box (since occlusion for the box and axes is computed
      assuming the box is a bounding box). (\PR{202})

      \item \code{RShowDoc()} can now display the licences shipped with
      \R, e.g. \code{RShowDoc("GPL-3")}.

      \item New wrapper function \code{showNonASCIIfile()} in package
      \pkg{tools}.

      \item \code{nobs()} now has a \code{"mle"} method in package
      \pkg{stats4}.

      \item \code{trace()} now deals correctly with S4 reference classes
      and corresponding reference methods (e.g., \code{$trace()})%$
      have been added.

      \item \command{xz} has been updated to 5.0.3 (very minor bugfix
      release).

      \item \code{tools::compactPDF()} gets more compression (usually a
      little, sometimes a lot) by using the compressed object streams of
      PDF 1.5.

      \item \code{cairo_ps(onefile = TRUE)} generates encapsulated EPS
      on platforms with cairo >= 1.6.

      \item Binary reads (e.g. by \code{readChar()} and \code{readBin()})
      are now supported on clipboard connections.  (Wish of \PR{14593}.)

      \item \code{as.POSIXlt.factor()} now passes \code{...} to the
      character method (suggestion of Joshua Ulrich).  [Intended for \R
      2.13.0 but accidentally removed before release.]

      \item \code{vector()} and its wrappers such as \code{integer()}
      and \code{double()} now warn if called with a \code{length}
      argument of more than one element.  This helps track down user
      errors such as calling \code{double(x)} instead of
      \code{as.double(x)}.
    }
  }

  \subsection{INSTALLATION}{
    \itemize{
      \item Building the vignette PDFs in packages \pkg{grid} and
      \pkg{utils} is now part of running \command{make} from an SVN
      checkout on a Unix-alike: a separate \command{make vignettes} step
      is no longer required.

      These vignettes are now made with \code{keep.source = TRUE} and
      hence will be laid out differently.

      \item \command{make install-strip} failed under some configuration
      options.

      \item Packages can customize non-standard installation of compiled
      code via a \code{src/install.libs.R} script. This allows packages that
      have architecture-specific binaries (beyond the package's shared
      objects/DLLs) to be installed in a multi-architecture setting.
    }
  }

  \subsection{SWEAVE & VIGNETTES}{
    \itemize{
      \item \code{Sweave()} and \code{Stangle()} gain an \code{encoding}
      argument to specify the encoding of the vignette sources if the
      latter do not contain a \samp{\\usepackage[]{inputenc}} statement
      specifying a single input encoding.

      \item There is a new Sweave option \code{figs.only = TRUE} to run
      each figure chunk only for each selected graphics device, and not
      first using the default graphics device.   This will become the
      default in \R 2.14.0.

      \item Sweave custom graphics devices can have a custom function
      \code{foo.off()} to shut them down.

      \item Warnings are issued when non-portable filenames are found
      for graphics files (and chunks if \code{split = TRUE}).  Portable
      names are regarded as alphanumeric plus hyphen, underscore, plus
      and hash (periods cause problems with recognizing file
      extensions).

      \item The \code{Rtangle()} driver has a new option
      \code{show.line.nos} which is by default false; if true it
      annotates code chunks with a comment giving the line number of the
      first line in the sources (the behaviour of \R >= 2.12.0).

      \item Package installation tangles the vignette sources: this step
      now converts the vignette sources from the vignette/package
      encoding to the current encoding, and records the encoding (if not
      ASCII) in a comment line at the top of the installed \file{.R}
      file.
    }
  }

  \subsection{LICENCE}{
    \itemize{
      \item No parts of \R are now licensed solely under GPL-2.  The
      licences for packages \CRANpkg{rpart} and \CRANpkg{survival} have been
      changed, which means that the licence terms for \R as distributed
      are GPL-2 | GPL-3.
    }
  }

  \subsection{DEPRECATED AND DEFUNCT}{
   \itemize{
     \item The internal functions \code{.readRDS()} and
      \code{.saveRDS()} are now deprecated in favour of the public
      functions \code{readRDS()} and \code{saveRDS()} introduced in \R
      2.13.0.

      \item Switching off lazy-loading of code \emph{via} the
      \samp{LazyLoad} field of the \file{DESCRIPTION} file is now
      deprecated.  In future all packages will be lazy-loaded.

      \item The off-line \code{help()} types \samp{"postscript"} and
      \samp{"ps"} are deprecated.
    }
  }

  \subsection{UTILITIES}{
    \itemize{
      \item \command{R CMD check} on a multi-architecture installation now
      skips the user's \file{.Renviron} file for the
      architecture-specific tests (which do read the
      architecture-specific \file{Renviron.site} files).  This is
      consistent with single-architecture checks, which use
      \option{--no-environ}.

      \item \command{R CMD build} now looks for \file{DESCRIPTION}
      fields \samp{BuildResaveData} and \samp{BuildKeepEmpty} for
      per-package overrides.  See \sQuote{Writing R Extensions}.
    }
  }

  \subsection{BUG FIXES}{
    \itemize{
      \item \code{plot.lm(which = 5)} was intended to order factor
      levels in increasing order of mean standardized residual.   It
      ordered the factor labels correctly, but could plot the wrong
      group of residuals against the label.  (\PR{14545})

      \item \code{mosaicplot()} could clip the factor labels, and could
      overlap them with the cells if a non-default value of
      \code{cex.axis} was used.  (Related to \PR{14550}.)

      \item \code{dataframe[[row,col]]} now dispatches on \code{[[}
      methods for the selected column.  (Spotted by Bill Dunlap).

      \item \code{sort.int()} would strip the class of an object, but
      leave its object bit set.  (Reported by Bill Dunlap.)

      \item \code{pbirthday()} and \code{qbirthday()} did not implement
      the algorithm exactly as given in their reference and so were
      unnecessarily inaccurate.

      \code{pbirthday()} now solves the approximate formula analytically
      rather than using \code{uniroot()} on a discontinuous function.

      The description of the problem was inaccurate: the probability is
      a tail probability (\sQuote{2 \emph{or more} people share a birthday})

      \item Complex arithmetic sometimes warned incorrectly about
      producing NAs when there were NaNs in the input.

      \item \code{seek(origin = "current")} incorrectly reported it was
      not implemented for a \code{gzfile()} connection.

      \item \code{c()}, \code{unlist()}, \code{cbind()} and
      \code{rbind()} could silently overflow the maximum vector length
      and cause a segfault.  (\PR{14571})

      \item The \code{fonts} argument to \code{X11(type = "Xlib")} was
      being ignored.

      \item Reading (e.g. with \code{readBin()}) from a raw connection
      was not advancing the pointer, so successive reads would read the
      same value.  (Spotted by Bill Dunlap.)

      \item Parsed text containing embedded newlines was printed
      incorrectly by \code{as.character.srcref()}.  (Reported by Hadley
      Wickham.)

      \item \code{decompose()} used with a series of a non-integer
      number of periods returned a seasonal component shorter than the
      original series.  (Reported by Rob Hyndman.)

      \item \code{fields = list()} failed for \code{setRefClass()}.
      (Reported by Michael Lawrence.)

      \item Reference classes could not redefine an inherited field
      which had class \code{"ANY"}. (Reported by Janko Thyson.)

      \item Methods that override previously loaded versions will now
      be installed and called.  (Reported by Iago Mosqueira.)

      \item \code{addmargins()} called \code{numeric(apos)} rather than
      \code{numeric(length(apos))}.

      \item The HTML help search sometimes produced bad links.
      (\PR{14608})

      \item Command completion will no longer be broken if
      \code{tail.default()} is redefined by the user. (Problem reported
      by Henrik Bengtsson.)

      \item LaTeX rendering of markup in titles of help pages has been
      improved; in particular, \code{\\eqn{}} may be used there.

      \item \code{isClass()} used its own namespace as the default of
      the \code{where} argument inadvertently.

      \item Rd conversion to latex mishandled multi-line titles
      (including cases where there was a blank line in the \samp{\title}
      section).  (It seems this happened only in 2.13.0 patched.)

      \item \code{postscript()} with an sRGB colormodel now uses sRGB
      for raster images (in \R 2.13.[01] it used uncalibrated RGB).

      There is no longer an undocumented 21845-pixel limit on raster images.
    }
  }
}

\section{\Rlogo CHANGES IN R VERSION 2.13.0}{
  \subsection{SIGNIFICANT USER-VISIBLE CHANGES}{
    \itemize{
      \item \code{replicate()} (by default) and \code{vapply()} (always)
      now return a higher-dimensional array instead of a matrix in the case
      where the inner function value is an array of dimension >= 2.

      \item Printing and formatting of floating point numbers is now
      using the correct number of digits, where it previously rarely
      differed by a few digits. (See \dQuote{scientific} entry below.)
      This affects \emph{many} \file{*.Rout.save} checks in packages.
    }
  }
  \subsection{NEW FEATURES}{
    \itemize{
      \item \code{normalizePath()} has been moved to the \pkg{base}
      package (from \pkg{utils}): this is so it can be used by
      \code{library()} and friends.

      It now does tilde expansion.

      It gains new arguments \code{winslash} (to select the separator on
      Windows) and \code{mustWork} to control the action if a canonical
      path cannot be found.

      \item The previously barely documented limit of 256 bytes on a
      symbol name has been raised to 10,000 bytes (a sanity check).
      Long symbol names can sometimes occur when deparsing expressions
      (for example, in \code{model.frame}).

      \item \code{reformulate()} gains a \code{intercept} argument.

      \item \code{cmdscale(add = FALSE)} now uses the more common
      definition that there is a representation in \code{n-1} or less
      dimensions, and only dimensions corresponding to positive
      eigenvalues are used.  (Avoids confusion such as \PR{14397}.)

      \item Names used by \code{c()}, \code{unlist()}, \code{cbind()}
      and \code{rbind()} are marked with an encoding when this can be
      ascertained.

      \item R colours are now defined to refer to the sRGB color space.

      The PDF, PostScript, and Quartz graphics devices record this fact.
      X11 (and cairo) and Windows just assume that your screen conforms.

      \item \code{system.file()} gains a \code{mustWork} argument
      (suggestion of Bill Dunlap).

      \item \code{new.env(hash = TRUE)} is now the default.

      \item \code{list2env(envir = NULL)} defaults to hashing (with a
      suitably sized environment) for lists of more than 100 elements.

      \item \code{text()} gains a formula method.

      \item \code{IQR()} now has a \code{type} argument which is passed
      to \code{quantile()}.

      \item \code{as.vector()}, \code{as.double()} etc duplicate less
      when they leave the mode unchanged but remove attributes.

      \code{as.vector(mode = "any")} no longer duplicates when it does
      not remove attributes.  This helps memory usage in \code{matrix()}
      and \code{array()}.

      \code{matrix()} duplicates less if \code{data} is an atomic vector
      with attributes such as names (but no class).

      \code{dim(x) <- NULL} duplicates less if \code{x} has neither
      dimensions nor names (since this operation removes names and
      dimnames).

      \item \code{setRepositories()} gains an \code{addURLs} argument.

      \item \code{chisq.test()} now also returns a \code{stdres}
      component, for standardized residuals (which have unit variance,
      unlike the Pearson residuals).

      \item \code{write.table()} and friends gain a \code{fileEncoding}
      argument, to simplify writing files for use on other OSes (e.g. a
      spreadsheet intended for Windows or Mac OS X Excel).

      \item Assignment expressions of the form \code{foo::bar(x) <- y}
      and \code{foo:::bar(x) <- y} now work; the replacement functions
      used are \code{foo::`bar<-`} and \code{foo:::`bar<-`}.

      \item \code{Sys.getenv()} gains a \code{names} argument so
      \code{Sys.getenv(x, names = FALSE)} can replace the common idiom
      of \code{as.vector(Sys.getenv())}.  The default has been changed
      to not name a length-one result.

      \item Lazy loading of environments now preserves attributes and
      locked status. (The locked status of bindings and active bindings
      are still not preserved; this may be addressed in the future).

      \item \code{options("install.lock")} may be set to \code{FALSE}
      so that \code{install.packages()} defaults to \option{--no-lock}
      installs, or (on Windows) to \code{TRUE} so that binary installs
      implement locking.

      \item \code{sort(partial = p)} for large \code{p} now tries
      Shellsort if quicksort is not appropriate and so works for
      non-numeric atomic vectors.

      \item \code{sapply()} gets a new option \code{simplify = "array"}
      which returns a \dQuote{higher rank} array instead of just a matrix
      when \code{FUN()} returns a \code{dim()} length of two or more.

      \code{replicate()} has this option set by default, and
      \code{vapply()} now behaves that way internally.

      \item \code{aperm()} becomes S3 generic and gets a \code{table}
      method which preserves the class.

      \item \code{merge()} and \code{as.hclust()} methods for objects of
      class \code{"dendrogram"} are now provided.

      \item The character method of \code{as.POSIXlt()} now tries to
      find a format that works for all non-\code{NA} inputs, not just
      the first one.

      \item \code{str()} now has a method for class \code{"Date"}
      analogous to that for class \code{"POSIXt"}.

      \item New function \code{file.link()} to create hard links on
      those file systems (POSIX, NTFS but not FAT) that support them.

      \item New \code{Summary()} group method for class \code{"ordered"}
      implements \code{min()}, \code{max()} and \code{range()} for
      ordered factors.

      \item \code{mostattributes<-()} now consults the \code{"dim"}
      attribute and not the \code{dim()} function, making it more useful
      for objects (such as data frames) from classes with methods for
      \code{dim()}.  It also uses \code{attr<-()} in preference to the
      generics \code{name<-()}, \code{dim<-()} and \code{dimnames<-()}.
      (Related to \PR{14469}.)

      \item There is a new option \code{"browserNLdisabled"} to disable
      the use of an empty (e.g. via the \sQuote{Return} key) as a synonym
      for \code{c} in \code{browser()} or \code{n} under \code{debug()}.
      (Wish of \PR{14472}.)

      \item \code{example()} gains optional new arguments
      \code{character.only} and \code{give.lines} enabling programmatic
      exploration.

      \item \code{serialize()} and \code{unserialize()} are no longer
      described as \sQuote{experimental}.  The interface is now regarded
      as stable, although the serialization format may well change in
      future releases.  (\code{serialize()} has a new argument
      \code{version} which would allow the current format to be written
      if that happens.)

      New functions \code{saveRDS()} and \code{readRDS()} are public
      versions of the \sQuote{internal} functions \code{.saveRDS()} and
      \code{.readRDS()} made available for general use.  The dot-name
      versions remain available as several package authors have made use
      of them, despite the documentation.  \code{readRDS()} no longer
      wraps non-file connections in a call to \code{gzcon()}, for
      efficiency (see its documentation).

      \code{saveRDS()} supports \code{compress = "xz"}.

      \item Many functions when called with a not-open connection will
      now ensure that the connection is left not-open in the event of
      error.  These include \code{read.dcf()}, \code{dput()},
      \code{dump()}, \code{load()}, \code{parse()}, \code{readBin()},
      \code{readChar()}, \code{readLines()}, \code{save()},
      \code{writeBin()}, \code{writeChar()}, \code{writeLines()},
      \code{.readRDS()}, \code{.saveRDS()} and \code{tools::parse_Rd()},
      as well as functions calling these.

      \item Public functions \code{find.package()} and \code{path.package()}
      replace the internal dot-name versions.

      \item The default method for \code{terms()} now looks for a
      \code{"terms"} attribute if it does not find a \code{"terms"}
      component, and so works for model frames.

      \item \code{httpd()} handlers receive an additional argument
      containing the full request headers as a raw vector (this can be
      used to parse cookies, multi-part forms etc.). The recommended
      full signature for handlers is therefore \code{function(url,
	query, body, headers, ...)}.

      \item \code{file.edit()} gains a \code{fileEncoding} argument to
      specify the encoding of the file(s).

      \item The format of the HTML package listings has changed.  If
      there is more than one library tree , a table of links to
      libraries is provided at the top and bottom of the page.  Where a
      library contains more than 100 packages, an alphabetic index is
      given at the top of the section for that library.  (As a
      consequence, package names are now sorted case-insensitively
      whatever the locale.)

      \item \code{isSeekable()} now returns \code{FALSE} on connections
      which have non-default \code{encoding}.  Although documented to
      record if \sQuote{in principle} the connection supports seeking,
      it seems safer to report \code{FALSE} when it may not work.

      \item \command{R CMD REMOVE} and \code{remove.packages()} now
      remove file \code{R.css} when removing all remaining packages in a
      library tree.  (Related to the wish of \PR{14475}: note that this
      file is no longer installed.)

      \item \code{unzip()} now has a \code{unzip} argument like
      \code{zip.file.extract()}.  This allows an external
      \command{unzip} program to be used, which can be useful to access
      features supported by Info-ZIP's \command{unzip} version 6 which
      is now becoming more widely available.

      \item There is a simple \code{zip()} function, as wrapper for an
      external \command{zip} command.

      \item \code{bzfile()} connections can now read from concatenated
      \command{bzip2} files (including files written with
      \code{bzfile(open = "a")}) and files created by some other
      compressors (such as the example of \PR{14479}).

      \item The primitive function \code{c()} is now of type BUILTIN.

      \item \code{plot(<dendrogram>, .., nodePar=*)} now obeys an optional
      \code{xpd} specification (allowing clipping to be turned off
      completely).

      \item \code{nls(algorithm="port")} now shares more code with
      \code{nlminb()}, and is more consistent with the other
      \code{nls()} algorithms in its return value.

      \item \command{xz} has been updated to 5.0.1 (very minor bugfix
      release).

      \item \code{image()} has gained a logical \code{useRaster}
      argument allowing it to use a bitmap raster for plotting a
      regular grid instead of polygons. This can be more efficient, but
      may not be supported by all devices. The default is \code{FALSE}.

      \item \code{list.files()}/\code{dir()} gains a new argument
      \code{include.dirs()} to include directories in the listing when
      \code{recursive = TRUE}.

      \item New function \code{list.dirs()} lists all directories,
      (even empty ones).

      \item \code{file.copy()} now (by default) copies
      read/write/execute permissions on files, moderated by the current
      setting of \code{Sys.umask()}.

      \item \code{Sys.umask()} now accepts \code{mode = NA} and returns
      the current \code{umask} value (visibly) without changing it.

      \item There is a \code{!} method for classes \code{"octmode"} and
      \code{"hexmode"}: this allows \code{xor(a, b)} to work if both
      \code{a} and \code{b} are from one of those classes.

      \item \code{as.raster()} no longer fails for vectors or matrices
      containing \code{NA}s.

      \item New hook \code{"before.new.plot"} allows functions to be run
      just before advancing the frame in \code{plot.new}, which is
      potentially useful for custom figure layout implementations.

      \item Package \pkg{tools} has a new function \code{compactPDF()}
      to try to reduce the size of PDF files \emph{via} \command{qpdf}
      or \command{gs}.

      \item \code{tar()} has a new argument \code{extra_flags}.

      \item \code{dotchart()} accepts more general objects \code{x} such
      as 1D tables which can be coerced by \code{as.numeric()} to a
      numeric vector, with a warning since that might not be
      appropriate.

      \item The previously internal function \code{create.post()} is now
      exported from \pkg{utils}, and the documentation for
      \code{bug.report()} and \code{help.request()} now refer to that
      for \code{create.post()}.

      It has a new \code{method = "mailto"} on Unix-alikes similar to
      that on Windows: it invokes a default mailer via \command{open}
      (Mac OS X) or \code{xdg-open} or the default browser (elsewhere).

      The default for \code{ccaddress} is now
      \code{getOption("ccaddress")} which is by default unset: using the
      username as a mailing address nowadays rarely works as expected.

      \item The default for \code{options("mailer")} is now
      \code{"mailto"} on all platforms.

      \item \code{unlink()} now does tilde-expansion (like most other
      file functions).

      \item \code{file.rename()} now allows vector arguments (of the
      same length).

      \item The \code{"glm"} method for \code{logLik()} now returns an
      \code{"nobs"} attribute (which \code{stats4::BIC()} assumed it
      did).

      The \code{"nls"} method for \code{logLik()} gave incorrect results
      for zero weights.

      \item There is a new generic function \code{nobs()} in package
      \pkg{stats}, to extract from model objects a suitable value for
      use in BIC calculations.  An S4 generic derived from it is defined
      in package \pkg{stats4}.

      \item Code for S4 reference-class methods is now examined for
      possible errors in non-local assignments.

      \item \code{findClasses}, \code{getGeneric}, \code{findMethods}
      and \code{hasMethods} are revised to deal consistently with
      the \code{package=} argument and be consistent with soft namespace
      policy for finding objects.

      \item \code{tools::Rdiff()} now has the option to return not only
      the status but a character vector of observed differences (which
      are still by default sent to \file{stdout}).

      \item The startup environment variables \env{R_ENVIRON_USER},
      \env{R_ENVIRON}, \env{R_PROFILE_USER} and \env{R_PROFILE} are now
      treated more consistently.  In all cases an empty value is
      considered to be set and will stop the default being used, and for
      the last two tilde expansion is performed on the file name.  (Note
      that setting an empty value is probably impossible on Windows.)

      \item Using \command{R --no-environ CMD}, \command{R
	--no-site-file CMD} or \command{R --no-init-file CMD} sets
      environment variables so these settings are passed on to child \R
      processes, notably those run by \code{INSTALL}, \code{check} and
      \code{build}. \command{R --vanilla CMD} sets these three options
      (but not \option{--no-restore}).

      \item \code{smooth.spline()} is somewhat faster.  With
      \code{cv=NA} it allows some leverage computations to be skipped,

      \item The internal (C) function \samp{scientific()}, at the heart
      of \R's \code{format.info(x)}, \code{format(x)}, \code{print(x)},
      etc, for numeric \code{x}, has been re-written in order to provide
      slightly more correct results, fixing \PR{14491}, notably in
      border cases including when \code{digits >= 16}, thanks to
      substantial contributions (code and experiments) from Petr
      Savicky.  This affects a noticeable amount of numeric output from
      \R.

      \item A new function \code{grepRaw()} has been introduced for
      finding subsets of raw vectors. It supports both literal searches
      and regular expressions.

      \item Package \pkg{compiler} is now provided as a standard
      package.  See \code{?compiler::compile} for information on how to
      use the compiler.  This package implements a byte code compiler
      for \R: by default the compiler is not used in this release.  See
      the \sQuote{R Installation and Administration Manual} for how to
      compile the base and recommended packages.

      \item Providing an \code{exportPattern} directive in a NAMESPACE
      file now causes classes to be exported according to the same
      pattern, for example the default from \code{package.skeleton()}
      to specify all names starting with a letter.  An explicit
      directive to \code{exportClassPattern} will still over-ride.

      \item There is an additional marked encoding \code{"bytes"} for
      character strings.  This is intended to be used for non-ASCII
      strings which should be treated as a set of bytes, and never
      re-encoded as if they were in the encoding of the current locale:
      \code{useBytes = TRUE} is automatically selected in functions such
      as \code{writeBin()}, \code{writeLines()}, \code{grep()} and
      \code{strsplit()}.

      Only a few character operations are supported (such as \code{substr()}).

      Printing, \code{format()} and \code{cat()} will represent non-ASCII
      bytes in such strings by a \samp{\xab} escape.

      \item The new function \code{removeSource()} removes the internally
      stored source from a function.

      \item \code{"srcref"} attributes now include two additional
      line number values, recording the line numbers in the order they
      were parsed.

      \item New functions have been added for source reference access:
      \code{getSrcFilename()}, \code{getSrcDirectory()},
      \code{getSrcLocation()} and \code{getSrcref()}.

      \item \code{Sys.chmod()} has an extra argument \code{use_umask}
      which defaults to true and restricts the file mode by the current
      setting of \code{umask}.  This means that all the \R functions
      which manipulate file/directory permissions by default respect
      \code{umask}, notably \command{R CMD INSTALL}.

      \item \code{tempfile()} has an extra argument \code{fileext} to
      create a temporary filename with a specified extension.
      (Suggestion and initial implementation by Dirk Eddelbuettel.)

      There are improvements in the way \code{Sweave()} and
      \code{Stangle()} handle non-ASCII vignette sources, especially in
      a UTF-8 locale: see \sQuote{Writing R Extensions} which now has a
      subsection on this topic.

      \item \code{factanal()} now returns the rotation matrix if a
      rotation such as \code{"promax"} is used, and hence factor
      correlations are displayed.  (Wish of \PR{12754}.)

      \item The \code{gctorture2()} function provides a more refined
      interface to the GC torture process.  Environment variables
      \env{R_GCTORTURE}, \env{R_GCTORTURE_WAIT}, and
      \env{R_GCTORTURE_INHIBIT_RELEASE} can also be used to control the
      GC torture process.

      \item \code{file.copy(from, to)} no longer regards it as an error
      to supply a zero-length \code{from}: it now simply does nothing.

      \item \code{rstandard.glm()} gains a \code{type} argument which can
      be used to request standardized Pearson residuals.

      \item A start on a Turkish translation, thanks to Murat Alkan.

      \item \code{.libPaths()} calls \code{normalizePath(winslash = "/")}
      on the paths: this helps (usually) to present them in a
      user-friendly form and should detect duplicate paths accessed via
      different symbolic links.

      \item \code{download.file()} can be now used with external methods
      even if there are spaces in the URL or the target filename.

      \item The first formal argument of \code{.C()}, \code{.Fortran()},
      \code{.Call()} and \code{.External()} is now referred to a
      \code{.NAME} rather than \code{name}: however actual calls should
      have an unnamed first argument.
    }
  }

  \subsection{SWEAVE CHANGES}{
    \itemize{
      \item \code{Sweave()} has options to produce PNG and JPEG figures,
      and to use a custom function to open a graphics device (see
      \code{?RweaveLatex}).\cr
      (Based in part on the contribution of \PR{14418}.)

      \item The default for \code{Sweave()} is to produce only PDF
      figures (rather than both EPS and PDF).

      \item Environment variable \env{SWEAVE_OPTIONS} can be used to
      supply defaults for existing or new options to be applied after
      the Sweave driver setup has been run.

      \item The Sweave manual is now included as a vignette in the
      \pkg{utils} package.

      \item \code{Sweave()} handles \code{keep.source=TRUE} much better:
      it could duplicate some lines and omit comments. (Reported by John
      Maindonald and others.)
    }
  }

  \subsection{C-LEVEL FACILITIES}{
    \itemize{
      \item Because they use a C99 interface which a C++ compiler is not
      required to support, \code{Rvprintf} and \code{REvprintf} are only
      defined by \file{R_ext/Print.h} in C++ code if the macro
      \code{R_USE_C99_IN_CXX} is defined when it is included.

      \item \code{pythag} duplicated the C99 function \code{hypot}.  It
      is no longer provided, but is used as a substitute for \code{hypot}
      in the very unlikely event that the latter is not available.

      \item \code{R_inspect(obj)} and \code{R_inspect3(obj, deep, pvec)}
      are (hidden) C-level entry points to the internal \code{inspect}
      function and can be used for C-level debugging (e.g., in conjunction
      with the \code{p} command in \code{gdb}).

      \item Compiling R with \option{--enable-strict-barrier} now also
      enables additional checking for use of unprotected objects. In
      combination with \code{gctorture()} or \code{gctorture2()} and a
      C-level debugger this can be useful for tracking down memory
      protection issues.
    }
  }

  \subsection{UTILITIES}{
    \itemize{
      \item \command{R CMD Rdiff} is now implemented in \R on
      Unix-alikes (as it has been on Windows since \R 2.12.0).

      \item \command{R CMD build} no longer does any cleaning in the
      supplied package directory: all the cleaning is done in the copy.

      It has a new option \option{--install-args} to pass arguments to
      \command{R CMD INSTALL} for \option{--build} (but not when
      installing to rebuild vignettes).

      There is new option, \option{--resave-data}, to call
      \code{tools::resaveRdaFiles()} on the \file{data} directory, to
      compress tabular files (\file{.tab}, \file{.csv} etc) and to
      convert \file{.R} files to \file{.rda} files.  The default,
      \option{--resave-data=gzip}, is to do so in a way compatible even
      with years-old versions of \R, but better compression is given by
      \option{--resave-data=best}, requiring \R >= 2.10.0.

      It now adds a \file{datalist} file for \file{data} directories of
      more than 1Mb.

      Patterns in \file{.Rbuildignore} are now also matched against all
      directory names (including those of empty directories).

      There is a new option, \option{--compact-vignettes}, to try
      reducing the size of PDF files in the \file{inst/doc} directory.
      Currently this tries \command{qpdf}: other options may be used in
      future.

      When re-building vignettes and a \file{inst/doc/Makefile} file is
      found, \code{make clean} is run if the makefile has a
      \code{clean:} target.

      After re-building vignettes the default clean-up operation will
      remove any directories (and not just files) created during the
      process: e.g. one package created a \file{.R_cache} directory.

      Empty directories are now removed unless the option
      \option{--keep-empty-dirs} is given (and a few packages do
      deliberately include empty directories).

      If there is a field \code{BuildVignettes} in the package
      \file{DESCRIPTION} file with a false value, re-building the
      vignettes is skipped.

      \item \command{R CMD check} now also checks for filenames that are
      case-insensitive matches to Windows' reserved file names with
      extensions, such as \file{nul.Rd}, as these have caused problems
      on some Windows systems.

      It checks for inefficiently saved \file{data/*.rda} and
      \file{data/*.RData} files, and reports on those large than 100Kb.
      A more complete check (including of the type of compression, but
      potentially much slower) can be switched on by setting environment
      variable \env{_R_CHECK_COMPACT_DATA2_} to \samp{TRUE}.

      The types of files in the \file{data} directory are now checked,
      as packages are \emph{still} misusing it for non-\R data files.

      It now extracts and runs the \R code for each vignette in a
      separate directory and \R process: this is done in the package's
      declared encoding.  Rather than call
      \code{tools::checkVignettes()}, it calls
      \code{tools::buildVignettes()} to see if the vignettes can be
      re-built as they would be by \command{R CMD build}.  Option
      \option{--use-valgrind} now applies only to these runs, and not
      when running code to rebuild the vignettes.  This version does a
      much better job of suppressing output from successful vignette
      tests.

      The \file{00check.log} file is a more complete record of what is
      output to \file{stdout}: in particular contains more details of
      the tests.

      It now checks all syntactically valid Rd usage entries, and warns
      about assignments (unless these give the usage of replacement
      functions).

      \file{.tar.xz} compressed tarballs are now allowed, if
      \command{tar} supports them (and setting environment variable
      \env{TAR} to \samp{internal} ensures so on all platforms).

      \item \command{R CMD check} now warns if it finds
      \file{inst/doc/makefile}, and \command{R CMD build} renames such a
      file to \file{inst/doc/Makefile}.
   }
  }

  \subsection{INSTALLATION}{
    \itemize{
      \item Installing \R no longer tries to find \command{perl}, and
      \command{R CMD} no longer tries to substitute a full path for
      \command{awk} nor \command{perl} -- this was a legacy from the
      days when they were used by \R itself.  Because a couple of
      packages do use \command{awk}, it is set as the \command{make}
      (rather than environment) variable \env{AWK}.

      \item \command{make check} will now fail if there are differences
      from the reference output when testing package examples and if
      environment variable \env{R_STRICT_PACKAGE_CHECK} is set to a true
      value.

      \item The C99 double complex type is now required.

      The C99 complex trigonometric functions (such as \samp{csin}) are
      not currently required (FreeBSD lacks most of them): substitutes
      are used if they are missing.

      \item The C99 system call \samp{va_copy} is now required.

      \item If environment variable \env{R_LD_LIBRARY_PATH} is set
      during configuration (for example in \file{config.site}) it is
      used unchanged in file \file{etc/ldpaths} rather than being
      appended to.

      \item \command{configure} looks for support for OpenMP and if
      found compiles \R with appropriate flags and also makes them
      available for use in packages: see \sQuote{Writing R Extensions}.

      This is currently experimental, and is only used in \R with a
      single thread for \code{colSums()} and \code{colMeans()}.  Expect
      it to be more widely used in later versions of \R.

      This can be disabled by the \option{--disable-openmp} flag.
    }
  }

  \subsection{PACKAGE INSTALLATION}{
    \itemize{
      \item \command{R CMD INSTALL --clean} now removes copies of a
      \file{src} directory which are created when multiple
      sub-architectures are in use.  (Following a comment from
      Berwin Turlach.)

      \item File \file{R.css} is now installed on a per-package basis
      (in the package's \file{html} directory) rather than in each
      library tree, and this is used for all the HTML pages in the
      package.  This helps when installing packages with static HTML
      pages for use on a webserver.  It will also allow future versions
      of \R to use different stylesheets for the packages they install.

      \item A top-level file \file{.Rinstignore} in the package sources
      can list (in the same way as \file{.Rbuildignore}) files under
      \code{inst} that should not be installed.  (Why should there be any
      such files?  Because all the files needed to re-build vignettes
      need to be under \code{inst/doc}, but they may not need to be
      installed.)

      \item \command{R CMD INSTALL} has a new option
      \option{--compact-docs} to compact any PDFs under the
      \file{inst/doc} directory.  Currently this uses \command{qpdf},
      which must be installed (see \sQuote{Writing R Extensions}).

      \item There is a new option \option{--lock} which can be used to
      cancel the effect of \option{--no-lock} or \option{--pkglock} earlier
      on the command line.

      \item Option \option{--pkglock} can now be used with more than one
      package, and is now the default if only one package is specified.

      \item Argument \code{lock} of \code{install.packages()} can now be
      use for Mac binary installs as well as for Windows ones.  The
      value \code{"pkglock"} is now accepted, as well as \code{TRUE} and
      \code{FALSE} (the default).

      \item There is a new option \option{--no-clean-on-error} for
      \command{R CMD INSTALL} to retain a partially installed package for
      forensic analysis.

      \item Packages with names ending in \file{.} are not portable
      since Windows does not work correctly with such directory names.
      This is now warned about in \command{R CMD check}, and will not be
      allowed in \R 2.14.x.

      \item The vignette indices are more comprehensive (in the style of
      \code{browseVignetttes()}).
    }
  }

  \subsection{DEPRECATED & DEFUNCT}{
    \itemize{
      \item \code{require(save = TRUE)} is defunct, and use of the
      \code{save} argument is deprecated.

      \item \command{R CMD check --no-latex} is defunct: use
      \option{--no-manual} instead.

      \item \command{R CMD Sd2Rd} is defunct.

      \item The \code{gamma} argument to \code{hsv()},
      \code{rainbow()}, and \code{rgb2hsv()} is deprecated
      and no longer has any effect.

      \item The previous options for \command{R CMD build --binary}
      (\option{--auto-zip}, \option{--use-zip-data} and
      \option{--no-docs}) are deprecated (or defunct):
      use the new option \option{--install-args} instead.

      \item When a character value is used for the \code{EXPR}
      argument in \code{switch()}, only a single unnamed alternative
      value is now allowed.

      \item The wrapper \code{utils::link.html.help()} is no longer
      available.

      \item Zip-ing data sets in packages (and hence \command{R CMD
	INSTALL} options \option{--use-zip-data} and \option{--auto-zip}, as
      well as the \samp{ZipData: yes} field in a \code{DESCRIPTION} file)
      is defunct.

      Installed packages with zip-ed data sets can still be used, but a
      warning that they should be re-installed will be given.

      \item The \sQuote{experimental} alternative specification of a
      namespace via \code{.Export()} etc is now defunct.

      \item The option \option{--unsafe} to \command{R CMD INSTALL} is
      deprecated: use the identical option \option{--no-lock} instead.

      \item The entry point \code{pythag} in \file{Rmath.h} is deprecated
      in favour of the C99 function \code{hypot}.  A wrapper for
      \code{hypot} is provided for \R 2.13.x only.

      \item Direct access to the \code{"source"} attribute of functions
      is deprecated; use \code{deparse(fn, control="useSource")} to access
      it, and \code{removeSource(fn)} to remove it.

      \item \command{R CMD build --binary} is now formally deprecated:
      \command{R CMD INSTALL --build} has long been the preferred
      alternative.

      \item Single-character package names are deprecated (and \pkg{R}
      is already disallowed to avoid confusion in \samp{Depends:}
      fields).
    }
  }

  \subsection{BUG FIXES}{
    \itemize{
      \item \code{drop.terms} and the \code{[} method for class
      \code{"terms"} no longer add back an intercept.  (Reported by
      Niels Hansen.)

      \item \code{aggregate} preserves the class of a column (e.g. a
      date) under some circumstances where it discarded the class
      previously.

      \item \code{p.adjust()} now always returns a vector result, as
      documented.  In previous versions it copied attributes (such as
      dimensions) from the \code{p} argument: now it only copies names.

      \item On PDF and PostScript devices, a line width of zero was
      recorded verbatim and this caused problems for some viewers
      (a very thin line combined with a non-solid line dash pattern
      could also cause a problem).  On these devices, the line width is
      now limited at 0.01 and for very thin lines with complex dash
      patterns the device may force the line dash pattern to be solid.
      (Reported by Jari Oksanen.)

      \item The \code{str()} method for class \code{"POSIXt"} now gives
      sensible output for 0-length input.

      \item The one- and two-argument complex maths functions failed to
      warn if NAs were generated (as their numeric analogues do).

      \item Added \code{.requireCachedGenerics} to the \code{dont.mind}
      list for \code{library()} to avoid warnings about duplicates.

      \item \code{$<-.data.frame} messed with the class attribute,
      breaking any S4 subclass.  The S4 \code{data.frame} class now has its own
      \code{$<-} method, and turns dispatch on for this primitive.

      \item \code{Map()} did not look up a character argument \code{f}
      in the correct frame, thanks to lazy evaluation.  (\PR{14495})

      \item \code{file.copy()} did not tilde-expand \code{from} and \code{to}
      when \code{to} was a directory.  (\PR{14507})

      \item It was possible (but very rare) for the loading test in
      \command{R CMD INSTALL} to crash a child \R process and so leave
      around a lock directory and a partially installed package.  That
      test is now done in a separate process.

      \item \code{plot(<formula>, data=<matrix>,..)} now works in more
      cases; similarly for \code{points()}, \code{lines()} and
      \code{text()}.

      \item \code{edit.default()} contained a manual dispatch for matrices
      (the \code{"matrix"} class didn't really exist when it was written).
      This caused an infinite recursion in the no-GUI case and has now
      been removed.

      \item \code{data.frame(check.rows = TRUE)} sometimes worked when
      it should have detected an error.  (\PR{14530})

      \item \code{scan(sep= , strip.white=TRUE)} sometimes stripped
      trailing spaces from within quoted strings.  (The real bug in
      \PR{14522}.)

      \item The rank-correlation methods for \code{cor()} and
      \code{cov()} with \code{use = "complete.obs"} computed the ranks
      before removing missing values, whereas the documentation implied
      incomplete cases were removed first.  (\PR{14488})

      They also failed for 1-row matrices.

      \item The perpendicular adjustment used in placing text and
      expressions in the margins of plots was not scaled by
      \code{par("mex")}. (Part of \PR{14532}.)

      \item Quartz Cocoa device now catches any Cocoa exceptions that
      occur during the creation of the device window to prevent crashes.
      It also imposes a limit of 144 ft^2 on the area used by a window
      to catch user errors (unit misinterpretation) early.

      \item The browser (invoked by \code{debug()}, \code{browser()}
      or otherwise) would display attributes such as \code{"wholeSrcref"}
      that were intended for internal use only.

      \item R's internal filename completion now properly handles
      filenames with spaces in them even when the readline library is
      used.  This resolves \PR{14452} provided the internal filename
      completion is used (e.g., by setting \code{rc.settings(files =
	TRUE)}).

      \item Inside \code{uniroot(f, ...)}, \code{-Inf} function values
      are now replaced by a maximally \bold{negative} value.

      \item \code{rowsum()} could silently over/underflow on integer
      inputs (reported by Bill Dunlap).

      \item \code{as.matrix()} did not handle \code{"dist"} objects with
      zero rows.
    }
  }
}

\section{\Rlogo CHANGES IN R VERSION 2.12.2 patched}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item \code{max()} and \code{min()} work harder to ensure that
      \code{NA} has precedence over \code{NaN}, so e.g. \code{min(NaN,
	NA)} is \code{NA}.  (This was not previously documented except
      for within a single numeric vector, where compiler optimizations
      often defeated the code.)
    }
  }

  \subsection{BUG FIXES}{
    \itemize{
      \item A change to the C function \samp{R_tryEval} had broken error
      messages in S4 method selection; the error message is now printed.

      \item PDF output with a non-RGB color model used RGB for the line
      stroke color.  (\PR{14511})

      \item \code{stats4::BIC()} assumed without checking that an object
      of class \code{"logLik"} has an \code{"nobs"} attribute:
      \code{glm()} fits did not and so \code{BIC()} failed for them.

      \item In some circumstances a one-sided \code{mantelhaen.test()}
      reported the p-value for the wrong tail.  (\PR{14514})

      \item Passing the invalid value \code{lty = NULL} to \code{axis()}
      sent an invalid value to the graphics device, and might cause the
      device to segfault.

      \item \code{Sweave()} with \code{concordance=TRUE} could lead to
      invalid PDF files; \file{Sweave.sty} has been updated to avoid
      this.

      \item Non-ASCII characters in the titles of help pages were
      not rendered properly in some locales, and could cause errors
      or warnings.

      \item \code{checkRd()} gave a spurious error if the \code{\\href}
      macro was used.
    }
  }
}

\section{\Rlogo CHANGES IN R VERSION 2.12.2}{
  \subsection{SIGNIFICANT USER-VISIBLE CHANGES}{
    \itemize{
      \item Complex arithmetic (notably \code{z^n} for complex \code{z}
      and integer \code{n}) gave incorrect results since \R 2.10.0 on
      platforms without C99 complex support.  This and some
      lesser issues in trigonometric functions have been corrected.

      Such platforms were rare (we know of Cygwin and FreeBSD).
      However, because of new compiler optimizations in the way complex
      arguments are handled, the same code was selected on x86_64 Linux
      with \command{gcc 4.5.x} at the default \code{-O2} optimization
      (but not at \code{-O}).

      \item There is a workaround for crashes seen with several packages
      on systems using \samp{zlib 1.2.5}: see the INSTALLATION section.
    }
  }

  \subsection{NEW FEATURES}{
    \itemize{
      \item PCRE has been updated to 8.12 (two bug-fix releases since 8.10).

      \item \code{rep()}, \code{seq()}, \code{seq.int()} and
      \code{seq_len()} report more often when the first element is taken
      of an argument of incorrect length.

      \item The Cocoa back-end for the \code{quartz()} graphics device
      on Mac OS X provides a way to disable event loop processing
      temporarily (useful, e.g., for forked instances of \R).

      \item \code{kernel()}'s default for \code{m} was not appropriate
      if \code{coef} was a set of coefficients.  (Reported by Pierre
      Chausse.)

      \item \code{bug.report()} has been updated for the current \R bug
      tracker, which does not accept emailed submissions.

      \item \command{R CMD check} now checks for the correct use of
      \samp{$(LAPACK_LIBS)} (as well as \samp{$(BLAS_LIBS)}), since
      several CRAN recent submissions have ignored \sQuote{Writing R
	Extensions}.
    }
  }

  \subsection{INSTALLATION}{
    \itemize{
      \item The \samp{zlib} sources in the distribution are now built
      with all symbols remapped: this is intended to avoid problems seen
      with packages such as \CRANpkg{XML} and \CRANpkg{rggobi} which link to
      \file{zlib.so.1} on systems using \samp{zlib 1.2.5}.

      \item The default for \env{FFLAGS} and \env{FCFLAGS} with
      \command{gfortran} on x86_64 Linux has been changed back to \samp{-g
	-O2}:  however, setting \samp{-g -O} may still be needed for
      \command{gfortran 4.3.x}.
    }
  }

  \subsection{PACKAGE INSTALLATION}{
    \itemize{
      \item A \samp{LazyDataCompression} field in the \file{DESCRIPTION}
      file will be used to set the value for the
      \option{--data-compress} option of \command{R CMD INSTALL}.

      \item Files \file{R/sysdata.rda} of more than 1Mb are now stored
      in the lazyload database using \command{xz} compression: this for
      example halves the installed size of package \CRANpkg{Imap}.

      \item \command{R CMD INSTALL} now ensures that directories
      installed from \file{inst} have search permission for everyone.

      It no longer installs files \file{inst/doc/Rplots.ps} and
      \file{inst/doc/Rplots.pdf}.  These are almost certainly left-overs
      from \code{Sweave} runs, and are often large.
    }
  }

  \subsection{DEPRECATED & DEFUNCT}{
    \itemize{
      \item The \sQuote{experimental} alternative specification of a
      namespace via \code{.Export()} etc is now deprecated.

      \item \code{zip.file.extract()} is now deprecated.

      \item Zip-ing data sets in packages (and hence \command{R CMD
	INSTALL --use-zip-data} and the \samp{ZipData: yes} field in a
	\code{DESCRIPTION} file) is deprecated: using efficiently
	compressed \file{.rda} images and lazy-loading of data has
	superseded it.
    }
  }

  \subsection{BUG FIXES}{
    \itemize{
      \item \code{identical()} could in rare cases generate a warning
      about non-pairlist attributes on CHARSXPs.  As these are used for
      internal purposes, the attribute check should be skipped.
      (Reported by Niels Richard Hansen).

      \item If the filename extension (usually \file{.Rnw}) was not
      included in a call to \code{Sweave()}, source references would not
      work properly and the \code{keep.source} option failed.  (\PR{14459})

      \item \code{format.data.frame()} now keeps zero character column names.

      \item \code{pretty(x)} no longer raises an error when \code{x}
      contains solely non-finite values. (\PR{14468})

      \item The \code{plot.TukeyHSD()} function now uses a line width of
      0.5 for its reference lines rather than \samp{lwd = 0} (which
      caused problems for some PDF and PostScript viewers).

      \item The \code{big.mark} argument to \code{prettyNum()},
      \code{format()}, etc. was inserted reversed if it was more than
      one character long.

      \item \command{R CMD check} failed to check the filenames under
      \file{man} for Windows' reserved names.

      \item The \code{"Date"} and \code{"POSIXt"} methods for
      \code{seq()} could overshoot when \code{to} was supplied and
      \code{by} was specified in months or years.

      \item The internal method of \code{untar()} now restores hard
      links as file copies rather than symbolic links (which did not
      work for cross-directory links).

      \item \code{unzip()} did not handle zip files which contained
      filepaths with two or more leading directories which were not in
      the zipfile and did not already exist.  (It is unclear if such
      zipfiles are valid and the third-party C code used did not support
      them, but \PR{14462} created one.)

      \item \code{combn(n, m)} now behaves more regularly for the border
      case \eqn{m = 0}. (\PR{14473})

      \item The rendering of numbers in plotmath expressions (e.g.
      \code{expression(10^2)}) used the current settings for conversion
      to strings rather than setting the defaults, and so could be
      affected by what has been done before.  (\PR{14477})

      \item The methods of \code{napredict()} and \code{naresid()} for
      \code{na.action = na.exclude} fits did not work correctly in the
      very rare event that every case had been omitted in the fit.
      (Reported by Simon Wood.)

      \item \code{weighted.residuals(drop0=TRUE)} returned a vector when
      the residuals were a matrix (e.g. those of class \code{"mlm"}).
      (Reported by Bill Dunlap.)

      \item Package HTML index files \file{<pkg>/html/00Index.html} were
      generated with a stylesheet reference that was not correct for
      static browsing in libraries.

      \item \code{ccf(na.action = na.pass)} was not implemented.

      \item The parser accepted some incorrect numeric constants,
      e.g. \code{20x2}.  (Reported by Olaf Mersmann.)

      \item \code{format(*, zero.print)} did not always replace the full
      zero parts.

      \item Fixes for subsetting or subassignment of \code{"raster"}
      objects when not both \code{i} and \code{j} are specified.

      \item \command{R CMD INSTALL} was not always respecting the
      \samp{ZipData: yes} field of a \file{DESCRIPTION} file (although
      this is frequently incorrectly specified for packages with no data
      or which specify lazy-loading of data).

      \command{R CMD INSTALL --use-zip-data} was incorrectly implemented
      as \option{--use-zipdata} since \R 2.9.0.

      \item \command{source(file, echo=TRUE)} could fail if the file
      contained \samp{#line} directives.  It now recovers more gracefully, but
      may still display the wrong line if the directive gives incorrect
      information.

      \item \code{atan(1i)} returned \code{NaN+Infi} (rather than
      \code{0+Infi}) on platforms without C99 complex support.

      \item \code{library()} failed to cache S4 metadata (unlike
      \code{loadNamespace()}) causing failures in S4-using packages
      without a namespace (e.g. those using reference classes).

      \item The function \code{qlogis(lp, log.p=TRUE)} no longer
      prematurely overflows to \code{Inf} when exp(lp) is close to 1.

      \item Updating S4 methods for a group generic function requires
      resetting the methods tables for the members of the group (patch
      contributed by Martin Morgan).

      \item In some circumstances (including for package \CRANpkg{XML}),
      \command{R CMD INSTALL} installed version-control directories from
      source packages.

      \item Added \code{PROTECT} calls to some constructed expressions
      used in C level \code{eval} calls.

      \item \code{utils:::create.post()} (used by \code{bug.report()}
      and \code{help.request()}) failed to quote arguments to the
      mailer, and so often failed.

      \item \code{bug.report()} was naive about how to extract
      maintainer email addresses from package descriptions, so would
      often try mailing to incorrect addresses.

      \item \code{debugger()} could fail to read the environment of a
      call to a function with a \code{\dots} argument.  (Reported by
      Charlie Roosen.)

      \item \code{prettyNum(c(1i, NA), drop0=TRUE)} or
      \code{str(NA_complex_)} now work correctly.
    }
  }
}

\section{\Rlogo CHANGES IN R VERSION 2.12.1}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item The DVI/PDF reference manual now includes the help pages for
      all the standard packages: \pkg{splines}, \pkg{stats4} and
      \pkg{tcltk} were previously omitted (intentionally).

      \item \url{http://www.rforge.net} has been added to the default
      set of repositories known to \code{setRepositories()}.

      \item xz-utils has been updated to version 5.0.0.

      \item \code{reshape()} now makes use of \code{sep} when forming
      names during reshaping to wide format.  (\PR{14435})

      \item \code{legend()} allows the length of lines to be set by the
      end user \emph{via} the new argument \code{seg.len}.

      \item New S4 reference class utility methods \code{copy()},
      \code{field()}, \code{getRefClass()} and \code{getClass()} have
      been added to package \pkg{methods}.

      \item When a character value is used for the \code{EXPR}
      argument in \code{switch()}, a warning is given if more than
      one unnamed alternative value is given.  This will become an
      error in \R 2.13.0.

      \item \code{StructTS(type = "BSM")} now allows series with just
      two seasons.  (Reported by Birgit Erni.)
    }
  }

  \subsection{INSTALLATION}{
    \itemize{
      \item The PDF reference manual is now built as PDF version 1.5
      with object compression, which on platforms for which this is not
      the default (notably MiKTeX) halves its size.

      \item Variable \env{FCLIBS} can be set during configuration, for
      any additional library flags needed when linking a shared object
      with the Fortran 9x compiler.  (Needed with Solaris Studio 12.2.)
    }
  }

  \subsection{BUG FIXES}{
    \itemize{
      \item \code{seq.int()} no longer sometimes evaluates arguments
      twice.  (\PR{14388})

      \item The \code{data.frame} method of \code{format()} failed if a
      column name was longer than 256 bytes (the maximum length allowed
      for an \R name).

      \item \code{predict(<lm object>, type ="terms", ...)} failed if
      both \code{terms} and \code{interval} were specified.  (Reported
      by Bill Dunlap.)

      Also, if \code{se.fit = TRUE} the standard errors were reported
      for all terms, not just those selected by a non-null \code{terms}.

      \item The TRE regular expressions engine could terminate \R rather
      than give an error when given certain invalid regular expressions.
      (\PR{14398})

      \item \code{cmdscale(eig = TRUE)} was documented to return \eqn{n-1}
      eigenvalues but in fact only returned \code{k}.  It now returns
      all \eqn{n} eigenvalues.

      \code{cmdscale(add = TRUE)} failed to centre the return
      configuration and sometimes lost the labels on the points.  Its
      return value was described wrongly (it is always a list and
      contains component \code{ac}).

      \item \code{promptClass()} in package \pkg{methods} now works for
      reference classes and gives a suitably specialized skeleton of
      documentation.

      Also, \code{callSuper()} now works via the \code{methods()}
      invocation as well as for initially specified methods.

      \item \code{download.file()} could leave the destination file open
      if the URL was not able to be opened.  (\PR{14414})

      \item Assignment of an environment to functions or as an attribute
      to other objects now works for S4 subclasses of \code{"environment"}.

      \item Use of \samp{[[<-} for S4 subclasses of
      \code{"environment"} generated an infinite recursion from the
      method.  The method has been replaced by internal code.

      \item In a reference class S4 method, \code{callSuper()} now works in
      \code{initialize()} methods when there is no explicit superclass
      method.

      \item \samp{!} dropped attributes such as names and dimensions from a
      length-zero argument.  (\PR{14424})

      \item When \code{list2env()} created an environment it was missing
      a \code{PROTECT} call and so was vulnerable to garbage collection.

      \item \code{Sweave()} with \code{keep.source=TRUE} dropped
      comments at the start and end of code chunks.  It could also
      fail when \samp{\SweaveInput} was combined with named chunks.

      \item The Fortran code used by \code{nls(algorithm = "port")}
      could infinite-loop when compiled with high optimization on a
      modern version of \code{gcc}, and \code{SAFE_FFLAGS} is now used
      to make this less likely.  (\PR{14427}, seen with 32-bit Windows
      using \code{gcc 4.5.0} used from \R 2.12.0.)

      \item \code{sapply()} with default \code{simplify = TRUE} and
      \code{mapply()} with default \code{SIMPLIFY = TRUE} wrongly
      simplified language-like results, as, e.g., in
      \code{mapply(1:2, c(3,7), FUN = function(i,j) call(':',i,j))}.

      \item Backreferences to undefined patterns in
      \code{[g]sub(pcre = TRUE)} could cause a segfault.  (\PR{14431})

      \item The \code{format()} (and hence the \code{print()}) method
      for class \code{"Date"} rounded fractional dates towards zero: it
      now always rounds them down.

      \item Reference S4 class creation could generate ambiguous
      inheritance patterns under very special circumstances.

      \item \samp{[[<-} turned S4 subclasses of \code{"environment"} into
      plain environments.

      \item Long titles for help pages were truncated in package indices
      and a few other places.

      \item Additional utilities now work correctly with S4 subclasses of
      \code{"environment"} (\code{rm}, locking tools and active
      bindings).

      \item \code{spec.ar()} now also work for the \code{"ols"} method.
      (Reported by Hans-Ruedi Kuensch.)

      \item The initialization of objects from S4 subclasses of
      \code{"environment"} now allocates a new environment object.

      \item \command{R CMD check} has more protection against (probably
      erroneous) example or test output which is invalid in the current
      locale.

      \item \code{qr.X()} with column names and pivoting now also pivots
      the column names.  (\PR{14438})

      \item \code{unit.pmax()} and \code{unit.pmin()} in package
      \pkg{grid} gave incorrect results when all inputs were of length 1.
      (\PR{14443})

      \item The parser for \file{NAMESPACE} files ignored misspelled
      directives, rather than signalling an error.  For 2.12.x a warning
      will be issued, but this will be correctly reported as an error
      in later releases. (Reported by Charles Berry.)

      \item Fix for subsetting of \code{"raster"} objects when only one
      of \code{i} or \code{j} is specified.

      \item \code{grid.raster()} in package \pkg{grid} did not accept
      \code{"nativeRaster"} objects (like \code{rasterImage()} does).

      \item Rendering raster images in PDF output was resetting the
      clipping region.

      \item Rendering of raster images on cairo X11 device was wrong,
      particularly when a small image was being scaled up using
      interpolation.

      With cairo < 1.6, will be better than before, though still a
      little clunky.  With cairo >= 1.6, should be sweet as.

      \item Several bugs fixed in \code{read.DIF()}:  single column
      inputs caused errors, cells marked as \code{"character"} could
      be converted to other types, and (in Windows) copying from the
      clipboard failed.
    }
  }
}

\section{\Rlogo CHANGES IN R VERSION 2.12.0}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item Reading a package's \file{CITATION} file now defaults to
       ASCII rather than Latin-1: a package with a non-ASCII
       \file{CITATION} file should declare an encoding in its
       \file{DESCRIPTION} file and use that encoding for the
       \file{CITATION} file.

      \item \code{difftime()} now defaults to the \code{"tzone"}
       attribute of \code{"POSIXlt"} objects rather than to the current
       timezone as set by the default for the \code{tz} argument.  (Wish
       of \PR{14182}.)

      \item \code{pretty()} is now generic, with new methods for
       \code{"Date"} and \code{"POSIXt"} classes (based on code
       contributed by Felix Andrews).

      \item \code{unique()} and \code{match()} are now faster on
       character vectors where all elements are in the global \code{CHARSXP}
       cache and have unmarked encoding (ASCII).  Thanks to Matthew
       Dowle for suggesting improvements to the way the hash code is
       generated in \file{unique.c}.

      \item The \code{enquote()} utility, in use internally, is exported now.

      \item \code{.C()} and \code{.Fortran()} now map non-zero return
       values (other than \code{NA_LOGICAL}) for logical vectors to
       \code{TRUE}: it has been an implicit assumption that they are
       treated as true.

      \item The \code{print()} methods for \code{"glm"} and \code{"lm"}
       objects now insert linebreaks in long calls in the same way that
       the \code{print()} methods for \code{"summary.[g]lm"} objects
       have long done.  This does change the layout of the examples for
       a number of packages, e.g. \CRANpkg{MASS}.  (\PR{14250})

      \item \code{constrOptim()} can now be used with method
      \code{"SANN"}.  (\PR{14245})

       It gains an argument \code{hessian} to be passed to
       \code{optim()}, which allows all the \code{...} arguments to be
       intended for \code{f()} and \code{grad()}.  (\PR{14071})

      \item \code{curve()} now allows \code{expr} to be an object of
       mode \code{"expression"} as well as \code{"call"} and
       \code{"function"}.

      \item The \code{"POSIX[cl]t"} methods for \code{Axis()} have been
       replaced by a single method for \code{"POSIXt"}.

       There are no longer separate \code{plot()} methods for
       \code{"POSIX[cl]t"} and \code{"Date"}: the default method has
       been able to handle those classes for a long time.  This
       \emph{inter alia} allows a single date-time object to be
       supplied, the wish of \PR{14016}.

       The methods had a different default (\code{""}) for \code{xlab}.

      \item Classes \code{"POSIXct"}, \code{"POSIXlt"} and
       \code{"difftime"} have generators \code{.POSIXct()},
       \code{.POSIXlt()} and \code{.difftime()}.  Package authors are
       advised to make use of them (they are available from \R 2.11.0) to
       proof against planned future changes to the classes.

       The ordering of the classes has been changed, so \code{"POSIXt"}
       is now the second class.  See the document \sQuote{Updating
       packages for changes in R 2.12.x} on
       \url{http://developer.r-project.org} for the consequences for a
       handful of CRAN packages.

      \item The \code{"POSIXct"} method of \code{as.Date()} allows a
       timezone to be specified (but still defaults to UTC).

      \item New \code{list2env()} utility function as an inverse of
       \code{as.list(<environment>)} and for fast multi-\code{assign()}
       to existing environment.  \code{as.environment()} is now generic
       and uses \code{list2env()} as list method.

      \item There are several small changes to output which \sQuote{zap}
       small numbers, e.g. in printing quantiles of residuals in
       summaries from \code{"lm"} and \code{"glm"} fits, and in test
       statistics in \code{print.anova()}.

      \item Special names such as \code{"dim"}, \code{"names"}, etc, are
       now allowed as slot names of S4 classes, with \code{"class"} the
       only remaining exception.

      \item File \file{.Renviron} can have architecture-specific versions
       such as \file{.Renviron.i386} on systems with sub-architectures.

      \item \code{installed.packages()} has a new argument
       \code{subarch} to filter on sub-architecture.

      \item The \code{summary()} method for \code{packageStatus()} now
       has a separate \code{print()} method.

      \item The default \code{summary()} method returns an object
       inheriting from class \code{"summaryDefault"} which has a
       separate \code{print()} method that calls \code{zapsmall()} for
       numeric/complex values.

      \item The startup message now includes the platform and if used,
       sub-architecture: this is useful where different
       (sub-)architectures run on the same OS.

      \item The \code{getGraphicsEvent()} mechanism now allows multiple windows
       to return graphics events, through the new functions
       \code{setGraphicsEventHandlers()}, \code{setGraphicsEventEnv()}, and
       \code{getGraphicsEventEnv()}.  (Currently implemented in the
       \code{windows()} and \code{X11()} devices.)

      \item \code{tools::texi2dvi()} gains an \code{index} argument,
       mainly for use by \command{R CMD Rd2pdf}.

       It avoids the use of \command{texindy} by \command{texinfo}'s
       \command{texi2dvi >= 1.157}, since that does not emulate
       'makeindex' well enough to avoid problems with special characters
       (such as \samp{(}, \samp{\{}, \samp{!}) in indices.

      \item The ability of \code{readLines()} and \code{scan()} to
       re-encode inputs to marked UTF-8 strings on Windows since \R 2.7.0
       is extended to non-UTF-8 locales on other OSes.

      \item \code{scan()} gains a \code{fileEncoding} argument to
      match \code{read.table()}.

      \item \code{points()} and \code{lines()} gain \code{"table"}
       methods to match \code{plot()}.  (Wish of \PR{10472}.)

      \item \code{Sys.chmod()} allows argument \code{mode} to be a
      vector, recycled along \code{paths}.

      \item There are \code{|}, \code{&} and \code{xor()} methods for
       classes \code{"octmode"} and \code{"hexmode"}, which work
       bitwise.

      \item Environment variables \env{R_DVIPSCMD}, \env{R_LATEXCMD},
       \env{R_MAKEINDEXCMD}, \env{R_PDFLATEXCMD} are no longer used
       nor set in an \R session.  (With the move to
       \code{tools::texi2dvi()}, the conventional environment variables
       \env{LATEX}, \env{MAKEINDEX} and \env{PDFLATEX} will be used.
       \code{options("dvipscmd")} defaults to the value of \env{DVIPS},
       then to \code{"dvips"}.)

      \item New function \code{isatty()} to see if terminal connections are
       redirected.

      \item \code{summaryRprof()} returns the sampling interval in
       component \code{sample.interval} and only returns in
       \code{by.self} data for functions with non-zero self times.

      \item \code{print(x)} and \code{str(x)} now indicate if an empty
      list \code{x} is named.

      \item \code{install.packages()} and \code{remove.packages()} with
       \code{lib} unspecified and multiple libraries in
       \code{.libPaths()} inform the user of the library location used
       with a message rather than a warning.

      \item There is limited support for multiple compressed streams on a
       file: all of \code{[bgx]zfile()} allow streams to be appended to an
       existing file, but \code{bzfile()} reads only the first stream.

      \item Function \code{person()} in package \pkg{utils} now uses
        a given/family scheme in preference to first/middle/last, is
        vectorized to handle an arbitrary number of persons, and gains a
        \code{role} argument to specify person roles using a controlled
        vocabulary (the MARC relator terms).

      \item Package \pkg{utils} adds a new \code{"bibentry"} class
        for representing and manipulating bibliographic information in
        enhanced BibTeX style, unifying and enhancing the previously
        existing mechanisms.

      \item A \code{bibstyle()} function has been added to the
       \pkg{tools} package with default JSS style for rendering
       \code{"bibentry"} objects, and a mechanism for registering other
       rendering styles.

      \item Several aspects of the display of text help are now
       customizable using the new \code{Rd2txt_options()} function.
       options(\code{"help_text_width"}) is no longer used.

      \item Added \samp{\href} tag to the Rd format, to allow
       hyperlinks to URLs without displaying the full URL.

      \item Added \samp{\newcommand} and \samp{\renewcommand} tags
       to the Rd format, to allow user-defined macros.

      \item New \code{toRd()} generic in the \pkg{tools} package to
       convert objects to fragments of Rd code, and added
       \code{"fragment"} argument to \code{Rd2txt()}, \code{Rd2HTML()},
       and \code{Rd2latex()} to support it.

      \item Directory \file{R_HOME/share/texmf} now follows the TDS
       conventions, so can be set as a \code{texmf} tree (\sQuote{root
       directory} in MiKTeX parlance).

      \item S3 generic functions now use correct S4 inheritance when
      dispatching on an S4 object.  See \code{?Methods}, section on
      \dQuote{Methods for S3 Generic Functions} for recommendations
      and details.

      \item \code{format.pval()} gains a \code{...} argument to pass
       arguments such as \code{nsmall} to \code{format()}.  (Wish of
       \PR{9574})

      \item \code{legend()} supports \code{title.adj}.  (Wish of
      \PR{13415})

      \item Added support for subsetting \code{"raster"} objects, plus
      assigning to a subset, conversion to a matrix (of colour
      strings), and comparisons (\samp{==} and \samp{!=}).

      \item Added a new \code{parseLatex()} function (and related
      functions \code{deparseLatex()} and \code{latexToUtf8()}) to
      support conversion of bibliographic entries for display in R.

      \item Text rendering of \samp{\itemize} in help uses a Unicode
      bullet in UTF-8 and most single-byte Windows locales.

      \item Added support for polygons with holes to the graphics
      engine.  This is implemented for the \code{pdf()},
      \code{postscript()}, \code{x11(type="cairo")}, \code{windows()},
      and \code{quartz()} devices (and associated raster formats),
      but not for \code{x11(type="Xlib")} or \code{xfig()} or
      \code{pictex()}.  The user-level interface is the
      \code{polypath()} function in \pkg{graphics} and
      \code{grid.path()} in \pkg{grid}.

      \item File \file{NEWS} is now generated at installation with a
      slightly different format: it will be in UTF-8 on platforms using
      UTF-8, and otherwise in ASCII.  There is also a PDF version,
      \file{NEWS.pdf}, installed at the top-level of the R distribution.

      \item \code{kmeans(x, 1)} now works.  Further, \code{kmeans} now
      returns between and total sum of squares.

      \item \code{arrayInd()} and \code{which()} gain an argument
      \code{useNames}.  For \code{arrayInd}, the default is now false,
      for speed reasons.

      \item As is done for closures, the default \code{print} method for
      the formula class now displays the associated environment if it is
      not the global environment.

      \item A new facility has been added for inserting code into a
      package without re-installing it, to facilitate testing
      changes which can be selectively added and backed out.  See
      \code{?insertSource}.

      \item New function \code{readRenviron} to (re-)read files in the
      format of \file{~/.Renviron} and \file{Renviron.site}.

      \item \code{require()} will now return \code{FALSE} (and not fail)
      if loading the package or one of its dependencies fails.

      \item \code{aperm()} now allows argument \code{perm} to be a
      character vector when the array has named dimnames (as the results
      of \code{table()} calls do).  Similarly, \code{array()} allows
      \code{MARGIN} to be a character vector.  (Based on suggestions of
      Michael Lachmann.)

      \item Package \pkg{utils} now exports and documents functions
      \code{aspell_package_Rd_files()} and
      \code{aspell_package_vignettes()} for spell checking package Rd
      files and vignettes using Aspell, Ispell or Hunspell.

      \item Package news can now be given in Rd format, and
      \code{news()} prefers these \file{inst/NEWS.Rd} files to old-style
      plain text \file{NEWS} or \file{inst/NEWS} files.

      \item New simple function \code{packageVersion()}.

      \item The PCRE library has been updated to version 8.10.

      \item The standard Unix-alike terminal interface declares its name
      to readline as 'R', so that can be used for conditional sections
      in \file{~/.inputrc} files.

      \item \sQuote{Writing R Extensions} now stresses that the standard
      sections in \file{.Rd} files (other than \samp{\alias},
      \samp{\keyword} and \samp{\note}) are intended to be unique, and
      the conversion tools now drop duplicates with a warning.

      The \file{.Rd} conversion tools also warn about an unrecognized type
      in a \samp{\docType} section.

      \item \code{ecdf()} objects now have a \code{quantile()} method.

      \item \code{format()} methods for date-time objects now attempt to make
      use of a \code{"tzone"} attribute with \code{"\%Z"} and
      \code{"\%z"} formats, but it is not always possible.  (Wish of
      \PR{14358}.)

      \item \code{tools::texi2dvi(file, clean = TRUE)} now works in more
      cases (e.g. where emulation is used and when \file{file} is not in
      the current directory).

      \item New function \code{droplevels()} to remove unused factor levels.

      \item \code{system(command, intern = TRUE)} now gives an error on
      a Unix-alike (as well as on Windows) if \code{command} cannot be
      run.  It reports a non-success exit status from running
      \code{command} as a warning.

      On a Unix-alike an attempt is made to return the actual exit
      status of the command in \code{system(intern = FALSE)}: previously
      this had been system-dependent but on POSIX-compliant systems the
      value return was 256 times the status.

      \item \code{system()} has a new argument \code{ignore.stdout}
      which can be used to (portably) ignore standard output.

      \item \code{system(intern = TRUE)} and \code{pipe()} connections
      are guaranteed to be available on all builds of \R.

      \item \code{Sys.which()} has been altered to return \code{""} if
      the command is not found (even on Solaris).

      \item A facility for defining reference-based S4 classes (in the
      OOP style of Java, C++, etc.) has been added experimentally to
      package \pkg{methods}; see \code{?ReferenceClasses}.

      \item The \code{predict} method for \code{"loess"} fits gains an
      \code{na.action} argument which defaults to \code{na.pass} rather
      than the previous default of \code{na.omit}.

      Predictions from \code{"loess"} fits are now named from the row
      names of \code{newdata}.

      \item Parsing errors detected during \code{Sweave()} processing
      will now be reported referencing their original location in the
      source file.

      \item New \code{adjustcolor()} utility, e.g., for simple
      translucent color schemes.

      \item \code{qr()} now has a trivial \code{lm} method with a
      simple (fast) validity check.

      \item An experimental new programming model has been added to
      package \pkg{methods} for reference (OOP-style) classes and
      methods.  See \code{?ReferenceClasses}.

      \item bzip2 has been updated to version 1.0.6 (bug-fix release).
      \option{--with-system-bzlib} now requires at least version 1.0.6.

      \item R now provides \file{jss.cls} and \file{jss.bst} (the class
      and bib style file for the Journal of Statistical Software) as
      well as \file{RJournal.bib} and \file{Rnews.bib}, and
      \command{R CMD} ensures that the \file{.bst} and \file{.bib} files
      are found by BibTeX.

      \item Functions using the \env{TAR} environment variable no longer
      quote the value when making \code{system} calls.  This allows
      values such as \samp{tar --force-local}, but does require
      additional quotes in, e.g., \code{TAR = "'/path with spaces/mytar'"}.
    }
  }

  \subsection{DEPRECATED & DEFUNCT}{
    \itemize{
      \item Supplying the parser with a character string containing both
       octal/hex and Unicode escapes is now an error.

      \item File extension \file{.C} for C++ code files in packages is
      now defunct.

      \item \command{R CMD check} no longer supports configuration
      files containing Perl configuration variables: use the environment
      variables documented in \sQuote{R Internals} instead.

      \item The \code{save} argument of \code{require()} now defaults to
      \code{FALSE} and \code{save = TRUE} is now deprecated.  (This
      facility is very rarely actually used, and was superseded by the
      \samp{Depends} field of the \file{DESCRIPTION} file long ago.)

      \item \command{R CMD check --no-latex} is deprecated in favour of
      \option{--no-manual}.

      \item \command{R CMD Sd2Rd} is formally deprecated and will be
      removed in \R 2.13.0.
    }
  }
  \subsection{PACKAGE INSTALLATION}{
    \itemize{
      \item \code{install.packages()} has a new argument
       \code{libs_only} to optionally pass \option{--libs-only} to
       \command{R CMD INSTALL} and works analogously for Windows binary
       installs (to add support for 64- or 32-bit Windows).

      \item When sub-architectures are in use, the installed
       architectures are recorded in the \code{Archs} field of the
       \file{DESCRIPTION} file.  There is a new default filter,
       \code{"subarch"}, in \code{available.packages()} to make use of
       this.

       Code is compiled in a copy of the \file{src} directory when a
       package is installed for more than one sub-architecture: this
       avoid problems with cleaning the sources between building
       sub-architectures.

      \item \command{R CMD INSTALL --libs-only} no longer overrides the
       setting of locking, so a previous version of the package will be
       restored unless \option{--no-lock} is specified.
    }
  }
  \subsection{UTILITIES}{
    \itemize{
      \item \command{R CMD Rprof|build|check} are now based on \R rather
       than Perl scripts.  The only remaining Perl scripts are the
       deprecated \command{R CMD Sd2Rd} and \code{install-info.pl} (used
       only if \code{install-info} is not found) as well as some
       maintainer-mode-only scripts.

       \strong{NB:} because these have been completely rewritten, users
       should not expect undocumented details of previous
       implementations to have been duplicated.

       \command{R CMD} no longer manipulates the environment variables
       \env{PERL5LIB} and \env{PERLLIB}.

      \item \command{R CMD check} has a new argument \option{--extra-arch}
       to confine tests to those needed to check an additional
       sub-architecture.

       Its check for \dQuote{Subdirectory 'inst' contains no files} is
       more thorough: it looks for files, and warns if there are only
       empty directories.

       Environment variables such as \env{R_LIBS} and those used for
       customization can be set for the duration of checking \emph{via} a file
       \file{~/.R/check.Renviron} (in the format used by
       \file{.Renviron}, and with sub-architecture specific versions
       such as \file{~/.R/check.Renviron.i386} taking precedence).

       There are new options \option{--multiarch} to check the package
       under all of the installed sub-architectures and
       \option{--no-multiarch} to confine checking to the
       sub-architecture under which \command{check} is invoked.  If
       neither option is supplied, a test is done of installed
       sub-architectures and all those which can be run on the current
       OS are used.

       Unless multiple sub-architectures are selected, the install done
       by \code{check} for testing purposes is only of the current
       sub-architecture (\emph{via} \command{R CMD INSTALL --no-multiarch}).

       It will skip the check for non-ascii characters in code or data
       if the environment variables \env{_R_CHECK_ASCII_CODE_} or
       \env{_R_CHECK_ASCII_DATA_} are respectively set to \env{FALSE}.
       (Suggestion of Vince Carey.)

      \item \command{R CMD build} no longer creates an \file{INDEX} file
       (\command{R CMD INSTALL} does so), and \option{--force} removes
       (rather than overwrites) an existing \file{INDEX} file.

       It supports a file \file{~/.R/build.Renviron} analogously to
       \command{check}.

       It now runs build-time \samp{\Sexpr} expressions in help files.

      \item \command{R CMD Rd2dvi} makes use of \code{tools::texi2dvi()}
       to process the package manual.  It is now implemented entirely in
       \R (rather than partially as a shell script).

      \item \command{R CMD Rprof} now uses \code{utils::summaryRprof()}
       rather than Perl.  It has new arguments to select one of the
       tables and to limit the number of entries printed.

      \item \command{R CMD Sweave} now runs \R with \option{--vanilla} so
       the environment setting of \env{R_LIBS} will always be used.
    }
  }
  \subsection{C-LEVEL FACILITIES}{
    \itemize{
      \item \code{lang5()} and \code{lang6()} (in addition to
       pre-existing \code{lang[1-4]()}) convenience functions for easier
       construction of \code{eval()} calls.  If you have your own
       definition, do wrap it inside \code{#ifndef lang5 .... #endif}
       to keep it working with old and new \R.

      \item Header \file{R.h} now includes only the C headers it itself needs,
       hence no longer includes \code{errno.h}.  (This helps avoid problems
       when it is included from C++ source files.)

      \item Headers \file{Rinternals.h} and \file{R_ext/Print.h} include
       the C++ versions of \file{stdio.h} and \file{stdarg.h}
       respectively if included from a C++ source file.
    }
  }

  \subsection{INSTALLATION}{
    \itemize{
      \item A C99 compiler is now required, and more C99 language features
       will be used in the \R sources.

      \item Tcl/Tk >= 8.4 is now required (increased from 8.3).

      \item System functions \code{access}, \code{chdir} and
       \code{getcwd} are now essential to configure \R.  (In practice
       they have been required for some time.)

      \item \command{make check} compares the output of the examples
       from several of the base packages to reference output rather than
       the previous output (if any).  Expect some differences due to
       differences in floating-point computations between platforms.

      \item File \file{NEWS} is no longer in the sources, but generated as
      part of the installation.  The primary source for changes is now
      \file{doc/NEWS.Rd}.

      \item The \code{popen} system call is now required to build \R.
      This ensures the availability of \code{system(intern = TRUE)},
      \code{pipe()} connections and printing from \code{postscript()}.

      \item The \command{pkg-config} file \file{libR.pc} now also works
      when \R is installed using a sub-architecture.

      \item \R has always required a BLAS that conforms to IE60559
      arithmetic, but after discovery of more real-world problems caused
      by a BLAS that did not, this is tested more thoroughly in this
      version.
    }
  }

  \subsection{BUG FIXES}{
    \itemize{
      \item Calls to \code{selectMethod()} by default no longer cache
       inherited methods.  This could previously corrupt methods used by
       \code{as()}.

      \item The densities of non-central chi-squared are now more accurate
       in some cases in the extreme tails, e.g. \code{dchisq(2000, 2,
       1000)}, as a series expansion was truncated too early.
       (\PR{14105})

      \item \code{pt()} is more accurate in the left tail for \code{ncp}
       large, e.g. \code{pt(-1000, 3, 200)}.  (\PR{14069})

      \item The default C function (\code{R_binary}) for binary ops now
       sets the S4 bit in the result if either argument is an S4 object.
       (\PR{13209})

      \item \code{source(echo=TRUE)} failed to echo comments that
       followed the last statement in a file.

      \item S4 classes that contained one of \code{"matrix"},
       \code{"array"} or \code{"ts"} and also another class now accept
       superclass objects in \code{new()}.  Also fixes failure to call
       \code{validObject()} for these classes.

      \item Conditional inheritance defined by argument \code{test} in
       \code{methods::setIs()} will no longer be used in S4 method
       selection (caching these methods could give incorrect results).
       See \code{?setIs}.

      \item The signature of an implicit generic is now used by
       \code{setGeneric()} when that does not use a definition nor
       explicitly set a signature.

      \item A bug in \code{callNextMethod()} for some examples with
       \code{"..."} in the arguments has been fixed.  See file
       \file{src/library/methods/tests/nextWithDots.R} in the sources.

      \item \code{match(x, table)} (and hence \code{\%in\%}) now treat
      \code{"POSIXlt"} consistently with, e.g., \code{"POSIXct"}.

      \item Built-in code dealing with environments (\code{get()},
      \code{assign()}, \code{parent.env()}, \code{is.environment()} and
      others) now behave consistently to recognize S4 subclasses;
      \code{is.name()} also recognizes subclasses.

      \item The \code{abs.tol} control parameter to \code{nlminb()}
      now defaults to \code{0.0} to avoid false declarations of
      convergence in objective functions that may go negative.

      \item The standard Unix-alike termination dialog to ask whether to
      save the workspace takes a EOF response as \code{n} to avoid
      problems with a damaged terminal connection.  (\PR{14332})

      \item Added \code{warn.unused} argument to \code{hist.default()}
      to allow suppression of spurious warnings about graphical
      parameters used with \code{plot=FALSE}.  (\PR{14341})

      \item \code{predict.lm()}, \code{summary.lm()}, and indeed
      \code{lm()} itself had issues with residual DF in zero-weighted
      cases (the latter two only in connection with empty
      models). (Thanks to Bill Dunlap for spotting the
      \code{predict()} case.)

      \item \code{aperm()} treated \code{resize = NA} as \code{resize =
      TRUE}.

      \item \code{constrOptim()} now has an improved convergence
      criterion, notably for cases where the minimum was (very close to)
      zero; further, other tweaks inspired from code proposals by Ravi
      Varadhan.

      \item Rendering of S3 and S4 methods in man pages has been
      corrected and made consistent across output formats.

      \item Simple markup is now allowed in \samp{\title} sections
      in \file{.Rd} files.

      \item The behaviour of \code{as.logical()} on factors (to use the
      levels) was lost in \R 2.6.0 and has been restored.

      \item \code{prompt()} did not backquote some default arguments in
      the \samp{\usage} section.  (Reported by Claudia Beleites.)

      \item \code{writeBin()} disallows attempts to write 2GB or
      more in a single call. (\PR{14362})

      \item \code{new()} and \code{getClass()} will now work if Class
      is a subclass of \code{"classRepresentation"} and should also be
      faster in typical calls.

      \item The \code{summary()} method for data frames makes a better
      job of names containing characters invalid in the current locale.

      \item \code{[[} sub-assignment for factors could create an invalid
      factor (reported by Bill Dunlap).

      \item \code{Negate(f)} would not evaluate argument \code{f}
      until first use of returned function (reported by Olaf Mersmann).

      \item \code{quietly=FALSE} is now also an optional argument of
      \code{library()}, and consequently, \code{quietly} is now
      propagated also for loading dependent packages, e.g., in
      \code{require(*, quietly=TRUE)}.

      \item If the loop variable in a \code{for} loop was deleted, it
      would be recreated as a global variable.  (Reported by Radford
      Neal; the fix includes his optimizations as well.)

      \item Task callbacks could report the wrong expression when
      the task involved parsing new code. (\PR{14368})

      \item \code{getNamespaceVersion()} failed; this was an
      accidental change in 2.11.0. (\PR{14374})

      \item \code{identical()} returned \code{FALSE} for external
      pointer objects even when the pointer addresses were the same.

      \item \code{L$a@x[] <- val} did not duplicate in a case it should have.

      \item \code{tempfile()} now always gives a random file name (even
      if the directory is specified) when called directly after
      startup and before the \R RNG had been used.  (\PR{14381})

      \item \code{quantile(type=6)} behaved inconsistently.  (\PR{14383})

      \item \code{backSpline(.)} behaved incorrectly when the knot
      sequence was decreasing.  (\PR{14386})

      \item The reference BLAS included in \R was assuming that
      \code{0*x} and \code{x*0} were always zero (whereas they could be
      \code{NA} or \code{NaN} in IEC 60559 arithmetic).  This was seen
      in results from \code{tcrossprod}, and for example that
      \code{log(0) \%*\% 0} gave \code{0}.

      \item The calculation of whether text was completely outside
      the device region (in which case, you draw nothing)
      was wrong for screen devices (which have [0, 0] at top-left).
      The symptom was (long) text disappearing when
      resizing a screen window (to make it smaller).  (\PR{14391})

      \item \code{model.frame(drop.unused.levels = TRUE)} did not
      take into account \code{NA} values of factors when deciding to
      drop levels. (\PR{14393})

      \item \code{library.dynam.unload} required an absolute path for
      \code{libpath}.  (\PR{14385})

      Both \code{library()} and \code{loadNamespace()} now record
      absolute paths for use by \code{searchpaths()} and
      \code{getNamespaceInfo(ns, "path")}.

      \item The self-starting model \code{NLSstClosestX} failed if some
      deviation was exactly zero.  (\PR{14384})

      \item \code{X11(type = "cairo")} (and other devices such as
      \code{png} using cairographics) and which use Pango font selection
      now work around a bug in Pango when very small fonts (those with
      sizes between 0 and 1 in Pango's internal units) are requested.
      (\PR{14369})

      \item Added workaround for the font problem with \code{X11(type =
      "cairo")} and similar on Mac OS X whereby italic and bold styles
      were interchanged.  (\PR{13463} amongst many other reports.)

      \item \code{source(chdir = TRUE)} failed to reset the working
      directory if it could not be determined -- that is now an error.

      \item Fix for crash of \code{example(rasterImage)} on
      \code{x11(type="Xlib")}.

      \item Force Quartz to bring the on-screen display up-to-date
      immediately before the snapshot is taken by \code{grid.cap()}
      in the Cocoa implementation. (\PR{14260})

      \item \code{model.frame} had an unstated 500 byte limit on
      variable names.  (Example reported by Terry Therneau.)

      \item The 256-byte limit on names is now documented.

      \item Subassignment by \code{[}, \code{[[} or \code{$} on an
      expression object with value \code{NULL} coerced the object to a
      list.
    }
  }
}


\section{\Rlogo CHANGES IN R VERSION 2.11.1 patched}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item \code{install.packages()} has a new optional argument
      \code{INSTALL_opts} which can be used to pass options to
      \command{R CMD INSTALL} for source-package installs.

      \item \command{R CMD check} now runs the package-specific tests
      with \env{LANGUAGE=en} to facilitate comparison to
      \file{.Rout.save} files.

      \item \code{sessionInfo()} gives more detailed platform
      information, including 32/64-bit and the sub-architecture if one
      is used.
    }
  }

  \subsection{DEPRECATED & DEFUNCT}{
    \itemize{
      \item The use of Perl configuration variables for \command{R CMD
	check} (as previously documented in \sQuote{Writing R
	Extensions}) is deprecated and will be removed in \R 2.12.0.  Use
      the environment variables documented in \sQuote{R Internals}
      instead.
    }
  }

  \subsection{BUG FIXES}{
    \itemize{
      \item \command{R CMD Rd2dvi} failed if run from a path containing
       space(s).  This also affected \command{R CMD check}, which calls
       \command{Rd2dvi}.

      \item \code{stripchart()} could fail with an empty factor
      level.  (\PR{14317})

      \item Text help rendering of \samp{\tabular\{\}} has been
      improved: under some circumstances leading blank columns were
      not rendered.

      \item \code{strsplit(x, fixed=TRUE)} marked UTF-8 strings with the
      local encoding when no splits were found.

      \item \code{weighted.mean(NA, na.rm=TRUE)} and similar now returns
      \code{NaN} again, as it did prior to \R 2.10.0.

      \item \command{R CMD} had a typo in its detection of whether the
      environment variable \env{TEXINPUTS} was set (reported by Martin
      Morgan).

      \item The command-line parser could mistake
      \option{--file=size...} for one of the options for setting limits
      for Ncells or Vcells.

      \item The internal \code{strptime()} could corrupt its copy of the
      timezone which would then lead to spurious warnings. (\PR{14338})

      \item \code{dir.create(recursive = TRUE)} could fail if one of the
      components existed but was a directory on a read-only file system.
      (Seen on Solaris, where the error code returned is not even listed
      as possible on the man page.)

      \item The \code{postscript()} and \code{pdf()} devices will now
      allow \code{lwd} values less than 1 (they used to force such
      values to be 1).

      \item Fixed font face for CID fonts in \code{pdf()} graphics output.
      (\PR{14326})

      \item \code{GERaster()} now checks for width or height of zero and
      does nothing in those cases; previously the behaviour was
      undefined, probably device-specific, and possibly dangerous.

      \item \code{wilcox.test(x, y, conf.int = TRUE)} failed with an unhelpful
      message if \code{x} and \code{y} were constant vectors, and
      similarly in the one-sample case.  (\PR{14329})

      \item Improperly calling \code{Recall()} from outside a function
      could cause a segfault.  (Reported by Robert McGehee.)

      \item \samp{\Sexpr[result=rd]} in an Rd file added a
      spurious newline, which was displayed as extra whitespace when
      rendered.

      \item \code{require(save = TRUE)} recorded the names of packages
      it failed to load.

      \item \code{packageStatus()} could return a data frame with
      duplicate row names which could then not be printed.

      \item \code{txtProgressBar(style = 2)} did not work correctly.

      \code{txtProgressBar(style = 3)} did not display until a
      non-minimum value was set.

      \item \code{contour()} did not display dashed line types
      properly when contour lines were labelled. (Reported by
      David B. Thompson.)

      \item \code{tools::undoc()} again detects undocumented data
      objects.  Of course, this also affects \command{R CMD check}.

      \item \code{ksmooth(x,NULL)} no longer segfaults.

      \item \code{approxfun()}, \code{approx()}, \code{splinefun()}
      and \code{spline()} could be confused by \code{x} values that
      were different but so close as to print identically.
      (\PR{14377})
    }
  }
}


\section{\Rlogo CHANGES IN R VERSION 2.11.1}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item \command{R CMD INSTALL} checks if dependent packages are
       available early on in the installation of source packages,
       thereby giving clearer error messages.

      \item \command{R CMD INSTALL --build} now names the file in the
       format used for Mac OS X binary files on that platform.

       \item \code{BIC()} in package \pkg{stats4} now also works with
       multiple fitted models, analogously to \code{AIC()}.
    }
  }
  \subsection{DEPRECATED & DEFUNCT}{
    \itemize{
      \item Use of file extension \file{.C} for C++ code in packages is now
       deprecated: it has caused problems for some \command{make}s on
       case-insensitive file systems (although it currently works
       with the recommended toolkits).
    }
  }
  \subsection{INSTALLATION}{
    \itemize{
      \item Command \command{gnutar} is preferred to \command{tar}
       when configure sets \env{TAR}.  This is needed on Mac OS 10.6,
       where the default \code{tar}, \code{bsdtar 2.6.2}, has been
       reported to produce archives with illegal extensions to tar
       (according to the POSIX standard).
    }
  }
  \subsection{BUG FIXES}{
    \itemize{
      \item The C function \code{mkCharLenCE} now no longer reads past
       \code{len} bytes (unlikely to be a problem except in user
       code).  (\PR{14246})

      \item On systems without any default \env{LD_LIBRARY_PATH} (not
       even \file{/usr/local/lib}), \env{[DY]LIB_LIBRARY_PATH} is now
       set without a trailing colon.  (\PR{13637})

      \item More efficient implementation of \code{utf8ToInt()} on long
       multi-byte strings with many multi-byte characters.  (\PR{14262})

      \item \code{aggregate.ts()} gave platform-dependent results due to
       rounding error for \code{ndeltat != 1}.

      \item \code{package.skeleton()} sometimes failed to fix filenames
        for \file{.R} or \file{.Rd} files to start with an alphanumeric.
        (\PR{14253})

	It also failed when only an S4 class without any methods was
        defined.  (\PR{14280})

      \item \code{splinefun(method = "monoH.FC")} was not
      quite monotone in rare cases.  (\PR{14215})

      \item \code{Rhttpd} no longer crashes due to \code{SIGPIPE} when
       the client closes the connection prematurely.  (\PR{14266})

      \item \code{format.POSIXlt()} could cause a stack overflow and
        crash when used on very long vectors.  (\PR{14267})

      \item \code{Rd2latex()} incorrectly escaped special characters in
      \samp{\usage} sections.

      \item \code{mcnemar.test()} could alter the levels (dropping
       unused levels) if passed \code{x} and \code{y} as factors
       (reported by Greg Snow).

      \item \command{Rd2pdf} sometimes needed a further
       \command{pdflatex} pass to get hyperlinked pages correct.

      \item \code{interaction()} produced malformed results when levels were
        duplicated, causing segfaults in \code{split()}.

      \item \code{cut(d, breaks = <n>)} now also works for \code{"Date"}
        or \code{"POSIXt"} argument \code{d}.
        (\PR{14288})

      \item \code{memDecompress()} could decompress incompletely rare
       \code{xz}-compressed input due to incorrect documentation of
       \command{xz utils}.  (Report and patch from Olaf Mersmann.)

      \item The S4 \code{initialize()} methods for \code{"matrix"},
        \code{"array"}, and \code{"ts"} have been fixed to call
        \code{validObject()}.  (\PR{14284})

      \item \command{R CMD INSTALL} now behaves the same way with or
       without \option{--no-multiarch} on platforms with only one
       installed architecture.  (It used to clean the \file{src}
       directory without \option{--no-multiarch}.)

      \item \code{[<-.data.frame} was not quite careful enough in
        assigning (and potentially deleting) columns
        right-to-left.  (\PR{14263})

      \item \code{rbeta(n, a, b)} no longer occasionally returns
       \code{NaN} for \code{a >> 1 > b}.  (\PR{14291})

      \item \code{pnorm(x, log.p = TRUE)} could return \code{NaN} not
       \code{-Inf} for \code{x} near (minus for \code{lower.tail=TRUE})
       the largest representable number.

      \item Compressed data files \file{*.(txt|tab|csv).(gz|bz2|xz)}
       were not recognized for the list of data topics and hence for
       packages using \code{LazyData}.  (\PR{14273})

      \item \code{textConnection()} did an unnecessary translation on
       strings in a foreign encoding (e.g. UTF-8 strings on Windows) and
       so was slower than it could have been on very long input strings.
       (\PR{14286})

      \item \code{tools::Rd2txt()} did not render poorly written Rd
       files consistently with other renderers.

       It computed widths of strings that would be \code{print()}ed with
       escapes incorrectly, for example in the computation of column
       width for \samp{\tabular}.

      \item \code{na.action()} did not extract the \code{na.action}
      component as documented.

      \item \code{do.call()}ing \code{NextMethod} in erroneous ways no
      longer segfaults. (\PR{13487})
    }
  }
}


\section{\Rlogo CHANGES IN R VERSION 2.11.0}{
  \subsection{SIGNIFICANT USER-VISIBLE CHANGES}{
    \itemize{
      \item Packages must have been installed under \R >= 2.10.0, as the
       current help system is the only one now supported.

      \item A port to 64-bit Windows is now available as well as binary
       package repositories: see the \sQuote{R Administration and
       Installation Manual}.

      \item Argument matching for primitive functions is now done in the
       same way as for interpreted functions except for the deliberate
       exceptions
       \preformatted{
	 call switch .C .Fortran .Call .External
       }
       all of which use positional matching for their first argument,
       and also some internal-use-only primitives.

      \item The default device for command-line \R at the console on Mac OS X
       is now \code{quartz()} and not \code{X11()}.
    }
  }
  \subsection{NEW FEATURES}{
    \itemize{
      \item The \code{open} modes for connections are now interpreted
       more consistently.  \code{open = "r"} is now equivalent to
       \code{open = "rt"} for all connections.  The default \code{open =
       ""} now means \code{"rt"} for all connections except the
       compressed-file connections \code{gzfile()}, \code{bzfile()} and
       \code{xzfile()} for which it means \code{"rb"}.

      \item \command{R CMD INSTALL} now uses the internal \code{untar()}
       function in package \pkg{utils}: this ensures that all platforms
       can install \command{bzip2}- and \command{xz}-compressed
       tarballs.  In case this causes problems (as it has on some
       Windows file systems when run from Cygwin tools) it can be
       overridden by the environment variable \env{R_INSTALL_TAR}:
       setting this to a modern external tar program will speed up
       unpacking of large (tens of Mb or more) tarballs.

      \item \code{help(try.all.packages = TRUE)} is much faster
       (although the time taken by the OS to find all the packages the
       first time it is used can dominate the time).

      \item \command{R CMD check} has a new option \option{--timings}
       to record per-example timings in file
       \file{<pkg>.Rcheck/<pkg>-Ex.timings}.

      \item The TRE library has been updated to version 0.8.0 (minor bugfixes).

      \item \code{grep[l]}, \code{[g]sub} and \code{[g]regexpr} now work
       in bytes in an 8-bit locales if there is no marked UTF-8 input
       string: this will be somewhat faster, and for \code{[g]sub()}
       give the result in the native encoding rather than in UTF-8
       (which returns to the behaviour prior to \R 2.10.0).

      \item A new argument \code{skipCalls} has been added to
       \code{browser()} so that it can report the original context when
       called by other debugging functions.

      \item More validity checking of UTF-8 and MBCS strings is done by
       \code{agrep()} and the regular-expression matching functions.

      \item The undocumented restriction on \code{gregexpr()} to
       \code{length(text) > 0} has been removed.

      \item Package \pkg{tcltk} now sends strings to Tcl in UTF-8: this
       means that strings with a marked UTF-8 encoding are supported in
       non-UTF-8 locales.

      \item The graphics engine now supports rendering of raster (bitmap)
       images, though not all graphics devices can provide (full)
       support.  Packages providing graphics devices (e.g., \code{Cairo},
       \code{RSvgDevice}, \code{cairoDevice}) will need to be reinstalled.

       There is also support in the graphics engine for capturing
       raster images from graphics devices (again not supported
       on all graphics devices).

      \item \command{R CMD check} now also checks if the package and
       namespace can be unloaded: this provides a check of the
       \code{.Last.lib()} and \code{.onUnload()} hook functions (unless
       \option{--install=fake}).

      \item \code{prop.table(x)} now accepts a one-dimensional table for
      \code{x}.

      \item A new function \code{vapply()} has been added, based on a
       suggestion from Bill Dunlap.  It requires that a template for the
       function value be specified, and uses it to determine the output
       type and to check for consistency in the function values.

      \item The main HTML help page now links to a reformatted copy of
       this \file{NEWS} file.  (Suggested by Henrik Bengtsson.)  Package
       index files link to the package \file{DESCRIPTION} and
       \file{NEWS} files and a list of demos when using dynamic help.

      \item The \code{[} method for class \code{"AsIs"} allows the next
       method to change the underlying class.  (Wish of Jens
       Oehlschlägel.)

      \item \code{write.csv[2]} no longer allow argument \code{append}
       to be changed: as ever, direct calls to \code{write.table()} give
       more flexibility as well as more room for error.

      \item The index page for HTML help for a package now collapses
       multiple signatures for S4 methods into a single entry.

      \item The use of \code{.required} by \code{require()} and
       \code{detach()} has been replaced by \code{.Depends} which is set
       from the \code{Depends} field of a package (even in packages with
       namespaces).  By default \code{detach()} prevents such
       dependencies from being detached: this can be overridden by the
       argument \code{force}.

      \item \code{bquote()} has been extended to work on function
       definitions.  (Wish of
       \PR{14031}).

      \item \code{detach()} when applied to an object other than a
       package returns the environment that has been detached, to
       parallel \code{attach()}.

      \item \code{readline()} in non-interactive use returns \code{""}
       and does not attempt to read from the \sQuote{terminal}.

      \item New function \code{file_ext()} in package \pkg{tools}.

      \item \code{xtfrm()} is now primitive and internally generic, as
       this allows S4 methods to be set on it without name-space scoping
       issues.

       There are now \code{"AsIs"} and \code{"difftime"} methods, and
       the default method uses \code{unclass(x)} if \code{is.numeric(x)}
       is true (which will be faster but relies on \code{is.numeric()}
       having been set correctly for the class).

      \item \code{is.numeric(x)} is now false for a \code{"difftime"}
       object (multiplication and division make no sense for such
       objects).

      \item The default method of \code{weighted.mean(x, w)} coerces
       \code{w} to be numeric (aka double); previously only integer
       weights were coerced.  Zero weights are handled specially so an
       infinite value with zero weight does not force an \code{NaN}
       result.

       There is now a \code{"difftime"} method.

      \item \code{bug.report()} now has arguments \code{package} and
       \code{lib.loc} to generate bug reports about packages.  When
       this is used, it looks for a \code{BugReports} field in the
       package \file{DESCRIPTION} file, which will be assumed to be a
       URL at which to submit the report, and otherwise generates an
       email to the package maintainer.  (Suggested by Barry
       Rowlingson.)

      \item \code{quantile()} now has a method for the date-time class
       \code{"POSIXt"}, and types 1 and 3 (which never interpolate) work
       for Dates and ordered factors.

      \item \code{length(<POSIXlt>)} now returns the length of the
       corresponding abstract timedate-vector rather than always 9 (the
       length of the underlying list structure).  (Wish of \PR{14073}
       and \PR{10507}.)

      \item The \code{readline} completion backend no longer sorts
       possible completions alphabetically (e.g., function argument
       names) if \R was built with \code{readline >= 6}.

      \item \code{select.list()} gains a \code{graphics} argument to
       allow Windows/Mac users to choose the text interface.  This
       changes the behaviour of \code{new.packages(ask=TRUE)} to be like
       \code{update.packages(ask=TRUE)} on those platforms in using a
       text menu: use ask=\code{"graphics"} for a graphical menu.

      \item New function \code{chooseBioCmirror()} to set the
      \code{"BioC_mirror"} option.

      \item The \R grammar now prevents using the argument \code{name}
       in signatures of S4 methods for \code{$} and \code{$<-}, since
       they will always be called with a character string value for
       \code{name}.  The implicit S4 generic functions have been
       changed to reflect this: packages which included \code{name} in
       the signature of their methods need to be updated and
       re-installed.

      \item The handling of the \code{method} argument of \code{glm()}
       has been refined following suggestions by Ioannis Kosmidis and
       Heather Turner.

      \item \code{str()} gains a new argument \code{list.len} with
       default 99, limiting the number of \code{list()} items (per
       level), thanks to suggestions from David Winsenius.

      \item Having formal arguments of an S4 method in a different order
       from the generic is now an error (the warning having been
       ignored by some package maintainers for a long time).

      \item New functions \code{enc2native()} and \code{enc2utf8()}
       convert character vectors with possibly marked encodings to the
       current locale and UTF-8 respectively.

      \item Unrecognized escapes and embedded nuls in character strings are
       now an error, not just a warning.  Thus option \code{"warnEscapes"}
       is no longer needed.  \code{rawToChar()} now removes trailing nuls
       silently, but other embedded nuls become errors.

      \item Informational messages about masked objects displayed when a
       package is attached are now more compact, using \code{strwrap()}
       instead of one object per line.

      \item \code{print.rle()} gains argument \code{prefix}.

      \item \code{download.file()} gains a \code{"curl"} method, mainly
       for use on platforms which have \command{curl} but not
       \command{wget}, but also for some hard-to-access URLs.

      \item In \code{Rd}, \samp{\eqn} and \samp{\deqn} will render in
       HTML (and convert to text) upper- and lower-case Greek letters
       (entered as \samp{\alpha} \dots), \samp{\ldots}, \samp{\dots},
       \samp{\ge} and \samp{\le}.

      \item \code{utf8ToInt()} and \code{intToUtf8()} now map \code{NA} inputs
      to \code{NA} outputs.

      \item \code{file()} has a new argument \code{raw} which may help
       if it is used with something other than a regular file, e.g. a
       character device.

      \item New function \code{strtoi()}, a wrapper for the C function
      \code{strtol}.

      \item \code{as.octmode()} and \code{as.hexmode()} now allow inputs
       of length other than one.

       The \code{format()} and \code{print()} methods for
       \code{"octmode"} now preserve names and dimensions (as those for
       \code{"hexmode"} did).

       The \code{format()} methods for classes \code{"octmode"} and
       \code{"hexmode"} gain a \code{width} argument.

      \item \code{seq.int()} returns an integer result in some further cases
       where \code{seq()} does, e.g. \code{seq.int(1L, 9L, by = 2L)}.

      \item Added \samp{\subsection\{\}\{\}} macro to Rd syntax, for
       subsections within sections.

      \item n-dimensional arrays with dimension names can now be indexed
       by an n-column character matrix.  The indices are matched against
       the dimension names.  \code{NA} indices are propagated to the
       result.  Unmatched values and \code{""} are not allowed and
       result in an error.

      \item \code{interaction(drop=TRUE)} uses less memory (related to
      \PR{14121}).

      \item \code{summary()} methods have been added to the
       \code{"srcref"} and \code{"srcfile"} classes, and various
       encoding issues have been cleaned up.

      \item If option \code{"checkPackageLicense"} is set to \code{TRUE} (not
       currently the default), users will be asked to agree to
       non-known-to-be-FOSS package licences at first use.

      \item Checking \code{setAs(a, b)} methods only gives a message
       instead of a warning, when one of \code{a} or \code{b} is
       unknown.

      \item New function \code{norm()} to compute a matrix norm.
       \code{norm()} and also \code{backsolve()} and \code{sample()}
       have implicit S4 generics.

      \item Files \file{Renviron.site} and \file{Rprofile.site} can have
       architecture-specific versions on systems with sub-architectures.

      \item \command{R CMD check} now (by default) also checks Rd files
       for auto-generated content in need of editing, and missing
       argument descriptions.

      \item \code{aggregate()} gains a formula method thanks to a
       contribution by Arni Magnusson.  The data frame method now allows
       summary functions to return arbitrarily many values.

      \item \code{path.expand()} now propagates \code{NA} values rather
       than converting them to \code{"NA"}.

      \item \code{file.show()} now disallows \code{NA} values for file names,
       headers, and pager.

      \item The \sQuote{fuzz} used by \code{seq()} and \code{seq.int()} has
       been reduced from \code{1e-7} to \code{1e-10}, which should be
       ample for the double-precision calculations used in \R.  It
       ensures that the fuzz never comes into play with sequences of
       integers (wish of \PR{14169}).

      \item The default value of \code{RSiteSearch(restrict=)} has been
       changed to include vignettes but to exclude R-help.  The R-help
       archives available have been split, with a new option of
       \code{"Rhelp10"} for those from 2010.

      \item New function \code{rasterImage()} in the \code{graphics}
       package for drawing raster images.

      \item \code{stats:::extractAIC.coxph()} now omits aliased terms
       when computing the degrees of freedom (suggestion of Terry
       Therneau).

      \item \code{cor()} and \code{cov()} now test for misuse with
       non-numeric arguments, such as the non-bug report \PR{14207}.

      \item \code{pchisq(ncp =, log.p = TRUE)} is more accurate for
       probabilities near one.  E.g. \code{pchisq(80, 4, ncp=1,
       log.p=TRUE)}.  (Maybe what was meant in \PR{14216}.)

      \item \code{maintainer()} has been added, to give convenient
       access to the name of the maintainer of a package (contributed by
       David Scott).

      \item \code{sample()} and \code{sample.int()} allow zero items to
       be sampled from a zero-length input.  \code{sample.int()} gains a
       default value \code{size=n} to be more similar to
       \code{sample()}.

      \item \code{switch()} returned \code{NULL} on error (not
       previously documented on the help page): it now does so
       invisibly, analogously to if-without-else.

       It is now primitive: this means that argument \code{EXPR} is
       always matched to the first argument and there is no danger of
       partial matching to later named arguments.

      \item Primitive functions \code{UseMethod()}, \code{attr()},
       \code{attr<-()}, \code{on.exit()}, \code{retracemem()} and
       \code{substitute()} now use standard argument matching (rather
       than positional matching).  This means that all multi-argument
       primitives which are not internal now use standard argument
       matching except where positional matching is desirable (as for
       \code{switch()}, \code{call()}, \code{.C()} ...).

      \item All the one-argument primitives now check that any name
       supplied for their first argument is a partial match to the
       argument name as documented on the help page: this also
       applies to replacement functions of two arguments.

      \item \code{base::which()} uses a new \code{.Internal} function
       when \code{arr.ind} is \code{FALSE} resulting in a 10x speedup.
       Thanks to Patrick Aboyoun for implementation suggestions.

      \item Help conversion to text now uses the first part of
       \samp{\enc\{\}\{\}} markup if it is representable in the current
       output encoding.  On the other hand, conversion to LaTeX with the
       default \code{outputEncoding = "ASCII"} uses the second part.

      \item A new class \code{"listOfMethods"} has been introduced to
       represent the methods in a methods table, to replace the
       deprecated class \code{"MethodsList"}.

      \item \code{any()} and \code{all()} return early if possible.
       This may speed up operations on long vectors.

      \item \code{strptime()} now accepts \code{"\%z"} (for the offset
       from UTC in the RFC822 format of \code{+/-hhmm}).

      \item The PCRE library has been updated to version 8.02, a bug-fix
       release which also updates tables to Unicode 5.02.

      \item Functions which may use a graphical \code{select.list()}
       (including \code{menu()} and \code{install.packages()}) now check
       on a Unix-alike that Tk can be started (and not just
       \code{capabilities("tcltk") && capabilities("X11")}).

      \item The parser no longer marks strings containing octal or hex
       escapes as being in UTF-8 when entered in a UTF-8 locale.

      \item On platforms with cairo but not Pango (notably Mac OS X) the
       initial default \code{X11()} type is set to \code{"Xlib"}: this avoids
       several problems with font selection when done by cairo rather
       than Pango (at least on Mac OS X).

      \item New function \code{arrayInd()} such that \code{which(x,
       arr.ind = TRUE)} for an array 'x' is now equivalent to
       \code{arrayInd(which(x), dim(x), dimnames(x))}.
    }
  }
  \subsection{DEPRECATED & DEFUNCT}{
    \itemize{
      \item Bundles of packages are defunct.

      \item \code{stats::clearNames()} is defunct: use \code{unname()}.

      \item Basic regular expressions are defunct, and
       \code{strsplit()}, \code{grep()}, \code{grepl()}, \code{sub()},
       \code{gsub()}, \code{regexpr()} and \code{gregexpr()} no longer
       have an \code{extended} argument.

      \item \code{methods::trySilent()} is defunct.

      \item \code{index.search()} (which was deprecated in 2.10.0) is no
       longer exported and has a different argument list.

      \item Use of multiple arguments to \code{return()} is now defunct.

      \item The use of \code{UseMethod()} with more than two arguments
      is now defunct.

      \item In the \pkg{methods} package, the \code{"MethodsList"}
       metadata objects which had been superseded by hash tables
       (environments) since \R 2.8.0 are being phased out.  Objects of
       this class are no longer assigned or used as metadata by the
       package.

       \code{getMethods()} is now deprecated, with its internal use replaced
       by \code{findMethods()} and other changes.  Creating objects from the
       \code{"MethodsList"} class is also deprecated.

      \item Parsing strings containing both octal/hex and Unicode escapes
       now gives a warning and will become an error in \R 2.12.0.
    }
  }
  \subsection{INSTALLATION}{
    \itemize{
      \item UTF-8 is now used for the reference manual and package
       manuals.  This requires LaTeX \sQuote{2005/12/01} or later.

      \item \command{configure} looks for a POSIX compliant
       \command{tr}, Solaris's \command{/usr/ucb/tr} having been found
       to cause \command{Rdiff} to malfunction.

      \item \command{configure} is now generated with \command{autoconf
       2.65}, which works better on recent systems and on Mac OS X.
    }
  }
  \subsection{PACKAGE INSTALLATION}{
    \itemize{
      \item Characters in \R source which are not translatable to the
       current locale are now handled more tolerantly:  these will be
       converted to hex codes with a warning.  Such characters are
       only really portable if they appear in comments.

      \item \command{R CMD INSTALL} now tests that the installed package
       can be loaded (and backs out the installation if it cannot): this
       can be suppressed by \option{--no-test-load}.  This avoids
       installing/updating a package that cannot be used: common causes
       of failures to load are missing/incompatible external software
       and missing/broken dependent packages.

      \item Package installation on Windows for a package with a
       \file{src} directory now checks if a DLL is created unless there
       is a \file{src/Makefile.win} file: this helps catch broken
       installations where the toolchain has not reported problems in
       building the DLL.  (Note: this can be any DLL, not just one named
       \file{<pkg-name>.dll}.)
     }
  }
  \subsection{BUG FIXES}{
    \itemize{

      \item Using \code{with()}, \code{eval()} etc with a list with some
      unnamed elements now works.
      (\PR{14035})

      \item The \code{"quick"} dispatch of S4 methods for primitive
       functions was not happening, forcing a search each time.
       (Dispatch for closures was not affected.)  A side effect is that
       default values for arguments in a method that do not have
       defaults in the generic will now be ignored.

      \item Trying to dispatch S4 methods for primitives during the search
       for inherited methods slows that search down and potentially
       could cause an infinite recursion.  An internal switch was
       added to turn off all such methods from
       \code{findInheritedMethods()}.

      \item \R framework installation (on Mac OS X) would not work
       properly if a rogue \code{Resources} directory was present at the
       top level.  Such a non-symlink will now be renamed to
       \code{Resources.old} (and anything previously named
       \code{Resources.old} removed) as part of the framework
       installation process.

      \item The checks for conforming S4 method arguments could fail
       when the signature of the generic function omitted some of the
       formal arguments (in addition to \code{...}).  Arguments omitted
       from the method definition but conforming (per the documentation)
       should now be ignored (treated as \code{"ANY"}) in dispatching.

      \item The computations for S4 method evaluation when \code{...}
       was in the signature could fail, treating \code{...} as an
       ordinary symbol.  This has been fixed, for the known cases.

      \item Various \code{ar()} fitting methods have more protection for
       singular fits.

      \item \code{callNextMethod} now works again with the \code{drop=}
      argument in \samp{[}

      \item \code{parse()} and \code{parse_Rd()} miscounted columns when
       multibyte UTF-8 characters were present.

      \item Formatting of help pages has had minor improvements: extra
       blank lines have been removed from the text format, and empty
       package labels removed from HTML.

      \item \code{cor(A, B)} where \code{A} is \eqn{n \times 1}{n x 1}
       and \code{B} a 1-dimensional array segfaulted or gave an internal
       error.  (The case \code{cor(B, A)} was
       \PR{7116}.)

      \item \code{cut.POSIXt()} applied to a start value after the DST
       transition on a DST-change day could give the wrong time for
       argument \code{breaks} in units of days or longer.  (\PR{14208})

      \item \code{do_par()} \code{UNPROTECT}ed too early  (\PR{14214})

      \item Subassignment \code{x[[....]] <- y} didn't check for a
       zero-length right hand side, and inserted a rubbish
       value.  (\PR{14217})

      \item \code{fisher.test()} no longer gives a P-value \strong{very}
       slightly > 1, in some borderline cases.

      \item Internal function \code{matchArgs()} no longer modifies the
       general purpose bits of the \code{SEXP}s that make up the formals
       list of \R functions.  This fixes an invalid error message that
       would occur when a garbage collection triggered a second call to
       \code{matchArgs} for the same function \emph{via} a finalizer.

      \item \code{gsub()} in 2.10.x could fail from stack overflow for
       extremely long strings due to temporary data being allocated on
       the stack.  Also, \code{gsub()} with \code{fixed=TRUE} is in some
       circumstances considerably faster.

      \item Several primitives, including \code{attributes()},
       \code{attr<-()} \code{interactive()}, \code{nargs()} and
       \code{proc.time()}, did not check that they were called with the
       correct number of arguments.

      \item A potential race condition in \code{list.files()} when other
       processes are operating on the directory has been fixed; the code
       now dynamically allocates memory for file listings in a single
       pass instead of making an initial count pass.

      \item \code{mean(x, trim=, na.rm = FALSE)} failed to return \code{NA} if
       \code{x} contained missing values.  (Reported by Bill Dunlap.)

      \item Extreme tail behavior of, \code{pbeta()} \{and hence
       \code{pf()}\}, e.g., \code{pbeta(x, 3, 2200, lower.tail=FALSE,
       log.p=TRUE)} now returns finite values instead of jumping to
       \code{-Inf} too early.  (\PR{14230}).

      \item \code{parse(text=x)} misbehaved for objects \code{x} that
       were not coerced internally to character, notably symbols.
       (Reported to R-devel by Bill Dunlap.)

      \item The internal C function \code{coerceSymbol} now handles
       coercion to character, and warns if coercion fails (rather than
       silently returning \code{NULL}).  This allows a name to be given
       where a character vector is required in functions which coerce
       internally.

      \item The interpretation by \code{strptime()} of \code{"\%c"} was
       non-standard (not that it is ever advisable to use locale- and
       system-specific input formats).

      \item capabilities(\code{"X11"}) now works the same way on Mac OS X as on
       other platforms (and as documented: it was always true for R
       built with \option{--with-aqua}, as the CRAN builds are).

      \item The \code{X11()} device with cairo but not Pango (notably Mac OS X)
       now checks validity of text strings in UTF-8 locales (since
       Pango does but cairo it seems does not).

      \item \code{read.fwf()} misread multi-line records when n was specified.
       (\PR{14241})

      \item \code{all.equal(tolerance = e)} passes the numeric
       tolerance also to the comparison of the attributes.

      \item \code{pgamma(0, 0)}, a boundary case, now returns 0, its
       limit from the left, rather than the limit from the right.

      \item Issuing \code{POST} requests to the internal web server could stall
       the request under certain circumstances.

      \item \code{gzcon( <textConnection> )}, an error, no longer
       damages the connection (in a way to have it
       segfault).  (\PR{14237})

      \item All the results from \code{hist()} now use the nominal
       \code{breaks} not those adjusted by the numeric \sQuote{fuzz}:
       in recent versions the nominal \code{breaks} were reported but
       the 'density' referred to the intervals used in the calculation
       -- which mattered very slightly for one of the extreme bins.
       (Based on a report by Martin Becker.)

      \item If \code{xy[z].coords} (used internally by many graphics
       functions) are given a list as \code{x}, they now check that
       the list has suitable names and give a more informative error
       message.  (\PR{13936})
    }
  }
}


\section{\Rlogo CHANGES IN R VERSION 2.10.1 patched}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item The handling of line textures in the \code{postscript()} and
       \code{pdf()} devices was set up for round end caps (the only type
       which existed at the time): it has now been adjusted for butt
       endcaps.

      \item \code{lchoose(a, k)} is now defined as
       \code{log(abs(choose(a,k)))}, analogously to \code{lfactorial()}.

      \item Although \code{\\eqn\{\}} in Rd files is defined as a
       \sQuote{verbatim} macro, many packages expected \code{\\dots} and
       \code{\\ldots} to be interpreted there (as was the case in \R <
       2.10.0), so this is now done (using an ellipsis in HTML
       rendering).

      \item Escaping of braces in quoted strings in \R-code sections of
       Rd files is allowed again.  This had been changed for the new Rd
       format in \R 2.10.0 but was only documented on the developer site
       and was handled inconsistently by the converters: text and
       example conversion removed the escapes but HTML conversion did
       not.

      \item The PCRE library has been updated to version 8.01, a bug-fix
       release.

      \item \code{tools::readNEWS()} now accepts a digit as the first
       character of a news section.
    }
  }
  \subsection{BUG FIXES}{
    \itemize{
      \item Using \code{read.table(header=TRUE)} on a header with an
       embedded new line would copy part of the header into the data.
       (\PR{14103})

      \item \code{qpois(p = 1, lambda = 0)} now gives 0 as for all other
      \code{p}.  (\PR{14135})

      \item Functions related to string comparison
       (e.g. \code{unique()}, \code{match()}) could cause crashes when
       used with strings not in the native encoding, e.g. UTF-8 strings
       on Windows.  (\PR{14114} and \PR{14125})

      \item \code{x[ , drop=TRUE]} dropped an \code{NA} level even if it
      was in use.

      \item The dynamic HTML help system reported the wrong MIME type
       for the style sheet.

      \item \code{tools::codoc()} (used by \command{R CMD check}) was
       missing cases where the function had no arguments but was
       documented to have some.

      \item Help links containing special characters (e.g. \code{"?"})
       were not generated correctly when rendered in
       HTML.  (\PR{14155})

      \item \code{lchoose(a, k)} no longer wrongly gives \code{NaN} for
      negative \code{a}.

      \item \code{ks.test()} could give a p-value that was off by one
       observation due to rounding error.  (\PR{14145})

      \item \code{readBin()}/\code{readChar()} when reading millions of
       character strings in a single call used excessive amounts of
       memory (which also slowed them down).

      \item \command{R CMD SHLIB} could fail if used with paths that
       were not alphanumeric, e.g. contained \code{+}.  (\PR{14168})

      \item \code{sprintf()} was not re-entrant, which potentially
       caused problems if an \code{as.character()} method called it.

      \item The \code{quartz()} device did not restore the clipping
       region when filling the background for a new page.  This could be
       observed in multi-page bitmap output as stale outer regions of
       the plot.

      \item \code{p.adjust(method, n)} now works correctly for the
       rare case \code{n > length(p)}, also when method differs from
       \code{"bonferroni"} or \code{"none"}, thanks to a patch from
       Gordon Smyth.

      \item \code{tools::showNonASCII()} failed to detect non-ASCII characters
       if \code{iconv()} (incorrectly) converted them to different ASCII
       characters.  (Seen on Windows only.)

      \item \code{tcrossprod()} wrongly failed in some cases when one of the
       arguments was a vector and the other a matrix.

      \item \code{[cr]bind(..., deparse.level=2)} was not always giving
       names when documented to do so.  (Discovered whilst investigating
       \PR{14189}.)

      \item \code{match(incomparables=<non-NULL>)} could in rare cases
       infinite-loop.

      \item \code{poisson.test()} needed to pass argument
       \code{conf.level} to \code{binom.test()}.  (\PR{14195})

      \item The \code{"nls"} method for \code{df.residual()} gave
       incorrect results for models fitted with \code{na.action =
       na.exclude}.  (\PR{14194})

      \item A change to \code{options(scipen=)} was only implemented
       when printing next occurred, even though it should have affected
       intervening calls to \code{axis()}, \code{contour()} and
       \code{filledcontour()}.

      \item \code{prettyNum(drop0trailing=TRUE)} did not handle signs of
       imaginary parts of complex numbers correctly (and this was used
       by \code{str()}: \PR{14201}).

      \item \code{system.time()} had the \code{sys.child} component
       wrong (copied \code{user.child} instead) on systems with
       \code{HAVE_GETRUSAGE}.
       (\PR{14210})

      \item Changing both line texture and line cap (end) resulted in the
       latter to be omitted form the PDF code.  In addition, line cap
       (end) and join are now set explicitly in PDF output to ensure
       correct defaults.

      \item The suppression of auto-rotation in \code{bitmap()} and
       \code{dev2bitmap()} with the \code{"pdfwrite"} device was not
       working correctly.

      \item \code{plot(ecdf(), log="x")} no longer gives an incorrect
      warning.

      \item \code{read.fwf()} works again when argument \code{file} is
      a connection.

      \item Startup files will now be found if their paths exceed 255
       bytes.  (\PR{14228})

      \item \code{contrasts<-} (in the \pkg{stats} package) no longer has an
       undeclared dependence on \pkg{methods} (introduced in 2.10.0).
    }
  }
}


\section{\Rlogo CHANGES IN R VERSION 2.10.1}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item The PCRE library has been updated to version 8.00.

      \item \command{R CMD INSTALL} has new options \option{--no-R},
      \option{--no-libs}, \option{--no-data}, \option{--no-help},
      \option{--no-demo}, \option{--no-exec}, and \option{--no-inst} to
      suppress installation of the specified part of the package.
      These are intended for special purposes (e.g. building a database
      of help pages without fully installing all packages).

      \item The documented line-length limit of 4095 bytes when reading
      from the console now also applies also to \code{parse(file="")}
      (which previously had a limit of around 1024 bytes).

      \item A Bioconductor mirror can be set for use by
      \code{setRepositories()} \emph{via} the option \code{"BioC_mirror"},
      e.g. the European mirror can be selected by
      \code{options(BioC_mirror="http://bioconductor.statistik.tu-dortmund.de")}.

      \item Double-clicking in a \code{tk_select.list()} list box now
      selects the item and closes the list box (as happens on the
      Windows \code{select.list()} widget).
    }
  }
  \subsection{INSTALLATION}{
    \itemize{
      \item \command{configure} will be able to find a usable
      \code{libtiff} in some rare circumstances where it did not
      previously (where \code{libtiff} needed to be linked explicitly against
      \code{-ljpeg}).

      \item Making \code{refman.pdf} works around a problem with the indexing
      with \code{hyperref 6.79d} and later.
    }
  }
  \subsection{DEPRECATED & DEFUNCT}{
    \itemize{
      \item The \code{extended} argument is deprecated in
      \code{strsplit()}, \code{grep()}, \code{grepl()}, \code{sub()},
      \code{gsub()}, \code{regexpr()} and \code{gregexpr()} (not just
      the value \code{extended = FALSE}) and will be removed in \R
      2.11.0.
    }
  }
  \subsection{BUG FIXES}{
    \itemize{
      \item \code{trigamma(x)} and other \code{psigamma(x, n)} calls
      are now accurate for very large \code{abs(x)}.  (\PR{14020})

      \item \code{[g]sub(perl=FALSE, fixed=FALSE)} could use excessive
      stack space when used with a very long vector containing some
      non-ASCII strings.

      \item The default method of \code{weighted.mean(na.rm = TRUE)}
      did not omit weights for \code{NA} observations in 2.10.0.
      (\PR{14032})

      \item \code{[g]regexpr(pattern, fixed = TRUE)} returned match
      positions in bytes (not characters) in an MBCS locale if
      \code{pattern} was a single byte.

      \code{[g]sub(fixed = TRUE)} with a single-byte pattern could
      conceivably have matched part of a multibyte character in a
      non-UTF-8 MBCS.

      \item \code{findLineNum()} and \code{setBreakpoint()} would
      sometimes fail if the specified file was not in the current
      directory.

      \item Package \pkg{tcltk}'s \code{demo(tkdensity)} was broken in
      2.9.0 when \code{demo()} was changed to set \code{par(ask = TRUE)}.

      \item \code{gsub()} with backrefs could fail on extremely long strings
      (hundreds of thousands of characters) due to integer overflow
      in a length calculation.

      \item \code{abline(untf=TRUE)} now uses a better x-grid in
      log-scale, e.g., for \code{plot(c(1,300), c(1,300), log="xy");
	abline(4,1, untf=TRUE)}.

      \item \code{detach()}/\code{unloadNamespace()} arrange to flush
      the package's lazyload cache of \R objects once the
      package/namespace is no longer needed.

      \item There have been small fixes to the rendering of help,
      e.g. \code{\\command} is now rendered verbatim (so e.g. \code{--}
      is not interpreted, \PR{14045}).

      Also, there are many small changes to help files where the new
      converters were not rendering them in the same way as before.

      \item \code{available.packages()} would fail when run on a
      repository with no packages meeting the filtering conditions.
      (\PR{14042})

      \item \code{rep(x, times, each = 2)} gave invalid results when the
      \code{times} argument was a vector longer than \code{x}.
      Reported by Bill Dunlap.

      \item An error when \code{unloadNamespace()} attempted to run the
      \code{.onUnload()} function gave an error in the reporting
      function and so was not reported properly.

      \item Text help rendering did not handle very long input lines
      properly.

      \item \code{promptMethods()} generated signature documentation
      improperly.

      \item \code{pgamma(x, a, lower.tail=FALSE)} and
      \code{qgamma(\dots)} are now considerably more accurate in some
      regions for very small \code{a}.  \code{qgamma()} now correctly
      returns \code{0} instead of \code{NaN} in similar extreme cases,
      and \code{qgamma()} no longer warns in the case of small
      \code{a}, see (\PR{12324}).

      \item \code{unname()} now also removes names from a zero length vector.

      \item Printing results from \code{ls.str()} no longer evaluates
      unevaluated calls.

      \item \code{complete.cases()} failed on a 0-column data frame
      argument.  (Underlies \PR{14066}.)

      It could return nonsensical results if no input determined the
      number of cases (seen in the no-segfault tests).

      \item An error in \code{nls()} with a long formula could cause a
      segfault.  (\PR{14059})

      \item \code{qchisq(p, df, ncp, lower.tail = FALSE)} with
      \code{ncp >= 80} was inaccurate for small \code{p} (as the help page
      said): it is now less inaccurate.  (In part,
      \PR{13999}.)

      For \code{ncp} less than but close to 80, \code{pchisq()} and
      \code{qchisq()} are more accurate for probabilities very close
      to 1 (a series expansion was truncated slightly too early).

      \code{pchisq(x, df, ncp)} can no longer return values just larger
      than one for large values of \code{ncp}.

      \item \code{intToUtf8()} could fail when asked to produce 10Mb or more
      strings, something it was never intended to do: unfortunately
      Windows crashed \R (other OSes reported a lack of resources).
      (\PR{14068})

      \item \code{chisq.test()} could fail when given argument
      \code{x} or \code{y} which deparsed to more than one line.
      (Reported by Laurent Gauthier.)

      \item S4 methods are uncached whenever the namespace containing
      them is unloaded (by \code{unloadNamespace()} as well as by
      \code{detach(unload = TRUE))}.

      \item The internal record-keeping by
      \code{dyn.load}/\code{dyn.unload} was incomplete, which could
      crash \R if a DLL that registered \code{.External} routines had
      earlier been unloaded.

      \item \code{bessel[JY](x, nu)} with \code{nu} a negative integer (a
      singular case) is now correct, analogously to \code{besselI()},
      see \PR{13556}.

      \item \code{tools::file_path_as_absolute()} doubled the file
      separator when applied to a file such as \file{"/vmunix"} or (on
      Windows) \file{"d:/afile"} in a directory for which
      \code{getwd()} would return a path with a trailing separator
      (largely cosmetic, as reasonable file systems handle such a path
      correctly).  (Perhaps what was meant by \PR{14078}.)

      \item \code{unsplit(drop = TRUE)} applied to a data frame failed
      to pass \code{drop} to the computation of row names.  (\PR{14084})

      \item The \code{"difftime"} method of \code{mean()} ignored its
      \code{na.rm} argument.

      \item \code{tcltk::tk_select.list()} is now more likely to remove the
      widget immediately after selection is complete.

      \item Adding/subtracting a \code{"difftime"} object to/from a
      \code{"POSIXt"} or \code{"Date"} object works again (it was
      broken by the addition of \code{Ops.difftime}).

      \item Conversion to latex of an Rd file with no aliases failed.

      \item \code{wilcox.test(conf.int=TRUE)} has achieved.level
      corrected and, for \code{exact=FALSE}, now returns a
      \code{estimate} component which does not depend on the
      alternative used.

      \item \code{help.search()} failed when the package argument was
      specified.  (\PR{14113})

      \item \code{switch(EXPR = "A")} now returns \code{NULL}, as does
      \code{switch(1)} (which used to signal an error).
    }
  }
}


\section{\Rlogo CHANGES IN R VERSION 2.10.0}{
  \subsection{SIGNIFICANT USER-VISIBLE CHANGES}{
    \itemize{
      \item Package help is now converted from \code{Rd} by the \R-based
       converters that were first introduced in 2.9.0.  This means

       \itemize{
	 \item Packages that were installed by R-devel after 2009-08-09
         should not be used with earlier versions of \R, and most
         aspects of package help (including the runnable examples)
         will be missing if they are so used.

	 \item Text, HTML and latex help and examples for packages
         installed under the new system are converted on-demand from
         stored parsed Rd files.  (Conversions stored in packages
         installed under \R < 2.10.0 are used if no parsed Rd files
         are found.  It is recommended that such packages be
         re-installed.)
       }

      \item HTML help is now generated dynamically using an HTTP server
       running in the \R process and listening on the loopback
       interface.

       \itemize{
	 \item Those worried about security implications of such
	 a server can disable it by setting the environment variable
	 \env{R_DISABLE_HTTPD} to a non-empty value.  This disables
	 \code{help.start()} and HTML help (so text help is shown
	 instead).

	 \item The Java/Javascript search engine has been replaced by an
         HTML interface to \code{help.search()}.  \code{help.start()} no
         longer has an argument \code{searchEngine} as it is no longer
         needed.

	 \item The HTML help can now locate cross-references of the form
         \code{\\link[pkg]\{foo\}} and \code{\\link[pkg:foo]\{bar\}}
         where \code{foo} is an alias in the package, rather than the
         documented (basename of a) filename (since the documentation
         has been much ignored).
       }
    }
  }
  \subsection{NEW FEATURES}{
    \itemize{
      \item \code{polygon()}, \code{pdf()} and \code{postscript()} now
       have an argument \code{fillOddEven} (default \code{FALSE}),
       which controls the mode used for polygon fills of
       self-intersecting shapes.

      \item New \code{debugonce()} function; further,
       \code{getOption("deparse.max.lines")} is now observed when
       debugging, from a code suggestion by John Brzustowski.
       (\PR{13647}/8)

      \item \code{plot()} methods for \code{"stepfun"} and hence
       \code{"ecdf"} no longer plot points by default for \code{n >= 1000}.

      \item \code{[g]sub(perl=TRUE)} now also supports \code{"\\E"}
      in order to \strong{end} \code{"\\U"} and \code{"\\L"} case changes,
      thanks to a patch from Bill Dunlap.

      \item \code{factor()}, \code{levels()<-}, etc, now ensure that
       the resulting factor levels are unique (as was always the implied
       intention).  Factors with duplicated levels are still
       constructible by low-level means, but are now declared illegal.

      \item New \code{print()} (S3) method for class \code{"function"},
       also used for auto-printing.  Further, \code{.Primitive}
       functions now print and auto-print identically.  The new method
       is based on code suggestions by Romain François.

      \item The \code{print()} and \code{toLatex()} methods for class
       \code{"sessionInfo"} now show the locale in a nicer format and
       have arguments to suppress locale information.

      \item In addition to previously only \code{round()}, there are
       other \code{Math} group (S3) methods for \code{"difftime"},
       such as \code{floor()}, \code{signif()}, \code{abs()}, etc.

      \item For completeness, \code{old.packages()} and
       \code{available.packages()} allow arguments \code{type} to be
       specified (you could always specify arguments \code{available}
       or \code{contriburl}).

      \item \code{available.packages()} by default only returns
       information on the latest versions of packages whose version
       requirements are satisfied by the currently running \R.

      \item \code{tools::write_PACKAGES()} has a new argument
       \code{latestOnly}, which defaults to \code{TRUE} when only the
       latest versions in the repository will be listed in the index.

      \item \code{getOption()} has a new argument \code{default} that
       is returned if the specified option is not set.  This simplifies
       querying a value and checking whether it is \code{NULL} or not.

      \item \code{parse()} now warns if the requested encoding is not supported.

      \item The \code{"table"} method of \code{as.data.frame()} gains a
       \code{stringsAsFactors} argument to allow the classifying
       factors to be returned as character vectors rather than the
       default factor type.

      \item If \code{model.frame.default()} encounters a character
       variable where \code{xlev} indicates a factor, it now converts
       the variable to a factor (with a warning).

      \item \code{curve()} now returns a list containing the points that
      w§ere drawn.

      \item \code{spineplot()} now accepts \code{axes = FALSE}, for
       consistency with other functions called by \code{plot.factor()}.

      \item The Kendall and Spearman methods of \code{cor.test()} can
       optionally use continuity correction when not computing exact
       p-values.  (The Kendall case is the wish of \PR{13691}.)

      \item \R now keeps track of line numbers during execution for code
       sourced with \code{options(keep.source = TRUE)}.  The source
       reference is displayed by debugging functions such as
       \code{traceback()}, \code{browser()}, \code{recover()}, and
       \code{dump.frames()}, and is stored as an attribute on each
       element returned by \code{sys.calls()}.

      \item More functions now have an implicit (S4) generic definition.

      \item \code{quantile.default()} now disallows factors (wish of
       \PR{13631}) and its help
       documents what numeric-like properties its input need to have to
       work correctly.

      \item \code{weighted.mean()} is now generic and has \code{"Date"},
       \code{"POSIXct"} and \code{"POSIXlt"} methods.

      \item Naming subscripts (e.g. \code{x[i=1, j=2]}) in
       \code{data.frame} methods for \code{[} and \code{[[} now gives a
       warning.  (Names are ignored in the default method, but could
       have odd semantics for other methods, and do for the data.frame
       ones.)

      \item \code{as.data.frame()} has an \code{"aovproj"} method.
      (Wish of \PR{13505})

      \item \code{as.character(x)} for numeric \code{x} no longer
       produces strings such as \code{"0.30"}, i.e., with trailing
       zeros.  This change also renders levels construction in
       \code{factor()} more consistent.

      \item \code{codocClasses()}, which checks consistency of the
       documentation of S4 class slots, now does so in considerably more
       cases.  The documentation of inherited slots (from superclasses)
       is now optional.  This affects \command{R CMD check <pkg>} when
       the package defines S4 classes.

      \item \code{codoc()} now also checks S4 methods for code/documentation
       mismatches.

      \item \code{for()}, \code{while()}, and \code{repeat()} loops now
       always return NULL as their (invisible) value.  This change was
       needed to address a reference counting bug without creating
       performance penalties for some common use cases.

      \item The \code{print()} method for \code{ls.str()} results now
       obeys an optional \code{digits} argument.

      \item The \code{method} argument of \code{glm()} now allows
      user-contributed methods.

      \item More general \code{reorder.default()} replaces functionality of
       \code{reorder.factor()} and \code{reorder.character()}.

      \item The function \code{aspell()} has been added to provide an
       interface to the \command{Aspell} spell-checker.

      \item Filters \code{RdTextFilter()} and \code{SweaveTeXFilter()}
       have been added to the tools package to provide support for
       \code{aspell()} or other spell checkers.

      \item \code{xtabs()} with the new argument \code{sparse = TRUE}
       now returns a sparse Matrix, using package \CRANpkg{Matrix}.

      \item \code{contr.sum()} etc gain an argument \code{sparse}
       which allows sparse matrices to be returned.

       \code{contrasts()} also gains a \code{sparse} argument which it
       passes to the actual contrast function if that has a formal
       argument \code{sparse}.

       \code{contrasts(f, .) <- val} now also works when \code{val}
       is a sparse Matrix.  It is planned that \code{model.matrix()}
       will work with such factors \code{f} in the future.

      \item \code{readNEWS()} will recognize a UTF-8 byte-order mark
       (BOM) in the \file{NEWS} file.  However, it is safer to use only
       ASCII code there because not all editors recognize BOMs.

      \item New utility function \code{inheritedSlotNames()} for S4
      class programming.

      \item \code{tabulate()} now allows NAs to pass through (and be ignored).

      \item If \code{debug()} is called on an S3 generic function then
       all methods are debugged as well.

      \item Outlier symbols drawn by \code{boxplot()} now obey the
       \code{outlwd} argument.  Reported by Jurgen Kluge.

      \item \code{svd(x)} and \code{eigen(x)} now behave analogously to
       \code{qr(x)} in accepting logical matrices \code{x}.

      \item File \file{NEWS} is now in UTF-8, and has a BOM (often
       invisible) on the first line, and \command{Emacs} local variables
       set for UTF-8 at the end.  \code{RShowDoc("NEWS")} should display
       this correctly, given suitable fonts.

      \item \code{terms.formula(simplify = TRUE)} now does not deparse
       the LHS and so preserves non-standard responses such as \code{`a:
       b`} (requested by Sundar Dorai-Raj).

      \item New function \code{news()} for building and querying \R or
       package news information.

      \item \code{z^n} for integer \code{n} and complex \code{z} is more
       accurate now if \code{|n| <= 65536}.

      \item \code{factor(NULL)} now returns the same as
       \code{factor(character(0))} instead of an error, and
       \code{table(NULL)} consequently does analogously.

      \item \code{as.data.frame.vector()} (and its copies) is slightly faster
       by avoiding a copy if there are no names (following a
       suggestion of Tim Hesterberg).

      \item \code{writeLines()}, \code{writeBin()} and
       \code{writeChar()} have a new argument \code{useBytes}.  If
       false, character strings with marked encodings are translated to
       the current locale (as before) but if true they are written
       byte-by-byte.

      \item \code{iconv()} has a new argument \code{mark} which can be
       used (by experts) to suppress the declaration of encodings.

      \item \file{DESCRIPTION} file's \code{LinkingTo} specs are now
       recognized as installation dependencies, and included in package
       management computations.

      \item Standardized \file{DESCRIPTION} file \code{License} specs
       are now available for package management computations.

      \item \code{"\\uxxxx"} and \code{"\\Uxxxxxxxx"} escapes can now be
       parsed to a UTF-8 encoded string even in non-UTF-8 locales (this
       has been implemented on Windows since \R 2.7.0).  The semantics
       have been changed slightly: a string containing such escapes is
       always stored in UTF-8 (and hence is suitable for portably
       including Unicode text in packages).

      \item New \code{as.raw()} method for \code{"tclObj"} objects (wish
      of \PR{13758}).

      \item \code{Rd.sty} now makes a better job of setting email addresses,
       including using a monospaced font.

      \item \code{textConnection()} gains an \code{encoding} argument
       to determine how input strings with marked encodings will be
       handled.

      \item \command{R CMD Rd2pdf} is available as a shortcut for
      \command{R CMD Rd2dvi --pdf}.

      \item \command{R CMD check} now checks links where a package is
       specified (\code{\\link[pkg]\{file\}} or
       \code{\\link[pkg:file]\{topic\}}), if the package is available.
       It notes if the package is not available, as in many cases this
       is an error in the link.

      \item \code{identical()} gains three logical arguments, which
       allow for even more differentiation, notably \code{-0} and
       \code{0}.

      \item \code{legend()} now can specify the \code{border} color of
       filled boxes, thanks to a patch from Frederic Schutz.

      \item Indexing with a vector index to \code{[[ ]]} has now been
       extended to all recursive types.

      \item Pairlists may now be assigned as elements of lists.  (Lists
       could always be created with pairlist elements, but \code{[[<-}
       didn't support assigning them.)

      \item The parser now supports C-preprocessor-like \code{#line}
       directives, so error messages and source references may refer to
       the original file rather than an intermediate one.

      \item New functions \code{findLineNum()} and
       \code{setBreakpoint()} work with the source references to find
       the location of source lines and set breakpoints (using
       \code{trace()}) at those lines.

      \item Namespace importing is more careful about warning on masked
       generics, thanks to a patch by Yohan Chalabi.

      \item \code{detach()} now has an argument \code{character.only}
       with the same meaning as for \code{library()} or
       \code{require()}.

      \item \code{available.packages()} gains a \code{filters}
       argument for specifying the filtering operations performed on the
       packages found in the repositories.  A new built-in
       \code{"license/FOSS"} filter only retains packages for which
       installation can proceed solely based on packages which can be
       verified as Free or Open Source Software (FOSS) employing the
       available license specifications.

      \item In registering an S3 class by a call to
       \code{setOldClass()}, the data part (e.g., the object type)
       required for the class can be included as one of the superclasses
       in the Classes argument.

      \item The argument \code{f} to \code{showMethods()} can be an
       expression evaluating to a generic function, allowing methods to
       be shown for non-exported generics and other nonstandard cases.

      \item \code{sprintf()} now supports \code{\%o} for octal
      conversions.

      \item New function \code{Sys.readlink()} for information about symbolic
       links, including if a file is a symbolic link.

      \item Package \pkg{tools} has new functions
       \code{checkRdaFiles()} and \code{resaveRdaFiles()} to report on
       the format of \file{.rda}/\file{.RData} data files, and to
       re-save them in a different compressed format, including choosing
       the most compact format available.

       A new \command{INSTALL} option, \option{--resave-data}, makes use
       of this.

      \item File \file{~/.R/config} is used in preference to
       \file{~/.Rconfig}, and these are now documented in \sQuote{R
       Installation and Administration}.

      \item Logic operations with complex numbers now work, as they were always
       documented to, and as in S.

      \item \code{arrows()} and \code{segments()} allow one of \code{x1}
       or \code{y1} to be omitted to simplify the specification of
       vertical or horizontal lines (suggestion of Tim Hesterberg).

      \item \code{approxfun()} is faster by avoiding repeated \code{NA}
       checks (diagnosis and patch by Karline Soetaert & Thomas
       Petzoldt).

      \item There are the beginnings of a Nynorsk translation by Karl Ove
       Hufthammer.

      \item \code{stripchart()} allows \code{par} \code{bg} to be
       passed in for the background colour for \code{pch = 21} (wish of
       \PR{13984}).

      \item New generic function \code{.DollarNames()} to enable class authors
       to customize completion after the \code{$} extractor.

      \item \code{load()}, \code{save()}, \code{dput()} and
       \code{dump()} now open a not-yet-open connection in the
       appropriate mode (as other functions using connections directly
       already did).
    }
  }
  \subsection{REGULAR EXPRESSIONS}{
    \itemize{
      \item A different regular expression engine is used for basic and
       extended regexps and is also for approximate matching.  This is
       based on the TRE library of Ville Laurikari, a modified copy of
       which is included in the \R sources.

       This is often faster, especially in a MBCS locale.

       Some known differences are that it is less tolerant of invalid
       inputs in MBCS locales, and in its interpretation of undefined
       (extended) regexps such as \code{"^*"}.  Also, the interpretation of
       ranges such as \code{[W-z]} in caseless matching is no longer to map
       the range to lower case.

       This engine may in future be used in \sQuote{literal} mode for
       \code{fixed = TRUE}, and there is a compile-time option in
       \code{src/main/grep.c} to do so.

      \item The use of repeated boundary regexps in \code{gsub()} and
       \code{gregexpr()} as warned about in the help page does not work
       in this engine (it did in the previous one since 2005).

      \item Extended (and basic) regexps now support same set of options
       as for \code{fixed = TRUE} and \code{perl = TRUE}, including
       \code{useBytes} and support for UTF-8-encoded strings in
       non-UTF-8 locales.

      \item \code{agrep()} now has full support for MBCS locales with a modest
       speed penalty.  This enables \code{help.search()} to use approximate
       matching character-wise rather than byte-wise.

      \item \code{[g]sub} use a single-pass algorithm instead of
       matching twice and so is usually faster.

      \item The \code{perl = TRUE} versions now work correctly in a
       non-UTF-8 MBCS locale, by translating the inputs to UTF-8.

      \item \code{useBytes = TRUE} now inhibits the translation of inputs with
       marked encodings.

      \item \code{strsplit()} gains a \code{useBytes} argument.

      \item The algorithm used by \code{strsplit()} has been reordered
       to batch by elements of \code{split}: this can be much faster for
       \code{fixed = FALSE} (as multiple compilation of regexps is avoided).

      \item The help pages, including \code{?regexp}, have been updated
       and should be consulted for details of the new implementations.
    }
  }
  \subsection{HELP & Rd FILE CHANGES}{
    \itemize{
      \item A new dynamic HTML help system is used by default, and may be
       controlled using \code{tools::startDynamicHelp()}.  With this enabled,
       HTML help pages will be generated on request, resolving links
       by searching through the current \code{.libPaths()}.  The user
       may set options(\code{"help.ports"}) to control which IP port is used
       by the server.

      \item \code{help.start()} no longer sets \code{options(htmlhelp =
       TRUE)} (it used to on Unix but not on Windows).  Nor does it on
       Unix reset the \code{"browser"} option if given an argument of
       that name.

       Arguments \code{update} and \code{remote} are now available
       on all platforms: the default is \code{update = FALSE} since the
       http server will update the package index at first use.

      \item \code{help()} has a new argument \code{help_type} (with
       default set by the option of that name) to supersede arguments
       \code{offline}, \code{htmlhelp} and \code{chmhelp}
       (although for now they still work if \code{help_type} is
       unset).  There is a new type, \code{"PDF"} to allow offline PDF
       (rather than PostScript).

       A function \code{offline_help_helper()} will be used if this exists
       in the workspace or further down the search path, otherwise
       the function of that name in the \pkg{utils} namespace is used.

      \item Plain text help is now used as the fallback for HTML help (as
       it always was for Compiled HTML help on Windows).

      \item It is possible to ask for static HTML pages to be prebuilt
       \emph{via} the configure option \option{--enable-prebuilt-html}.  This
       may be useful for those who wish to make HTML help available
       outside R, e.g. on a local web site.

      \item An experimental tag \code{\\Sexpr} has been added to Rd
       files, to evaluate expressions at build, install, or render time.
       Currently install time and render time evaluation are supported.

      \item Tags \code{\\if}, \code{\\ifelse} and \code{\\out} have been
       added to allow format-specific (or more general, using
       \code{\\Sexpr}) conditional text in man pages.

      \item The \code{parse_Rd()} parser has been made more tolerant of coding
       errors in Rd files:  now all syntax errors are reported as
       warnings, and an attempt is made to continue parsing.

      \item \code{parse_Rd()} now has an argument \code{fragment}
       (default \code{FALSE}) to accept small fragments of Rd files (so
       that \code{\\Sexpr} can output Rd code which is then parsed).

      \item \code{parse_Rd()} now always converts its input to UTF-8.
       The \code{Rd2*} rendering functions have a new argument,
       \code{outputEncoding}, which controls how their output is
       encoded.

      \item \code{parse_Rd()} no longer includes the newline as part of
       a \code{"\%"}-style comment.

      \item There have been various bug fixes and code reorganization in
       the Rd renderers \code{Rd2HTML}, \code{Rd2latex}, \code{Rd2txt},
       and \code{Rd2ex}.

       All example files are now created with either ASCII or UTF-8
       encoding, and the encoding is only marked in the file if there
       is any non-UTF-8 code (previously it was marked if the help
       file had non-ASCII contents, possibly in other sections).

      \item \code{print.Rd()} now adds necessary escape characters so that
       printing and re-parsing an Rd object should produce an
       equivalent object.

      \item \code{parse_Rd()} was incorrectly handling multiple backslashes in
       \R code strings, converting 4n+3 backslashes to 2n+1 instead
       of 2n+2.

      \item \code{parse_Rd()} now recognizes the \code{\\var} tag within
       a quoted string in \R-like text.

      \item \code{parse_Rd()} now treats the argument of
       \code{\\command} as LaTeX-like, rather than verbatim.
    }
  }
  \subsection{COMPRESSION}{
    \itemize{
      \item New function \code{untar()} to list or unpack tar archives,
       possibly compressed.  This uses either an external \command{tar}
       command or an internal implementation.

      \item New function \code{tar()} to create (possibly compressed)
      tar archives.

      \item New functions \code{memCompress()} and
       \code{memDecompress()} for in-memory compression and
       decompression.

      \item \code{bzfile()} has a \code{compress} argument to select
       the amount of effort put into compression when writing.

      \item New function \code{xzfile()} for use with
       \command{xz}-compressed files.  (This can also read files
       compressed by some versions of \command{lzma}.)

      \item \code{gzfile()} looks at the file header and so can now also
       read \command{bzip2}-ed files and \command{xz}-compressed files.

      \item There are the new options of \code{save(compress = "bzip2")}
       and \code{"xz"} to use \command{bzip2} or \command{xz}
       compression (which will be slower, but can give substantially
       smaller files).  Argument \code{compression_level} gives finer
       control over the space/time tradeoffs.

       \code{load()} can read such saves (but only as from this version of \R).

      \item \command{R CMD INSTALL}/\command{check} and
       \code{tools::writePACKAGES} accept a wider range of compressed
       tar archives.  Precisely how wide depends on the capabilities of
       the host system's \command{tar} command: they almost always
       include \command{.tar.bz2} archives, and with modern versions of
       \command{tar} other forms of compression such as \command{lzma}
       and \command{xz}, and arbitrary extensions.

      \item \command{R CMD INSTALL} has a new option
       \option{--data-compress} to control the compression used when
       lazy-loading data.  New possibilities are
       \option{--data-compress=bzip2} which will give ca 15\% better
       compression at the expense of slower installation times, and
       \option{--data-compress=xz}, often giving even better compression
       on large datasets at the expense of much longer installation
       times.  (The latter is used for the recommended packages: it is
       particularly effective for \CRANpkg{survival}.)

      \item \code{file()} for open = \code{""}, \code{"r"} or
       \code{"rt"} will automagically detect compressed files (from
       \command{gzip}, \command{bzip2} or \command{xz}).  This means
       that compressed files can be specified by file name (rather than
       \emph{via} a \code{gzfile()} connection) to \code{read.table()},
       \code{readlines()}, \code{scan()} and so on.

      \item \code{data()} can handle compressed text files with extensions
       \file{.\{txt,tab,csv\}.\{gz,bz2,xz\}} .
    }
  }
  \subsection{DEPRECATED & DEFUNCT}{
    \itemize{
      \item \code{png(type="cairo1")} is defunct: the value is no longer
      recognized.

      \item \code{tools::Rd_parse()} is defunct (as this version of \R
       uses only Rd version 2).

      \item Use of file \file{~/.Rconf} (which was deprecated in favour
       of \file{~/.Rconfig} in 2004) has finally been removed.

      \item Bundles of packages are deprecated.  See \sQuote{Writing R
       Extensions} for the steps needed to unbundle a bundle.

      \item \code{help()} arguments \code{offline}, \code{htmlhelp} and
       \code{chmhelp} are deprecated in favour of \code{help_type}.

      \item \code{clearNames()} (in package \pkg{stats}) is deprecated
      for \code{unname()}.

      \item Basic regular expressions (\code{extended = FALSE}) are
       deprecated in \code{strsplit}, \code{grep} and friends.  There is
       a precise POSIX standard for them, but it is not what recent RE
       engines implement, and it seems that in almost all cases package
       authors intended \code{fixed = TRUE} when using \code{extended =
       FALSE}.

      \item \code{methods::trySilent()} is deprecated in favour of
      \code{try(silent=TRUE)} or -- more efficiently and flexibly --
      something like \code{tryCatch(error = function(e) e)}.

      \item \code{index.search()} is deprecated: there are no longer
       directories of types other than \code{help}.
    }
  }
  \subsection{INSTALLATION}{
    \itemize{
      \item cairo >= 1.2 is now required (1.2.0 was released in July 2006)
       for cairo-based graphics devices (which remain optional).

      \item A suitable \code{iconv()} is now required: support for
       configure option \option{--without-iconv} has been withdrawn (it
       was deprecated in \R 2.5.0).

      \item Perl is no longer \sQuote{essential}.  \R can be built
       without it, but scripts \command{R CMD build}, \command{check},
       \command{Rprof} and \command{Sd2d} currently require it.

      \item A system \command{glob} function is now essential (a working
       \code{Sys.glob()} has been assumed since \R 2.9.0 at least).

      \item C99 support for MBCS is now required, and configure option
       \option{--disable-mbcs} has been withdrawn.

      \item Having a version of \command{tar} capable of automagically
       detecting compressed archives is useful for
       \code{utils::untar()}, and so \command{gtar} (a common name for
       GNU \command{tar}) is preferred to \command{tar}: set environment
       variable \env{TAR} to specify a particular \command{tar} command.
    }
  }
  \subsection{INTERNATIONALIZATION}{
    \itemize{
      \item There is some makefile support for adding/updating translations
       in packages: see \code{po/README} and \sQuote{Writing R Extensions}.

       There is support for the use of \code{dngettext} for C-level
       translations in packages: see \sQuote{Writing R Extensions}.
    }
  }
  \subsection{BUG FIXES}{
    \itemize{
      \item Assigning an extra 0-length column to a data frame by
       \code{DF[, "foo"] <- value} now works in most cases (by filling
       with \code{NA}s) or fails.  (It used to give a corrupt data
       frame.)

      \item \code{validObject()} avoids an error during evaluation in the case
       of various incorrect slot definitions.

      \item \code{n:m} now returns a result of type \code{"integer"} in
       a few more boundary cases.

      \item The \code{zap.ind} argument to \code{printCoefmat()} did
       not usually work as other code attempted to ensure that non-zero
       values had a non-zero representation.

      \item \code{printCoefmat()} formatted groups of columns together,
       not just the \code{cs.ind} group but also the \code{zap.ind}
       group and a residual group.  It now formats all columns except
       the \code{cs.ind} group separately (and zaps the \code{zap.ind}
       group column-by-column).  The main effect will be see in the
       output from \code{print.anova()}, as this grouped SS-like columns
       in the \code{zap.ind} group.

      \item \code{R_ReplDLLinit()} initializes the top-level jump so
       that some embedded applications on Windows no longer crash on
       error.

      \item \code{identical()} failed to take the encoding of character
       strings into account, so identical byte patterns are not
       necessarily identical strings, and similarly Latin-1 and UTF-8
       versions of the same string differ in byte pattern.

      \item \code{methods(f)} used to warn unnecessarily for an S4
       generic \code{f} which had been created based on an existing S3
       generic.

      \item The check for consistent ordering of superclasses was not
       ignoring all conditional relations (the symptom was usually
       spurious warnings for classes extending \code{"array"}).

      \item Trying to assign into a raw vector with an index vector
       containing \code{NA}s could cause a segfault.  Reported by Hervé
       Pagès.

      \item \command{Rscript} could segfault if (by user error) its
       filename argument was missing.  Reported by Martin Morgan.

      \item \code{getAnywhere()} (and functions that use it, including argument
       completion in the console) did not handle special built-in
       functions.  Reported by Romain Francois.

      \item \code{order()} was missing a \code{PROTECT()} call and so
       could segfault when called on character data under certain (rare)
       circumstances involving marked non-native encodings.

      \item \code{prettyNum(z, drop0trailing=TRUE)} did not work
       correctly when \code{z} was a complex vector.  Consequently,
       \code{str(z, \dots)} also did not.  (\PR{13985})

      \item \command{make distclean} removed too many files in
       \code{etc/} if builddir = srcdir.

      \item \command{R CMD} replaced \code{TEXINPUTS} rather than
       appending to it (as documented and intended).

      \item \code{help.start()} no longer fails on unix when
       \code{"browser"} is a function.

      \item \code{pbeta(x, ..., log.p = TRUE)} is sometimes more accurate,
       e.g., for very small \code{x}.

      \item Unserializing a pre-2.8 workspace containing pure ASCII
       character objects with a Latin-1 or UTF-8 encoding would corrupt
       the \code{CHARSXP} cache.
    }
  }
}

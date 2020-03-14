% -*- coding: utf-8 -*-
\newcommand{\Rlogo}{\if{html}{\figure{../../html/Rlogo.svg}{options: class="toplogo" alt="[R logo]"}}\if{latex}{\figure{Rlogo.pdf}{options: width=0.5in}}}

\name{NEWS}
\title{R News}
\encoding{UTF-8}

\section{\Rlogo CHANGES IN 4.0.0}{
  \subsection{SIGNIFICANT USER-VISIBLE CHANGES}{
    \itemize{
       \item \code{matrix} objects now also inherit from class
       \code{"array"}, namely, e.g., \code{class(diag(1))} is
       \code{c("matrix", "array")} which invalidates code assuming that
       \code{length(class(obj)) == 1}, an incorrect assumption that is
       less frequently fulfilled now.

       S3 methods for \code{"array"}, i.e., \code{<someFun>.array()}, are
       now also dispatched for \code{matrix} objects.

       \item \R now provides a syntax for specifying raw character
       constants similar to the one used in C++: \code{r"(...)"} with
       \code{...} any character sequence, except that it must not
       contain the closing sequence \samp{)"}. %}) \samp{*}: Rd-parser bug for \code{*}"
       This makes it easier to write strings that contain backslashes or
       both single and double quotes.  For more details see \code{?Quotes}.

       \item \R now uses a \samp{stringsAsFactors = FALSE} default, and
       hence by default no longer converts strings to factors in calls
       to \code{data.frame()} and \code{read.table()}.

       \item The \code{plot()} S3 generic function is now in package
       \pkg{base} rather than package \pkg{graphics}, as it is
       reasonable to have methods that do not use the \pkg{graphics}
       package. The generic is currently re-exported from the
       \pkg{graphics} namespace to allow packages importing it from
       there to continue working, but this may change in future versions
       of \R.

       Packages which define S4 generics for \code{plot()} should be
       re-installed and package code using such generics from other
       packages needs to ensure that they are imported rather than rely
       on their being found on the search path (as in a namespace, the
       base namespace has priority over the search path).
     }
   }

  \subsection{REFERENCE COUNTING}{
    \itemize{
       \item \R now uses reference counting instead of the \code{NAMED}
       mechanism for determining when objects can be safely mutated in
       base C code.  This reduces the need for copying in some cases and
       should allow further optimizations in the future.  It should also
       help make the internal code easier to maintain.  This change is
       expected to have minimal impact on packages not using unsupported
       coding practices in their C/C++ code.
     }
   }

  \subsection{MIGRATION TO PCRE2}{
    \itemize{
       \item \R is now built against a PCRE2 library for Perl-like
       regular expressions.  (On non-Windows platforms PCRE1 can
       optionally be used if PCRE2 is not available at build time.)  The
       version of PCRE in use can be obtained via \code{extSoftVersion()}:
       PCRE1 has versions < 10, PCRE2 versions >= 10.

       \item Making PCRE2 available when building \R from source is
       strongly recommended (preferably version 10.30 or later) as PCRE1
       is no longer developed (it only receives bug fixes and has
       outdated Unicode tables)

       \item PCRE2 reports errors for some regular expressions that were
       accepted by PCRE1.  A hyphen now has to be escaped in a character
       class to be interpreted as a literal (unless first or last in the
       class definition).  \samp{\\R}, \samp{\\B} and \samp{\\X} are no
       longer allowed in character classes (PCRE1 treated these as
       literals).

       \item Option \code{PCRE_study} is no longer used with PCRE2, and is
       reported as \code{FALSE} when that is in use.
     }
  }

  \subsection{NEW FEATURES}{
    \itemize{
      \item \code{assertError()} and \code{assertWarning()} (in package
      \pkg{tools}) can now check for \emph{specific} error or warning
      classes \emph{via} the new optional second argument \code{classes}
      (which is not back compatible with previous use of an unnamed second
      argument).

      \item \code{DF2formula()}, the utility for the data frame method of
      \code{formula()}, now works without parsing and explicit evaluation,
      starting from Suharto Anggono's suggestion in \PR{17555}.

      \item \code{approxfun()} and \code{approx()} gain a new argument
      \code{na.rm} defaulting to true.  If set to false, missing
      \code{y} values now propagate into the interpolated values.

      \item Long vectors are now supported as the \code{seq} argument of
      a \code{for()} loop.

      \item \code{str(x)} gets a new \code{deparse.lines} option with a
      default to speed it up when \code{x} is a large \code{call} object.

      \item The internal traceback object produced when an error is
      signalled (\code{.Traceback}), now contains the \code{call}s
      rather than the \emph{\code{deparse()}d} calls, deferring the
      deparsing to the user-level functions \code{.traceback()} and
      \code{traceback()}, fulfilling the wish of \PR{17580}, reported
      incl.\sspace{}two patch proposals by Brodie Gaslam.

      \item \code{data.matrix()} now converts character columns to
      factors and from this to integers.

      \item \code{package.skeleton()} now explicitly lists all exports
      in the \file{NAMESPACE} file.

      \item New function \code{.S3method()} to register S3 methods in R
      scripts.

      \item \code{file.path()} has some support for file paths not in
      the session encoding, e.g.\sspace{}with UTF-8 inputs in a
      non-UTF-8 locale the output is marked as UTF-8.

      \item Most functions with file-path inputs will give an explicit
      error if a file-path input in a marked encoding cannot be
      translated (to the native encoding or in some cases on Windows to
      UTF-8), rather than translate to a different file path using
      escapes.  Some (such as \code{dir.exists()}, \code{file.exists()},
      \code{file.access()}, \code{file.info()}, \code{list.files()},
      \code{normalizePath()} and \code{path.expand()}) treat this like
      any other non-existent file, often with a warning.

      \item New function \code{list2DF()} for creating data frames from
      lists of variables.

      \item \code{iconv()} has a new option \code{sub = "Unicode"} to
      translate UTF-8 input invalid in the \samp{to} encoding using
      \samp{<U+xxxx>} escapes.

      \item There is a new help document accessed by
      \code{help("file path encoding")}
      detailing how file paths with marked encodings are handled.

      \item There is a new function \code{infoRDS()} providing information
      about the serialization format of a serialized object.

      \item S3 method lookup now by default skips the elements of the
      search path between the global and base environments.

      \item Added an argument \code{add_datalist(*, small.size = 0)} to
      allow the creation of a \file{data/datalist} file even when the
      total size of the data sets is small.

      \item The backquote function \code{bquote()} has a new argument
      \code{splice} to enable splicing a computed list of values into an
      expression, like \code{,@} in LISP's backquote.

      \item The formula interface to \code{t.test()} and
      \code{wilcox.test()} has been extended to handle one-sample and
      paired tests.

      \item The \code{palette()} function has a new default set of
      colours (which are less saturated and have better accessibility
      properties).  There are also some new built-in palettes, which
      are listed by the new \code{palette.pals()} function.  These
      include the old default palette under the name \code{"R3"}. Finally,
      the new \code{palette.colors()} function allows a subset of
      colours to be selected from any of the built-in palettes.

      \item \code{n2mfrow()} gains an option \code{asp = 1} to specify the
      aspect ratio, fulfilling the wish and extending the proposal of
      Michael Chirico in \PR{17648}.

      \item \code{head(x, n)} and \code{tail()} default and other S3
      methods notably for \emph{vector} \code{n}, e.g. to get a
      \dQuote{corner} of a matrix, also extended for \code{array}'s of
      higher dimension, thanks to the patch proposal by Gabe Becker in
      \PR{17652}.  Consequently, optional argument \code{addrownums} is
      deprecated and replaced by the (more general) \code{keepnums}.
      Invalid (second argument) \code{n} now leads to typically more
      easily readable error messages.

      \item New function \code{.class2()} provides the full character
      vector of class names used for S3 method dispatch.

      \item Printing \code{methods(..)} now uses new \code{format()} method.

      \item \code{sort.list(x)} now works for non-atomic objects \code{x}
      and \code{method="auto"} (default) or \code{"radix"} in cases
      \code{order(x)} works.

      \item Where available, \code{writeBin()} now allows long vectors
      (those of length \eqn{\ge 2^{31}}{>= 2^31}).

      \item New function \code{deparse1()} produces one string, wrapping
      \code{deparse()}, to be used typically in
      \code{deparse1(substitute(*))}, e.g., to fix \PR{17671}.

      \item \code{wilcox.test()} enhancements: In the (non-paired)
      two-sample case, \code{Inf} values are treated as very large for
      robustness consistency.  If exact computations are used, the
      result now has \code{"exact"} in the \code{method} element of its
      return value.  New arguments \code{tol.root} and
      \code{digits.rank}.  The latter may be used for stability to treat
      very close numbers as ties.

      \item \code{readBin()} and \code{writeBin()} now report an error for
      invalid \code{endian} value.  The affected code needs to be fixed with
      care as the old undocumented behavior was to swap endian-ness in such
      cases.

      \item \code{sequence()} is now an S3 generic, with an internally
      implemented default method, and gains arguments to generate more
      complex sequences.  Based on code from the \pkg{S4Vectors}
      Bioconductor package and the advice of Hervé Pagès.

      \item \code{print()}'s default method and many other methods (by
      calling the default eventually and passing \code{...}) now make
      use of a new optional \code{width} argument, avoiding the need for
      the user to set and reset \code{options("width")}.

      \item \code{memDecompress()} supports the RFC 1952 format
      (e.g.\sspace{}in-memory copies of \command{gzip}-compressed files)
      as well as RFC 1950.

      \item \code{memCompress()} and \code{memDecompress()} support long
      raw vectors for types \code{"gzip"} and \code{"zx"}.

      \item \code{sweep()} and \code{slice.index()} can now use names
      of dimnames for their \code{MARGIN} argument (\code{apply} has
      had this for almost a decade).

      \item New function \code{proportions()} and
      \code{marginSums()}. These should replace the unfortunately named
      \code{prop.table()} and \code{margin.table()}. They are drop-in
      replacements, but also add named-margin functionality. The old
      function names are retained as aliases for back-compatibility.

      \item Functions \code{rbinom()}, \code{rgeom()}, \code{rhyper()},
      \code{rpois()}, \code{rnbinom(),} \code{rsignrank()} and
      \code{rwilcox()} which have returned integer since \R 3.0.0 and hence
      \code{NA} when the numbers would have been outside the integer range,
      now return double vectors (without NAs, typically) in these cases.

      \item \code{matplot(x,y)} (and hence \code{matlines()} and
      \code{matpoints()}) now call the corresponding methods of
      \code{plot()} and \code{lines()}, e.g, when \code{x} is a
      \code{"Date"} or \code{"POSIXct"} object; prompted by Spencer Graves'
      suggestion.

      \item \code{stopifnot()} now allows customizing error messages via
      argument names, thanks to a patch proposal by Neal Fultz in \PR{17688}.

      \item  \code{unlink()} gains a new argument \code{expand} to
      disable wildcard and tilde expansion.

      \item \code{mle()} in the \code{stats4} package has had its
      interface extended so that arguments to the negative
      log-likelihood function can be one or more vectors, with similar
      conventions applying to bounds, start values, and parameter values
      to be kept fixed.  This required a minor extension to class
      \code{"mle"}, so saved objects from earlier versions may need to
      be recomputed.

      \item The default for \code{pdf()} is now
      \code{useDingbats = FALSE}.

      \item The default (fill colour) for both \code{hist()} and
      \code{boxplot()} is now \code{col = "lightgray"}.

      \item The default order of the levels on the y-axis
      for both \code{spineplot()} and \code{cdplot()}
      has been reversed.

      \item If the \env{R_ALWAYS_INSTALL_TESTS} environment variable is
      set to a true value, \command{R CMD INSTALL} behaves as if
      the \option{--install-tests} option is always specified. Thanks to
      Reinhold Koch for the suggestion.

      \item New function \code{R_user_dir()} in package \pkg{tools} for
      paths appropriate for storing R-related user-specific data,
      configuration and cache files.

      \item \code{capabilities()} gains a new logical option \code{Xchk}
      to avoid warnings about X11-related capabilities.

      \item The internal implementation of \pkg{grid} units has changed,
      but the only visible effects at user-level should be: a slightly
      different print format for some units (especially unit
      arithmetic); faster performance (for unit operations); and two new
      functions \code{unitType()} and \code{unit.psum()}.  Based on code
      contributed by Thomas Lin Pedersen.

      \item When internal dispatch for \code{rep.int()}
      and \code{rep_len()} fails, there is an attempt to dispatch on the
      equivalent call to \code{rep()}.

      \item Object \code{.Machine} now contains new \code{longdouble.*}
      entries (when \R uses long doubles internally).

      \item \code{news()} has been enhanced to cover the news on \R 3.x
      and 2.x.

       \item \code{N <- NULL; N[[1]] <- val} now turns \code{N} into a
       \code{list} also when \code{length(val) == 1}, for consistency.
       This enables \code{dimnames(r1)[[1]] <- "R1"} for a 1-row matrix
       \code{r1}, fixing \PR{17719}, reported by Serguei Sokol.

       \item \code{deparse(..)}, \code{dump(..)}, and \code{dput(x,
	 control="all")} now include using control option \code{"digits17"}
       which typically ensures 1:1 invertibility.  New option \code{control
	 = "exact"} ensures numeric exact invertibility via \code{"hexDigits"}.

      \item When loading data sets via \code{read.table()},
       \code{data()} now uses \samp{LC_COLLATE=C} to ensure locale
       independent results for possible string-to-factor conversions.
    }
  }% end{ NEW FEATURES }

  \subsection{Windows}{
    \itemize{
      \item \command{Rterm} now works also when invoked from MSYS2
      terminals.  Line editing is possible when command \command{winpty} is
      installed.

      \item \code{normalizePath()} now resolves symbolic links and
      normalizes case of long names of path elements in case-insensitive
      folders (\PR{17165}).

      \item \code{md5sum()} supports UTF-8 file names with characters
      that cannot be translated to the native encoding (\PR{17633}).

      \item \command{Rterm} gains a new option \option{--workspace} to
      specify the workspace to be restored.  This allows equals to be
      part of the name when opening via Windows file associations
      (reported by Christian Asseburg).

      \item \command{Rterm} now accepts \code{ALT+xxx} sequences also
      with NumLock on.  Tilde can be pasted with an Italian keyboard
      (\PR{17679}).
   }
  }

  \subsection{DEPRECATED AND DEFUNCT}{
    \itemize{
      \item Make macro \samp{F77_VISIBILITY} has been removed
      and replaced by \samp{F_VISIBILITY}.

      \item Make macros \samp{F77}, \samp{FCPIFCPLAGS} and
      \samp{SHLIB_OPENMP_FCFLAGS} have been removed and replaced by
      \samp{FC}, \samp{FPICFLAGS} and \samp{SHLIB_OPENMP_FFLAGS}
      respectively.  (Most \command{make} programs will set \samp{F77}
      to the value of \samp{FC}, which is set for package compilation.
      But portable code should not rely on this.)

      \item The deprecated support for specifying C++98 for package
      installation has been removed.

      \item \command{R CMD config} no longer knows about the
      unused settings \samp{F77} and \samp{FCPIFCPLAGS}, nor
      \samp{CXX98} and similar.

      \item Either PCRE >= 8.32 (Nov 2012) or PCRE2 is required: the
      deprecated provision for 8.20--8.31 has been removed.
    }
  }

  \subsection{C-LEVEL FACILITIES}{
    \itemize{
      \item \code{installChar} is now remapped in \file{Rinternals.h} to
      \code{installTrChar}, of which it has been a wrapper since
      \R\sspace{}3.6.0.  Neither are part of the API, but packages using
      \code{installChar} can replace it if they depend on \samp{R >= 3.6.2}.

      \item Header \file{R_ext/Print.h} defines \samp{R_USE_C99_IN_CXX}
      and hence exposes \code{Rvprintf} and \code{REvprintf} if used
      with a C++11 (or later) compiler.

      \item There are new Fortran subroutines \code{dblepr1},
      \code{realpr1} and \code{intpr1} to print a scalar variable
      (\command{gfortran} 10 enforces the distinction between scalars
      and length-one arrays).  Also \code{labelpr} to print just a label.
    }
  }

  \subsection{INSTALLATION on a UNIX-ALIKE}{
    \itemize{
      \item User-set \samp{DEFS} (e.g., in \file{config.site}) is now
      used for compiling packages (including base packages).

      \item There is a new variant option \option{--enable-lto=check}
      for checking consistency of BLAS/LAPACK/LINPACK calls --- see
      \sQuote{Writing R Extensions}.

      \item A C++ compiler default is set only if the C++11 standard is
      supported: it no longer falls back to C++98.

      \item PCRE2 is used if available.  To make use of PCRE1 if PCRE2
      is unavailable, configure with option \option{--with-pcre1}.

      \item The minimum required version of \code{libcurl} is now 7.28.0
      (Oct 2012).
    }
  }

  \subsection{UTILITIES}{
    \itemize{
      \item \command{R --help} now mentions the option \command{--no-echo}
      (renamed from \command{--slave}) and its previously undocumented
      short form \command{-s}.

      \item \command{R CMD check} now optionally checks
      \command{configure} and \command{cleanup} scripts for
      non-Bourne-shell code (\sQuote{bashisms}).

      %% is now the overall default
      %% \item \command{R CMD check --as-cran} now uses a
      %% \samp{stringsAsFactors = FALSE} default setting.
    }
  }

  \subsection{BUG FIXES}{
    \itemize{
      \item \code{formula(x)} with \code{length(x) > 1} character vectors,
      is deprecated now.  Such use has been rare, and has \sQuote{worked}
      as expected in some cases only.  In other cases, wrong \code{x} have
      silently been truncated, not detecting previous errors.

      \item Long-standing issue where the X11 device could lose events
      shortly after startup has been addressed (\PR{16702}).

      \item The \code{data.frame} method for \code{rbind()} no longer
      drops \code{<NA>} levels from factor columns by default
      (\PR{17562}).

      \item \code{available.packages()} and hence \code{install.packages()}
      now pass their \code{...} argument to \code{download.file()},
      fulfilling the wish of \PR{17532}; subsequently,
      \code{available.packages()} gets new argument \code{quiet}, solving
      \PR{17573}.

      \item \code{stopifnot()} gets new argument \code{exprObject} to allow
      an \R object of class \code{expression} (or other \sQuote{language})
      to work more consistently, thanks to suggestions by Suharto Anggono.

      \item \code{conformMethod()} now works correctly in cases containing
      a \dQuote{\code{&&} logic} bug, reported by Henrik Bengtsson.  It now
      creates methods with \code{"missing"} entries in the signature.
      Consequently, \code{rematchDefinition()} is amended to use
      appropriate \code{.local()} calls with named arguments where needed.

      \item \code{format.default(*, scientific = FALSE)} now corresponds to
      a practically most extreme \code{options(scipen = n)} setting rather
      than arbitrary \code{n = 100}.

      \item \code{format(as.symbol("foo"))} now works (returning \code{"foo"}).

      \item \code{postscript(.., title = *)} now signals an error when the
      title string contains a character which produces corrupt PostScript,
      thanks to Daisuko Ogawa's \PR{17607}.

      \item Certain \code{Ops} (notably comparison such as \code{==}) now
      also work for 0-length data frames, after reports by Hilmar Berger.

      \item \code{methods(class = class(glm(..)))} now warns more usefully
      and only once.

      \item \code{write.dcf()} no longer mangles field names (\PR{17589}).

      \item \code{Sys.glob()} on Windows now supports all characters
      from the Unicode Basic Multilingual Plane, no longer corrupting
      (less commonly used) characters (\PR{17638}).

      \item Primitive replacement functions no longer mutate a
      referenced first argument when used outside of a complex
      assignment context.

      \item Better error message for \code{contour(*, levels = Inf)}.

      \item The return value of \code{contourLines()} is no longer
      \code{invisible()}.

      \item (Windows) \command{Rterm} now correctly displays
      multi-byte-coded characters representable in the current native
      encoding (at least on Windows 10 they were sometimes omitted,
      \PR{17632}).

      \item The Fortran code for calculating the \code{coefficients}
      component in \code{lm.influence()} was very inefficient. It has
      (for now) been replaced with much faster \R{} code (\PR{17624}).

      \item \code{cm.colors(n)} \emph{etc} no longer append the code for
      \code{alpha = 1}, \code{"FF"}, to all colors.  Hence all eight
      \code{*.colors()} functions and \code{rainbow()} behave
      consistently and have the same non-explicit default (\PR{17659}).

      \item \code{dnorm} had a problematic corner case with \code{sd ==
      -Inf} and negative \code{sd} which was not flagged as an error in
      all cases. Thanks to Stephen D. Weigand for reporting and Wang
      Jiefei for analyzing this; similar change has been made in
      \code{dlnorm()}.

      \item The optional \code{iter.smooth} argument of
      \code{plot.lm()}, (the \code{plot()} method for \code{lm} and
      \code{glm} fits) now defaults to \code{0} for all \code{glm} fits.
      Especially for binary observations with high or low fitted
      probabilities, this effectively deleted all observations of 1 or
      0.  Also, the type of residuals used in the \code{glm} case has
      been switched to \code{"pearson"} since deviance residuals do not
      in general have approximately zero mean.

      \item In \code{plot.lm}, Cook's distance was computed from unweighted
      residuals, leading to inconsistencies.  Replaced with usual weighted
      version. (\PR{16056})

      \item Time-series \code{ts(*, start, end, frequency)} with
      fractional \code{frequency} are supported more consistently;
      thanks to a report from Johann Kleinbub and analysis and patch by
      Duncan Murdoch in \PR{17669}.

      \item In case of errors \code{mcmapply()} now preserves attributes
      of returned \code{"try-error"} objects and avoids simplification,
      overriding \code{SIMPLIFY} to \code{FALSE}. (\PR{17653})

      \item \code{as.difftime()} gets new optional \code{tz = "UTC"}
      argument which should fix behaviour during
      daylight-savings-changeover days, fixing \PR{16764}, thanks to
      proposals and analysis by Johannes Ranke and Kirill Müller.

      \item \code{round()} does a better job of rounding
      \emph{\dQuote{to nearest}} by \emph{measuring} and
      \emph{\dQuote{to even}}; thanks to a careful algorithm originally
      prompted by the report from Adam Wheeler and then others, in
      \PR{17668}.  \cr \code{round(x, dig)} for \emph{negative} digits
      is much more rational now, notably for large \eqn{|dig|}.

      \item Inheritance information on S4 classes is maintained more
      consistently, particularly in the case of class unions (in part
      due to \PR{17596} and a report from Ezra Tucker).

      \item \code{is()} behaves more robustly when its argument
      \code{class2} is a \code{classRepresentation} object.

      \item The warning message when attempting to export an nonexistent
      class is now more readable; thanks to Thierry Onkelinx for
      recognizing the problem.

      \item \code{scan()} issues with UTF-8 data when running in a DBCS
      locale (on Windows) have been resolved (\PR{16520},\PR{16584}).

      \item \code{choose()} misbehaved in corner cases where it switched
      \code{n - k} for \code{k} and \code{n} was only \emph{nearly} integer
      (report from Erik Scott Wright).

      \item \code{mle()} in the \code{stats4} package had problems
      combining use of box constraints and fixed starting values (in
      particular, confidence intervals were affected).

      \item Operator \code{?} now has lower precedence than \code{=}
      to work as documented and to behave like \code{<-} in help
      expressions (\PR{16710}).

      \item \code{smoothEnds(x)} now returns \code{integer} type in
      \emph{both} cases when \code{x} is \code{integer}, thanks to report
      and proposal by Bill Dunlap \PR{17693}.

      \item The \pkg{methods} package does a better job of tracking
      inheritance relationships across packages.

      \item \code{norm(diag(c(1, NA)), "2")} now works.

      \item \code{subset()} had problems with 0-col dataframes (Bill Dunlap,
      \PR{17721})

      \item Several cases of integer overflow detected by the
      \sQuote{undefined behaviour analyser} of \command{clang} 10 have
      been circumvented.  One in \code{rhyper()} may change the
      generated value for large input values.

      \item \code{dotchart()} now places the y-axis label (\code{ylab})
      much better, not overplotting labels, thanks to a report and
      suggestion by Alexey Shipunov.
    }
  }
}

\section{CHANGES in previous versions}{
  \itemize{
    \item Older news can be found in text format in files
    \ifelse{html}{\href{../NEWS.0}{NEWS.0}, \href{../NEWS.1}{NEWS.1},
      \href{../NEWS.2}{NEWS.2} and
      and \href{../NEWS.3}{NEWS.3}}{\file{NEWS.0}, \file{NEWS.1},
      \file{NEWS.2} and \file{NEWS.3}}
    in the \file{doc} directory.  News in HTML format for
    \Rversions 3.x and from 2.10.0 to 2.15.3 is in
    \ifelse{html}{\url{NEWS.3.html}}{\file{doc/html/NEWS.3.html}}.
    and
    \ifelse{html}{\url{NEWS.2.html}}{\file{doc/html/NEWS.2.html}}.
  }
}

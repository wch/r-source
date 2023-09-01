% -*- coding: utf-8 -*-
\newcommand{\Rlogo}{\if{html}{\figure{../../html/Rlogo.svg}{options: class="toplogo" alt="[R logo]"}}\if{latex}{\figure{Rlogo.pdf}{options: width=0.5in}}}

\name{NEWS}
\title{R News}
\encoding{UTF-8}

\section{\Rlogo CHANGES IN R-devel}{
  \subsection{SIGNIFICANT USER-VISIBLE CHANGES}{
    \itemize{
      \item Startup banners, \command{R --version}, \code{sessionInfo()}
      and \command{R CMD check} no longer report \code{(64-bit)} as part of
      the platform as this is almost universal -- the increasingly rare
      32-bit platforms will still report \code{(32-bit)}.

      On Windows, ditto for window titles.
    }
  }

  \subsection{NEW FEATURES}{
    \itemize{
      \item The \code{confint()} methods for \code{"glm"} and
      \code{"nls"} objects have been copied to the \pkg{stats}
      package.  Previously, they were stubs which called versions in
      package \pkg{MASS}. The \pkg{MASS} namespace is no longer loaded
      if you invoke (say) \code{confint(glmfit)}.  Further, the
      \code{"glm"} method for \code{profile()} and the \code{plot()}
      and \code{pairs()} methods for class \code{"profile"} have been
      copied from \pkg{MASS} to \pkg{stats}.  (\code{profile.nls()} and
      \code{plot.profile.nls()} were already in \code{stats}.)

      \item The \code{confint()} and \code{profile} methods for
      \code{"glm"} objects have gained a possibility to do profiling
      based on the Rao Score statistic in addition to the default
      Likelihood Ratio. This is controlled by a new \code{test =}
      argument.

      \item The \code{"glm"} method for \code{anova()} computes test
      statistics and p-values by default, using a chi-squared test or
      an F test depending on whether the dispersion is fixed or
      free.  Test statistics can be suppressed by giving argument
      \code{test} a false logical value.

      \item In \code{setRepositories()} the repositories can be set
      using their names via \code{name =} instead of index \code{ind =}.

      \item \code{methods()} and \code{.S3methods()} gain a
      \code{all.names} option for the (rare) case where functions starting
      with a \samp{.} should be included.

      \item Serializations can now be interrupted (e.g., by \kbd{Ctrl-C} on
      a Unix-alike) if they take too long, e.g., from \code{save.image()},
      thanks to suggestions by Ivan Krylov and others on R-devel.

      \item New startup option \option{--max-connections} to set the
      maximum number of connections for the session.  Defaults to 128 as
      before: allowed values up to 4096 (but resource limits may in practice
      restrict to smaller values).

      \item \R on Windows (since Windows 10 2004) now uses the new Segment
      Heap allocator. This may improve performance of some memory-intensive
      applications.

      \item When \R packages are built, typically by \command{R CMD build <pkg>},
      the new \command{--user=<build_user>} option overrides the
      (internally determined) user name, currently \code{Sys.info()["user"]}
      or \env{LOGNAME}.  This is a (modified) fulfillment of Will Landau's
      suggestion in \PR{17530}.

      \item \pkg{tools}\code{::testInstalledBasic()} gets new optional
      arguments \code{outDir} and \code{testSrcdir}, e.g., allowing to use
      it in a \sQuote{<builddir> != <srcdir>} setup, and in standard
      \dQuote{binary} Windows installation \bold{if} a source \file{tests/}
      folder is present.

      \item \code{range(<DT_with_Inf>, finite = TRUE)} now work for objects
      of class \code{"Date"}, \code{"POSIXct"}, and \code{"POSIXlt"} with
      infinite entries, analogously to \code{range.default()}, as proposed
      by Davis Vaughan on R-devel.  Other \code{range()}-methods can make
      use of new \code{.rangeNum()}.

      \item New \code{.internalGenerics} complementing
      \code{.S3PrimitiveGenerics}, for documentation and low-level book
      keeping.

      \item \code{grid()} now invisibly returns the x- and y- coordinates
      at which the grid-lines were drawn.

      \item \code{norm(., type)} now also works for complex matrices.

      \item \code{kappa(., exact = TRUE, norm = *)} now works for all
      norms and also for complex matrices.  In symmetric / triangular
      cases, new argument \code{uplo = "U" | "L"} allows to specify the
      upper or lower triangular part.

      \item \code{memDecompress(type = "unknown")} recognizes compression
      in the default \sQuote{zlib} format as used by
      \code{memCompress(type = "gzip")}.

      \item \code{memCompress()} and \code{memDecompress()} will use the
      \code{libdeflate} library
      (\url{https://github.com/ebiggers/libdeflate}) if installed.  This
      uses the same type of compression for \code{type = "gzip"} but is
      1.5-2x faster than the system \code{libz} library on some
      common platforms: the speed-up may depend on the library version.
      %% libdeflate 1.13 claims significant improvements in
      %% decompression speed.

      \item \code{diff()} for objects of class \code{"Date"},
      \code{"POSIXct"}, and \code{"POSIXlt"} accepts a \code{units}
      argument passed via \code{...}.

      \item Dynamic help now does a much better job rendering package
      \file{DESCRIPTION} metadata.

      \item \code{Rprof()} gains an \code{event} argument and support
      for elapsed (real) time profiling on Unix (\PR{18076}).

      \item \code{filled.contour()} gains \code{key.border} argument.

      \item \code{tools::update_pkg_po()} gets \code{pot_make} and
      \code{mo_make} options for \emph{not} re-making the corresponding
      files, and additionally option \code{verbose}.

      \item Hexadecimal string colour specifications are now accepted
      in short form, so, for example, we can use \code{"#123"}, which is
      equivalent to \code{"#112233"}.

      Thanks to MikeFC for the original idea and Ella Kaye, Malcolm
      Barrett, George Stagg, Hanne Oberman, and Michael Chirico for the
      patch.
    }
  }

  \subsection{INSTALLATION on a UNIX-ALIKE}{
    \itemize{
      \item System valgrind headers are required to use
      \command{configure} option \option{--with-valgrind-instrumentation}
      with value \code{1} or \code{2}.

      \item \command{configure} will warn if it encounters a 32-bit
      build, as that is nowadays almost untested.

      \item Enviroment variable \env{R_SYSTEM_ABI} is no longer used and
      so no longer recorded in \file{etc/Renviron} (it was not on Windows
      and was only ever used when preparing package \pkg{tools}).

      \item There is an experimental \command{configure} option
      \option{--with-libdeflate-compression} which uses
      \code{libdeflate} rather than \code{libz} to (de)compress \R
      objects in lazy-load databases, if the \code{libdeflate} library
      and headers are available.  Typically tasks spend up to 5\% of
      their time on such operations, although creating lazy-data
      databases is one of the exceptions.

      \emph{pro tem} this can also be selected by setting environment
      variable \env{R_USE_LIBDEFLATE_FOR_LAZYLOAD} to \code{yes}.
    }
  }

  %% \subsection{PACKAGE INSTALLATION}{
  %%   \itemize{
  %%     \item .
  %%   }
  %% }

  \subsection{UTILITIES}{
    \itemize{
      \item \command{R CMD check} notes when S4-style exports are used
      without declaring a strong dependence on package \pkg{methods}.

      \item \code{tools::checkRd()} (used by \command{R CMD check})
      detects more problems with \verb{\Sexpr}-based dynamic content,
      including bad nesting of \verb{\Sexpr}s and invalid arguments.

      \item \code{tools::checkRd()} now reports Rd titles and section
      names ending in a period; this is ignored by \command{R CMD check}
      unless environment variable \env{_R_CHECK_RD_CHECKRD_MINLEVEL_} is
      set to -5 or smaller.

      \item \command{R CMD check} now notes Rd files without an
      \verb{\alias}, as long documented in \sQuote{Writing R Extensions}
      §1.3.1.
      The check for a missing \verb{\description} has been moved from
      \code{tools::checkRd()} to \code{tools::checkRdContents()}.

      \item \command{R CMD check} now visits \file{inst/NEWS.Rd}
      when checking Rd files.

      \item \code{tools::checkDocFiles()} and
      \code{tools::checkRdContents()} now also check internal Rd files
      by default, but \dQuote{specially} (ignoring missing documentation
      of arguments).

      \item \command{R CMD Rdiff} gets option \option{--useEx}.
    }
  }

  \subsection{BUG FIXES}{
    \itemize{
      \item Arguments are now properly forwarded to methods on S4 generics
      with \dots{} in the middle of their formal arguments. This was
      broken for the case when a method introduced an argument but did
      not include \dots{} in its own formals. Thanks to Hervé Pagès for
      the report \PR{18538}.

      \item Probability distribution functions \code{[dpq]<distrib>(x, *)},
      but also \code{bessel[IKJY](x, .)} are now consistently preserving
      \code{attributes(x)} when \code{length(x) == 0}, e.g., for a
      \eqn{2 \times 0}{2 x 0} matrix, thanks to Karolis Koncevičius'
      report \PR{18509}.

      \item Group \dQuote{Summary} computations such as
      \code{sum(1:3, 4, na.rm = 5, NA, 7, na.rm = LL)} now give an error
      instead of either \code{17} or \code{NN} for \code{LL} true or false,
      as proposed by Ivan Krylov on the R-devel mailing list.  (This
      also means it is now an error to specify \code{na.rm} more than once.)

      \item \code{as.complex(x)} now returns \code{NA_complex_} also for
      \code{NA_real_} arguments, as it does for all other \code{NA}s.

      \item Directories are now omitted by
      \code{file.copy(,recursive = FALSE)} and in \code{file.append()}
      (\PR{17337}).

      \item \code{gsub()} and \code{sub()} are now more robust to integer
      overflow when reporting errors caused by too large input strings
      (\PR{18346}).

      \item Top-level handlers are now more robust to attempts to remove a
      handler whilst handlers are running (\PR{18508}).

      \item The handling of \code{Alt+F4} in dialogs created on Windows using
      GraphApp has been fixed (\PR{13870}).

      \item \code{density()} more consistently computes grid values for the
      FFT-based convolution, following Robert Schlicht's analysis and
      proposal in \PR{18337}, correcting density values typically by a
      factor of about 0.999.  Optional \code{old.coords=TRUE} provides
      back compatibility.

      \item \code{palette.colors()} gains a \code{name} argument that
      defaults to \code{FALSE} controlling whether the vector of colours
      that is returned has names (where possible).  \PR{18529}.

      \item \code{tools::xgettext()} no longer extracts the
      (non-translatable) class names from \code{warningCondition} and
      \code{errorCondition} calls.

      \item \code{S3method(<gen>, <class>, <func>)} in the \file{NAMESPACE}
      file now works (again) when \code{<func>} is visible from the
      namespace, e.g., imported, or in base.

      \item \code{getParseData(f)} now also works for a function defined
      in the first of several \file{<pkg>/R/*.R} source files, thanks to
      Kirill Müller's report and Duncan Murdoch's patch in \PR{16756}.

      \item \code{tools::checkRd()} no longer produces spurious notes
      about \dQuote{unnecessary braces} from multi-line Rd results of
      \verb{\Sexpr} macros.

      \item Rd \verb{\Sexpr} macros with nested \verb{#ifdef}
      conditionals were not processed.

      \item A non-blocking connection with non-default encoding such as a
      socket, now correctly returns from \code{readLines()} after new data
      has arrived also when its EOF had been reached previously.  Thanks to
      Peter Meilstrup's report on R-devel and Ivan Krylov's report and
      patch proposal in \PR{18555}.

      \item \code{tools::checkRdContents()} failed to detect empty
      argument descriptions when they spanned multiple lines,
      including those generated by \code{prompt()}.
      These cases are now noted by \command{R CMD check}.

      \item Plain-text help no longer outputs spurious colons in the
      arguments list (for multi-line \verb{\item} labels in the Rd source).

      \item \code{kappa()} and \code{rcond()} work correctly in more cases;
      \code{kappa(., norm = "2")} now warns that it computes the 1-norm with
      (default) \code{exact = FALSE}; prompted by Mikael Jagan's quite
      comprehensive \PR{18543}.

      \item Rd skeletons generated by \code{prompt()} or
      \code{promptData()} now use a dummy title (so \command{R CMD
      build} works).  \code{tools::checkRdContents()} has been updated
      to detect such template leftovers, including from
      \code{promptPackage()}.

      \item When S4 method dispatch fails because no method was found, the
      error message now includes the signature argument names; thanks to
      Michael Chirico's proposal on R-devel.

      \item \code{withAutoprint({ .. })} now preserves \code{srcref}s
      previously lost, thanks to Andrew Simmons' report plus fix in
      \PR{18572}.

      \item \code{transform.data.frame()} no longer adjusts names;
      in particular, untransformed variables are kept as-is,
      including those with syntactically invalid names (\PR{17890}).
    }
  }
}

\section{\Rlogo CHANGES IN R 4.3.1 patched}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item The default initialization of the \code{"repos"} option
      from the \file{repositories} file at startup can be skipped by
      setting environment variable \env{R_REPOSITORIES} to \code{NULL}
      such that \code{getOption("repos")} is empty if not set elsewhere.

      \item \code{qr.X()} is now an implicit S4 generic in \pkg{methods}.

      \item \code{iconv(to = "ASCII//TRANSLIT")} is emulated using
      substitution on platforms which do not support it (notably Alpine
      Linux).  This should give a human-readable conversion in ASCII on
      all platforms (rather than \code{NA_character_}).

      \item \code{trans3d()} gains options \code{continuous} and
      \code{verbose} addressing the problem of possible \dQuote{wrap
	around} when projecting too long curves, as reported by Achim Zeileis
      in \PR{18537}.
    }
  }

  \subsection{INSTALLATION on a UNIX-ALIKE}{
    \itemize{
      \item There is some support for building with Intel's LLVM-based
      compilers on \samp{x86_64} Linux, such as (C) \command{icx}, (C++)
      \command{ipcx} and (Fortran) \command{ifx} from oneAPI 2023.x.0.

      \item There is support for using LLVM's \command{flang-new} as the
      Fortran compiler from LLVM 16.0.x (preferably 17.0.0
      or later).
    }
  }

  \subsection{UTILITIES}{
    \itemize{
      \item \command{R CMD check} reports the use of Fortran 90 random
      number generator \code{RANDOM_NUMBER()} and the subroutines to
      initialize it.

      \sQuote{Writing R Extensions} has example code to use \R{}'s RNGs
      from Fortran.
    }
  }
  
  \subsection{BUG FIXES}{
    \itemize{
      \item \code{substr(x, n, L) <- cc} now works (more) correctly for
      multibyte UTF-8 strings \code{x} when \code{L > nchar(x)}, thanks to
      a report and patch by \sQuote{Architect 95}.

      \item \code{contrib.url(character())} now returns 0-length
      \code{character()} as documented, which also avoids spurious
      warnings from \code{available.packages()} et al.\sspace{}in the
      edge case of an empty vector of repository URLs.

      \item \code{readChar(., 4e8)} no longer fails, thanks to Kodi Arfer's
      report (\PR{18557}).

      \item \code{lapply(<list>, as.data.frame)} no longer warns falsely for
      some base vector components.

      \item Communication between parent and child processes in the
      \code{multicore} part of \code{parallel} could fail on platforms
      that do not support an arbitrarily large payload in system
      functions \code{read()}/\code{write()} on pipes (seen on macOS
      where a restriction to \code{INT_MAX} bytes is documented, without
      doing a partial read unlike Linux). The payload is now split into
      1Gb chunks to avoid that problem. (\PR{18571})

      \item \code{qqplot(x,y, conf.level=.)} gives better confidence bounds
      when \code{length(x) != length(y)}, thanks to Alexander Ploner's
      report and patch proposal (\PR{18557}).

      \item \code{norm(<0-length>, "2")} now gives zero instead of an
      error, as all the other norm types, thanks to Mikael Jagan's \PR{18542}.

      \item Build-stage Rd macros \verb{\packageAuthor} and
      \verb{\packageMaintainer} now process \samp{Authors@R},
      fixing \samp{NA} results when the package \file{DESCRIPTION} omits
      \samp{Author} and \samp{Maintainer} fields.
    }
  }
}

\section{\Rlogo CHANGES IN R 4.3.1}{
  \subsection{C-LEVEL FACILITIES}{
    \itemize{
      \item The C-level API version of \R's \code{integrate()},
      \code{Rdqags()} in \file{Applic.h}, now returns the correct number of
      integrand evaluations \code{neval}, fixing \PR{18515} reported and
      diagnosed by Stephen Wade.

      \item The C prototypes for LAPACK calls \code{dspgv} and \code{dtptrs}
      in \file{R_exts/Lapack.h} had one too many and one too few
      character length arguments --- but this has not caused any known
      issues. To get the corrected prototypes, include
      \preformatted{
	#include <Rconfig.h> // for PR18534fixed
	#ifdef PR18534fixed
	# define usePR18534fix 1
        #endif
	#include <R_exts/Lapack.h>
      }
      in your C/C++ code (\PR{18534}).
    }
  }

  \subsection{INSTALLATION}{
    \itemize{
      \item Many of the checks of esoteric Internet operations and those
      using unreliable external sites have been moved to a new target
      that is not run by default and primarily intended for the core
      developers.  To run them use
\preformatted{
	cd tests; make test-Internet-dev
}
    }
  }

  \subsection{BUG FIXES}{
    \itemize{
      \item \code{.S3methods()}, typically called from \code{methods()},
      again marks methods from package \pkg{base} as \code{visible}.

      Also, the visibility of non-\pkg{base} methods is again determined by
      the method's presence in \code{\link{search}()}.

      \item \code{tools::Rdiff()} is now more robust against invalid
      strings, fixing installation tests on Windows without Rtools
      installed (\PR{18530}).

      \item Fix (new) bug in \code{hcl.colors(2, *)}, by Achim Zeileis (\PR{18523}).

      \item \code{head(., <illegal>)} and \code{tail(..)} now produce
      more useful \code{"Error in ...."} error messages, fixing \PR{18362}.

      \item Package code syntax on Windows is checked in UTF-8 when UTF-8 is
      the native encoding.

      \item \code{na.contiguous(x)} now also returns the first run, when it
      is at the beginning and there is a later one of the same length;
      reported to R-devel, including a fix, by Georgi Boshnakov.
      Further, by default, it modifies only an existing
      \code{attr(*,"tsp")} but otherwise no longer sets one.

      \item \code{chol(<not pos.def>, pivot = <T|F>)} now gives a correct
      error or warning message (depending on \code{pivot}), thanks to
      Mikael Jagan's (\PR{18541}).
    }
  }
}

\section{\Rlogo CHANGES IN R 4.3.0}{
  \subsection{SIGNIFICANT USER-VISIBLE CHANGES}{
    \itemize{
      \item Calling \code{&&} or \code{||} with LHS or (if evaluated) RHS of
      length greater than one is now always an error, with a report of
      the form
\preformatted{    'length = 4' in coercion to 'logical(1)'}

      Environment variable \env{_R_CHECK_LENGTH_1_LOGIC2_} no longer has
      any effect.
    }
  }

  \subsection{NEW FEATURES}{
    \itemize{
      \item The included BLAS sources have been updated to those shipped
      with LAPACK version 3.10.1.  (This caused some platform-dependent
      changes to package check output.)  And then to the sources from
      LAPACK version 3.11.0 (with changes only to double complex
      subroutines).

      \item The included LAPACK sources have been updated to include the
      four Fortran 90 routines rather than their Fortran 77
      predecessors.  This may give some different signs in SVDs or
      eigendecompositions..  (This completes the transition to LAPACK
      3.10.x begun in \R 4.2.0.)

      \item The LAPACK sources have been updated to version 3.11.0.  (No
      new subroutines have been added, so this almost entirely bug
      fixes: Those fixes do affect some computations with \code{NaN}s,
      including \R's \code{NA}.)

      \item The parser now signals \emph{classed} errors, notably in
      case of the pipe operator \code{|>}.  The error object and
      message now give line and column numbers, mostly as proposed and
      provided by Duncan Murdoch in \PR{18328}.

      \item \code{toeplitz()} is now generalized for asymmetric cases,
      with a \code{toeplitz2()} variant.

      \item \code{xy.coords()} and \code{xyz.coords()} and consequently,
      e.g., \code{plot(x,y, log = "y")} now signal a \emph{classed} warning
      about negative values of y (where \code{log(.)} is \code{NA}).
      Such a warning can be specifically suppressed or caught otherwise.

      \item Regular expression functions now check more thoroughly whether
      their inputs are valid strings (in their encoding, e.g.\sspace{}in UTF-8).

      \item The performance of \code{grep()}, \code{sub()},
      \code{gsub()} and \code{strsplit()} has been improved,
      particularly with \code{perl = TRUE} and \code{fixed = TRUE}.  Use
      of \code{useBytes = TRUE} for performance reasons should no longer
      be needed and is discouraged: it may lead to incorrect results.

      \item \code{apropos()} gains an argument \code{dot_internals} which
      is used by the completion (\code{help(rcompgen)}) engine to also
      see \pkg{base} internals such as \code{.POSIXct()}.

      \item Support in \code{tools::Rdiff()} for comparing uncompressed
      PDF files is further reduced -- see its help page.

      \item \code{qqplot(x, y, ...)} gains \code{conf.level} and
      \code{conf.args} arguments for computing and plotting a confidence
      band for the treatment function transforming the distribution of
      \code{x} into the distribution of \code{y } (Switzer, 1976,
      \emph{Biometrika}). Contributed by Torsten Hothorn.

      \item Performance of \code{package_dependencies()} has been improved
      for cases when the number of dependencies is large.

      \item Strings newly created by \code{gsub()}, \code{sub()} and
      \code{strsplit()}, when any of the inputs is marked as \code{"bytes"},
      are also marked as \code{"bytes"}.  This reduces the risk of creating
      invalid strings and accidental substitution of bytes deemed invalid.

      \item Support for \code{readLines(encoding = "bytes")} has been added to
      allow processing special text files byte-by-byte, without creating
      invalid strings.

      \item \code{iconv(from = "")} now takes into account any declared
      encoding of the input elements and uses it in preference to the native
      encoding.  This reduces the risk of accidental creation of invalid
      strings, particularly when different elements of the input have
      different encoding (including \code{"bytes"}).

      \item Package repositories in \code{getOption("repos")} are now
      initialized from the \file{repositories} file when \pkg{utils} is
      loaded (if not already set, e.g., in \file{.Rprofile}).
      (From a report and patch proposal by Gabriel Becker in \PR{18405}.)

      \item \code{compactPDF()} gets a \code{verbose} option.

      \item \code{type.convert()} and hence \code{read.table()} get new
      option \code{tryLogical = TRUE} with back compatible default.  When
      set to false, converts \code{"F"} or \code{"T"} columns to character.

      \item Added new unit prefixes \code{"R"} and \code{"Q"} for
      abbreviating (unrealistically large) sizes beyond \eqn{10^{27}} in
      \code{standard = "SI"}, thanks to Henrik Bengtsson's \PR{18435}.

      \item \code{as.data.frame()}'s default method now also works fine
      with atomic objects inheriting from classes such as \code{"roman"},
      \code{"octmode"} and \code{"hexmode"}, such fulfilling the wish of
      \PR{18421}, by Benjamin Feakins.

      \item The \code{as.data.frame.vector()} utility now errors for
      wrong-length \code{row.names}.  It warned for almost six years, with
      \dQuote{Will be an error!}.

      \item \code{sessionInfo()} now also contains \code{La_version()} and
      reports codepage and timezone when relevant, in both \code{print()}
      and \code{toLatex()} methods which also get new option \code{tzone}
      for displaying timezone information when \code{locale = FALSE}.

      \item New function \code{R_compiled_by()} reports the C and
      Fortran compilers used to build \R, if known.

      \item \code{predict(<lm>, newdata = *)} no longer unnecessarily creates
      an \code{offset} of all \code{0}s.

      \item \code{solve()} for complex inputs now uses argument \code{tol}
      and by default checks for \sQuote{computational singularity} (as
      it long has done for numeric inputs).

      \item \code{predict(<rank-deficient lm>, newdata=*)} now obeys a new
      argument \code{rankdeficient}, with new default \code{"warnif"},
      warning only if there are non-estimable cases in \code{newdata}.
      Other options include \code{rankdeficient = "NA"}, predicting \code{NA}
      for non-estimable \code{newdata} cases.
      This addresses \PR{15072} by Russ Lenth and is based on his original
      proposal and discussions in \PR{16158} also by David Firth and Elin Waring.
      Still somewhat experimental.

      \item \command{Rgui} console implementation now works better with the
      \code{NVDA} screen reader when the full blinking cursor is selected.
      The underlying improvements in cursor handling may help also other
      screen readers on Windows.

      \item The drop-field control in GraphApp can now be left with the TAB
      key and all controls can be navigated in the reverse order using the
      Shift+TAB key, improving accessibility of the \command{Rgui}
      configuration editor.

      \item \code{qnorm(<very large negative>, log.p=TRUE)} is now fully
      accurate (instead of to \dQuote{only} minimally five digits).

      \item \code{demo(error.catching)} now also shows off
      \code{withWarnings()} and \code{tryCatchWEMs()}.

      \item As an experimental feature the placeholder \code{_} can now
      also be used in the \code{rhs} of a forward pipe \code{|>}
      expression as the first argument in an extraction call, such as
      \code{_$coef}. More generally, it can be used as the head of a
      chain of extractions, such as \code{_$coef[[2]]}.

      \item Spaces in the environment variable used to choose the \R
      session's temporary directory (\env{TMPDIR}, \env{TMP} and
      \env{TEMP} are tried in turn) are now fatal.  (On Windows the
      \sQuote{short path} version of the path is tried and used if that
      does not contain a space.)

      \item \code{all.equal.numeric()} gets a new optional switch
      \code{giveErr} to return the numeric error as attribute.  Relatedly,
      \code{stopifnot(all.equal<some>(a, b, ..))} is as \dQuote{smart} now,
      as \code{stopifnot(all.equal(....))} has been already, thus allowing
      customized \code{all.equal<Some>()} wrappers.

      \item \R on Windows is now able to work with path names longer than
      260 characters when these are enabled in the system (requires at least
      Windows 10 version 1607).  Packages should be updated to work with
      long paths as well, instead of assuming \code{PATH_MAX} to be the
      maximum length.  Custom front-ends and applications embedding \R need
      to update their manifests if they wish to allow this feature.  See
      \url{https://blog.r-project.org/2023/03/07/path-length-limit-on-windows}
      for more information.

      \item \sQuote{Object not found} and \sQuote{Missing argument}
      errors now give a more accurate error context. Patch provided by
      Lionel Henry in \PR{18241}.

      \item The \code{@} operator is now an S3 generic. Based on
      contributions by Tomasz Kalinowski in \PR{18482}.

      \item New generic \code{chooseOpsMethod()} provides a mechanism
      for objects to resolve cases where two suitable methods are found
      for an Ops Group Generic. This supports experimenting with
      alternative object systems. Based on contributions by Tomasz
      Kalinowski in \PR{18484}.

      \item \code{inherits(x, what)} now accepts values other than a
      simple character vector for argument \code{what}. A new generic,
      \code{nameOfClass()}, is called to resolve the class name from
      \code{what}. This supports experimenting with alternative object
      systems. Based on contributions by Tomasz Kalinowski in
      \PR{18485}.

      \item Detection of BLAS/LAPACK in use (\code{sessionInfo()}) with
      FlexiBLAS now reports the current backend.

      \item The \code{"data.frame"} method for \code{subset()} now warns
      about extraneous arguments, typically catching the use of \samp{=}
      instead of \samp{==} in the \code{subset} expression.

      \item Calling \code{a:b} when numeric \code{a} or \code{b} is longer
      than one may now be made into an error by setting environment variable
      \env{_R_CHECK_LENGTH_COLON_} to a true value, along the proposal in
      \PR{18419} by Henrik Bengtsson.

      \item \code{density(x, weights = *)} now warns if automatic bandwidth
      selection happens without using \code{weights}; new optional
      \code{warnWbw} may suppress the warning.  Prompted by Christoph
      Dalitz' \PR{18490} and its discussants.

      \item \code{rm(list = *)} is faster and more readable thanks to Kevin
      Ushey's \PR{18492}.

      \item The \code{plot.lm()} function no longer produces a normal
      Q-Q plot for GLMs. Instead it plots a half-normal Q-Q plot of the
      absolute value of the standardized deviance residuals.

      \item The \code{print()} method for class \code{"summary.glm"} no
      longer shows summary statistics for the deviance residuals by
      default.  Its optional argument \code{show.residuals} can be used
      to show them if required.

      \item The \code{tapply()} function now accepts a data frame as its
      \code{X} argument, and allows \code{INDEX} to be a formula in that
      case. \code{by.data.frame()} similarly allows \code{INDICES} to be
      a formula.

      \item The performance of \code{df[j] <- value} (including for
      missing \code{j}) and \code{write.table(df)} has been improved for
      data frames \code{df} with a large number of columns.  (Thanks to
      Gabriel Becker's \PR{18500}, \PR{18503} and discussants, prompted
      by a report from Toby Dylan Hocking on the R-devel mailing list.)

      \item The matrix multiply operator \code{\%*\%} is now an S3 generic,
      belonging to new group generic \code{matrixOps}.  From Tomasz
      Kalinowski's contribution in \PR{18483}.

      \item New function \code{array2DF()} to convert arrays to data
      frames, particularly useful for the list arrays created by
      \code{tapply()}.
    }
  }

  \subsection{DATES and TIMES}{
    \itemize{
      \item On platforms where (non-UTC) datetimes before 1902 (or
      before 1900 as with system functions on recent macOS) are guessed
      by extrapolating time zones from 1902-2037, there is a warning at
      the first use of extrapolation in a session.  (As all time zones
      post 2037 are extrapolation, we do not warn on those.)

      \item (Platforms using \option{--with-internal-tzone}, including
      Windows and by default macOS). How years are printed in dates or
      date-times can be controlled by environment variable
      \env{R_PAD_YEARS_BY_ZERO}.  The default remains to pad to 4 digits
      by zeroes, but setting value \samp{no} gives no padding (as used
      by default by \code{glibc}).

      \item \code{strftime()} tries harder to determine the offset for
      the \code{"\%z"} format, and succeeds on the mainstream \R platforms.

      \item \code{strftime()} has a limit of 2048 bytes on the string
      produced -- attempting to exceed this is an error.  (Previously it
      silently truncated at 255 bytes.)

      \item \code{sessionInfo()} records (and by default prints) the
      system time zone as part of the locale information.  Also, the
      source (system/internal) of the date-time conversion and printing
      functions.

      \item Objects of class \code{"POSIXlt"} created in this version of
      \R always have 11 components: component \code{zone} is always set,
      and component \code{gmtoff} is set for times in UTC and usually
      set on the (almost all) platforms which have C-level support,
      otherwise is \code{NA}.

      \item There are comprehensive validity checks on the structure of
      objects of class \code{"POSIXlt"} when converting (including
      formatting and printing).  (This avoids mis-conversions of
      hand-crafted objects.)

      \item There is some support for using the native date-time
      routines on macOS: this is only viable on recent versions
      (e.g.\sspace{}12.6 and 13) and does get wrong some historical changes
      (before 1900, during WWII).  Use of \option{--with-internal-tzone}
      remains the default.

      \item \code{as.POSIXct(<numeric>)} and \code{as.POSIXlt(.)} (without
      specifying \code{origin}) now work.
      So does \code{as.Date(<numeric>)}.

      \item \code{as.Date.POSIXct(., tz)} now treats several \code{tz}
      values, notably \code{"GMT"} as equivalent to \code{"UTC"}, proposed
      and improved by Michael Chirico and Joshua Ulrich in \PR{17674}.

      \item Experimental \code{balancePOSIXlt()} utility allows using
      \dQuote{ragged} and or out-of-range \code{"POSIXlt"} objects more
      correctly, e.g., in subsetting and subassignments.  Such objects
      are now documented.

      More experimentally, a \code{"POSIXlt"} object may have an attribute
      \code{"balanced"} indicating if it is known to be filled or fully
      balanced.

      \item Functions \code{axis.Date()} and \code{axis.POSIXct()} are
      rewritten to gain better default tick locations and better default
      formats by using \code{prettyDate()}. Thanks to Swetlana
      Herbrandt.

      \item The mapping of Windows' names for time zones to IANA's
      \sQuote{Olson} names has been updated.  When ICU is available (it is
      by default), it is used to get a mapping for the current region set in
      Windows.  This can be overridden by setting environment variable
      \env{TZ} to the desired Olson name --- see \code{OlsonNames()} for
      those currently available.
     }
  }

  \subsection{GRAPHICS}{
    \itemize{
      \item The graphics engine version, \code{R_GE_version}, has been
      bumped to \code{16} and so packages that provide graphics devices
      should be reinstalled.

      \item The \pkg{grDevices} and \pkg{grid} packages have new
      functions for rendering typeset glyphs, primarily:
      \code{grDevices::glyphInfo()} and \code{grid::grid.glyph()}.

      Rendering of typeset glyphs is only supported so far on the
      Cairo-based graphics devices and on the \code{pdf()} and
      \code{quartz()} devices.

      \item The defined behaviour for \code{"clear"} and \code{"source"}
      compositing operators (via \code{grid::grid.group()}) has been
      changed (to align better with
      simple interpretation of original Porter-Duff definitions).

      \item Support for gradients, patterns, clipping paths, masks,
      groups, compositing operators, and affine transformations has
      been added to the \code{quartz()} device.
    }
  }

  \subsection{INSTALLATION on a UNIX-ALIKE}{
    \itemize{
      \item A system installation of generic LAPACK 3.10.0 or later will
      be preferred to the version in the \R sources.

      \command{configure} option \option{--with-lapack=no} (equivalently
      \option{--without-lapack}) forces compilation of the internal
      LAPACK sources.

      If \option{--with-lapack} is not specified, a system
      \code{liblapack} is looked for and used if it reports version
      3.10.0 or later and does not contain BLAS routines.

      Packages using LAPACK will need to be reinstalled if this changes
      to using an external library.

      \item On \samp{aarch64} Linux platforms using GCC,
      \command{configure} now defaults to \option{-fPIC} (instead of
      \option{-fpic}), as desired in \PR{18326}.

      \item \command{configure} now checks conversion of datetimes
      between \code{POSIXlt} and \code{POSIXct} around year 2020.  Failure
      (which has been seen on platforms missing \code{tzdata}) is fatal.

      \item If \command{configure} option
      \option{--with-valgrind-instrumentation} is given value \code{1}
      or \code{2}, option \option{--with-system-valgrind-headers} is now
      the default and ignored (with a warning).  It is highly
      recommended that the system headers are installed alongside
      \command{valgrind}: they are part of its packaging on some Linux
      distributions and packaged separately (e.g.\sspace{}in the
      \file{valgrind-devel} RPM) on others.  \command{configure} will
      give a warning if they are not found.

      The system headers will be required in a future release of \R
      to build with \command{valgrind} instrumentation.

      \item \code{libcurl} 8.x is now accepted by \command{configure}:
      despite a change in major version number it changes neither API
      nor ABI.
    }
  }

  \subsection{INSTALLATION on WINDOWS}{
    \itemize{
      \item The makefiles and installer scripts for Windows have been
      tailored to \file{Rtools43}, an update of the \file{Rtools42}
      toolchain.  It is based on \command{gcc} 12 and newer versions of
      MinGW-W64, binutils and libraries.  At this time R-devel can still
      be built using Rtools42 without changes, but when R-devel is
      installed via the installer, it will by default look for Rtools43.

      \item Old make targets \code{rsync-extsoft} and 32-bit ones that
      are no longer needed have been removed.

      \item Default builds (including for packages) no longer select
      C99.  Thus the C standard used is the default for the compiler,
      which for the toolchain in \file{Rtools43} is C17.  (This is
      consistent with Unix builds.)
    }
  }

  \subsection{PACKAGE INSTALLATION}{
    \itemize{
      \item The default C++ standard has been changed to C++17 where
      available (which it is on all currently checked platforms): if not
      C++14 or C++11 is used if available otherwise C++ is not supported.

      \item \code{USE_FC_LEN_T} is the default: this uses the
      correct (compiler-dependent) prototypes for Fortran BLAS/LAPACK
      routines called from C/C++, and requires adjustment of many such
      calls -- see \sQuote{Writing R Extensions} §6.6.1.

      %% g++ 11 supported c++23.
      %% 2022-09 Recent LLVM (>= 13?) and Apple (>= 13?) clang support c++2b.
      \item There is initial support for C++23 as several compilers are
      now supporting \code{-std=c++23} or \code{-std=c++2b} or similar.
      As for C++20, there no additional \command{configure} checks for
      C++23 features beyond a check that the compiler reports a
      \code{__cplusplus} value greater than that in the C++20 standard.
      C++ feature tests should be used.

      \item There is support for a package to indicate the version of
      the C standard which should be used to compile it, and for the
      installing user to specify this.  In most cases \R defaults to the
      C compiler's default standard which is C17 (a `bug-fix' of C11) --
      earlier versions of \R or compilers may have defaulted to C99.

      Current options are:
      \describe{
	\item{USE_C17}{Use a standard that is at most C17.  The
	  intention is to allow legacy packages to still be installed when
	  later C standards become the default, including packages
	  using new keywords as identifiers or with K&R-style function
	  declarations.  This will use C17 if available, falling back
	  to C11.
	}
	\item{USE_C90}{Use the C90 (aka C89) standard.  (As that standard
	  did not require compilers to identify that version, all we can
	  verify is that the compiler does not claim to be using a later
	  standard.  It may accept C99 features -- for example
	  \command{clang} accepts // to make comments.)}
	\item{USE_C99}{Use the C99 standard. This should be rarely
	  needed -- it avoids the few new features of C11/C17 which can
	  be useful if a package assumes them if C17 is specified and
	  they are not implemented.}
	\item{USE_C23}{Use C23 (or in future, later).  Compiler/library
	  support for C23 is still being implemented, but LLVM clang
	  from 15.0.0 and the upcoming GCC 13 have quite extensive support.
	}
      }
      These can be specified as part of the \samp{SystemRequirements}
      field in the package's \file{DESCRIPTION} file or \emph{via}
      options \option{--use-C17} and so on of \command{R CMD INSTALL}
      and \command{R CMD SHLIB}.

      For further details see \dQuote{Writing R Extensions} §1.2.5.

      \item (Windows) A \file{src/Makefile.ucrt} or
      \file{src/Makefile.win} file is now included after
      \file{\var{R_HOME}/etc\var{R_ARCH}/Makeconf} and so no longer
      needs to include that file itself.  Installation of a package with
      such a file now uses a site \file{Makevars} file in the same way
      as a package with a \file{src/Makevars.win} file would.

      \item \command{configure} is now passed crucial variables such as
      \env{CC} and \env{CFLAGS} in its environment, as many packages
      were not setting them (as documented in \sQuote{Writing R
	Extensions} §1.2).

      This has most effect where \command{configure} is used to compile
      parts of the package -- most often by \command{cmake} or
      \command{libtool} which obfuscate the actual compile commands
      used.

      Also used for \command{configure.win} and \command{configure.ucrt}
      on Windows.
    }
  }

  \subsection{FORTRAN FLAGS}{
    \itemize{
      \item The flag \option{-fno-optimize-sibling-calls} is no longer
      forced for \command{gfortran} 7 and later.  It should no longer be
      needed now using \sQuote{hidden} character-length arguments when
      calling BLAS/LAPACK routines from C/C++ is the default even for
      packages.  (Unless perhaps packages call Fortran code from C/C++
      without using \R{}'s headers and without allowing for these
      arguments.)
    }
  }

  \subsection{C-LEVEL FACILITIES}{
    \itemize{
      \item The deprecated S-compatibility macros \code{DOUBLE_*} in
      \file{R_ext/Constants.h} (included by \file{R.h}) have been
      removed.

      \item The deprecated legacy typedefs of \code{Sint} and
      \code{Sfloat} in header \file{R.h} are no longer defined, and that
      header no longer includes header \file{limits.h} from C nor
      \file{climits} from C++.

      \item New macro \code{CAD5R()} is provided in \file{Rinternals.h}
      and used in a few places in the \R sources.

      \item ALTREP now supports \code{VECSXP} vectors. Contributed by
      Gabor Csardi in \PR{17620}.

      \item The \code{Rcomplex} definition (in header
      \file{R_ext/Complex.h}) has been extended to prevent possible
      mis-compilation when interfacing with Fortran (\PR{18430}).  The
      new definition causes compiler warnings with static initializers
      such as \code{\{1, 2\}}, which can be changed to \code{\{.r=1,
	.i=2\}}.

      Using the new definition from C++ depends on compiler extensions
      supporting C features that have not been incorporated into the C++
      standards but are available in \command{g++} and
      \command{clang++}: this may result in C++ compiler warnings but
      these have been worked around for recent versions of common
      compilers (GCC, Apple/LLVM clang, Intel).

      It is intended to change the inclusion of header
      \file{R_ext/Complex.h} by other \R{} headers, so C/C++ code files
      which make use of \code{Rcomplex} should include that header
      explicitly.
    }
  }

  \subsection{UTILITIES}{
    \itemize{
      \item \command{R CMD check} does more checking of package \file{.Rd}
      files, warning about invalid email addresses and (some) invalid
      URIs and noting empty \samp{\item} labels in description lists.

      \item \command{R CMD check} now also reports problems when reading
      package news in md (file \file{NEWS.md}) and (optionally) plain
      text (file \file{NEWS}) formats.

      \item \env{_R_CHECK_TIMINGS_} defaults to a value from the
      environment even for \command{R CMD check --as-cran}; this allows
      for exceptionally fast or slow platforms.

      It now applies to checking PDF and HTML versions of the manuals,
      and \sQuote{checking CRAN incoming feasibility}.

      \item \command{R CMD check} can optionally (but included in
      \option{--as-cran}) check whether HTML math rendering \emph{via}
      KaTeX works for the package \file{.Rd} files.

      \item Non-interactive debugger invocations can be trapped by
      setting the environment variable
      \env{_R_CHECK_BROWSER_NONINTERACTIVE_} to a true value.  This is
      enabled by \command{R CMD check --as-cran} to detect the use of
      leftover \code{browser()} statements in the package.

      \item The use of \code{sprintf} and \code{vsprintf} from C/C++ has
      been deprecated in macOS 13 and is a known security risk.
      \command{R CMD check} now reports (on all platforms) if their use
      is found in compiled code: replace by \code{snprintf} or
      \code{vsnprintf} respectively. [\bold{NB:} whether such calls get
      compiled into the package is platform-dependent.]

      \item Where recorded at installation, \command{R CMD check}
      reports the C and Fortran compilers used to build \R.

      It reports the OS in use (if known, as given by
      \code{osVersion}) as well as that \R was built for.

      It notes if a C++ standard was specified which is older than the
      current default: many packages have used C++11 to mean \sQuote{not
      C++98} --- as C++11 is the minimum supported since \R 4.0.0, that
      specification can probably be removed.

      \item \command{R CMD INSTALL} reports the compilers (and on
      macOS, the SDK) used, and this is copied to the output of
      \command{R CMD check}.

      Where a C++ standard is specified, it is reported.

      \item \command{R CMD check}'s \sQuote{checking compilation flags in
	Makevars} has been relaxed to accept the use of flags such as
      \option{-std=f2008} in \samp{PKG_FFLAGS}.

      \item \code{tools::buildVignettes()} has a new argument \code{skip},
      which is used by \command{R CMD check} to skip (and note)
      vignettes with unavailable \samp{\VignetteDepends} (\PR{18318}).

      \item New generic \code{.AtNames()} added to enable class-specific
      completions after \code{@}. The formerly internal function
      \code{findMatches()} is now exported, mainly for use in methods
      for \code{.DollarNames()} and \code{.AtNames()}.

    }
  }

  \subsection{DEPRECATED AND DEFUNCT}{
    \itemize{
      \item \code{default.stringsAsFactors()} is defunct.

      \item Calling \code{as.data.frame.<class>()} directly (for 12 atomic
      classes) is going to be formally deprecated, currently activated by
      setting the environment variable
      \env{_R_CHECK_AS_DATA_FRAME_EXPLICIT_METHOD_} to non-empty, which
      also happens in \command{R CMD check --as-cran}.
    }
  }

  \subsection{BUG FIXES}{
    \itemize{
      \item Hashed \code{environment}s with sizes less than 5 can now grow.
      (Reported to R-devel by Duncan Garmonsway.)

      \item \code{as.character(<Rd>, deparse = TRUE)} failed to
      re-escape curly braces in LaTeX-like text.
      (Reported by Hadley Wickham in \PR{18324}.)

      \item \code{library()} now passes its \code{lib.loc} argument when
      requiring \code{Depends} packages; reported (with fix) in \PR{18331}
      by Mikael Jagan.

      \item \command{R CMD Stangle}: improved message about \sQuote{Output}
      files.

      \item \code{head(x, n)} and \code{tail(x, n)} now signal an error if
      \code{n} is not numeric, instead of incidentally \dQuote{working}
      sometimes returning all of \code{x}.  Reported and discussed by Colin
      Fay, in \PR{18357}.

      \item The \code{"lm"} method for \code{summary()} now gives the
      correct F-statistic when the model contains an offset.  Reported in
      \PR{18008}.

      \item \code{C()} and \code{`contrasts<-`()} now preserve factor
      level names when given a function object (as opposed a function
      name which did preserve names). Reported in \PR{17616}.

      \item \code{c(a = 1, 2)[[]]} no longer matches \code{2} but rather
      signals a \emph{classed} error.  Reported and analysed by Davis Vaughan in
      \PR{18367}, a duplicate of \PR{18004}, by Jan Meis et al.
      For consistency, \code{NULL[[]]} is also erroneous now.
      \code{x[[]] <- v} gives an error of the same class \code{"MissingSubscriptError"}.

      \item The \code{relist()} function of \pkg{utils} now supports
      \code{NULL} elements in the skeleton (\PR{15854}).

      \item \code{ordered(levels = *)} (missing \code{x}) now works
      analogously to \code{factor(, ordered=TRUE)}; reported (with fix)
      by Achim Zeileis in \PR{18389}.

      \item User-defined Rd macro definitions can now span multiple lines,
      thanks to a patch from Duncan Murdoch. Previously, the Rd parser
      silently ignored everything after the first line.

      \item Plain-text help (\code{tools::Rd2txt()}) now preserves an
      initial blank line for text following description list items.

      \item \code{tools::Rd2HTML()} and \code{tools::Rd2latex()} no
      longer split \verb{\arguments} and \verb{\value} lists at Rd comments.

      \item \code{tools::Rd2latex()} now correctly handles optional text
      outside \verb{\item}s of argument lists as well as bracketed text
      at the beginning of sections, e.g., \verb{\value{[NULL]}}.

      \item \code{as.character(<POSIXt>)} now behaves more in line with the
      methods for atomic vectors such as numbers, and is no longer
      influenced by \code{options()}.
      Ditto for \code{as.character(<Date>)}.
      The \code{as.character()} method gets arguments \code{digits} and \code{OutDec}
      with defaults \emph{not} depending on \code{options()}.
      Use of \code{as.character(*, format = .)} now warns.

      \item Similarly, the \code{as.character.hexmode()} and
      \code{*.octmode()} methods also behave as \code{good citizen}
      methods and back compatibility option \code{keepStr = TRUE}.

      \item The \code{as.POSIXlt(<POSIXlt>)} and
      \code{as.POSIXct(<POSIXct>)} default methods now do obey their
      \code{tz} argument, also in this case.

      \item \code{as.POSIXlt(<Date>)} now does apply a \code{tz} (time zone)
      argument, as does \code{as.POSIXct()}; partly suggested by Roland Fuß on
      the R-devel mailing list.

      \item \code{as.Date.POSIXlt(x)} now also works when the list components
      are of unequal length, aka \dQuote{partially filled} or \dQuote{ragged}.

      \item \code{expand.model.frame()} looked up variables in the wrong
      environment when applied to models fitted without \code{data}.
      Reported in \PR{18414}.

      \item \code{time()} now (also) uses the
      \code{ts.eps = getOption("ts.eps")}
      argument and thus by default rounds values very close to the start (or
      end) of a year.  Based on a proposal by Andreï V. Kostyrka on R-help.

      \item Printing of a \code{factanal()} result with just one factor and
      \code{sort = TRUE} now works regularly, fixing \PR{17863} by Timothy
      Bates, thanks to the \sQuote{R Contributors} working group.

      \item Printing 0-length objects of class
      \code{"factor"}, \code{"roman"}, \code{"hexmode"},
      \code{"octmode"}, \code{"person"}, \code{"bibentry"}, or
      \code{"citation"} now prints something better, one
      of which fixes \PR{18422}, reported by Benjamin Feakins.

      \item \code{Sys.timezone()} queries \command{timedatectl} only if
      \command{systemd} is loaded; addressing a report by Jan Gorecki in
      \PR{17421}.

      \item The formula method of \code{cor.test()} had scoping problems
      when \code{environment(formula)} was not the calling environment;
      reported with a patch proposal by Mao Kobayashi in \PR{18439}.

      \item \code{attach()} of an environment with active bindings now
      preserves the active bindings.  Reported by Kevin Ushey in
      \PR{18425}.

      \item BLAS detection now works also with system-provided libraries not
      available as regular files.  This fixes detection of the Accelerate
      framework on macOS since Big Sur.  Reported by David Novgorodsky.

      \item \code{download.file()} gives a helpful error message in case of
      an invalid \code{download.file.method} option, thanks to Colin Fay's
      report in \PR{18455}.

      \item Sporadic crashes of \command{Rterm} when using completion have
      been fixed.

      \item \code{Rprof()} is now more reliable.  A livelock in thread
      initialization with too short sampling interval has been fixed on
      macOS.  A deadlock in using the C runtime has been fixed on Windows.
      A potential deadlock has been prevented on Unix.

      \item Cursor placement in \command{Rgui} now works even after a
      fixed-width font is selected.

      \item Mandatory options (\code{options()}) are now set on startup so
      that saving and restoring them always works (\PR{18372}).

      \item Package installation, \command{R CMD INSTALL} or \code{install.packages(*)},
      now parses each of the \file{<pkg>/R/*.R} files individually instead
      of first concatenating and then \code{parse()}ing the large resulting
      file.  This allows parser or syntax errors to be diagnosed with
      correct file names and line numbers, thanks to Simon Dedman's report
      and Bill Dunlap's patch in \PR{17859}.

      This \emph{does} require syntactically self contained R source
      files now, fixing another inadvertent bug.

      \item \code{predict.lm(<model with offset>)} now finds the offset in
      the correct environment, thanks to André Gillibert's report and patch
      in \PR{18456}.

      \item \code{getInitial(<formula>)} now finds the \code{selfStart}
      model in the correct environment.
      (Reported by Ivan Krylov in \PR{18368}.)

      \item Fix for possible segfault when using recently-added graphics
      features, such as gradients, clipping paths, masks, and groups with
      \code{pdf(file=NULL)}.

      \item \code{class(m) <- class(m)} no longer changes a matrix \code{m}
      by adding a class \emph{attribute}.

      \item \code{packageDate(pkg)} now only warns once if there is no \code{pkg}.

      \item When \code{ts()} creates a multivariate time series,
      \code{"mts"}, it also inherits from \code{"array"} now, and
      \code{is.mts()} is documented \emph{and} stricter.

      \item \code{Rd2txt()} now preserves line breaks of \verb{\verb} Rd
      content and from duplicated \verb{\cr}.  The former also
      fixes the rendering of verbatim output from Rd
      \verb{\Sexpr} in plain-text help.

      \item \code{uniroot(f, interval)} should no longer wrongly converge
      \emph{outside} the interval in some cases where \code{abs(f(x)) == Inf}
      for an \code{x} at the interval boundary, thanks to posts by Ben
      Bolker and Serguei Sokol on R-devel.

      \item Vectorized alpha handling in palette functions such as in
      \code{gray()}, \code{rainbow()}, or \code{hcl.colors()} works
      correctly now, thanks to Achim Zeileis' report and patch in
      \PR{18476}.

      \item Formatting and \code{print()}ing of \code{bibentry} objects has
      dropped the deprecated \code{citation.bibtex.max} argument, such that
      the \code{bibtex} argument's default for \code{print.bibentry()}
      depends directly on the \code{citation.bibtex.max} option, whereas in
      \code{format.bibentry()} the option no longer applies.

      \item Attempting to use a character string naming a foreign
      function entry point in a foreign function call in a package will
      now signal an error if the packages has called
      \code{R_forceSymbols} to specify that symbols must be
      used.

      \item An error in \code{table()} could permanently set
      \code{options(warn=2)} promoting all subsequent warnings to errors.

      \item The \code{sigma()} function gave misleading results for
      binary GLMs.  A new method for objects of class \code{"glm"} returns the
      square root of the estimate of the dispersion parameter using the
      same calculation as \code{summary.glm()}.

      \item \code{bs()} and \code{ns()} in the (typical) case of automatic
      knot construction, when some of the supposedly inner knots coincide
      with boundary knots, now moves them inside (with a warning), building
      on \PR{18442} by Ben Bolker.

      \item \command{R CMD} on Windows now skips the site profile with
      \option{--no-site-file} and \option{--vanilla} even when
      \env{R_PROFILE} is set (\PR{18512}, from Kevin Ushey).
    }
  }
}

\section{\Rlogo CHANGES IN R 4.2.3}{
  \subsection{C-LEVEL FACILITIES}{
    \itemize{
      \item The definition of \code{DL_FUNC} in \file{R_ext/Rdynload.h}
      has been changed to be fully C-compliant.  This means that
      functions loaded \emph{via} for example \code{R_GetCCallable} need
      to be cast to an appropriate type if they have any arguments.

      \item \code{.Machine} has a new element \code{sizeof.time_t} to
      identify old systems with a 32-bit type and hence a limited range
      of date-times (and limited support for dates millions of years
      from present).
    }
  }

  \subsection{PACKAGE INSTALLATION}{
    \itemize{
      \item (Windows) The default C++ standard had accidentally been
      left at C++11 when it was changed to C++14 on Unix.
    }
  }

  \subsection{BUG FIXES}{
    \itemize{
      \item As \code{"POSIXlt"} objects may be \dQuote{partially filled}
      and their list components meant to be recycled, \code{length()} now
      is the length of the longest component.

      \item \code{as.POSIXlt.Date()} could underflow for dates in the
      far past (more than half a million years BCE).

      \item \code{as.Date.POSIXlt(x)} would return \code{"1970-01-01"}
      instead of \code{NA} in R 4.2.2, e.g., for \preformatted{
	x <- as.POSIXlt(c("2019-01-30","2001-1-1"))
	x$mon <- c(0L, NA); as.Date(x)}

      \item \command{R CMD check} failed to apply enabled
      \env{_R_CHECK_SUGGESTS_ONLY_} to examples and vignettes
      (regression in \R 4.2.0).

      \item \command{R CMD check} did not re-build vignettes in
      separate processes by default (regression in \R 4.2.0).

      \item Running examples from HTML documentation now restores
      previous \pkg{knitr} settings and options (\PR{18420}).

      \item Quartz: fonts are now located using Core Graphics API
      instead of deprecated ATS which is no longer supported in the
      macOS 13 SDK (\PR{18426}).  This also addresses an issue where
      the currently used font in the Quartz device context was not
      correctly retained.

      \item (Windows) Math symbols in text drawing functions are again
      rendered correctly (\PR{18440}).  This fixes a regression in \R 4.2.1
      caused by a fix in \PR{18382} which uncovered an issue in GraphApp
      due to which the symbol charset was not used with TT Symbol font face.

      \item (Windows) Installing a package with a
      \file{src/Makefile.\{win,ucrt\}} file includes
      \file{~/.R/Makevars.win64} in the search for user makevars, as
      documented in \dQuote{R Installation and Administration} and done
      for packages with a \file{src/Makevars.\{win,ucrt\}} file.

      \item \code{format(<POSIXlt_w/_unbalanced_sec>, "....\%OS<n>")} with
      \eqn{n > 0} no longer accidentally uses the unbalanced seconds,
      thanks to Suharto Anggono's report (including patch) in \PR{18448}.

      \item \code{solve.default(a, b)} works around issues with
      some versions of LAPACK when \code{a} contains \code{NA} or
      \code{NaN} values.

      \item When \code{UseMethod()} cannot dispatch, it no longer segfaults
      producing the error message in case of a long \code{class()}, thanks to
      Joris Vankerschaver's report (including patch) in \PR{18447}.

      \item When \code{example(foo, ..)} produces graphics on an interactive
      device it needs to open itself, it now leaves \code{devAskNewPage()}
      unchanged even when it was \code{FALSE}, thus fixing a 14 years old
      \sQuote{<FIXME>}.

      \item \code{packageDescription()} again catches errors from encoding
      conversions. This also fixes broken \code{packageVersion()} in
      C locale on systems where \code{iconv} does not support transliteration.
    }
  }
}

\section{\Rlogo CHANGES IN R 4.2.2}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item \code{tools::Rdiff(useDiff = TRUE)} checks for the presence
      of an external \command{diff} command and switches to
      \code{useDiff = FALSE} if none is found.
      This allows \command{R CMD Rdiff} to always work.

      \item On Windows, environment variable
      \env{R_LIBCURL_SSL_REVOKE_BEST_EFFORT} can be used to switch to
      only \sQuote{best-effort} SSL certificate revocation checks with
      the default \code{"libcurl"} download method.  This reduces
      security, but may be needed for downloads to work with MITM
      proxies (\PR{18379}).

      \item (macOS) The run-time check for libraries from XQuartz for
      X11 and Tcl/Tk no longer uses \code{otool} from the Apple
      Developer Tools (PR#18400).

      \item The \LaTeX style for producing the PDF manuals, \file{Rd.sty},
      now loads the standard \samp{amsmath}, \samp{amsfonts} and
      \samp{amssymb} packages for greater coverage of math commands in
      the Rd \verb{\eqn} and \verb{\deqn} macros.
      The \verb{\mathscr} \LaTeX command is also provided (via the
      \samp{mathrsfs} package, if available, or the \samp{amsfonts}
      bundle otherwise), fulfilling the wish of \PR{18398}.

      \item (Windows) The default format of \code{readClipboard()} and
      \code{writeClipboard()} has been changed to \code{13}
      (\code{CF_UNICODETEXT}).
    }
  }

  \subsection{INSTALLATION on a UNIX-ALIKE}{
    \itemize{
      \item The PDF manuals (if built) can be compacted by the new
      target \command{make compact-pdf} (at the top level or in
      directory \file{doc/manual}).

      \item There is now \command{configure} support for LLVM clang 15
      on Linux, which defaults to position-independent (PIE) executables
      whereas \command{gfortran} does not.

      \item Many small changes to ease compilation (and suppress
      warnings) with LLVM \command{clang} 15.
    }
  }

  \subsection{BUG FIXES}{
    \itemize{
      \item \code{Rscript -e} would fail if \file{stdin} were closed
      (Reported by Henrik Bengtsson.)

      \item \code{qt(*, log.p=TRUE)} in outer tails no longer produces
      \code{NaN} in its final steps, thus fixing \PR{18360}.

      \item \code{tools::Rd2latex()} now escapes hashes and ampersands
      when writing URLs, fixing LaTeX errors with such URLs in \verb{\tabular}.

      \item When \code{isGeneric(f, fdef=*)} is used with mismatching
      names, the warning is better understandable; reported (with fix) in
      \PR{18370} by Gabe Becker.

      \item \code{poly(x, n)} now works again (and is now documented)
      when \code{x} is a \code{"Date"} or \code{"POSIXct"} object, or of
      another class while fulfilling \code{mode(x) == "numeric"}.  This
      also enables \code{poly(x, *, raw=TRUE)} for such variables.
      Reported by Michael Chirico to R-devel.

      \item \code{write.table()}, \code{write.csv()} and
      \code{write.csv2()} restore their numerical precision (internal
      equivalent of \code{digits = 15}) after an interrupt (\PR{18384}).

      \item One can now read also byte \code{FF} from a clipboard connection
      (\PR{18385}).

      \item \code{source("")} and \code{source(character())} now give more
      helpful error messages.

      \item \command{R CMD check --as-cran} set \env{_R_CHECK_TIMINGS_}
      too late to have the intended effect.

      \item \code{as.POSIXlt(x)} now also works with very large dates
      \code{x}, fixing \PR{18401} reported by Hannes Mühleisen.

      \item Files can now be extracted even from very large zip archives
      (\PR{18390}, thanks to Martin Jakt).

      \item Non-finite objects of class \code{"POSIXlt"} are now
      correctly coerced to classes \code{"Date"} and \code{"POSIXct"};
      following up on the extension to \code{format()} them correctly.

      \item Added methods for \code{is.finite()}, \code{is.infinite()}
      and \code{is.nan()} for \code{"POSIXlt"} date-time objects.
    }
  }

  \subsection{BUG FIXES on Windows}{
    \itemize{
     \item Non-ASCII characters are now properly displayed on Windows
      in windows created using GraphApp via
      e.g.\sspace{}\code{winDialogString} thanks to a workaround for an
      at least surprising Windows behavior with UTF-8 as the system
      encoding (\PR{18382}).

      \item Find and replace operations work again in the script editor in
      \command{Rgui} on Windows.

      \item Computation of window size based on requested client size in
      GraphApp when running in a multi-byte locale on Windows has been fixed
      (regression in \R 4.2.0 for users of systems where \R 4.1 used a
      single-byte locale).  \command{Rgui} again respects the number of
      console rows and columns given in \file{Rconsole} file.

      \item \command{Rterm} support for \code{Alt+xxx} sequences has been
      fixed to produce the corresponding character (only) once. This fixes
      pasting text with tilde on Italian keyboard (PR#18391).
    }
  }
}

\section{\Rlogo CHANGES IN R 4.2.1}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item New function \code{utils::findCRANmirror()} to find out if a
      \acronym{CRAN} mirror has been selected, otherwise fallback to
      the main site.  This behaves in the same way as
      \code{tools::CRAN_package_db()} and is intended for packages
      wishing to access \acronym{CRAN} for purposes other than
      installing packages.

      The need for this was shown by a day when the main \acronym{CRAN}
      website was offline and a dozen or so packages which had its URL
      hardcoded failed their checks.
    }
  }

  \subsection{INSTALLATION on a UNIX-ALIKE}{
    \itemize{
      \item The libraries searched for by \option{--with-blas} (without
      a value) now include BLIS (after OpenBLAS but before ATLAS).  And
      on macOS, the Accelerate framework (after ATLAS).  (This is
      patterned after the \code{AX_BLAS} macro from the Autoconf Archive.)

      \item The included LAPACK sources have been updated to 3.10.1.
    }
  }
  \subsection{UTILITIES}{
    \itemize{
      \item The (full path to) the command \command{tidy} to be used for
      HTML validation can be set by environment variable \env{R_TIDYCMD}.

      \item Setting environment variable
      \env{_R_CHECK_RD_VALIDATE_RD2HTML_} to a false value will override
      \command{R CMD check --as-cran} and turn off HTML validation.
      This provides a way to circumvent a problematic \command{tidy}.

      The 2006 version that ships with macOS is always skipped.
    }
  }

  \subsection{C-LEVEL FACILITIES}{
    \itemize{
      \item The undocumented legacy declarations of \code{Sint},
      \code{Sfloat}, \code{SINT_MAX} and \code{SINT_MIN} in header
      \file{R.h} are deprecated.
    }
  }

  \subsection{BUG FIXES}{
    \itemize{
      \item \code{fisher.test(d)} no longer segfaults for \dQuote{large}
      \code{d}; fixing \PR{18336} by preventing/detecting an integer
      overflow reliably.

      \item \code{tar(., files=*)} now produces correctly the warning
      about invalid uid or gid of files, fixing \PR{18344}, reported by
      Martin Morgan.

      \item \code{tk_choose.files()} with \code{multi = FALSE}
      misbehaved on paths containing spaces (\PR{18334}) (regression
      introduced in \R 4.0.0).

      \item \code{sort(x, partial = ind, *)} now works correctly notably for
      the non-default \code{na.last = FALSE} or \code{TRUE}, fixing
      \PR{18335} reported by James Edwards.

      \item Environment variable \env{_R_CHECK_XREFS_REPOSITORIES_} is
      only used for checking \file{.Rd} cross-references in \command{R
      CMD check} (as documented) and not for other uses looking for a
      \acronym{CRAN} mirror.

      \item The search for a \acronym{CRAN} mirror when checking
      packages now uses \code{getOption("repos")} if that specifies a
      \acronym{CRAN} mirror, even when it does not also specify all three
      Bioconductor repositories (as was previously required).

      \item The \acronym{HTML} code generated by \code{tools::Rd2HTML()}
      has been improved to pass \command{tidy} 5.8.0.
    }
  }

  \subsection{BUG FIXES on Windows}{
    \itemize{
      \item Writing to a clipboard connection works again, fixing a
      regression in \R 4.2.0 (\PR{18332}).  Re-using a closed clipboard
      connection no longer issues a spurious warning about an ignored
      encoding argument.

      \item C function \code{getlocale} no longer attempts to query an
      unsupported category from the OS, even when requested at R level,
      which may cause crashes when \R 4.2.0 (which uses UCRT) is
      embedded (reported by Kevin Ushey).

      \item Accent keys now work in GraphApp Unicode windows, which are
      used by \command{Rgui} whenever running in a multibyte locale (so
      also in UTF-8, hence fixing a regression in \R 4.2.0 for users of
      systems where \R 4.1 used a single-byte locale).

      \item Completion in \command{Rgui} now works also with
      non-ASCII characters.

      \item \command{Rgui} no longer truncates usage information with
      \option{--help}.

      \item Text injection from external applications via
      \code{SendInput} now works in GraphApp Unicode windows, fixing a
      regression in \R 4.2.0 for \command{Rgui} users of systems where
      \R 4.1 used a single-byte locale but \R 4.2.0 uses UTF-8.

      \item Performance of \code{txtProgressBar()} in \command{Rgui} when
      running in a multi-byte locale has been improved (fixing a
      performance regression in \R 4.2.0 for users of systems where \R
      4.1 used a single-byte locale).

      \item The script editor in \command{Rgui} now works also on
      systems using UTF-8 as the native encoding.  Users of the script
      editor have to convert their scripts with non-ASCII characters to
      UTF-8 before reading them in \R 4.2.1 or newer (on recent Windows
      where UTF-8 is used).  This fixes a regression in \R 4.2.0, which
      prevented some operations with scripts when they contained
      non-ASCII characters.
    }
  }
}

\section{\Rlogo CHANGES IN R 4.2.0}{
  \subsection{SIGNIFICANT USER-VISIBLE CHANGES}{
    \itemize{
      \item The \code{formula} method of \code{aggregate()} now matches
      the generic in naming its first argument \code{x} (resolving
      \PR{18299} by Thomas Soeiro).

      This means that calling \code{aggregate()} with a formula as a
      named first argument requires name \code{formula} in earlier
      versions of \R and name \code{x} now, so portable code should not
      name the argument (code in many packages did).

      \item Calling \code{&&} or \code{||} with either argument of
      length greater than one now gives a warning (which it is intended
      will become an error).

      \item Calling \code{if()} or \code{while()} with a condition of
      length greater than one gives an error rather than a warning.
      Consequently, environment variable
      \env{_R_CHECK_LENGTH_1_CONDITION_} no longer has any effect.

      \item Windows users should consult the WINDOWS section below for
      some profound changes including
      \itemize{
	\item Support for 32-bit builds has been dropped.
	\item UTF-8 locales are used where available.
	\item The default locations for the \R installation and personal
	library folder have been changed.
      }

      Thanks to Tomas Kalibera for months of work on the Windows port
      for this release.
    }
  }

  \subsection{NEW FEATURES}{
    \itemize{
      \item \code{matrix(x, n, m)} now warns in more cases where
      \code{length(x)} differs from \code{n * m}, as suggested by Abby
      Spurdle and Wolfgang Huber in Feb 2021 on the R-devel mailing
      list.

      This warning can be turned into an error by setting environment
      variable \env{_R_CHECK_MATRIX_DATA_} to \samp{TRUE}: \command{R
	CMD check --as-cran} does so unless it is already set.

      \item Function \code{file_test()} in package \pkg{utils} gains
      tests for symlinks, readability and writability.


      \item \code{capabilities("libxml")} is now false.

      The description of \code{capabilities("http/ftp")} now reflects
      that it refers to the default method, no longer the internal one.

      \item \code{simplify2array()} gains an \code{except} argument for
      controlling the exceptions used by \code{sapply()}.

      \item Environment variables \env{R_LIBS_USER} and
      \env{R_LIBS_SITE} are both now set to the \R system default if
      unset or empty, and can be set to \code{NULL} to indicate an empty
      list of user or site library directories.

      \item The warning for \code{axis()}(-like) calls in cases of relatively
      small ranges (typically in log-scale situations) is slightly improved
      \emph{and} suppressed from explicit calls to \code{.axisPars()} as
      has always been the intention.

      \item The \code{contrasts} setter function \code{`contrasts<-`}
      gains an explicit default \code{how.many = NULL} rather than just
      using \code{missing(how.many)}.

      \item \code{grid.pretty()} gains a new optional argument \code{n = 5}.

      \item There is a new function \code{.pretty()} with option
      \code{bounds} as a technical-utility version of \code{pretty()}.  It
      and \code{pretty()} gain a new argument \code{f.min} with a better
      than back-compatible default.

      \item Function \code{grDevices::axisTicks()} and related functions
      such as \code{graphics::axis()} work better, notably for the log
      scale; partly because of the \code{pretty()} improvements, but also
      because care is taken e.g., when \code{ylim} is finite but
      \code{diff(ylim)} is infinite.

      \item \code{nclass.FD()} gains a \code{digits} option.


      \item The R Mathlib internal C function \code{bd0()} (called
      indirectly from a dozen probability density and distribution
      functions such as \code{dpois()}, \code{dbinom()},
      \code{dgamma()}, \code{pgamma()} \emph{etc}) has been complemented
      by a more sophisticated and (mostly) more accurate C function
      \code{ebd0()}, currently called only by internal
      \code{dpois_raw()} improving accuracy for \R level \code{dpois()}
      and potentially others calling it such as \code{dnbinom()},
      \code{dgamma()} or \code{pgamma()}.  (Thanks to Morten Welinder's
      \PR{15628}.)

      \item \code{write.ftable()} gains \code{sep = " "} argument as
      suggested by Thomas Soeiro.

      \item The names of the locale categories supported by \R's
      \code{Sys.getlocale()} and \code{Sys.setlocale()} are now provided
      by variable \code{.LC.categories} in the \code{base} namespace.

      \item The \code{Date} and \code{POSIXt} methods for \code{hist()}
      and the \code{histogram} method for \code{plot()} now also use the
      new default \code{col = "lightgray"} in consistency with the
      corresponding change to \code{hist()}'s default for \R 4.0.0.

      \item \code{hist.default()} gains new \code{fuzz} argument, and the
      histogram \code{plot} method no longer uses fractional axis ticks
      when displaying counts (\code{"Frequency"}).

      \item \code{mapply()} and hence \code{Map()} now also obey the
      \dQuote{max-or-0-if-any} recycling rule, such that, e.g.,
      \code{Map(`+`, 1:3, 1[0])} is valid now.

      \item \code{as.character(<obj>)} for \code{"hexmode"} or
      \code{"octmode"} objects now fulfils the important basic rule
      \code{as.character(x)[j] === as.character(x[j])}.

      \item The set utility functions, notably \code{intersect()} have been
      tweaked to be more consistent and symmetric in their two set
      arguments, also preserving a common \code{mode}.

      \item \code{substr(ch, start,end) <- new} now e.g., preserves
      \code{names(ch)}; ditto for \code{substring()}, thanks to a patch
      from Brodie Gaslam.

      \item \code{plot(<lm>)} gains a \code{extend.ylim.f} argument, in
      partial response to \PR{15285}; further \PR{17784} is fixed thanks to
      several contributors and a patch by Elin Waring.
      The Cook's dist contours get customizable via \code{cook.col} and
      \code{cook.lty} with a different default color and their legend is
      nicer by default and customizable via \code{cook.legendChanges}.

      \item Attempting to subset an object that is not subsettable now
      signals an error of class \code{notSubsettableError}. The
      non-subsettable object is contained in the \code{object} field of
      the error condition.

      \item Subscript-out-of-bounds errors are now signaled as errors of
      class \code{subscriptOutOfBoundsError}.

      \item Stack-overflow errors are now signaled as errors inheriting
      from class \code{stackOverflowError}. See
      \code{?stackOverflowError} for more details.

      \item New partly experimental \code{Sys.setLanguage()} utility,
      solving the main problem of \PR{18055}.

      \item \code{gettext()} and \code{gettextf()} get a new option
      \code{trim = TRUE} which when set to false allows translations for
      strings such as \code{"Execution halted\n"} typical for C code.

      \item An experimental implementation of hash tables is now
      available.  See \code{?hashtab} for more details.

      \item \code{identical()} gains a \code{extptr.as.ref} argument for
      requesting that external pointer objects be compared as reference
      objects.

      \item \code{reorder()} gets an argument \code{decreasing} which it
      passes to \code{sort()} for level creation; based on the wish and
      patch by Thomas Soeiro in \PR{18243}.

      \item \code{as.vector()} gains a \code{data.frame} method which
      returns a simple named list, also clearing a long standing
      \sQuote{FIXME} to enable \code{as.vector(<data.frame>,
      mode="list")}.  This breaks code relying on
      \code{as.vector(<data.frame>)} to return the unchanged data frame.

      \item \code{legend()} is now vectorized for arguments \code{cex},
      \code{x.intersp}, and \code{text.width}.
      The latter can now also be specified as a vector (one element for
      each column of the legend) or as \code{NA} for computing a proper
      column wise maximum value of \code{strwidth(legend)}.
      The argument \code{y.intersp} can be specified as a vector with
      one entry for each row of the legend.

      \code{legend()} also gains new arguments \code{title.cex} and
      \code{title.font}. Thanks to Swetlana Herbrandt.

      \item Deparsing no longer remaps attribute names \code{dim},
      \code{dimnames}, \code{levels}, \code{names} and \code{tsp} to
      historical S-compatible names (which \code{structure()} maps back).

      \item \code{sample()} and \code{sample.int()} have additional
      sanity checks on their \code{size} and \code{n} arguments.

      \code{all.equal.numeric()} gains a sanity check on its
      \code{tolerance} argument -- calling \code{all.equal(a, b, c)} for
      three numeric vectors is a surprisingly common error.

      \code{mean(na.rm =)}, \code{rank(na.last =)},
      \code{barplot(legend.text =)}, \code{boxplot()},
      \code{contour(drawlabels =)}, \code{polygon(border =)} and
      \code{methods::is(class2 =)} have more robust sanity checks on
      their arguments.

      \command{R CMD Rd2pdf} (used by \command{R CMD check}) has a more
      robust sanity check on the format of \code{\\alias{}} commands.

      \item \code{psigamma(x, deriv)} for negative \code{x} now also works
      for \code{deriv = 4} and \code{5}; their underlying C level
      \code{dpsifn()} is documented in \sQuote{Writing R Extensions}.

      \item The HTML help system now uses HTML5 (wish of \PR{18149}).

      \item \code{ks.test()} now provides exact p-values also with ties
      and MC p-values in the two-sample (Smirnov) case.  By Torsten
      Hothorn.

      \item \code{ks.test()} gains a formula interface, with \code{y ~ 1}
      for the one-sample (Kolmogorov) test and \code{y ~ group} for the
      two-sample (Smirnov) test.  Contributed by Torsten Hothorn.

      \item The return value from \code{ks.test()} now has class
      \code{c("ks.test", "htest")} -- packages using \code{try()} need
      to take care to use \code{inherits()} and not \code{==} on the class.

      \item New functions \code{psmirnov()}, \code{qsmirnov()} and
      \code{rsmirnov()} in package \pkg{stats} implementing the
      asymptotic and exact distributions of the two-sample Smirnov statistic.

      \item \code{iconv()} now allows \code{sub = "c99"} to use C99-style
      escapes for UTF-8 inputs which cannot be converted to encoding \code{to}.

      \item In a forward pipe \code{|>} expression it is now possible to
      use a named argument with the placeholder \code{_} in
      the \code{rhs} call to specify where the \code{lhs} is to be
      inserted.  The placeholder can only appear once on the \code{rhs}.

      \item The included LAPACK sources have been updated to version 3.10.0,
      except for the four Fortran 77 routines which 3.10.0 has
      re-implemented in Fortran 90 (where the older versions have been
      retained as the \R build process does not support Fortran 90).

      \item \code{path.expand()} and most other uses of tilde expansion now
      warn if a path would be too long if expanded.  (An exception is
      \code{file.exists()}, which silently returns false.)

      \item \code{trunc(<Date>, *)} now supports \code{units = "months"}
      or \code{"years"} for consistency with the \code{POSIXt} method,
      thanks to Dirk Eddelbuettel's proposal in \PR{18099}.

      \item \code{list2DF()} now checks that its arguments are of the
      same length, rather than use recycling.

      \item The HTML help system has several new features: LaTeX-like
      math can be typeset using either \href{https://katex.org/}{KaTeX}
      or \href{https://www.mathjax.org/}{MathJax}, usage and example
      code is highlighted using \href{https://prismjs.com}{Prism}, and
      for dynamic help the output of examples and demos can be shown
      within the browser if the \CRANpkg{knitr} package is
      installed. These features can be disabled by setting the
      environment variable \code{_R_HELP_ENABLE_ENHANCED_HTML_} to a
      false value.

    }
  }

  \subsection{GRAPHICS}{
    \itemize{
      \item The graphics engine version, \code{R_GE_version}, has been
      bumped to \code{15} and so packages that provide graphics devices
      should be reinstalled.

      \item The \pkg{grid} package now allows the user to specify a
      \dQuote{vector} of pattern fills.  The \code{fill} argument
      to \code{gpar()} accepts a list of gradients and/or patterns and
      the functions \code{linearGradient()}, \code{radialGradient()},
      and \code{pattern()} have a new \code{group} argument.

      Points grobs (data symbols) can now also have a pattern fill.

      The \code{grobCoords()} function now returns a more informative
      and complex result.

      \item The \pkg{grid} package has new functions for drawing
      isolated groups: \code{grid.group()}, \code{grid.define()}, and
      \code{grid.use()}.  These functions add compositing operators and
      affine transformations to R's graphics capabilities.

      The \pkg{grid} package also has new functions for stroking
      and filling paths:  \code{grid.stroke()}, \code{grid.fill()},
      and \code{grid.fillStroke()}.

      A new function \code{as.path()} allows the user to specify the
      fill rule for a path that is to be used for clipping, stroking, or
      filling; available options are \code{"winding"} and
      \code{"evenodd"}.  A new function \code{as.mask()} allows the user
      to specify the type of a mask; available options are
      \code{"alpha"} and \code{"luminance"}.

      These new features are only supported so far (at most) on the
      Cairo-based graphics devices and on the \code{pdf()} device.

      \item \code{dev.capabilities()} reports on device support
      for the new features.

      \item \code{par()} now warns about unnamed non-character arguments
      to prevent misuse such as \code{{usr <- par("usr"); par(usr)}}.
    }
  }

  \subsection{WINDOWS}{
    \itemize{
      \item \R uses UTF-8 as the native encoding on recent Windows
      systems (at least Windows 10 version 1903, Windows Server 2022 or
      Windows Server 1903).  As a part of this change, R uses UCRT as
      the C runtime.  UCRT should be installed manually on systems older
      than Windows 10 or Windows Server 2016 before installing \R.

      \item The default personal library on Windows, folder
      \file{R\\win-library\\x.y} where \file{x.y} stands for \R release
      \file{x.y.z}, is now a subdirectory of Local Application Data
      directory (usually a hidden directory
      \file{C:\\Users\\username\\AppData\\Local}).  Use
      \code{shell.exec(.libPaths()[1])} from \R to open the personal
      library in Explorer when it is first in the list (\PR{17842}).

      \item \R uses a new 64-bit Tcl/Tk bundle.  The previous
      32-bit/64-bit bundle had a different layout and can no longer be
      used.

      \item Make files and installer scripts for Windows have been
      tailored to \file{Rtools42}, the newly recommended 64-bit
      \command{gcc} 10.3 MinGW-W64 UCRT toolchain.

      \item \file{Rtools42} by default uses the Windows security
      features ASLR and DEP; hence \acronym{CRAN} builds of \R and packages also
      do.

      \item \R now supports files \file{Makevars.ucrt},
      \file{Makefile.ucrt}, \file{configure.ucrt} and
      \file{cleanup.ucrt} in packages, which are used in preference to
      the \file{.win} variants.  This allows keeping the \file{.win}
      files around to support older versions of \R.  This feature will
      be removed in the future once support for older versions of \R
      would no longer be needed.

      \item \code{R.version} gains a new field \code{crt} (only on
      Windows) to denote the C runtime.  The value is \code{"ucrt"}.

      \item On Windows, \code{download.file(method = "auto")} and
      \code{url(method = "default")} now follow Unix in using
      \code{"libcurl"} for all except \samp{file://} URIs.

      \item \file{Rtools42} includes an unpatched Msys2 build of GNU
      \command{tar}.  Paths including drive letters can be made to work
      by adding \option{--force-local} to environment variable
      \env{TAR_OPTIONS}.  (\file{Rtools40} and earlier included a
      patched version which defaulted to this option.)

      \item Installer builds of \R automatically find the
      \file{Rtools42} software collection as well as the compiler
      toolchain.  No \env{PATH} setting is required from the user.

      \item The default installation directory of R for a user-only
      installation has been changed to the User Program Files directory
      (usually a hidden directory
      \file{C:\\Users\\username\\AppData\\Local\\Programs}) to follow
      Windows conventions.  Use \code{shell.exec(R.home())} from \R{} to
      open the \R installation directory in Explorer (\PR{17842}).

      \item \R now supports installation-time patching of packages.
      Patches may be installed from a supplied URL or a local directory
      or disabled.  Patches are included into the installed packages for
      reference.  This experimental feature may be removed in the
      future.

      \item \code{libcurl} is now required for building from source.

      \item The clipboard connection now works also with text in other
      than the current native encoding (\PR{18267}, with Hiroaki
      Yutani).  Text is always pasted to the clipboard  in UTF16-LE and
      the \code{encoding} argument is ignored.

      \item The internal case-changing functions are now used by default
      on Windows -- this circumvents problems (for example with E acute)
      of the UCRT Windows' runtime.

      \item \R on Windows now uses the system memory allocator.  Doug
      Lea's allocator was used since \R 1.2.0 to mitigate performance
      limitations seen with system allocators on earlier versions of
      Windows.

      \item \code{memory.limit()} and \code{memory.size()} are now stubs on
      Windows (as on Unix-alikes).

      \item Applications embedding \R on Windows can now use additional
      callbacks, which have so far only been available only on Unix
      (\PR{18286}).
    }
  }


  \subsection{INSTALLATION}{
    \itemize{
      \item Facilities for accessing \samp{ftp://} sites are no longer
      tested (except \emph{pro tem} for \code{curlGetHeaders()}) as modern
      browsers have removed support.
      %% Chrome and Firefox did in early 2021: Safari and Edge never had it.

      \item \R can now be built with \samp{DEFS = -DSTRICT_R_HEADERS} .
    }
  }


  \subsection{PACKAGE INSTALLATION}{
    \itemize{
      \item \command{R CMD INSTALL} no longer tangles vignettes. This
      completes an \command{R CMD build} change in \R 3.0.0 and affects
      packages built before \R 3.0.2. Such packages should be re-made
      with \command{R CMD build} to have the tangled \R code of vignettes
      shipped with the tarball.

      \item \code{USE_FC_LEN_T} will become the default: this uses the
      correct prototypes for Fortran BLAS/LAPACK routines called from
      C/C++, and requires adjustment of most such calls -- see
      \sQuote{Writing R Extensions} §6.6.1.   (This has been supported
      since \R 3.6.2.)

      \item Package installation speed for packages installed with
      \code{keep.source} has been improved. This resolve the issue
      reported by Ofek Shilon in \PR{18236}.
    }
  }

  \subsection{UTILITIES}{
    \itemize{
      \item \command{R CMD check} can optionally report
      files/directories left behind in home, \file{/tmp} (even though
      \env{TMPDIR} is set) and other directories.  See the \dQuote{R
	Internals} manual for details.

      \item \command{R CMD check} now reports byte-compilation errors
      during installation.  These are not usually fatal but may result
      in parts of the package not being byte-compiled.

      \item \env{_R_CHECK_DEPENDS_ONLY_} can be applied selectively to
      examples, tests and/or vignettes in \command{R CMD check}: see the
      \dQuote{R Internals} manual.

      \item \env{_R_CHECK_SRC_MINUS_W_IMPLICIT_} now defaults to true:
      recent versions of Apple \command{clang} on macOS have made
      implicit function declarations in C into a compilation error.

      \item \command{R CMD check --as-cran} makes use of the
      environment variable \env{AUTORECONF}.  See the
      \dQuote{R Internals} manual §8 for further details.

      \item \command{R CMD check --use-valgrind} also uses
      \command{valgrind} when re-building vignettes as some non-Sweave
      vignettes unhelpfully comment out all their code when
      \command{R CMD check} runs vignettes.

      \item Errors in re-building vignettes (unless there are LaTeX
      errors) are reported by \command{R CMD check} as \samp{ERROR} rather
      than \samp{WARNING} when running vignettes has been skipped (as it
      frequently is in \acronym{CRAN} checks and by \option{--as-cran}).

      \item \command{R CMD Rd2pdf} gains a \option{--quiet} option that is
      used by \command{R CMD build} when building the PDF package manual.

      \item \command{R CMD Rd2pdf} now always runs LaTeX in batch mode,
      consistent with Texinfo \eqn{\ge}{>=} 6.7. The \option{--batch}
      option is ignored.

      \item \command{R CMD build} and \command{R CMD check} now include
      the Rd file name and line numbers in the error message of an
      \verb{\Sexpr} evaluation failure.

      \item For packages using the \verb{\doi} Rd macro (now an
      install-time \verb{\Sexpr}) but no other dynamic Rd content,
      \command{R CMD build} now produces a smaller tarball and is
      considerably faster -- skipping temporary package installation.

      \item \command{R CMD check} can optionally (but included in
      \option{--as-cran}) validate the HTML produced from the packages
      \file{.Rd} files.  See
      \url{https://blog.r-project.org/2022/04/08/enhancements-to-html-documentation/}:
      this needs a fairly recent version of HTML Tidy to be available.
    }
  }

  \subsection{C-LEVEL FACILITIES}{
    \itemize{
      \item The non-API header \file{R_ext/R-ftp-http.h} is no longer
      provided, as the entry points it covered are now all defunct.

      \item A number of non-API declarations and macro definitions have
      been moved from the installed header \file{Rinternals.h} to the
      internal header \file{Defn.h}. Packages that only use entry points
      and definitions documented to be part of the API as specified in
      \sQuote{Writing R Extensions} §6 should not be affected.

      \item The macro \code{USE_RINTERNALS} no longer has any effect
      when compiling package code.  Packages which also use
      \code{R_NO_REMAP} will need to ensure that the remapped names are
      used for calls to API functions that were formerly also made
      available as macros.

      \item The deprecated legacy S-compatibility macros \code{PROBLEM},
      \code{MESSAGE}, \code{ERROR}, \code{WARN}, \code{WARNING},
      \code{RECOVER}, \dots{} are no longer defined in
      \file{R_exts/RS.h} (included by \file{R.h}).  Replace these by
      calls to \code{Rf_error} and \code{Rf_warning} (defined in header
      \file{R_ext/Error.h} included by \file{R.h}).

      Header \file{R_ext/RS.h} no longer includes \file{R_ext/Error.h}.

      \item Header \file{R_ext/Constants.h} (included by \file{R.h})
      when included from C++ now includes the C++ header \file{cfloat}
      rather than the C header \file{float.h} (now possible as C++11 is
      required).

      \item The legacy S-compatibility macros \code{DOUBLE_*} in
      \file{R_ext/Constants.h} (included by \file{R.h}) are deprecated.

      \item The deprecated S-compatibility macros \code{SINGLE_*} in
      \file{R_ext/Constants.h} (included by \file{R.h}) have been
      removed.

      \item \code{R_Calloc}, \code{R_Free} and \code{R_Realloc} are
      preferred to their unprefixed forms and error messages now use the
      prefix. These forms were introduced in \R 3.4.0 and are available
      even when \code{STRICT_R_HEADERS} is defined.

      \item \code{rmultinom} has been documented in \sQuote{Writing R
	Extensions} §6 so is now part of the \R API.

      \item Similarly, \code{Rtanpi}, called from \R level \code{tanpi()}
      is now part of the \R API.

      \item The long-deprecated, undocumented and non-API entry point
      \code{call_R} is no longer declared in \file{R_ext/RS.h} (included
      by \file{R.h}).

      \item The header \file{S.h} which has been unsupported since Jan
      2016 has been removed.   Use \file{R.h} instead.
    }
  }

  \subsection{DEPRECATED AND DEFUNCT}{
    \itemize{
      \item The (non-default and deprecated) \code{method = "internal"}
      for \code{download.file()} and \code{url()} no longer supports
      \samp{http://} nor \samp{ftp://} URIs.  (It is used only for
      \samp{file://} URIs.)

      On Windows, \code{download.file(method = "wininet")} no longer
      supports \samp{ftp://} URIs.  (It is no longer the default method,
      which is \code{"libcurl"} and does.)

      On Windows, the deprecated \code{method = "wininet"} now gives a
      warning for \samp{http://} and \samp{https://} URIs for both
      \code{download.file()} and \code{url()}. (It is no longer the default
      method.)

      \item On Windows, the command-line option \option{--max-mem-size}
      and environment variable \env{R_MAX_MEM_SIZE} are defunct.  The
      memory allocation limit was important for 32-bit builds,
      but these are no longer supported.

      \item \code{default.stringsAsFactors()} is now formally deprecated,
      where that was only mentioned on its regular help page,
      previously.  So it now gives a warning if called.

      \item \code{unix.time()} is defunct now; it had been deprecated since
      \R 3.4.0.
    }
  }

  \subsection{BUG FIXES}{
    \itemize{
      \item Setting \code{digits = 0} in \code{format()},
      \code{print.default()} (and hence typically \code{print()}) or
      \code{options()} is again invalid.  Its behaviour was
      platform-dependent, and it is unclear what \dQuote{zero
	significant digits} should mean (\PR{18098}).

      \item Messages from C code in the \file{cairo} section of package
      \pkg{grDevices} are now also offered for translation, thanks to
      Michael Chirico's \PR{18123}.

      \item \code{mean(x)} with finite \code{x} now is finite also without
      "long.double" capability.

      \item \command{R CMD Rd2pdf} no longer leaves an empty build
      directory behind when it aborts due to an already existing output
      file.  (Thanks to Sebastian Meyer's \PR{18141}.)

      \item \code{density(x, weights = w, na.rm = TRUE)} when \code{anyNA(x)}
      is true, now removes weights \dQuote{in parallel} to \code{x}, fixing
      \PR{18151}, reported by Matthias Gondan.
      Additionally, it gets a \code{subdensity} option.

      \item Conversion of \verb{\Sexpr[]{<expR>}} to LaTeX or HTML no longer
      produces long blocks of empty lines when \code{<expR>} itself
      contains several lines all producing empty output.  Thanks to a
      report and patch by Ivan Krylov posted to R-devel.

      \item \command{R CMD build} no longer fails if a package vignette
      uses child documents and \file{inst/doc} exists.  (Thanks to
      Sebastian Meyer's \PR{18156}.)

      \item When an R documentation (\sQuote{help} source) file
      \file{man/foo.Rd} in a package has \verb{\donttest{..}} examples
      with a syntax error, it is now signalled as ERROR and with correct
      line numbers relating to the \file{*-Ex.R} file, thanks to Duncan
      Murdoch and Sebastian Meyer's reports and patch proposals in
      \PR{17501}.

      \item Improved determination of the correct translation domain in
      non-base packages, addressing the combination of \PR{18092} and
      \PR{17998} (\code{#c6}) with reports and \emph{augmented} patch
      #2904 by Suharto Anggono.

      Note that \code{"R-base"} is no longer the default domain e.g.,
      for top-level calls to \code{gettext()}; rather translation needs
      explicit \code{domain = *} specification in such cases.

      \item \code{identical(attrib.as.set=FALSE)} now works correctly with
      data frames with default row names (Thanks to Charlie Gao's
      \PR{18179}).

      \item \code{txtProgressBar()} now enforces a non-zero width for
      argument \code{char}, without which no progress can be visible.

      \item \code{dimnames(table(d))} is more consistent in the case where
      \code{d} is a list with a single component, thanks to Thomas Soeiro's
      report to R-devel.

      Further, \code{table(d1, d2)} now gives an error when \code{d1} and
      \code{d2} are data frames as suggested by Thomas in \PR{18224}.

      \item Fix for drawing semi-transparent lines and fills on the
      native Windows graphics device (\PR{18219} and \PR{16694}).
      Thanks to Nick Ray for helpful diagnosis on Bugzilla.

      \item The deparser now wraps sub-expressions such as \code{if(A) .. }
      with parentheses when needed; thanks to Duncan Murdoch's \PR{18232}
      and Lionel Henry's patches there.

      \item \code{remove.packages()} no longer tries to uninstall
      \code{Priority: base} packages, thanks to a report and suggestions
      by Colin Fay in \PR{18227}.

      \item \code{win.metafile()} now has \code{xpinch} and
      \code{ypinch} arguments so that the user can override Windows'
      (potentially wrong) guess at device dimensions.

      \item \code{x[i]} and \code{x[[i]]} for non-integer \code{i}
      should now behave in all cases as always documented: the index used is
      equivalent to \code{as.integer(i)} unless that would overflow where
      \code{trunc(i)} is used instead; thanks to Suharto Anggono's report
      and patch proposals in \PR{17977}.

      \item \code{asOneSidedFormula()} now associates the resulting
      formula with the global environment rather than the evaluation
      environment created for the call.

      \item \code{<bibentry>$name} now matches the field name
      case-insensitively, consistent with \code{bibentry()} creation and
      the replacement method.

      \item \code{cbind()} failed to detect some length mismatches with
      a mixture of time-series and non-time-series inputs.

      \item The default LaTeX style file \file{Sweave.sty} used by the
      \code{RweaveLatex} driver no longer loads the obsolete \samp{ae}
      package; thanks to a report by Thomas Soeiro in \PR{18271}.
      Furthermore, it now skips \samp{\\usepackage[T1]{fontenc}} for
      engines other than pdfTeX (if detected) or if the new
      \samp{[nofontenc]} option is used.

      \item \code{smooth.spline()} now stores its logical \code{cv}
      argument more safely, fixing a rare bug when printing, and also
      stores \code{n}.

      \item \code{smooth.spline(x,y,*)} now computes the \code{cv.crit}
      statistic correctly, also when \code{is.unsorted(x)}, fixing
      \PR{18294}.

      \item The \code{data.frame} method of \code{rbind()} now warns
      when binding not-wholly-recycling vectors, by analogy to the default
      method (for matrices).

      \item \code{setAs()} finds the correct class for name \code{to}
      when multiple packages define a class with that name. Thanks to
      Gabor Csardi for the report.

      \item Fix for detaching a package when two classes of the same name
      are present in method signatures for the same generic. Thanks to
      Gabor Csardi for the report.

      \item \code{match.arg("", c("", "a", "B"))} gives a better error
      message, in part from \PR{17959}, thanks to Elin Waring.

      \item \command{R CMD Sweave --clean} no longer removes
      pre-existing files or subdirectories (\PR{18242}).

      \item The \code{quartz()} device no longer splits polylines into
      subpaths. That has caused narrowly-spaced lines with many points
      to always look solid even when dashed line type was used due to
      dash phase restarts.

      \item Deparsing constructs such as \code{quote(1 + `!`(2) + 3)} works
      again as before R 3.5.0, thanks to the report and patch in \PR{18284}
      by Suharto Anggono.

      \item \code{as.list(f)} for a \code{factor} \code{f} now keeps
      \code{names(f)}, fixing \PR{18309}.

      \item \code{qbeta(.001, .9, .009)} and analogous \code{qf()} calls now
      return a correct value instead of \code{NaN} or wrongly \code{1}, all
      with a warning; thanks to the report by Ludger Goeminne in \PR{18302}.

      \item \code{plot.lm()} failed to produce the plot of residuals vs.
      factor levels (i.e., \code{which=5} when leverages are constant)
      for models with character predictors (\PR{17840}).

      \item \code{interaction.plot(..., xtick = TRUE)} misplaced the
      x-axis line (\PR{18305}).

      \item Not strictly fixing a bug, \code{format()}ing and
      \code{print()}ing of non-finite \code{Date} and \code{POSIXt}
      values \code{NaN} and \eqn{\pm}{+/-}\code{Inf} no longer show as
      \code{NA} but the respective string, e.g., \code{Inf}, for
      consistency with numeric vector's behaviour, fulfilling the wish
      of \PR{18308}.

      \item \command{R CMD check} no longer runs test scripts generated
      from corresponding \file{.Rin} files twice and now signals an
      ERROR if processing an \file{.Rin} script fails.

      \item \code{tools::Rd2txt()} used for plain-text help pages now renders
      \verb{\href}s (if \code{tools::Rd2txt_options(showURLs = TRUE)})
      and \verb{\url}s with percent-encoding and standards-compliant
      delimiting style (angle brackets and no \samp{URL: } prefix).
      \verb{\email} is now rendered with a \samp{mailto:} prefix.
    }
  }
}

\section{\Rlogo CHANGES IN R 4.1.3}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item The default version of Bioconductor has been changed to
      3.14. (This is used by \code{setRepositories} and the menus in
      GUIs.)
    }
  }

  \subsection{UTILITIES}{
    \itemize{
      \item \command{R CMD check --as-cran} has a workaround for a bug
      in versions of \command{file} up to at least 5.41 which
      mis-identify DBF files last changed in 2022 as executables.
    }
  }

  \subsection{C-LEVEL FACILITIES}{
    \itemize{
      \item The legacy S-compatibility macros \code{SINGLE_*} in
      \file{R_ext/Constants.h} (included by \file{R.h}) are deprecated
      and will be removed in \R 4.2.0.
    }
  }

  \subsection{BUG FIXES}{
    \itemize{
      \item Initialization of self-starting \code{nls()} models with
      initialization functions following the pre-R-4.1.0 API
      (without the \code{...} argument) works again for now, with a
      deprecation warning.

      \item Fixed quoting of \code{~autodetect~} in Java setting defaults
      to avoid inadvertent user lookup due to leading \code{~}, reported
      in \PR{18231} by Harold Gutch.

      \item \code{substr(., start, stop) <- v} now treats \emph{negative}
      \code{stop} values correctly.  Reported with a patch in \PR{18228} by
      Brodie Gaslam.

      \item Subscripting an array \code{x} without dimnames by a
      \code{length(dim(x))}-column character matrix gave "random"
      non-sense, now an error; reported in \PR{18244} by Mikael Jagan.

      \item \code{...names()} now matches \code{names(list(...))} closely,
      fixing \PR{18247}.

      \item \code{all.equal(*, scale = s)} now works as intended when
      \code{length(s) > 1}, partly thanks to Michael Chirico's \PR{18272}.

      \item \code{print(x)} for long vectors \code{x} now also works for
      named atomic vectors or lists and prints the correct number when
      reaching the \code{getOption("max.print")} limit; partly thanks to a
      report and proposal by Hugh Parsonage to the R-devel list.

      \item \code{all.equal(<selfStart>, *)} no longer signals a deprecation
      warning.

      \item \code{reformulate(*, response=r)} gives a helpful error message
      now when \code{length(r) > 1}, thanks to Bill Dunlap's \PR{18281}.

      \item Modifying \code{globalCallingHandlers} inside
      \code{withCallingHandlers()} now works or fails correctly, thanks to
      Henrik Bengtsson's \PR{18257}.

      \item \code{hist(<Date>, breaks = "days")} and
      \code{hist(<POSIXt>, breaks = "secs")}
      no longer fail for inputs of length 1.

      \item \code{qbeta(.001, .9, .009)} and similar cases now converge
      correctly thanks to Ben Bolker's report in \PR{17746}.

      \item \code{window(x, start, end)} no longer wrongly signals
      \dQuote{'start' cannot be after 'end'}, fixing \PR{17527} and
      \PR{18291}.

      \item \code{data()} now checks that its (rarely used) \code{list}
      argument is a character vector -- a couple of packages passed other
      types and gave incorrect results.

      \item \code{which()} now checks its \code{arr.ind} argument is
      TRUE rather coercing to logical and taking the first element --
      which gave incorrect results in package code.

      \item \code{model.weights()} and \code{model.offset()} more carefully
      extract their model components, thanks to Ben Bolker and Tim Taylor's
      R-devel post.

      \item \code{list.files(recursive = TRUE)} now shows all broken
      symlinks (previously, some of them may have been omitted, \PR{18296}).
    }
  }
}

\section{\Rlogo CHANGES IN R 4.1.2}{
  \subsection{C-LEVEL FACILITIES}{
    \itemize{
      \item The workaround in headers \file{R.h} and \file{Rmath.h}
      (\code{using namespace std;}) for the Oracle Developer Studio
      compiler is no longer needed now C++11 is required so has been
      removed.  A couple more usages of \code{log()} (which should have
      been \code{std::log()}) with an \code{int} argument are reported on
      Solaris.

      \item The undocumented limit of 4095 bytes on messages from the
      S-compatibility macros \code{PROBLEM} and \code{MESSAGE} is now
      documented and longer messages will be silently truncated rather
      than potentially causing segfaults.

      \item If the \code{R_NO_SEGV_HANDLER} environment variable is
      non-empty, the signal handler for SEGV/ILL/BUS signals (which
      offers recovery user interface) is not set. This allows more
      reliable debugging of crashes that involve the console.
    }
  }

  \subsection{DEPRECATED AND DEFUNCT}{
    \itemize{
      \item The legacy S-compatibility macros \code{PROBLEM},
      \code{MESSAGE}, \code{ERROR}, \code{WARN}, \code{WARNING},
      \code{RECOVER}, \dots{} are deprecated and will be hidden in \R
      4.2.0. \R{}'s native interface of \code{Rf_error} and
      \code{Rf_warning} has long been preferred.
    }
  }

  \subsection{BUG FIXES}{
    \itemize{
      \item \code{.mapply(F, dots, .)} no longer segfaults when
      \code{dots} is not a \code{list} and uses \code{match.fun(F)} as
      always documented; reported by Andrew Simmons in \PR{18164}.

      \item \code{hist(<Date>, ...)} and \code{hist(<POSIXt>, ...)}
      no longer pass arguments for \code{rect()} (such as \code{col} and
      \code{density}) to \code{axis()}.  (Thanks to Sebastian Meyer's
      \PR{18171}.)

      \item \verb{\Sexpr{ch}} now preserves \code{Encoding(ch)}. (Thanks to
      report and patch by Jeroen Ooms in \PR{18152}.)

      \item Setting the RNG to \code{"Marsaglia-Multicarry"} e.g., by
      \code{RNGkind()}, now warns in more places, thanks to
      André Gillibert's report and patch in \PR{18168}.

      \item \code{gray(numeric(), alpha=1/2)} no longer segfaults, fixing
      \PR{18183}, reported by Till Krenz.

      \item Fixed \code{dnbinom(x, size=<very_small>, .., log=TRUE)}
      regression, reported by Martin Morgan.

      \item \code{as.Date.POSIXlt(x)} now keeps \code{names(x)}, thanks to
      Davis Vaughan's report and patch in \PR{18188}.

      \item \code{model.response()} now strips an \code{"AsIs"} class typically,
      thanks to Duncan Murdoch's report and other discussants in \PR{18190}.

      \item \code{try()} is considerably faster in case of an error and
      long call, as e.g., from some \code{do.call()}.  Thanks to
      Alexander Kaever's suggestion posted to R-devel.

      \item \code{qqline(y = <object>)} such as \code{y=I(.)}, now works,
      see also \PR{18190}.

      \item Non-integer \code{mgp} \code{par()} settings are now handled
      correctly in \code{axis()} and \code{mtext()}, thanks to Mikael
      Jagan and Duncan Murdoch's report and suggestion in \PR{18194}.

      \item \code{formatC(x)} returns length zero \code{character()} now,
      rather than \code{""} when \code{x} is of length zero, as documented,
      thanks to Davis Vaughan's post to R-devel.

      \item \code{removeSource(fn)} now retains (other) \code{attributes(fn)}.
    }
  }
}

\section{\Rlogo CHANGES IN R 4.1.1}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item \code{require(\var{pkg}, quietly = TRUE)} is quieter and in
      particular does not warn if the package is not found.
    }
  }

  \subsection{DEPRECATED AND DEFUNCT}{
    \itemize{
      \item Use of \samp{ftp://} URIs should be regarded as
      deprecated, with on-going support confined to \code{method =
	"libcurl"} and not routinely tested.  (Nowadays no major browser
      supports them.)

      \item The non-default \code{method = "internal"} is deprecated for
      \samp{http://} and \samp{ftp://} URIs for both
      \code{download.file} and \code{url}.

      \item On Windows, \code{method = "wininet"} is deprecated for
      \samp{http://}, \samp{https://} and \samp{ftp://} URIs for both
      \code{download.file} and \code{url}.  (A warning is only given for
      \samp{ftp://}.)

      For \samp{ftp://} URIs the default method is now \code{"libcurl"}
      if available (which it is on \acronym{CRAN} builds).

      \code{method = "wininet"} remains the default for \samp{http://}
      and \samp{https://} URIs but if \code{libcurl} is available, using
      \code{method = "libcurl"} is preferred.
    }
  }

  \subsection{INSTALLATION}{
    \itemize{
      \item \command{make check} now works also without a LaTeX
      installation.  (Thanks to Sebastian Meyer's \PR{18103}.)
    }
  }

  \subsection{BUG FIXES}{
    \itemize{
      \item \command{make check-devel} works again in an \R build
      configured with \option{--without-recommended-packages}.

      \item \code{qnbinom(p, size, mu)} for large \code{size/mu} is correct
      now in a range of cases (\PR{18095}); similarly for the \code{(size,
      prob)} parametrization of the negative binomial.  Also \code{qpois()}
      and \code{qbinom()} are better and or faster for extreme cases.  The
      underlying C code has been modularized and is common to all four cases
      of discrete distributions.

      \item \code{gap.axis} is now part of the \code{axis()} arguments which
      are passed from \code{bxp()}, and hence \code{boxplot()}.  (Thanks to
      Martin Smith's report and suggestions in \PR{18109}.)

      \item \code{.First} and \code{.Last} can again be set from the site
      profile.

      \item \code{seq.int(from, to, *)} and \code{seq.default(..)} now work
      better in large range cases where \code{from-to} is infinite where
      the two boundaries are finite.

      \item \code{all.equal(x,y)} now returns \code{TRUE} correctly also
      when several entries of \code{abs(x)} and \code{abs(y)} are close to
      \code{.Machine$double.xmax}, the largest finite \code{numeric}.

      \item \code{model.frame()} now clears the object bit when removing the
      \code{class} attribute of a value via \code{na.action} (\PR{18100}).

      \item \code{charClass()} now works with multi-character
      strings on Windows (\PR{18104}, fixed by Bill Dunlap).

      \item \code{encodeString()} on Solaris now works again in Latin-1
      encoding on characters represented differently in UTF-8.  Support for
      surrogate pairs on Solaris has been improved.

      \item \code{file.show()} on Windows now works with non-ASCII path
      names representable in the current native encoding (\PR{18132}).

      \item Embedded \R on Windows can now find \R home directory via the
      registry even when installed only for the current user (\PR{18135}).

      \item \code{pretty(x)} with finite \code{x} now returns finite values
      also in the case where the extreme \code{x} values are close in size
      to the maximal representable number \code{.Machine$double.xmax}.

      Also, it's been tweaked for very small ranges and when a boundary is
      close (or equal) to zero; e.g., \code{pretty(c(0,1e-317))} no longer
      has negative numbers, currently still warning about a very small
      range, and \code{pretty(2^-(1024 - 2^-1/(c(24,10))))} is more accurate.

      \item The error message for not finding vignette files when weaving
      has correct file sizes now. (Thanks to Sebastian Meyer's \PR{18154}.)

      \item \code{dnbinom(20, <large>, 1)} now correctly gives 0, and
      similar cases are more accurate with underflow precaution.
      (Reported by Francisco Vera Alcivar in \PR{18072}.)
    }
  }
}


\section{\Rlogo CHANGES IN R 4.1.0}{
  \subsection{FUTURE DIRECTIONS}{
    \itemize{
      \item It is planned that the 4.1.x series will be the last to
      support 32-bit Windows, with production of binary packages for
      that series continuing until early 2023.
    }
  }

  \subsection{SIGNIFICANT USER-VISIBLE CHANGES}{
    \itemize{
      \item Data set \code{esoph} in package \pkg{datasets} now provides
      the correct numbers of controls; previously it had the numbers of
      cases added to these.  (Reported by Alexander Fowler in \PR{17964}.)
    }
  }

  \subsection{NEW FEATURES}{
    \itemize{
      \item \samp{www.omegahat.net} is no longer one of the repositories
      known by default to \code{setRepositories()}.  (Nowadays it only
      provides source packages and is often unavailable.)
      %% it had one Windows binary package for 4.0, none for 3.6.

      \item Function \code{package_dependencies()} (in package
      \pkg{tools}) can now use different dependency types for direct and
      recursive dependencies.

      \item The checking of the size of tarball in
      \command{R CMD check --as-cran <pkg>} may be tweaked via the new
      environment variable
      \env{_R_CHECK_CRAN_INCOMING_TARBALL_THRESHOLD_}, as suggested in
      \PR{17777} by Jan Gorecki.

      \item Using \code{c()} to combine a factor with other factors now
      gives a factor, an ordered factor when combining ordered factors
      with identical levels.

      \item \code{apply()} gains a \code{simplify} argument to allow
      disabling of simplification of results.

      \item The \code{format()} method for class \code{"ftable"} gets a
      new option \code{justify}.  (Suggested by Thomas Soeiro.)

      \item New \code{...names()} utility.  (Proposed by Neal Fultz in
      \PR{17705}.)

      \item \code{type.convert()} now warns when its \code{as.is} argument
      is not specified, as the help file always said it \emph{should}.  In
      that case, the default is changed to \code{TRUE} in line with its
      change in \code{read.table()} (related to \code{stringsAsFactors}) in
      \R 4.0.0.

      \item When printing list arrays, classed objects are now shown
      \emph{via} their \code{format()} value if this is a short enough
      character string, or by giving the first elements of their class
      vector and their length.

      \item \code{capabilities()} gets new entry \code{"Rprof"} which is
      \code{TRUE} when \R has been configured with the equivalent of
      \command{--enable-R-profiling} (as it is by default).  (Related to
      Michael Orlitzky's report \PR{17836}.)

      \item \code{str(xS4)} now also shows extraneous attributes of an
      S4 object \code{xS4}.

      \item Rudimentary support for vi-style tags in \code{rtags()} and
      \command{R CMD rtags} has been added.  (Based on a patch from Neal
      Fultz in \PR{17214}.)

      \item \code{checkRdContents()} is now exported from \pkg{tools}; it
      and also \code{checkDocFiles()} have a new option \code{chkInternal}
      allowing to check Rd files marked with keyword \code{"internal"} as
      well.  The latter can be activated for \command{R CMD check} via
      environment variable \env{_R_CHECK_RD_INTERNAL_TOO_}.

      \item New functions \code{numToBits()} and \code{numToInts()}
      extend the \code{raw} conversion utilities to (double precision)
      \code{numeric}.

      \item Functions \code{URLencode()} and \code{URLdecode()} in
      package \pkg{utils} now work on vectors of URIs.
      (Based on patch from Bob Rudis submitted with \PR{17873}.)

      \item \code{path.expand()} can expand \samp{~user} on most
      Unix-alikes even when \code{readline} is not in use.  It tries
      harder to expand \samp{~}, for example should environment variable
      \env{HOME} be unset.

      \item For HTML help (both dynamic and static), Rd file links to
      help pages in external packages are now treated as references to
      topics rather than file names, and fall back to a file link only
      if the topic is not found in the target package. The earlier rule
      which prioritized file names over topics can be restored by
      setting the environment variable \env{_R_HELP_LINKS_TO_TOPICS_} to
      a false value.

      \item \code{c()} now removes \code{NULL} arguments before
      dispatching to methods, thus simplifying the implementation of
      \code{c()} methods, \emph{but} for back compatibility keeps
      \code{NULL} when it is the first argument.  (From a report and
      patch proposal by Lionel Henry in \PR{17900}.)

      \item \code{Vectorize()}'s result function's environment no longer
      keeps unneeded objects.

      \item Function \code{...elt()} now propagates visibility
      consistently with \code{..n}.  (Thanks to Lionel Henry's
      \PR{17905}.)

      \item \code{capture.output()} no longer uses non-standard
      evaluation to evaluate its arguments.  This makes evaluation of
      functions like \code{parent.frame()} more consistent.  (Thanks to
      Lionel Henry's \PR{17907}.)

      \item \code{packBits(bits, type="double")} now works as inverse of
      \code{numToBits()}.  (Thanks to Bill Dunlap's proposal in
      \PR{17914}.)

      \item \code{curlGetHeaders()} has two new arguments,
      \code{timeout} to specify the timeout for that call (overriding
      \code{getOption("timeout")}) and \code{TLS} to specify the minimum
      TLS protocol version to be used for \code{https://} URIs
      (\emph{inter alia} providing a means to check for sites using
      deprecated TLS versions 1.0 and 1.1).

      \item For \code{nls()}, an optional constant \code{scaleOffset}
      may be added to the denominator of the relative offset convergence
      test for cases where the fit of a model is expected to be exact,
      thanks to a proposal by John Nash.  \code{nls(*, trace=TRUE)} now
      also shows the convergence criterion.

      \item Numeric differentiation \emph{via} \code{numericDeriv()}
      gets new optional arguments \code{eps} and \code{central}, the
      latter for taking central divided differences.  The latter can be
      activated for \code{nls()} via \code{nls.control(nDcentral =
      TRUE)}.

      \item \code{nls()} now passes the \code{trace} and \code{control}
      arguments to \code{getInitial()}, notably for all self-starting models,
      so these can also be fit in zero-noise situations via a
      \code{scaleOffset}.  For this reason, the \code{initial} function of a
      \code{selfStart} model must now have \code{...} in its argument list.

      \item \code{bquote(splice = TRUE)} can now splice expression
      vectors with attributes: this makes it possible to splice the
      result of \code{parse(keep.source = TRUE)}.  (Report and patch
      provided by Lionel Henry in \PR{17869}.)

      \item \code{textConnection()} gets an optional \code{name} argument.

      \item \code{get()}, \code{exists()}, and \code{get0()} now signal
      an error if the first argument has length greater than 1.
      Previously additional elements were silently ignored.  (Suggested
      by Antoine Fabri on R-devel.)

      \item \R{} now provides a shorthand notation for creating functions,
      e.g. \code{\(x) x + 1} is parsed as \code{function(x) x + 1}.

      \item \R{} now provides a simple native forward pipe syntax
      \code{|>}.  The simple form of the forward pipe inserts the
      left-hand side as the first argument in the right-hand side call.
      The pipe implementation as a syntax transformation was motivated
      by suggestions from Jim Hester and Lionel Henry.

      \item \code{all.equal(f, g)} for \code{function}s now by default also
      compares their \code{environment(.)}s, notably via new
      \code{all.equal} method for class \code{function}.  Comparison of
      \code{nls()} fits, e.g., may now need \code{all.equal(m1, m2,
	check.environment = FALSE)}.

      \item \code{.libPaths()} gets a new option \code{include.site},
      allowing to \emph{not} include the site library.  (Thanks to Dario
      Strbenac's suggestion and Gabe Becker's \PR{18016}.)

      \item Lithuanian translations are now available.  (Thanks to
      Rimantas Žakauskas.)

      \item \code{names()} now works for \code{DOTSXP} objects.  On the
      other hand, in \file{R-lang}, the R language manual, we now warn
      against relying on the structure or even existence of such
      dot-dot-dot objects.

      \item \code{all.equal()} no longer gives an error on \code{DOTSXP}
      objects.

      %% The Homebrew arm64 libs have cairo but not cairo-xlib
      \item \code{capabilities("cairo")} now applies only to the
      file-based devices as it is now possible (if very unusual) to
      build \R with Cairo support for those but not for \code{X11()}.

      \item There is optional support for tracing the progress of
      \code{loadNamespace()} --- see its help.

      \item (Not Windows.)
      \code{l10n_info()} reports an additional element, the name of the
      encoding as reported by the OS (which may differ from the
      encoding part (if any) of the result from
      \code{Sys.getlocale("LC_CTYPE")}).

      \item New function \code{gregexec()} which generalizes \code{regexec()}
      to find \emph{all} disjoint matches and all substrings
      corresponding to parenthesized subexpressions of the given regular
      expression.  (Contributed by Brodie Gaslam.)

      \item New function \code{charClass()} in package \pkg{utils} to
      query the wide-character classification functions in use (such as
      \code{iswprint}).

      \item The names of \code{quantile()}'s result no longer depend on the
      global \code{getOption("digits")}, but \code{quantile()} gets a new
      optional argument \code{digits = 7} instead.

      \item \code{grep()}, \code{sub()}, \code{regexp} and variants work
      considerably faster for long factors with few levels.  (Thanks to
      Michael Chirico's \PR{18063}.)

      \item Provide grouping of \code{x11()} graphics windows within
      a window manager such as \code{Gnome} or \code{Unity}; thanks to a
      patch by Ivan Krylov posted to R-devel.

      \item The \code{split()} method for class \code{data.frame} now
      allows the \code{f} argument to be specified as a formula.

      \item \code{sprintf} now warns on arguments unused by the format
      string.

      \item New palettes \code{"Rocket"} and \code{"Mako"} for
      \code{hcl.colors()} (approximating palettes of the same name
      from the \pkg{viridisLite} package).

      Contributed by Achim Zeileis.

      \item The base environment and its namespace are now locked (so
      one can no longer add bindings to these or remove from these).

      \item \command{Rterm} handling of multi-byte characters has been
      improved, allowing use of such characters when supported by the
      current locale.

      \item \command{Rterm} now accepts \code{ALT+ +xxxxxxxx} sequences to
      enter Unicode characters as hex digits.

      \item Environment variable \env{LC_ALL} on Windows now takes
      precedence over \env{LC_CTYPE} and variables for other supported
      categories, matching the POSIX behaviour.

      \item \code{duplicated()} and \code{anyDuplicated()} are now
      optimized for integer and real vectors that are known to be sorted
      via the ALTREP framework. Contributed by Gabriel Becker via \PR{17993}.
    }
  }

  \subsection{GRAPHICS}{
    \itemize{
      \item The graphics engine version, \code{R_GE_version}, has been
      bumped to \code{14} and so packages that provide graphics devices
      should be reinstalled.

      \item Graphics devices should now specify \code{deviceVersion} to
      indicate what version of the graphics engine they support.

      \item Graphics devices can now specify \code{deviceClip}.  If
      \code{TRUE}, the graphics engine will never perform any clipping
      of output itself.

      The clipping that the graphics engine does perform (for both
      \code{canClip = TRUE} and \code{canClip = FALSE}) has been
      improved to avoid producing unnecessary artifacts in clipped
      output.

      \item The \pkg{grid} package now allows \code{gpar(fill)} to be
      a \code{linearGradient()}, a \code{radialGradient()}, or a
      \code{pattern()}.  The \code{viewport(clip)} can now also be
      a grob, which defines a clipping path, and there is a new
      \code{viewport(mask)} that can also be a grob, which defines
      a mask.

      These new features are only supported so far on the Cairo-based
      graphics devices and on the \code{pdf()} device.

      \item (Not Windows.)
      A warning is given when a Cairo-based type is specified for a
      \code{png()}, \code{jpeg()}, \code{tiff()} or \code{bmp()}
      device but Cairo is unsupported (so \code{type = "Xlib"}
      is tried instead).

      \item \code{grSoftVersion()} now reports the versions of FreeType
      and FontConfig if they are used directly (not \emph{via} Pango),
      as is most commonly done on macOS.
    }
  }

  \subsection{C-LEVEL FACILITIES}{
    \itemize{
      \item The \emph{standalone} \file{libRmath} math library and \R's C
      API now provide \code{log1pexp()} again as documented, and gain
      \code{log1mexp()}.
    }
  }

  \subsection{INSTALLATION on a UNIX-ALIKE}{
    \itemize{
      \item \command{configure} checks for a program \command{pkgconf}
      if program \command{pkg-config} is not found.  These are now only
      looked for on the path (like almost all other programs) so if needed
      specify a full path to the command in \code{PKG_CONFIG}, for example
      in file \file{config.site}.

      \item C99 function \code{iswblank} is required -- it was last seen
      missing ca 2003 so the workaround has been removed.

      \item There are new \command{configure} options
      \option{--with-internal-iswxxxxx},
      \option{--with-internal-towlower} and
      \option{--with-internal-wcwidth} which allows the system functions
      for wide-character classification, case-switching and width
      (\code{wcwidth} and \code{wcswidth}) to be replaced by internal
      ones.  The first has long been used on macOS, AIX (and Windows)
      but this enables it to be unselected there and selected for other
      platforms (it is the new default on Solaris).  The second is new
      in this version of \R and is selected by default on macOS and
      Solaris.  The third has long been the default and remains so as it
      contains customizations for East Asian languages.

      System versions of these functions are often minimally implemented
      (sometimes only for ASCII characters) and may not cover the full
      range of Unicode points: for example Solaris (and Windows) only
      cover the Basic Multilingual Plane.

      \item Cairo installations without X11 are more likely to be
      detected by \command{configure}, when the file-based Cairo
      graphics devices will be available but not \code{X11(type =
      "cairo")}.

      \item There is a new \command{configure} option
      \option{--with-static-cairo} which is the default on macOS.  This
      should be used when only static cairo (and where relevant, Pango)
      libraries are available.

      \item Cairo-based graphics devices on platforms without Pango but
      with FreeType/FontConfig will make use of the latter for font selection.
    }
  }


  \subsection{LINK-TIME OPTIMIZATION on a UNIX-ALIKE}{
    \itemize{
      \item Configuring with flag \option{--enable-lto=R} now also uses
      LTO when installing the recommended packages.

      \item \command{R CMD INSTALL} and \command{R CMD SHLIB} have a new
      flag \option{--use-LTO} to use LTO when compiling code, for use
      with \R configured with \option{--enable-lto=R}.  For \R
      configured with \option{--enable-lto}, they have the new flag
      \option{--no-use-LTO}.

      Packages can opt in or out of LTO compilation \emph{via} a
      \samp{UseLTO} field in the \file{DESCRIPTION} file.  (As usual this
      can be overridden by the command-line flags.)
    }
  }

  \subsection{BUILDING R on Windows}{
    \itemize{
      \item for GCC \eqn{\ge}{>=} 8, \code{FC_LEN_T} is defined in
      \file{config.h} and hence character lengths are passed from C to
      Fortran in \emph{inter alia} BLAS and LAPACK calls.

      \item There is a new text file
      \file{src/gnuwin32/README.compilation}, which outlines how C/Fortran
      code compilation is organized and documents new features:
      \itemize{
	\item \R can be built with Link-Time Optimization with a
	suitable compiler -- doing so with GCC 9.2 showed several
	inconsistencies which have been corrected.

	\item There is support for cross-compiling the C and Fortran
	code in \R and standard packages on suitable (Linux) platforms.
	This is mainly intended to allow developers to test later
	versions of compilers -- for example using GCC 9.2 or 10.x has
	detected issues that GCC 8.3 in Rtools40 does not.

	\item There is experimental support for cross-building \R
	packages with C, C++ and/or Fortran code.
      }

      \item The R installer can now be optionally built to support a single
      architecture (only 64-bit or only 32-bit).
    }
  }

  \subsection{PACKAGE INSTALLATION}{
    \itemize{
      \item The default C++ standard has been changed to C++14 where
      available (which it is on all currently checked platforms): if not
      (as before) C++11 is used if available otherwise C++ is not
      supported.

      Packages which specify C++11 will still be installed using C++11.

      C++14 compilers may give deprecation warnings, most often for
      \code{std::random_shuffle} (deprecated in C++14 and removed in
      C++17).  Either specify C++11 (see \sQuote{Writing R Extensions})
      or modernize the code and if needed specify C++14.  The latter has
      been supported since \R 3.4.0 so the package's \file{DESCRIPTION}
      would need to include something like
\preformatted{
     Depends: R (>= 3.4)
}
    }
  }

  \subsection{PACKAGE INSTALLATION on Windows}{
    \itemize{
      \item \command{R CMD INSTALL} and \command{R CMD SHLIB} make use
      of their flag \option{--use-LTO} when the \samp{LTO_OPT} make
      macro is set in file \file{etc/$\{R_ARCH\}/Makeconf} or in a
      personal/site \file{Makevars} file.  (For details see
      \sQuote{Writing R Extensions} §4.5.)

      This provides a valuable check on code consistency.  It does work
      with GCC 8.3 as in Rtools40, but that does not detect everything
      the \acronym{CRAN} checks with current GCC do.
    }
  }

  \subsection{PACKAGE INSTALLATION on macOS}{
    \itemize{
      \item The default personal library directory on builds with
      \option{--enable-aqua} (including \acronym{CRAN} builds) now
      differs by CPU type, one of
\preformatted{
      ~/Library/R/x86_64/x.y/library
      ~/Library/R/arm64/x.y/library
}
      This uses the CPU type \R (and hence the packages) were built for,
      so when a \samp{x86_64} build of R is run under Rosetta emulation on
      an \samp{arm64} Mac, the first is used.
    }
  }

  \subsection{UTILITIES}{
    \itemize{
      \item \command{R CMD check} can now scan package functions for
      bogus \code{return} statements, which were possibly intended as
      \code{return()} calls (wish of \PR{17180}, patch by Sebastian
      Meyer). This check can be activated via the new environment
      variable \env{_R_CHECK_BOGUS_RETURN_}, true for \code{--as-cran}.

      \item \command{R CMD build} omits tarballs and binaries of
      previous builds from the top-level package directory.
      (\PR{17828}, patch by Sebastian Meyer.)

      \item \command{R CMD check} now runs sanity checks on the use of
      \samp{LazyData}, for example that a \file{data} directory is
      present and that \samp{LazyDataCompression} is not specified
      without \samp{LazyData} and has a documented value.  For packages
      with large LazyData databases without specifying
      \samp{LazyDataCompression}, there is a reference to the code given
      in \sQuote{Writing R Extensions} §1.1.6 to test the choice of
      compression (as in all the \acronym{CRAN} packages tested a
      non-default method was preferred).

      \item \command{R CMD build} removes \samp{LazyData} and
      \samp{LazyDataCompression} fields from the \file{DESCRIPTION} file
      of packages without a \file{data} directory.
    }
  }

  \subsection{ENCODING-RELATED CHANGES}{
    \itemize{
      \item The parser now treats \samp{\\Unnnnnnnn} escapes larger than
      the upper limit for Unicode points (\samp{\\U10FFFF}) as an error
      as they cannot be represented by valid UTF-8.

      Where such escapes are used for outputting non-printable
      (including unassigned) characters, 6 hex digits are used (rather
      than 8 with leading zeros).  For clarity, braces are used, for
      example \samp{\\U\{0effff\}}.

      \item The parser now looks for non-ASCII spaces on Solaris (as
      previously on most other OSes).

      \item There are warnings (including from the parser) on the use of
      unpaired surrogate Unicode points such as \samp{\\uD834}.  (These
      cannot be converted to valid UTF-8.)

      \item Functions \code{nchar()}, \code{tolower()}, \code{toupper()}
      and \code{chartr()} and those using regular expressions have more
      support for inputs with a marked Latin-1 encoding.

      \item The character-classification functions used (by default) to
      replace the system \code{iswxxxxx} functions on Windows, macOS and
      AIX have been updated to Unicode 13.0.0.

      The character-width tables have been updated to include new
      assignments in Unicode 13.0.0.  This included treating all control
      characters as having zero width.

      \item The code for evaluating default (extended) regular
      expressions now uses the same character-classification functions
      as the rest of \R (previously they differed on Windows, macOS and
      AIX).

      \item There is a build-time option to replace the system's
      wide-character \code{wctrans} C function by tables shipped with
      \R: use \command{configure} option
      \option{--with-internal-towlower} or (on Windows)
      \samp{-DUSE_RI18N_CASE} in \samp{CFLAGS} when building \R.  This
      may be needed to allow \code{tolower()} and \code{toupper()} to
      work with Unicode characters beyond the Basic Multilingual Plane
      where not supported by system functions (e.g. on Solaris where it
      is the new default).

      \item R is more careful when truncating UTF-8 and other multi-byte
      strings that are too long to be printed, passed to the system or
      libraries or placed into an internal buffer.  Truncation will no
      longer produce incomplete multibyte characters.
    }
  }

  \subsection{DEPRECATED AND DEFUNCT}{
    \itemize{
      \item Function \code{plclust()} from the package \pkg{stats} and
      \code{package.dependencies()}, \code{pkgDepends()},
      \code{getDepList()}, \code{installFoundDepends()}, and
      \code{vignetteDepends()} from package \pkg{tools} are defunct.

      \item Defunct functions \code{checkNEWS()} and \code{readNEWS()} from
      package \pkg{tools} and \code{CRAN.packages()} from \pkg{utils} have
      been removed.

      \item \command{R CMD config CXXCPP} is defunct (it was deprecated
      in \R{} 3.6.2).

      \item \code{parallel::detectCores()} drops support for Irix
      (retired in 2013).

      \item The \code{LINPACK} argument to \code{chol.default()},
      \code{chol2inv()}, \code{solve.default()} and \code{svd()} has
      been defunct since \R 3.1.0.  It was silently ignored up to \R
      4.0.3 but now gives an error.

      \item Subsetting/indexing, such as \code{ddd[*]} or \code{ddd$x}
      on a \code{DOTSXP} (dot-dot-dot) object \code{ddd} has been
      disabled; it worked by accident only and was undocumented.
    }
  }

  \subsection{BUG FIXES}{
    \itemize{
      \item Many more C-level allocations (mainly by \code{malloc} and
      \code{strdup}) are checked for success with suitable alternative
      actions.

      \item Bug fix for \code{replayPlot()};  this was turning off
      graphics engine display list recording if a recorded plot was
      replayed in the same session.  The impact of the bug became
      visible if resize the device after replay OR if attempted another
      \code{savePlot()} after replay (empty display list means empty
      screen on resize or empty saved plot).

      \item \command{R CMD check} etc now warn when a package exports
      non-existing S4 classes or methods, also in case of no
      \dQuote{methods} presence.  (Reported by Alex Bertram;
      reproducible example and patch by Sebastian Meyer in \PR{16662}.)

      \item \code{boxplot()} now also accepts \code{call}s for labels such
      as \code{ylab}, the same as \code{plot()}.  (Reported by Marius Hofert.)

      \item The help page for \code{xtabs()} now correctly states that
      \code{addNA} is setting \code{na.action = na.pass} among others.
      (Reported as \PR{17770} by Thomas Soeiro.)

      \item The \command{R CMD check <pkg>} gives a longer and more
      comprehensible message when \file{DESCRIPTION} misses dependencies,
      e.g., in \code{Imports:}.  (Thanks to the contributors of \PR{17179}.)

      \item \code{update.default()} now calls the generic \code{update()}
      on the formula to work correctly for models with extended formulas.
      (As reported and suggested by Neal Fultz in \PR{17865}.)

      \item The horizontal position of leaves in a dendrogram is now
      correct also with \code{center = FALSE}.  (\PR{14938}, patch from
      Sebastian Meyer.)

      \item \code{all.equal.POSIXt()} no longer warns about and
      subsequently ignores inconsistent \code{"tzone"} attributes, but
      describes the difference in its return value (\PR{17277}).
      This check can be disabled \emph{via} the new argument
      \code{check.tzone = FALSE} as suggested by Sebastian Meyer.

      \item \code{as.POSIXct()} now populates the \code{"tzone"}
      attribute from its \code{tz} argument when \code{x} is a logical
      vector consisting entirely of \code{NA} values.

      \item \code{x[[2^31]] <- v} now works.  (Thanks to the report and
      patch by Suharto Anggono in \PR{17330}.)

      \item In log-scale graphics, \code{axis()} ticks and label positions
      are now computed more carefully and symmetrically in their range,
      typically providing \emph{more} ticks, fulfilling wishes
      in \PR{17936}.  The change really corresponds to an improved
      \code{axisTicks()} (package \pkg{grDevices}), potentially influencing
      \pkg{grid} and \pkg{lattice}, for example.

      \item \code{qnorm(<very large negative>, log.p=TRUE)} is now correct
      to at least five digits where it was catastrophically wrong,
      previously.

      \item \code{sum(df)} and similar \code{"Summary"}- and
      \code{"Math"}-group member functions now work for data frames
      \code{df} with \code{\link{logical}} columns, notably also of zero
      rows.  (Reported to R-devel by Martin \dQuote{b706}.)

      \item \code{unsplit()} had trouble with tibbles due to unsound use of
      \code{rep(NA, len)}-indexing, which should use \code{NA_integer_}
      (Reported to R-devel by Mario Annau.)

      \item \code{pnorm(x, log.p = TRUE)} underflows to \code{-Inf} slightly
      later.

      \item \code{show(<hidden S4 generic>)} prints better and without
      quotes for non-hidden S4 generics.

      \item \code{read.table()} and relatives treated an "NA" column name as
      missing when \code{check.names = FALSE} \PR{18007}.

      \item Parsing strings containing UTF-16 surrogate pairs such as
      \code{"\uD834\uDD1E"} works better on some (uncommon) platforms.
      \code{sprintf("\%X", utf8ToInt("\uD834\uDD1E"))} should now give
      \code{"1D11E"} on all platforms.
      %% failed on Solaris

      \item \code{identical(x,y)} is no longer true for differing
      \code{DOTSXP} objects, fixing \PR{18032}.

      \item \code{str()} now works correctly for \code{DOTSXP} and related
      exotics, even when these are doomed.

      Additionally, it no longer fails for \code{list}s with a \code{class} and
      \dQuote{irregular} method definitions such that e.g. \code{lapply(*)} will
      necessarily fail, as currently for different \CRANpkg{igraph} objects.

      \item Message translation domains, e.g., for errors and warnings, are
      now correctly determined also when e.g., a \pkg{base} function is
      called from \dQuote{top-level} function (i.e., defined in \code{globalenv()}),
      thanks to a patch from Joris Goosen fixing \PR{17998}.

      \item Too long lines in environment files (e.g., \file{Renviron}) no
      longer crash \R. This limit has been increased to 100,000 bytes.
      (\PR{18001}.)

      \item There is a further workaround for FreeType giving
      incorrect italic font faces with cairo-based graphics devices on
      macOS.

      \item \code{add_datalist(*, force = TRUE)} (from package
      \pkg{tools}) now actually updates an existing \file{data/datalist}
      file for new content.  (Thanks to a report and patch by Sebastian
      Meyer in \PR{18048}.)

      \item \code{cut.Date()} and \code{cut.POSIXt()} could produce an
      empty last interval for \code{breaks = "months"} or \code{breaks =
      "years"}.  (Reported as \PR{18053} by Christopher Carbone.)

      \item Detection of the encoding of \sQuote{regular} macOS locales
      such as \samp{en_US} (which is UTF-8) had been broken by a macOS
      change: fortunately these are now rarely used with
      \samp{en_US.UTF-8} being preferred.

      \item \code{sub()} and \code{gsub(pattern, repl, x, *)} now keep
      attributes of \code{x} such as \code{names()} also when
      \code{pattern} is \code{NA} (\PR{18079}).

      \item Time differences (\code{"difftime"} objects) get a replacement
      and a \code{rep()} method to keep \code{"units"} consistent.
      (Thanks to a report and patch by Nicolas Bennett in \PR{18066}.)

      \item The \verb{\RdOpts} macro, setting defaults for \verb{\Sexpr}
      options in an Rd file, had been ineffective since \R 2.12.0: it
      now works again.
      (Thanks to a report and patch by Sebastian Meyer in \PR{18073}.)

      \item \code{mclapply} and \code{pvec} no longer accidentally terminate
      parallel processes started before by \code{mcparallel} or related
      calls in package \pkg{parallel} (\PR{18078}).

      \item \code{grep} and other functions for evaluating (extended)
      regular expressions handle in Unicode also strings not explicitly
      flagged UTF-8, but flagged native when running in UTF-8 locale.

      \item Fixed a crash in \code{fifo} implementation on Windows
      (\PR{18031}).

      \item Binary mode in \code{fifo} on Windows is now properly detected
      from argument \code{open} (\PR{15600}, \PR{18031}).
    }
  }
}

\section{\Rlogo CHANGES IN R 4.0.5}{
  \subsection{BUG FIXES}{
    \itemize{
      \item The change to the internal table in \R 4.0.4 for
      \code{iswprint} has been reverted: it contained some errors in
      printability of \sQuote{East Asian} characters.

      \item For packages using \samp{LazyData}, \command{R CMD build}
      ignored the \option{--resave-data} option and the
      \samp{BuildResaveData} field of the \file{DESCRIPTION} file (in \R
      versions 4.0.0 to 4.0.4).
    }
  }
}

\section{\Rlogo CHANGES IN R 4.0.4}{

  \subsection{NEW FEATURES}{
    \itemize{
      \item File \file{share/texmf/tex/latex/jss.cls} has been updated
      to work with LaTeX versions since Oct 2020.

      \item Unicode character width tables (as used by
      \code{nchar(, type = "w")}) have been updated to Unicode 12.1
      by Brodie Gaslam (\PR{17781}), including many emoji.

      \item The internal table for \code{iswprint} (used on Windows,
      macOS and AIX) has been updated to include many recent Unicode
      characters.
    }
  }

  \subsection{INSTALLATION on a UNIX-ALIKE}{
    \itemize{
      \item If an external BLAS is specified by \option{--with-blas=foo}
      or \emph{via} environment variable \env{BLAS_LIBS} is not found,
      this is now a configuration error.  The previous behaviour was not
      clear from the documentation: it was to continue the search as if
      \option{--with-blas=yes} was specified.
    }
  }

  \subsection{BUG FIXES}{
    \itemize{
      \item \code{all.equal(x,y)} now \dQuote{sees} the two different
      \code{NA}s in factors, thanks to Bill Dunlap and others in
      \PR{17897}.

      \item \code{(~ NULL)[1]} and similar formula subsetting now works,
      thanks to a report and patch by Henrik Bengtsson in \PR{17935}.
      Additionally, subsetting leaving an empty formula now works too,
      thanks to suggestions by Suharto Anggono.

      \item \code{.traceback(n)} keeps source references again, as before
      \R 4.0.0, fixing a regression; introduced by the \PR{17580}, reported
      including two patch proposals by Brodie Gaslam.

      \item \code{unlist(plst, recursive=FALSE)} no longer drops content
      for pairlists with list components, thanks to the report and patch
      by Suharto Anggono in \PR{17950}.

      \item \code{iconvlist()} now also works on MUSL based (Linux)
      systems, from a report and patch suggestion by Wesley Chan in
      \PR{17970}.

      \item \code{round()} and \code{signif()} no longer tolerate wrong
      argument names, notably in 1-argument calls; reported by Shane
      Mueller on R-devel (mailing list); later reported as \PR{17976}.

      \item \code{.Machine} has \code{longdouble.*} elements only if
      \code{capabilities("long.double")} is true, as documented.
      (Previously they were included if the platform had \code{long
	double} identical to \code{double}, as ARM does.)

      \item \code{p.adjust(numeric(), n=0)} now works, fixing \PR{18002}.

      \item \code{identical(x,y)} no longer prints "Unknown Type .." for
      \code{typeof(x) == "..."} objects.

      \item Fix (auto-)\code{print()}ing of named complex vectors, see
      \PR{17868} and \PR{18019}.

      \item \code{all.equal(<language>, <...>)} now works, fixing \PR{18029}.

      \item \code{as.data.frame.list(L, row.names=NULL)} now behaves in line
      with \code{data.frame()}, disregarding names of components of
      \code{L}, fixing \PR{18034}, reported by Kevin Tappe.

      \item \code{checkRdaFiles(ff)$version} is now correct also when
      \code{ff} contains files of different versions, thanks to a report
      and patch from Sebastian Meyer in \PR{18041}.

      \item macOS: Quartz device live drawing could fail (no plot is shown)
      if the system changes the drawing context after view update (often
      the case since macOS Big Sur). System log may show
      "CGContextDelegateCreateForContext: invalid context" error.
    }
  }
}

\section{\Rlogo CHANGES IN R 4.0.3}{

  \subsection{NEW FEATURES}{
    \itemize{
      \item On platforms using \command{configure} option
      \option{--with-internal-tzcode}, additional values
      \code{"internal"} and (on macOS only) \code{"macOS"} are accepted
      for the environment variable \env{TZDIR}.  (See \code{?TZDIR}.)

      On macOS, \code{"macOS"} is used by default if the system timezone
      database is a newer version than that in the \R installation.

      \item When \code{install.packages(type = "source")} fails to find
       a package in a repository it mentions package versions which are
       excluded by their \R version requirement and links to hints on
       why a package might not be found.

       \item The default value for \code{options("timeout")} can be set
       from environment variable \env{R_DEFAULT_INTERNET_TIMEOUT}, still
       defaulting to 60 (seconds) if that is not set or invalid.

       This may be needed when child \R processes are doing downloads,
       for example during the installation of source packages which
       download jars or other forms of data.
     }
  }

  \subsection{LINK-TIME OPTIMIZATION on a UNIX-ALIKE}{
    \itemize{
      \item There is now support for parallelized Link-Time Optimization
      (LTO) with GCC and for \sQuote{thin} LTO with
      \command{clang} \emph{via} setting the \samp{LTO} macro.

      \item There is support for setting a different LTO flag for the
      Fortran compiler, including to empty when mixing \command{clang}
      and \command{gfortran} (as on macOS).  See file \file{config.site}.

      \item There is a new \samp{LTO_LD} macro to set linker options for
      LTO compilation, for example to select an alternative linker or
      to parallelize thin LTO.
    }
  }

    \subsection{DEPRECATED AND DEFUNCT}{
    \itemize{

      \item The \code{LINPACK} argument to \code{chol.default()},
      \code{chol2inv()}, \code{solve.default()} and \code{svd()} has been
      defunct since \R 3.1.0.  Using it now gives a warning which will
      become an error in \R 4.1.0.
    }
  }

  \subsection{BUG FIXES}{
    \itemize{
      \item The code mitigating stack overflow with PCRE regexps on very
      long strings is enabled for PCRE2 < 10.30 also when JIT is enabled,
      since stack overflows have been seen in that case.
      %% Ubuntu 16.04LTS has PCRE2 10.21

      \item Fix to correctly show the group labels in \code{dotchart()}
      (which where lost in the \code{ylab} improvement for \R 4.0.0).

      \item \code{addmargins(*, ..)} now also works when \code{fn()} is a
      local function, thanks to bug report and patch \PR{17124} from Alex
      Bertram.

      \item \code{rank(x)} and hence \code{sort(x)} now work when \code{x}
      is an object (as per \code{is.object(x)}) of type \code{"raw"}
      \emph{and} provides a valid \code{`[`} method, e.g., for
      \code{gmp::as.bigz(.)} numbers.

      \item \code{chisq.test(*, simulate.p.value=TRUE)} and
      \code{r2dtable()} now work correctly for large table entries (in the
      millions).  Reported by Sebastian Meyer and investigated by more
      helpers in \PR{16184}.

      \item Low-level socket read/write operations have been fixed to
      correctly signal communication errors. Previously, such errors could
      lead to a segfault due to invalid memory access. Reported
      and debugged by Dmitriy Selivanov in \PR{17850}.

      \item \code{quantile(x, pr)} works more consistently for \code{pr}
      values slightly outside [0,1], thanks to Suharto Anggono's \PR{17891}.

      Further, \code{quantile(x, prN, names=FALSE)} now works even when
      \code{prN} contains \code{NA}s, thanks to Anggono's \PR{17892}.
      Ditto for ordered factors or \code{Date} objects
      when \code{type = 1} or \code{3}, thanks to \PR{17899}.

      \item Libcurl-based internet access, including
      \code{curlGetHeaders()}, was not respecting the \code{"timeout"}
      option.  If this causes unanticipated timeouts, consider
      increasing the default by setting \env{R_DEFAULT_INTERNET_TIMEOUT}.

      \item \code{as.Date(<char>)} now also works with an initial
      \code{""}, thanks to Michael Chirico's \PR{17909}.

      \item \code{isS3stdGeneric(f)} now detects an S3 generic also when
      it is \code{trace()}d, thanks to Gabe Becker's \PR{17917}.

      \item \code{R_allocLD()} has been fixed to return memory aligned for
      long double type \PR{16534}.

      \item \code{fisher.test()} no longer segfaults when called again after
      its internal stack has been exceeded \PR{17904}.

      \item Accessing a long vector represented by a compact integer
      sequence no longer segfaults (reported and debugged by Hugh
      Parsonage).

      \item \code{duplicated()} now works also for strings with multiple
      encodings inside a single vector \PR{17809}.

      \item \code{phyper(11, 15, 0, 12, log.p=TRUE)} no longer gives
      \code{NaN}; reported as \PR{17271} by Alexey Stukalov.

      \item Fix incorrect calculation in \code{logLik.nls()} \PR{16100}, patch
      from Sebastian Meyer.

      \item A very old bug could cause a segfault in \code{model.matrix()}
      when terms involved logical variables.  Part of \PR{17879}.

      \item \code{model.frame.default()} allowed \code{data = 1}, leading to
      involuntary variable capture (rest of \PR{17879}).

      \item \code{tar()} no longer skips non-directory files, thanks to a
      patch by Sebastian Meyer, fixing the remaining part of \PR{16716}.
    }
  }
}


\section{\Rlogo CHANGES IN R 4.0.2}{

  \subsection{UTILITIES}{
    \itemize{
      \item \command{R CMD check} skips vignette re-building (with a
      warning) if the \samp{VignetteBuilder} package(s) are not available.
    }
  }

  \subsection{BUG FIXES}{
    \itemize{
      \item Paths with non-ASCII characters caused problems for
      package loading on Windows \PR{17833}.

      \item Using \pkg{tcltk} widgets no longer crashes R on
      Windows.

      \item \code{source(*, echo=TRUE)} no longer fails in some cases with
      empty lines; reported by Bill Dunlap in \PR{17769}.

      \item \code{on.exit()} now correctly matches named arguments, thanks
      to \PR{17815} (including patch) by Brodie Gaslam.

      \item \code{regexpr(*, perl=TRUE)} no longer returns incorrect
      positions into text containing characters outside of the Unicode
      Basic Multilingual Plane on Windows.
    }
  }
}

\section{\Rlogo CHANGES IN R 4.0.1}{

  \subsection{NEW FEATURES}{
    \itemize{
      \item \code{paste()} and \code{paste0()} gain a new optional
      argument \code{recycle0}.  When set to true, zero-length
      arguments are recycled leading to \code{character(0)} after the
      \code{sep}-concatenation, i.e., to the empty string \code{""} if
      \code{collapse} is a string and to the zero-length value
      \code{character(0)} when \code{collapse = NULL}.

      A package whose code uses this should depend on \samp{R (>= 4.0.1)}.

      \item The \code{summary(<warnings>)} method now maps the counts
      correctly to the warning messages.
    }
  }

  \subsection{BUG FIXES}{
    \itemize{
      \item \code{aov(frml, ...)} now also works where the \code{formula}
      deparses to more than 500 characters, thanks to a report and patch
      proposal by Jan Hauffa.

      \item Fix a dozen places (code, examples) as \code{Sys.setlocale()}
      returns the new rather than the previous setting.

      \item Fix for adding two complex \pkg{grid} units via \code{sum()}.
      Thanks to Gu Zuguang for the report and Thomas Lin Pedersen for
      the patch.

      \item Fix \code{parallel::mclapply(..., mc.preschedule=FALSE)}
      to handle raw vector results correctly. \PR{17779}

      \item Computing the \code{base} value, i.e., 2, \dQuote{everywhere},
      now uses \code{FLT_RADIX}, as the original \file{machar} code looped
      indefinitely on the ppc64 architecture for the \code{longdouble} case.

      \item In \R 4.0.0, \code{sort.list(x)} when \code{is.object(x)} was
      true, e.g., for \code{x <- I(letters)}, was accidentally using
      \code{method = "radix"}.  Consequently, e.g., \code{merge(<data.frame>)}
      was much slower than previously; reported in \PR{17794}.

      \item \code{plot(y ~ x, ylab = quote(y[i]))} now works, as e.g., for
      \code{xlab}; related to \PR{10525}.

      \item \code{parallel::detect.cores(all.tests = TRUE)} tries a
      matching OS name before the other tests (which were intended only
      for unknown OSes).

      \item Parse data for raw strings is now recorded correctly. Reported
      by Gabor Csardi.
    }
  }
}

\section{\Rlogo CHANGES IN R 4.0.0}{

  \subsection{SIGNIFICANT USER-VISIBLE CHANGES}{
    \itemize{
      \item Packages need to be (re-)installed under this version
      (4.0.0) of \R.

      \item \code{matrix} objects now also inherit from class
      \code{"array"}, so e.g., \code{class(diag(1))} is \code{c("matrix",
	"array")}.  This invalidates code incorrectly assuming that
      \code{class(matrix_obj))} has length one.

      S3 methods for class \code{"array"} are now dispatched for
      \code{matrix} objects.

      \item There is a new syntax for specifying \emph{raw} character
      constants similar to the one used in C++: \code{r"(...)"} with
      \code{...} any character sequence not containing the sequence
      \samp{)"}. %}) \samp{*}: Rd-parser bug for \code{*}"
      This makes it easier to write strings that contain backslashes or
      both single and double quotes.  For more details see \code{?Quotes}.

      \item \R now uses a \samp{stringsAsFactors = FALSE} default, and
      hence by default no longer converts strings to factors in calls
      to \code{data.frame()} and \code{read.table()}.

      A large number of packages relied on the previous behaviour and
      so have needed/will need updating.

      \item The \code{plot()} S3 generic function is now in package
      \pkg{base} rather than package \pkg{graphics}, as it is
      reasonable to have methods that do not use the \pkg{graphics}
      package.  The generic is currently re-exported from the
      \pkg{graphics} namespace to allow packages importing it from
      there to continue working, but this may change in future.

      Packages which define S4 generics for \code{plot()} should be
      re-installed and package code using such generics from other
      packages needs to ensure that they are imported rather than rely
      on their being looked for on the search path (as in a namespace,
      the base namespace has precedence over the search path).
    }
  }

  \subsection{REFERENCE COUNTING}{
    \itemize{
       \item Reference counting is now used instead of the \code{NAMED}
       mechanism for determining when objects can be safely mutated in
       base C code.  This reduces the need for copying in some cases and
       should allow further optimizations in the future.  It should
       help make the internal code easier to maintain.

       This change is expected to have almost no impact on packages
       using supported coding practices in their C/C++ code.
     }
   }

  \subsection{MIGRATION TO PCRE2}{
    \itemize{
       \item This version of \R is built against the PCRE2 library for
       Perl-like regular expressions, if available.  (On non-Windows
       platforms PCRE1 can optionally be used if PCRE2 is not available
       at build time.)  The version of PCRE in use can be obtained
       \emph{via} \code{extSoftVersion()}: PCRE1 (formerly known as
       \sQuote{PCRE}) has versions <= 8, PCRE2 versions >= 10.

       \item Making PCRE2 available when building \R from source is
       strongly recommended (preferably version 10.30 or later) as PCRE1
       is no longer developed: version 8.44 is
       \sQuote{likely to be the final release}.
       % https://lists.exim.org/lurker/message/20200212.174850.6edd8277.en.html

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
      \code{traceback()}.  This fulfils the wish of \PR{17580}, reported
      including two patch proposals by Brodie Gaslam.

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

      \item There is a new help document accessed by
      \code{help("file path encoding")} detailing how file paths with
      marked encodings are handled.

      \item New function \code{list2DF()} for creating data frames from
      lists of variables.

      \item \code{iconv()} has a new option \code{sub = "Unicode"} to
      translate UTF-8 input invalid in the \samp{to} encoding using
      \samp{<U+xxxx>} escapes.

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

      \item For \code{head(x, n)} and \code{tail()} the default and
      other S3 methods notably for \emph{vector} \code{n}, e.g. to get a
      \dQuote{corner} of a matrix, has been extended to \code{array}'s
      of higher dimension thanks to the patch proposal by Gabe Becker in
      \PR{17652}.  Consequently, optional argument \code{addrownums} is
      deprecated and replaced by the (more general) argument
      \code{keepnums}.  An invalid second argument \code{n} now leads
      to typically more easily readable error messages.

      \item New function \code{.class2()} provides the full character
      vector of class names used for S3 method dispatch.

      \item Printing \code{methods(..)} now uses a new \code{format()} method.

      \item \code{sort.list(x)} now works for non-atomic objects
      \code{x} and \code{method = "auto"} (the default) or
      \code{"radix"} in cases \code{order(x)} works, typically via a
      \code{xtfrm()} method.

      \item Where they are available, \code{writeBin()} allows
      long vectors.% (those of length \eqn{\ge 2^{31}}{>= 2^31}).

      \item New function \code{deparse1()} produces one string, wrapping
      \code{deparse()}, to be used typically in
      \code{deparse1(substitute(*))}, e.g., to fix \PR{17671}.

      \item \code{wilcox.test()} enhancements: In the (non-paired)
      two-sample case, \code{Inf} values are treated as very large for
      robustness consistency.  If exact computations are used, the
      result now has \code{"exact"} in the \code{method} element of its
      return value.  New arguments \code{tol.root} and
      \code{digits.rank} where the latter may be used for stability to
      treat very close numbers as ties.

      \item \code{readBin()} and \code{writeBin()} now report an error
      for an invalid \code{endian} value.  The affected code needs to be
      fixed with care as the old undocumented behavior was to swap
      endian-ness in such cases.

      \item \code{sequence()} is now an S3 generic with an internally
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

      \item  \code{unlink()} gains a new argument \code{expand} to disable
      wildcard and tilde expansion.  Elements of \code{x} of value
      \code{"~"} are now ignored.

      \item \code{mle()} in the \code{stats4} package has had its
      interface extended so that arguments to the negative
      log-likelihood function can be one or more vectors, with similar
      conventions applying to bounds, start values, and parameter values
      to be kept fixed.  This required a minor extension to class
      \code{"mle"}, so saved objects from earlier versions may need to
      be recomputed.

      \item The default for \code{pdf()} is now
      \code{useDingbats = FALSE}.

      \item The default fill colour for \code{hist()} and
      \code{boxplot()} is now \code{col = "lightgray"}.

      \item The default order of the levels on the y-axis for
      \code{spineplot()} and \code{cdplot()} has been reversed.

      \item If the \env{R_ALWAYS_INSTALL_TESTS} environment variable is
      set to a true value, \command{R CMD INSTALL} behaves as if
      the \option{--install-tests} option is always specified. Thanks to
      Reinhold Koch for the suggestion.

      \item New function \code{R_user_dir()} in package \pkg{tools}
      suggests paths appropriate for storing \R-related user-specific
      data, configuration and cache files.

      \item \code{capabilities()} gains a new logical option \code{Xchk}
      to avoid warnings about X11-related capabilities.

      \item The internal implementation of \pkg{grid} units has changed,
      but the only visible effects at user-level should be
      \itemize{
	\item a slightly different print format for some units
	(especially unit arithmetic),
	\item faster performance (for unit operations) and
	\item two new functions \code{unitType()} and \code{unit.psum()}.
      }
      Based on code contributed by Thomas Lin Pedersen.

      \item When internal dispatch for \code{rep.int()}
      and \code{rep_len()} fails, there is an attempt to dispatch on the
      equivalent call to \code{rep()}.

      \item Object \code{.Machine} now contains new \code{longdouble.*}
      entries (when \R uses long doubles internally).

      \item \code{news()} has been enhanced to cover the news on \R 3.x
      and 2.x.

       \item For consistency, \code{N <- NULL; N[[1]] <- val} now turns
       \code{N} into a \code{list} also when \code{val)} has length one.
       This enables \code{dimnames(r1)[[1]] <- "R1"} for a 1-row matrix
       \code{r1}, fixing \PR{17719} reported by Serguei Sokol.

       \item \code{deparse(..)}, \code{dump(..)}, and \code{dput(x,
	 control = "all")} now include control option \code{"digits17"}
       which typically ensures 1:1 invertibility.  New option
       \code{control = "exact"} ensures numeric exact invertibility
       via \code{"hexNumeric"}.

      \item When loading data sets via \code{read.table()},
       \code{data()} now uses \samp{LC_COLLATE=C} to ensure
       locale-independent results for possible string-to-factor
       conversions.

      \item A server socket connection, a new connection type
      representing a listening server socket, is created via
      \code{serverSocket()} and can accept multiple socket connections
      via \code{socketAccept()}.

      \item New function \code{socketTimeout()} changes the connection
      timeout of a socket connection.

      \item The time needed to start a homogeneous \samp{PSOCK} cluster on
      \samp{localhost} with many nodes has been significantly reduced
      (package \pkg{parallel}).

      \item New \code{globalCallingHandlers()} function to establish
      global condition handlers.  This allows registering default
      handlers for specific condition classes. Developed in
      collaboration with Lionel Henry.

      \item New function \code{tryInvokeRestart()} to invoke a specified
      restart if one is available and return without signaling an error
      if no such restart is found.  Contributed by Lionel Henry in
      \PR{17598}.

      \item \code{str(x)} now shows the length of \code{attributes} in some
      cases for a data frame \code{x}.

      \item \code{Rprof()} gains a new argument \code{filter.callframes}
      to request that intervening call frames due to lazy evaluation or
      explicit \code{eval()} calls be omitted from the recorded profile
      data.  Contributed by Lionel Henry in \PR{17595}.

      \item The handling of \code{$\{FOO-bar\}} and \code{$\{FOO:-bar\}}
      in \file{Renviron} files now follows POSIX shells (at least on a
      Unix-alike), so the first treats empty environment variables as
      set and the second does not.  Previously both ignored empty
      variables.  There are several uses of the first form in
      \file{etc/Renviron}.

      \item New \code{classes} argument for \code{suppressWarnings()}
      and \code{suppressMessages()} to selectively suppress only
      warnings or messages that inherit from particular classes.
      Based on patch from Lionel Henry submitted with \PR{17619}.

      \item New function \code{activeBindingFunction()} retrieves the
      function of an active binding.

      \item New \code{"cairoFT"} and \code{"pango"} components in the
      output of \code{grSoftVersion()}.

      \item New argument \code{symbolfamily} in cairo-based graphics
      devices and new function \code{cairoSymbolFont()} that can be used
      to provide the value for that argument.
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
      part of the name when opening \emph{via} Windows file associations
      (reported by Christian Asseburg).

      \item \command{Rterm} now accepts \code{ALT+xxx} sequences also
      with NumLock on.  Tilde can be pasted with an Italian keyboard
      (\PR{17679}).

      \item \R falls back to copying when junction creation fails during
      package checking (patch from Duncan Murdoch).
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

      \item Either PCRE2 or PCRE1 >= 8.32 (Nov 2012) is required: the
      deprecated provision for 8.20--8.31 has been removed.

      \item Defunct functions \code{mem.limits()}, \code{.readRDS()},
      \code{.saveRDS()}, \code{.find.package()}, and \code{.path.package()}
      from package \pkg{base}
      and \code{allGenerics()}, \code{getAccess()}, \code{getAllMethods()},
      \code{getClassName()}, \code{getClassPackage()}, \code{getExtends()},
      \code{getProperties()}, \code{getPrototype()},
      \code{getSubclasses()}, \code{getVirtual()}, \code{mlistMetaName()},
      \code{removeMethodsObject()}, \code{seemsS4Object()},
      \code{traceOff()}, and \code{traceOn()} from \pkg{methods} have been
      removed.
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

      \item \code{R_withCallingErrorHandler} is now available for
      establishing a calling handler in C code for conditions inheriting
      from class \code{error}.
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

      \item New make target \command{distcheck} checks
      \itemize{
	\item \R can be rebuilt from the tarball created by
	\command{make dist},
	\item the build from the tarball passes \command{make check-all},
	\item the build installs and uninstalls,
	\item the source files are properly cleaned by \command{make distclean}.
      }
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

      \item \command{R CMD check --as-cran} now runs \code{\\donttest}
      examples (which are run by \code{example()}) instead of
      instructing the tester to do so.  This can be temporarily
      circumvented during development by setting environment variable
      \env{_R_CHECK_DONTTEST_EXAMPLES_} to a false value.
    }
  }

  \subsection{PACKAGE INSTALLATION}{
    \itemize{
      \item There is the beginnings of support for the recently approved
      C++20 standard, specified analogously to C++14 and C++17.  There is
      currently only limited support for this in compilers, with flags
      such as \option{-std=c++20} and \option{-std=c++2a}.  For the time
      being the \command{configure} test is of accepting one of these
      flags and compiling C++17 code.
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

      \item \code{postscript(.., title = *)} now signals an error when
      the title string contains a character which would produce corrupt
      PostScript, thanks to \PR{17607} by Daisuko Ogawa.

      \item Certain \code{Ops} (notably comparison such as \code{==}) now
      also work for 0-length data frames, after reports by Hilmar Berger.

      \item \code{methods(class = class(glm(..)))} now warns more usefully
      and only once.

      \item \code{write.dcf()} no longer mangles field names (\PR{17589}).

      \item Primitive replacement functions no longer mutate a
      referenced first argument when used outside of a complex
      assignment context.

      \item  A better error message for \code{contour(*, levels = Inf)}.

      \item The return value of \code{contourLines()} is no longer
      \code{invisible()}.

      \item The Fortran code for calculating the \code{coefficients}
      component in \code{lm.influence()} was very inefficient. It has
      (for now) been replaced with much faster \R{} code (\PR{17624}).

      \item \code{cm.colors(n)} \emph{etc} no longer append the code for
      \code{alpha = 1}, \code{"FF"}, to all colors.  Hence all eight
      \code{*.colors()} functions and \code{rainbow()} behave
      consistently and have the same non-explicit default (\PR{17659}).

      \item \code{dnorm} had a problematic corner case with \code{sd ==
      -Inf} or negative \code{sd} which was not flagged as an error in
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

      \item \code{choose()} misbehaved in corner cases where it switched
      \code{n - k} for \code{k} and \code{n} was only \emph{nearly} integer
      (report from Erik Scott Wright).

      \item \code{mle()} in the \code{stats4} package had problems
      combining use of box constraints and fixed starting values (in
      particular, confidence intervals were affected).

      \item Operator \code{?} now has lower precedence than \code{=} to
      work as documented, so \code{=} behaves like \code{<-} in help
      expressions (\PR{16710}).

      \item \code{smoothEnds(x)} now returns \code{integer} type in
      \emph{both} cases when \code{x} is \code{integer}, thanks to a
      report and proposal by Bill Dunlap \PR{17693}.

      \item The \pkg{methods} package does a better job of tracking
      inheritance relationships across packages.

      \item \code{norm(diag(c(1, NA)), "2")} now works.

      \item \code{subset()} had problems with 0-col dataframes (reported
      by Bill Dunlap, \PR{17721}).

      \item Several cases of integer overflow detected by the
      \sQuote{undefined behaviour sanitizer} of \command{clang} 10 have
      been circumvented.  One in \code{rhyper()} may change the
      generated value for large input values.

      \item \code{dotchart()} now places the y-axis label (\code{ylab})
      much better, not overplotting labels, thanks to a report and
      suggestion by Alexey Shipunov.

      %% detected by clang-ASAN in examples/tests of adehabitatHR.
      \item A rare C-level array overflow in \code{chull()} has been
      worked around.

      \item Some invalid specifications of the day-of-the-year
      (\emph{via} \code{\%j}, e.g.\sspace{}day 366 in 2017) or week
      plus day-of-the-week are now detected by \code{strptime()}.
      They now return \code{NA} but give a warning as they may have
      given random results or corrupted memory in earlier versions of \R.

      \item \code{socketConnection(server = FALSE)} now respects the
      connection timeout also on Linux.

      \item \code{socketConnection(server = FALSE)} no longer leaks a
      connection that is available right away without waiting (e.g. on
      \samp{localhost}).

      \item Socket connections are now robust against spurious readability
      and spurious availability of an incoming connection.

      \item \code{blocking = FALSE} is now respected also on the server side
      of a socket connection, allowing non-blocking read operations.

      \item \code{anova.glm()} and \code{anova.glmlist()} computed
      incorrect score (Rao) tests in no-intercept cases. (André
      Gillibert, \PR{17735})

      \item \code{summaryRprof()} now should work correctly for the
      \code{Rprof(*, memory.profiling=TRUE)} case with small chunk size (and
      \code{"tseries"} or similar) thanks to a patch proposal by Benjamin
      Tyner, in \PR{15886}.

      \item \code{xgettext()} ignores strings passed to
      \code{ngettext()}, since the latter is handled by
      \code{xngettext()}. Thanks to Daniele Medri for the report and all
      the recent work he has done on the Italian translations.

      \item \code{data(package = "P")} for \code{P} in \pkg{base} and
      \pkg{stats} no longer reports the data sets from package
      \pkg{datasets} (which it did for back compatibility for 16 years),
      fixing \PR{17730}.

      \item \code{x[[Inf]]} (returning \code{NULL}) no longer leads to
      undefined behavior, thanks to a report by Kirill Müller in
      \PR{17756}.  Further, \code{x[[-Inf]]} and \code{x[[-n]]} now give
      more helpful error messages.

      \item \code{Gamma()} family sometimes had trouble storing
      link name \PR{15891}
    }
  }

  \subsection{BUG FIXES (Windows)}{
    \itemize{
      \item \code{Sys.glob()} now supports all characters
      from the Unicode Basic Multilingual Plane, no longer corrupting
      some (less commonly used) characters (\PR{17638}).

      \item \command{Rterm} now correctly displays
      multi-byte-coded characters representable in the current native
      encoding (at least on Windows 10 they were sometimes omitted,
      \PR{17632}).

      \item \code{scan()} issues with UTF-8 data when running in a DBCS
      locale have been resolved (\PR{16520}, \PR{16584}).

      \item \command{Rterm} now accepts enhanced/arrow keys also with
      ConPTY.

      \item \R can now be started \emph{via} the launcher icon in a
      user documents directory whose path is not representable in the
      system encoding.

      \item \code{socketConnection(server = FALSE)} now returns instantly
      also on Windows when connection failure is signalled.

      \item Problems with UTF-16 surrogate pairs have been fixed in
      several functions, including \command{tolower()} and
      \command{toupper()} (\PR{17645}).
    }
  }
}

\section{CHANGES in previous versions}{
  \itemize{
    \item Older news can be found in text format in files
    \ifelse{html}{\href{../NEWS.0}{NEWS.0}, \href{../NEWS.1}{NEWS.1},
      \href{../NEWS.2}{NEWS.2} and
      \href{../NEWS.3}{NEWS.3}}{\file{NEWS.0}, \file{NEWS.1},
      \file{NEWS.2} and \file{NEWS.3}}
    in the \file{doc} directory.  News in HTML format for
    \R  versions 3.x and from 2.10.0 to 2.15.3 are available at
    \ifelse{html}{\url{NEWS.3.html}}{\file{doc/html/NEWS.3.html}}
    and
    \ifelse{html}{\url{NEWS.2.html}}{\file{doc/html/NEWS.2.html}}.
  }
}

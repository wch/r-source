% -*- coding: utf-8 -*-
\newcommand{\Rlogo}{\if{html}{\figure{../../html/Rlogo.svg}{options: class="toplogo" alt="[R logo]"}}\if{latex}{\figure{Rlogo.pdf}{options: width=0.5in}}}

\name{NEWS}
\title{R News}
\encoding{UTF-8}

\section{\Rlogo CHANGES IN R 3.6.1 patched}{

  \subsection{NEW FEATURES}{
    \itemize{
      \item \code{runmed(x, *)} gets new option \code{na.action}
      determining \emph{how} to handle \code{NaN} or \code{NA} in \code{x}.
    }
  }

  \subsection{INSTALLATION on a UNIX-ALIKE}{
    \itemize{
      \item Detection of the C stack direction has been moved from
      run-time to \command{configure}: this is safer with LTO builds and
      allows the detection to be overridden -- see file
      \file{config.site}.
    }
  }

  \subsection{C-LEVEL FACILITIES}{
    \itemize{
      \item \code{installTrChar} (which is nowadays is wrapped by
      \code{installChar}) is defined in \file{Rinternals.h}.  (Neither
      are part of the API.)
    }
  }

  \subsection{PACKAGE INSTALLATION}{
    \itemize{
      \item Header \file{Rconfig.h} contains the value of
      \code{FC_LEN_T} deduced at installation which is used by the
      prototypes in headers \file{R_ext/BLAS.h} and
      \file{R_ext/Lapack.h} but to avoid extensive breakage this is
      only exposed when \code{USE_FC_LEN_T} is defined.

      If a package's C/C++ calls to BLAS/LAPACK allow for the
      \sQuote{hidden} arguments used by most Fortran compilers to pass
      the lengths of Fortran character arguments, define
      \code{USE_FC_LEN_T} and include \file{Rconfig.h} (possibly
      \emph{via} \file{R.h}) before including \file{R_ext/BLAS.h} or
      \file{R_ext/Lapack.h}.
    }
  }

  \subsection{UTILITIES}{
    \itemize{
      \item \command{R CMD check} has a new option to mitigate checks
      leaving files/directories in \file{/tmp}.  See the \sQuote{R
	Internals} manual -- this is part of \code{--as-cran}.
    }
  }

  \subsection{DEPRECATED AND DEFUNCT}{
    \itemize{
      \item Support for specifying C++98 in package installation is
      deprecated.
 
      \item Support in \command{R CMD config} for \samp{F77},
      \samp{FCPIFCPLAGS}, \samp{CPP}, \samp{CXXCPP} and \samp{CXX98} and
      similar is deprecated.  (\samp{CPP} is found from the system
      \command{make} and may well not be set.)
    }
  }

  \subsection{BUG FIXES}{
    \itemize{
      \item \code{runmed(x, *)} when \code{x} contains missing values
      now works consistently for both \code{algorithm="Stuetzle"} and
      \code{"Turlach"}, and no longer segfaults for \code{"Turlach"}, as
      reported by Hilmar Berger.

      \item \code{apply(diag(3), 2:3, mean)} now gives a helpful error message.

      \item \code{dgamma(x, shape, log=TRUE)} now longer overflows
      to \code{Inf} for shape < 1 and very small \code{x}, fixing
      \PR{17577}, reported by Jonathan Rougier.

      \item Buffer overflow in building error messages fixed. Reported
      by Benjamin Tremblay.

      \item \code{options(str = .)} is correctly initialized at
      package \pkg{utils} load time, now.  A consequence is that
      \code{str()} in scripts now is more consistent to interactive use,
      e.g., when displaying function(**) argument lists.

      \item \code{as.numeric(<call>)} now gives correct error message.

      \item Printing \code{ls.str()} no longer wrongly shows
      \code{"<missing>"} in rare cases.

      \item Auto-printing S4 objects no longer duplicates the object,
      for faster speed and reduced memory consumption. Reported by Aaron
      Lun.
    }
  }
}

\section{\Rlogo CHANGES IN R 3.6.1}{

  \subsection{INSTALLATION on a UNIX-ALIKE}{
    \itemize{
      \item The default detection of the shell variable \code{libNN} is
      overridden for derivatives of Debian Linux, some of which have
      started to have a \file{/usr/lib64} directory.
      (E.g.\sspace{}Ubuntu 19.04.)  As before, it can be specified in
      \file{config.site}.
    }
  }

  \subsection{UTILITIES}{
    \itemize{
      \item \command{R CMD config} knows the values of \code{AR} and
      \code{RANLIB}, often set for LTO builds.
    }
  }

  \subsection{DEPRECATED AND DEFUNCT}{
    \itemize{
      \item The use of a character vector with \code{.Fortran()} is
      formally deprecated and gives a non-portability warning.  (It has
      long been strongly discouraged in \sQuote{Writing R Extensions}.)
    }
  }

  \subsection{BUG FIXES}{
    \itemize{
      \item On Windows, GUI package installation via \code{menuInstallPkgs()}
      works again, thanks to Len Weil's and Duncan Murdoch's \PR{17556}.

      \item \command{R CMD check} on \code{data()} fixing \PR{17558}
      thanks to Duncan Murdoch.

      \item \code{quasi(*, variance = list(..))} now works more
      efficiently, and should work in all cases fixing \PR{17560}.
      Further, \code{quasi(var = mu(1-mu))} and \code{quasi(var = "mu ^ 3")}
      now work, and \code{quasi(variance = "log(mu)")} now gives a correct
      error message.

      \item Creation of lazy loading database during package installation is
      again robust to Rprofile changing the current working directory
      (\PR{17559}).

      \item \code{boxplot(y ~ f, horizontal=TRUE)} now produces correct x-
      and y-labels.

      \item \code{rbind.data.frame()} allows to keep \code{<NA>} levels
      from factor columns (\PR{17562}) via new option \code{factor.exclude}.

      Additionally, it works in one more case with matrix-columns which
      had been reported on 2017-01-16 by Krzysztof Banas.

      \item Correct messaging in C++ pragma checks in \pkg{tools} code for
      \command{R CMD check}, fixing \PR{17566} thanks to Xavier Robin.

      \item{print()}ing and auto-printing no longer differs for
      \code{function}s with a user defined \code{print.function}, thanks to
      Bill Dunlap's report.

      \item On Windows, \code{writeClipboard(.., format = <n>)} now
      does correctly pass \code{format} to the underlying C code, thanks to
      a bug report (with patch) by Jenny Bryan.

      \item \code{as.data.frame()} treats 1D arrays the same as vectors,
      \PR{17570}.

      \item Improvements in \code{smoothEnds(x, *)} working with
      \code{NA}s (towards \code{runmed()} working in that case, in the next
      version of R).

      \item \code{vcov(glm(<quasi>), dispersion = *)} works correctly
      again, fixing \PR{17571} thanks to Pavel Krivitsky.

      \item \command{R CMD INSTALL} of binary packages on Windows now
      works also with per-directory locking.

      \item \command{R CMD INSTALL} and \code{install.packages()} on Windows
      are now more robust against a locked file in an earlier installation
      of the package to be installed.  The default value of option
      \code{install.lock} on Windows has been changed to \code{TRUE}.

      \item On Unix alikes (when \command{readline} is active), only
      expand tilde (\code{~}) file names starting with a tilde, instead of
      almost all tildes.

      \item In R documentation (\file{*.Rd}) files, \samp{\item [..]} is no
      longer treated specially when rendered in LaTeX and hence pdf, but
      rather shows the brackets in all cases.
    }
  }
}

\section{\Rlogo CHANGES IN R 3.6.0}{

   \subsection{SIGNIFICANT USER-VISIBLE CHANGES}{
     \itemize{
       \item Serialization format version 3 becomes the default for
       serialization and saving of the workspace (\code{save()},
       \code{serialize()}, \code{saveRDS()}, \code{compiler::cmpfile()}).
       Serialized data in format 3 cannot be read by versions of \R prior
       to version 3.5.0.  Serialization format version 2 is still
       supported and can be selected by \code{version = 2} in the
       save/serialization functions.  The default can be changed back for
       the whole \R session by setting environment variables
       \env{R_DEFAULT_SAVE_VERSION} and \env{R_DEFAULT_SERIALIZE_VERSION}
       to \code{2}.  For maximal back-compatibility, files
       \file{vignette.rds} and \file{partial.rdb} generated by \command{R
	 CMD build} are in serialization format version 2, and resave by
       default produces files in serialization format version 2 (unless
       the original is already in format version 3).

       \item The default method for generating from a discrete uniform
       distribution (used in \code{sample()}, for instance) has been
       changed.  This addresses the fact, pointed out by Ottoboni and
       Stark, that the previous method made \code{sample()} noticeably
       non-uniform on large populations.  See \PR{17494} for a
       discussion.  The previous method can be requested using
       \code{RNGkind()} or \code{RNGversion()} if necessary for
       reproduction of old results.  Thanks to Duncan Murdoch for
       contributing the patch and Gabe Becker for further assistance.

       The output of \code{RNGkind()} has been changed to also return the
       \sQuote{kind} used by \code{sample()}.
     }
  }

   \subsection{NEW FEATURES}{
    \itemize{
      \item \code{Sys.setFileTime()} has been vectorized so arguments
      \code{path} and \code{time} of length greater than one are now
      supported.

      \item \code{axis()} gets new option \code{gap.axis = NA} for
      specifying a multiplication factor for the minimal \dQuote{gap}
      (distance) between axis labels drawn.   Its default is \code{1}
      for labels \emph{parallel} to the axis, and \code{0.25} for
      perpendicular ones.

      Perpendicular labels no longer overlap, fixing bug \PR{17384}.

      \item The default method of \code{plot()} gains new arguments
      \code{xgap.axis = NA} and \code{ygap.axis = NA} to be passed to
      the x-- and y-- \code{axis(.., gap.axis=*)} calls.

      \item \code{removeSource()} now works not only for functions but
      also for some language objects.

      \item \code{as.call()}, \code{rep.int()}, \code{rep_len()} and
      \code{nchar()} dispatch internally.

      \item \code{is(object, class2)} looks for \code{class2} in the
      calling namespace after looking in the namespace of
      \code{class(object)}.

      \item \code{extendrange(.., f)} with a length-2 \code{f} now
      extends separately to the left and the right.

      \item \code{lengths()} dispatches internally to S4 methods.

      \item \code{download.file()} on Windows now uses \code{URLdecode()}
      to determine the file extension, and uses binary transfer
      (\code{mode = "wb"}) also for file extension \file{.rds}.

      The help page for \code{download.file()} now contains the same
      information on all platforms.

      \item Setting \samp{C} locale for collation \emph{via} environment
      variables \env{LC_ALL} and \env{LC_COLLATE} and \emph{via} a call
      to \code{Sys.setlocale()} now takes precedence over environment
      variable \env{R_ICU_LOCALE}.

      \item There is a new function, \code{nullfile()}, to give the file
      name of the null system device (e.g., \file{/dev/null}) on the
      current platform. % not the null graphics device.

      \item There are two new options, \code{keep.parse.data} and
      \code{keep.parse.data.pkgs}, which control whether parse data are
      included into sources when \code{keep.source} or
      \code{keep.source.pkgs} is \code{TRUE}.  By default,
      \code{keep.parse.data.pkgs} is now \code{FALSE}, which changes
      previous behavior and significantly reduces space and time overhead
      when sources are kept when installing packages.

      \item In \code{rapply(x, ..)}, \code{x} can also be
      \dQuote{list-like} and of length \eqn{\ge 2^{31}}{>= 2^{31}}.

      \item \code{trimws()} gets new optional \code{whitespace}
      argument, allowing more extensive definitions of \dQuote{space},
      such as including Unicode spaces (as wished in \PR{17431}).

      \item \code{weighted.mean()} no longer coerces the weights to a
      double/numeric vector, since \code{sum()} now handles integer
      overflow. This makes \code{weighted.mean()} more polymorphic and
      endomorphic, but be aware that the results are no longer
      guaranteed to be a vector of type \samp{double}.

      \item When loading namespaces, S3 method registrations which
      overwrite previous registrations are now noted by default (using
      \code{packageStartupMessage()}).

      \item \code{compiler::cmpfile()} gains a \code{version} argument, for
      use when the output file should be saved in serialization format 2.

      \item The axis labeling in the default method of \code{pairs()}
      may now be toggled by new options \code{horOdd} and \code{verOdd}.

      \item (Not Windows nor macOS.) Package \pkg{tcltk} now supports an
      environment variable \env{R_DONT_USE_TK} which if set disables Tk
      initialization.  This is intended for use to circumvent errors in
      loading the package, e.g.\sspace{}with recent Linux running under
      an address sanitizer.

      \item The \code{numeric} method of \code{all.equal()} gets
      optional arguments \code{countEQ} and \code{formatFUN}.  If
      \code{countEQ} is true, the mean error is more sensible when many
      entries are \bold{eq}ual.

      \item \code{outer(x,y, FUN = "*")} is more efficient using
      \code{tcrossprod(u,v)} instead of \code{u \%*\% t(v)}.

      \item \code{vcov(<mlm>)} is more efficient via new optional
      arguments in \code{summary.mlm()}.

      \item The default method of \code{summary()} gets an option to
      choose the \emph{kind} of \code{quantile()}s to use; wish of
      \PR{17438}.

      \item Fitting multiple linear models \emph{via} \code{lm()} does
      work with \emph{matrix} offsets, as suggested in \PR{17407}.

      \item The new functions \code{mem.maxVSize()} and
      \code{mem.maxMSize()} allow the maximal size of the vector heap
      and the maximal number of nodes allowed in the current \command{R}
      process to be queried and set.

      \item \code{news()} gains support for \file{NEWS.md} files.

      \item An effort has been started to have our reference manuals,
      i.e., all help pages. show platform-independent information (rather
      than Windows or Unix-alike specifics visible only on that platform).
      Consequently, the Windows version of \code{X11()} / \code{x11()}
      got identical formal arguments to the Unix one.

      \item \code{sessionInfo()$running} has been factored out in a new
      variable \code{osVersion}. % precomputed at utils namespace load time

      \item \code{slice.index()} now also works for multi-dimensional
      margins.

      \item \code{untar()} used with an external \command{tar} command
      assumes this supports decompression including \command{xz} and
      automagically detecting the compression type.  This has been true
      of all mainstream implementations since 2009 (for GNU
      \command{tar}, since version 1.22): older implementations are
      still supported \emph{via} the new argument
      \code{support_old_tars} whose default is controlled by environment
      variable \env{R_SUPPORT_OLD_TARS}.  (It looks like NetBSD and
      OpenBSD have \sQuote{older} \command{tar} commands for this
      purpose.)

      \item The new function \code{asplit()} allow splitting an array or
      matrix by its margins.

      \item New functions \code{errorCondition()} and
      \code{warningCondition()} provide a convenient way to create
      structured error and warning objects.

      \code{.Deprecated()} now signals a warning of class
      \code{"deprecatedWarning"}, and \code{.Defunct()} now signals an
      error of class \code{"defunctError"}.

      \item Many \sQuote{package not found} errors are now signaled as
      errors of class \code{"packageNotFoundError"}.

      \item As an experimental feature, when \code{loadNamespace()} fails
      because the requested package is not available the error is
      initially signaled with a \code{retry_loadNamespace} restart
      available.  This allows a calling handler to try to install the
      package and continue.

      \item \code{S3method()} directives in \file{NAMESPACE} can now
      also be used to perform \emph{delayed} S3 method registration.

      \item Experimentally, setting environment variable
      \env{_R_CHECK_LENGTH_1_LOGIC2_} will lead to warnings (or errors
      if the variable is set to a \sQuote{true} value) when \code{&&} or
      \code{||} encounter and use arguments of length more than one.

      \item Added \code{"lines"} and \code{"chars"} coordinate systems
      to \code{grconvertX()} and \code{grconvertY()}.

      \item \code{getOption()} is more efficient notably for the rare
      case when called with two arguments, from several contributors in
      \PR{17394}.

      \item In \code{.col(dim)} and \code{.row(dim)}, \code{dim} now may
      also be an integer-valued \code{"double"}.

      \item \code{sQuote()} and \code{dQuote()} get an explicit \code{q}
      argument with obvious default instead of using
      \code{getOption("fancyQuotes")} implicitly and unconditionally.

      \item \code{unzip()} can list archives with comments and with
      spaces in file names even using an external \command{unzip} command.

      \item Command line completion has a new setting
      \code{rc.settings(dots = FALSE)} to remove \code{...} from the
      list of possible function arguments.

      \item \code{library()} no longer checks packages with compiled
      code match \samp{R.version$platform}.  \code{loadNamespace()}
      never has, and increasingly the \sQuote{canonical name} does not
      reflect the important characteristics of compiled code.
      %% E.g. 32/64-bit compiler settings

      \item The primitive functions \code{drop()} and \code{unclass()}
      now avoid duplicating their data for atomic vectors that are large
      enough, by returning \code{ALTREP} wrapper objects with adjusted
      attributes.  \R{}-level assignments to change attributes will also
      use wrapper objects to avoid duplicating data for larger atomic
      vectors. \R functions like \code{structure()} and \code{unname()}
      will therefore not duplicate data in these settings.  Generic
      vectors as produced by \code{list()} are not yet covered by this
      optimization but may be in due course.

      \item In \code{formals()}, \code{envir} becomes an optional
      argument instead of being hardwired.

      \item Instead of signalling an error for an invalid S4 object
      \code{x}, \code{str(x)} now gives a warning and subsequently still
      shows most parts of \code{x}, e.g., when slots are missing.

      \item \code{gamma(x)} and \code{lgamma(x)} no longer warn when
      correctly returning \code{Inf} or underflowing to zero.  This
      helps maximum likelihood and similar computations.

      \item \code{convertColor()} is now vectorized, so a lot faster for
      converting many colours at once.  The new argument
      \code{vectorized} to \code{colorConverter()} ensures that
      non-vectorized colour converters still work.  (Thanks to Brodie
      Gaslam.)

      \item \code{download.file()} and \code{url()} get new argument
      \code{headers} for custom HTTP headers, e.g., allowing to perform
      basic http authentication, thanks to a patch contributed by Gábor
      Csárdi.

      \item File-based connection functions \code{file()},
      \code{gzfile()}, \code{bzfile()} and \code{xzfile()} now signal an
      error when used on a directory.

      \item For \code{approx()}, \code{splinefun()} \emph{etc}, a new
      setting \code{ties = c("ordered", <fun>)} allows skipping the
      sorting and still treat ties.

      \item \code{format(x)} gives a more user friendly error message in
      the case where no method is defined.  A minimal method is provided
      in \code{format.default(x)} when \code{isS4(x)} is true.

      \item \code{which(x)} now also works when \code{x} is a long
      vector, thanks to Suharto Anggono's \PR{17201}.  \strong{NB}: this
      may return a double result, breaking the previous guarantee of an
      integer result.

      \item \code{seq.default()} is more careful to return an
      \code{integer} (as opposed to \code{double}) result when its
      arguments are large and/or classed objects; see comment #9 of
      Suharto Anggono's \PR{17497}.

      \item The \code{plot()} method for \code{lm} and \code{glm} fits,
      \code{plot.lm()}, gains a new option \code{iter.smooth} with a
      default of \code{0} for binomial fits, no longer down-weighting
      when smoothing the residuals.

      \item \code{zip()} passes its list of files \emph{via} standard
      input to the external command when too long for the command line
      (on some platforms).

      \item \code{data()} gains an \code{overwrite} argument.

      \item \code{t.test()} now also returns the standard error (in
      list component \code{stderr}).

      \item \code{model.matrix(*, contrasts.arg = CC)} now warns about
      invalid \code{contrasts.arg}s.

      \item Performance of \code{substr()} and \code{substring()} has been
      improved.

      \item \code{stopifnot()} has been simplified thanks to Suharto
      Anggono's proposals to become considerably faster for cheap
      expressions.

      \item The default \sQuote{user agent} has been changed when
      accessing \samp{http://} and \samp{https://} sites using
      \samp{libcurl}.  (A site was found which caused \samp{libcurl} to
      infinite-loop with the previous default.)

      \item \code{sessionInfo()} now also contains \code{RNGkind()} and
      prints it when it differs from the default; based on a proposal and
      patch by Gabe Becker in \PR{17535}.
      Also, \code{RNGversion(getRversion())} works directly.

      \item \code{library()} and \code{require()} now allow more control
      over handling search path conflicts when packages are
      attached. The policy is controlled by the new
      \code{conflicts.policy} option.

      \item \code{barplot()} gets a formula method, thanks to a patch
      proposal by Arni Magnusson in \PR{17521}.

      \item \code{pmax()} and \code{pmin(x)} now also work for long
      vectors, thanks to Suharto Anggono's \PR{17533}.

      \item \code{bxp()} now warns when omitting duplicated arguments.

      \item New \code{hcl.colors()} function to provide wide range of
      HCL-based colour palettes with much better perceptual properties
      than the existing RGB/HSV-based palettes like \code{rainbow()}.

      Also a new \code{hcl.pals()} function to list available palette
      names for \code{hcl.colors()}.

      Contributed by Achim Zeileis.

      \item The default colours for \code{image()} and
      \code{filled.contour()} are now based on \code{hcl.colors()}.

      \item The palette-generating functions \code{rainbow()},
      \code{gray.colors()}, etc.\sspace{}get a new \code{rev} argument
      to facilitate reversing the order of colors.

      \item New \code{str2lang()} and \code{str2expression()} as streamlined
      versions of \code{parse(text=., keep.source=FALSE)} allow to abstract
      typical call constructions, e.g., in \code{formula} manipulations.
      (Somewhat experimental) % << TO DO

      \item Add \code{update_PACKAGES()} for incrementally updating a
      package repository index, instead of rebuilding the index from
      scratch. Thanks to Gabe Becker in \PR{17544} for the patch, based
      on part of his \pkg{switchr} package.
    }
  }

  \subsection{INSTALLATION on a UNIX-ALIKE}{
    \itemize{
      \item The options selected for the C++ compiler default to the
      C++11 standard if supported, otherwise to the C++98 standard.

      \item Visibility macros such as \samp{C_VISIBILITY} can now be
      user-set (including to empty), e.g.\sspace{}in \file{config.site}.

      \item Macro \samp{FCLIBS}, which has sometimes been needed on
      Solaris, has been renamed to \samp{FCLIBS_XTRA}.

      \item Macro \samp{F77} is always set to the value of \samp{FC}, so
      the latter should be set to user-select the Fortran compiler for
      both fixed-form and free-form Fortran.  In particular,
      \command{gfortran} is now the first choice for \samp{F77}, not
      \command{f95}.

      Macros \samp{FFLAGS} and \samp{FCFLAGS} remain distinct to allow
      for a compiler which needs a flag to select free- or fixed-form
      Fortran (most use the source-file extension to choose: \file{.f}
      is fixed-form and \file{.f90} and \file{.f95} are free-form).

      If only one of them is set, its value is used for both.

      \item The special-casing of \samp{CFLAGS}, \samp{CXXFLAGS} and
      \samp{FFLAGS} for Intel compilers on Linux has been removed: we do
      not have recent experience but the generic defaults now chosen are
      the same as those previously special-cased for \samp{x86_64}.

      If necessary, override the defaults on the \command{configure}
      command line or in file \file{config.site}.

      \item Long-untested \command{configure} support for HP-UX and very
      old versions of Linux has been removed.

      \item \command{configure --with-blas} (without specifying a value)
      includes OpenBLAS in its search (before ATLAS and a generic
      BLAS).  This follows recent versions of the \samp{ax_blas}
      autoconf macro.

      \item The \command{configure} macro \samp{MAKEINFO} has been
      updated to \samp{TEXI2ANY}.

      \item Support for \command{make install-strip} has been enhanced.
    }
  }

  \subsection{PACKAGE INSTALLATION}{
    \itemize{
      \item Source package installation is by default \sQuote{staged}:
      the package is installed into a temporary location under the final
      library directory and moved into place once the installation is
      complete.  The benefit is that partially-installed packages are
      hidden from other \R sessions.

      The overall default is set by environment variable
      \env{R_INSTALL_STAGED}.  \command{R CMD INSTALL} has new options
      \option{--staged-install} and \option{--no-staged-install}, and
      packages can use the \samp{StagedInstall} field in their
      \file{DESCRIPTION} file to opt out.  (That opt-out is a temporary
      measure which may be withdrawn in future.)

      Staged installation requires either \option{--pkglock} or
      \option{--lock}, one of which is used by default.

      \item The interpretation of source code with extension \file{.f}
      is changing.  Previously this denoted FORTRAN 77 code, but current
      compilers no longer have a FORTRAN 77 mode and interpret it as
      \sQuote{fixed-form} Fortran 90 (or later where supported) code.
      Extensions \file{.f90} and \file{.f95} continue to indicate
      \sQuote{free-form} Fortran code.

      Legal FORTRAN 77 code is also legal fixed-form Fortran 9x; however
      this change legitimizes the use of later features, in particular
      to replace features marked \sQuote{obsolescent} in Fortran 90 and
      \sQuote{deleted} in Fortran 2018 which \command{gfortran} 8.x and
      later warn about.

      \item Packages containing files in the \file{src} directory with
      extensions \file{.f90} or \file{.f95} are now linked using the C
      or C++ compiler rather than the Fortran 9x compiler.  This is
      consistent with fixed-form Fortran code and allows mixing of C++
      and free-form Fortran on most platforms.

      Consequentially, a package which includes free-form Fortran 9x
      code which uses OpenMP should include \samp{SHLIB_OPENMP_CFLAGS}
      (or the \samp{CXXFLAGS} version if they also include C++ code) in
      \samp{PKG_LIBS} rather than \samp{SHLIB_OPENMP_FCFLAGS} ---
      fortunately on almost all current platforms they are the same
      flag.

      \item Macro \samp{PKG_FFLAGS} will be used for the compilation of
      both fixed-form and free-form Fortran code unless
      \samp{PKG_FCFLAGS} is also set (in \file{src/Makevars} or
      \file{src/Makevars.win}).

      \item The make macro \samp{F_VISIBILITY} is now preferred for
      both fixed-form and free-form Fortran, for use in
      \file{src/Makevars} and similar.

      \item \command{R CMD INSTALL} gains a new option \option{--strip}
      which (where supported) strips installed shared object(s): this
      can also be achieved by setting the environment variable
      \env{_R_SHLIB_STRIP_} to a true value.

      The new option \option{--strip-lib} attempts stripping of
      static and shared libraries installed under \file{lib}.

      These are most useful on platforms using GNU \code{binutils} (such
      as Linux) and compiling with \option{-g} flags.

      \item There is more support for installing UTF-8-encoded packages
      in a strict Latin-1 locale (and probably for other Latin locales):
      non-ASCII comments in \R code (and \file{NAMESPACE} files) are
      worked around better.
    }
  }

  \subsection{UTILITIES}{
    \itemize{
      \item \command{R CMD check} now optionally checks makefiles for
      correct and portable use of the \samp{SHLIB_OPENMP_*FLAGS} macros.

      \item \command{R CMD check} now evaluates \code{\\Sexpr{}}
      expressions (including those in macros) before checking the
      contents of \file{Rd} files and so detects issues both in
      evaluating the expressions and in the expanded contents.

      \item \command{R CMD check} now lists missing packages separated
      by commas and with regular quotes such as to be useful as argument
      in calling \code{install.packages(c(..))}; from a suggestion by
      Marcel Ramos.

      \item \code{tools::Rd2latex()} now uses UTF-8 as its default
      output encoding.

      \item \command{R CMD check} now checks line endings of files with
      extension \file{.hpp} and those under \file{inst/include}.  The
      check now includes that a non-empty file is terminated with a
      newline.

      \command{R CMD build} will correct line endings in such files.

      \item \command{R CMD check} now tries re-building all vignettes
      rather than stopping at the first error: whilst doing so it adds
      \sQuote{bookmarks} to the log.  By default (see the \sQuote{R
      Internals} manual) it re-builds each vignette in a separate
      process.

      It now checks for duplicated vignette titles (also known as
      \sQuote{index entries}): they are used as hyperlinks on
      \acronym{CRAN} package pages and so do need to be unique.

      \item \command{R CMD check} has more comprehensive checks on the
      \file{data} directory and the functioning of \code{data()} in a
      package.

      \item \command{R CMD check} now checks
      \command{autoconf}-generated \file{configure} files have their
      corresponding source files, including optionally attempting to
      regenerate them on platforms with \command{autoreconf}.

      \item \command{R CMD build} has a new option
      \option{--compression} to select the compression used for the tarball.

      \item \command{R CMD build} now removes \file{src/*.mod} files on
      all platforms.
    }
  }

  \subsection{C-LEVEL FACILITIES}{
    \itemize{
      \item New pointer protection C functions \code{R_PreserveInMSet} and
      \code{R_ReleaseFromMSet} have been introduced to replace
      \code{UNPROTECT_PTR}, which is not safe to mix with \code{UNPROTECT}
      (and with \code{PROTECT_WITH_INDEX}). Intended for use in parsers
      only.

      \item \code{NAMEDMAX} has been raised to 7 to allow further
      protection of intermediate results from (usually ill-advised)
      assignments in arguments to \code{BUILTIN} functions. Properly
      written package code should not be affected.

      \item \code{R_unif_index} is now considered to be part of the C API.

      \item \code{R_GetCurrentEnv()} allows C code to retrieve the current
      environment.
    }
  }

  \subsection{DEPRECATED AND DEFUNCT}{
    \itemize{
      \item Argument \code{compressed} of \code{untar()} is
      deprecated --- it is only used for external \code{tar} commands
      which increasingly for extraction auto-detect compression and
      ignore their \option{zjJ} flags.

      \item \code{var(f)} and hence \code{sd(f)} now give an error for
      \code{factor} arguments; they gave a deprecation warning since \R
      3.2.3, \PR{16564}.

      \item Package \pkg{tools}' \code{vignetteDepends()} has been
      deprecated (it called a function deprecated since Feb 2016), being
      partly replaced by newly exported \code{vignetteInfo()}.

      \item The \command{f77_f2c} script has been removed: it
      no longer sufficed to compile the \file{.f} files in \R.

      \item The deprecated legacy support of \command{make} macros such
      as \samp{CXX1X} has been removed: use the \samp{CXX11} forms
      instead.

      \item Make macro \samp{F77_VISIBILITY} is deprecated in favour of
      \samp{F_VISIBILITY}.

      \item Make macros \samp{F77}, \samp{FCPIFCPLAGS} and
      \samp{SHLIB_OPENMP_FCFLAGS} are deprecated in favour of \samp{FC},
      \samp{FPICFLAGS} and \samp{SHLIB_OPENMP_FFLAGS} respectively.

      \item \code{$.data.frame} had become an expensive version of
      the default method, so has been removed. (Thanks to Radford Neal
      for picking this up and to Duncan Murdoch for providing a patch.)
    }
  }

  \subsection{BUG FIXES}{
    \itemize{
      \item \code{replayPlot(r)} now also works in the same \R session
      when \code{r} has been \dQuote{reproduced} from serialization,
      typically after saving to and reading from an RDS file.

      \item \code{substr()} and \code{substring()} now signal an error
      when the input is invalid UTF-8.

      \item \code{file.copy()} now works also when its argument \code{to}
      is of length greater than one.

      \item \code{mantelhaen.test()} no longer suffers from integer
      overflow in largish cases, thanks to Ben Bolker's \PR{17383}.

      \item Calling \code{setGeneric("foo")} in a package no longer
      fails when the enclosing environment of the implicit generic
      \code{foo()} is \code{.GlobalEnv}.

      \item \code{untar(file("<some>.tar.gz"), *)} now gives a better
      error message, suggesting to use \code{gzfile()} instead.

      \item Method dispatch uses more relevant environments when looking
      up class definitions.

      \item The documentation for \code{identify()} incorrectly claimed
      that the indices of identified points were returned in the order
      that the points were selected.  \code{identify()} now has a new
      argument \code{order} to allow the return value to include the
      order in which points were identified; the documentation has been
      updated.  Reported by Richard Rowe and Samuel Granjeaud.

      \item \code{order(...., decreasing=c(TRUE, FALSE))} could fail in
      some cases. Reported from StackOverflow via Karl Nordström.

      \item User macros in Rd files now accept empty and multi-line
      arguments.

      \item Changes in \code{print.*()}, thanks to Lionel Henry's
      patches in \PR{17398}:
      \itemize{
	\item Printing lists, pairlists or attributes containing calls
	with S3 class no longer evaluate those.

	\item Printing S4 objects within lists and pairlists dispatches
	with \code{show()} rather than \code{print()}, as with
	auto-printing.

	\item The indexing tags (names or \code{[[<n>]]}) of recursive
	data structures are now printed correctly in complex cases.

	\item Arguments supplied to \code{print()} are now properly
	forwarded to methods when printing lists, pairlists or
	attributes containing S3 objects.

	\item The print parameters are now preserved when printing S3
	objects or deparsing symbols and calls.  Previously, printing
	lists containing S3 objects or expressions would reset these
	parameters.

	\item Printing lists, pairlists or attributes containing
	functions now uses \code{srcref} attributes if present.
      }

      \item Calling \code{install.packages()} with a length zero
      \code{pkgs} argument now is a no-op (\PR{17422}).

      \item \code{unlist(x)} now returns a correct \code{factor} when
      \code{x} is a nested list with factor leaves, fixing \PR{12572}
      and \PR{17419}.

      \item The documentation \code{help(family)} gives more details
      about the \code{aic} component, thanks to Ben Bolker's prompting.

      \item The documentation for \code{attributes} and
      \code{`attributes<-`} now gives \code{x} as name of the first and
      main argument which the implementation has been requiring, fixing
      \PR{17434}.  For consistency, the first argument name is also
      changed from \code{obj} to \code{x} for \code{`mostattributes<-`}.

      \item \code{strwidth()} now uses \code{par("font")} as default
      font face (\PR{17352}).

      \item \code{plot(<table>, log="x")} no longer warns about \code{log}.

      \item The \code{print()} method for \code{"htest"} objects now
      formats the test statistic and parameter directly and hence no
      longer rounds to units \emph{before} the decimal point.  Consequently,
      printing of \code{t.test()} results with a small number of digits
      now shows non-large \code{df}'s to the full precision (\PR{17444}).

      \item \code{kruskal.test()} and \code{fligner.test()} no longer
      erroneously insist on numeric \code{g} group arguments (\PR{16719}).

      \item Printing a news db via the browser now does a much better
      job (\PR{17433}).

      \item \code{print.aov()} missed column names in the multivariate case
      due to misspelling (reported by Chris Andrews).

      \item \code{axis()} now creates valid \code{at} locations also for
      small subnormal number ranges in log scale plots.

      \item \code{format.POSIXlt()} now also recycles the \code{zone}
      and \code{gmtoff} list components to full length when needed,
      and its internal C code detects \code{have_zone} in more cases.
      In some cases, this changes its output to become compatible with
      \code{format.POSIXct()}.

      \item On Windows, \code{detectCores()} in package \pkg{parallel}
      now detects processors in all processor groups, not just the group
      \R is running in (impacts particularly systems with more than 64
      logical processors).  Reported by Arunkumar Srinivasan.

      \item On Windows, \code{socketSelect()} would hang with more than
      64 sockets, and hence \code{parallel::clusterApplyLB()} would hang
      with more than 64 workers.  Reported by Arunkumar Srinivasan.

      \item \code{as(1L, "double")} now does coerce (\PR{17457}).

      \item \code{lm.influence()}, \code{influence.measures()},
      \code{rstudent()} etc now work (more) correctly for multivariate
      models (\code{"mlm"}), thanks to (anonymous) stackoverflow remarks.

      \item \code{sample.int(2.9, *, replace=TRUE)} again behaves as
      documented and as in R < 3.0.0, namely identically to
      \code{sample.int(2, ..)}.% consider back porting to 3.5.x !

      \item Fixes to \code{convertColor()} for chromatic adaptation;
      thanks to Brodie Gaslam \PR{17473}.

      \item Using \code{\\Sexpr[stage=install]{..}} to create an
      \file{Rd} section no longer gives a warning in \command{R CMD
	check}; problem originally posted by Gábor Csárdi, then reported
      as \PR{17479} with a partial patch by Duncan Murdoch.

      \item Parse data now include a special node for equal assignment.

      \item \code{split.default()} no longer relies on \code{[[<-()}, so
      it behaves as expected when splitting an object by a factor with
      the empty string as one of its levels.  Thanks to Brad Friedman for
      the report.

      \item Line numbers in messages about \file{.Rd} files are now more
      reliable, thanks to a patch from Duncan Murdoch.

      \item In the \code{numeric} method for \code{all.equal()}, a
      numeric \code{scale} argument is now checked to be positive and
      allowed to be of length > 1. (The latter worked originally and
      with a warning in recent years).

      \item Deferred string conversions now record the \code{OutDec}
      option setting when not equal to the default.  Reported by Michael
      Sannella.

      \item When \code{y} is numeric and \code{f} a \code{factor},
      \code{plot(y ~ f)} nicely uses "y" and "f" as y- and x-labels.
      The more direct \code{boxplot(y ~ f)} now does too.  The new
      argument \code{ann = FALSE} may be used to suppress these.

      \item Subassignment to no/empty rows of a data frame is more
      consistent and typically a no-op in all cases instead of sometimes
      an error; part of Emil Bode's \PR{17483}.

      \item Calls like \code{formatC(*, zero.print = "< 0.001")} no
      longer give an error and are further improved via new optional
      argument \code{replace.zero}.  Reported by David Hugh-Jones.

      \item \code{methods::formalArgs("<fn>")} now finds the same
      function as \code{formals("<fn>")}, fixing Emil Bode's \PR{17499}.

      \item The \pkg{methods} package better handles duplicated class
      names across packages.

      \item The default method of \code{seq()} now avoids integer
      overflow, thanks to the report and "cumsum" patch of Suharto
      Anggono's \PR{17497}.

      \item \code{sub()} no longer loses encodings for non-ASCII
      replacements (\PR{17509}).

      \item Fix for rotated raster image on X11 device.  (Partial
      fix for \PR{17148};  thanks to Mikko Korpela).

      \item \code{formula(model.frame(frml, ..))} now returns \code{frml}
      in all cases, thanks to Bill Dunlap.   The previous behavior is
      available as \code{DF2formula(<model.frame>)}.

      \item \code{ar.ols()} also returns scalar \code{var.pred} in
      univariate case (\PR{17517}).

      \item \code{normalizePath()} now treats \code{NA} path as non-existent
      and normalizes it to \code{NA}.  \code{file.access()} treats \code{NA}
      file name as non-existent.  \code{file.edit()} and connection
      functions such as \code{file()} now treat \code{NA} file names as
      errors.

      \item The internal \code{regularize.values()} auxiliary of
      \code{approx()}, \code{splinefun()} etc now warns again when there
      are ties and the caller did not specify \code{ties}.  Further, it no
      longer duplicates \code{x} and \code{y} unnecessarily when \code{x}
      is already sorted (\PR{17515}).

      \item \code{strtoi("", base)} now gives \code{NA} on all platforms,
      following its documentation.  Reported by Michael Chirico.

      \item In the definition of an S4 class, prototype elements are
      checked against the slots of the class, with giving a prototype
      for an undefined slot now being an error.  (Reported by Bill
      Dunlap.)

      \item From \code{setClassUnion()}, if environment variable
      \env{_R_METHODS_SHOW_CHECKSUBCLASSES} is set
      to true, the internal \code{.checkSubclasses()} utility prints
      debugging info to see where it is used.

      \item \code{max.col(m)} with an \code{m} of zero columns now
      returns integer \code{NA} (instead of \code{1}).

      \item \code{axTicks()} no longer returns small \dQuote{almost zero}
      numbers (in exponential format) instead of zero, fixing Ilario
      Gelmetti's \PR{17534}.

      \item \code{isSymmetric(matrix(0, dimnames=list("A","b")))} is
      \code{FALSE} again, as always documented.

      \item The \code{cairo_pdf} graphics device (and other Cairo-based
      devices) now clip correctly to the right and bottom border.

      There was an off-by-one-pixel bug, reported by Lee Kelvin.

      \item \code{as.roman(3) <= 2:4} and all other comparisons now work,
      as do group \code{"Summary"} function calls such as
      \code{max(as.roman(sample(20)))} and \code{as.roman(NA)}.
      (Partly reported by Bill Dunlap in \PR{17542}.)

      \item \code{reformulate("x", response = "sin(y)")} no longer produces
      extra back quotes, \PR{17359}, and gains new optional argument
      \code{env}.

      \item When reading console input from \file{stdin} with re-encoding
      (\command{R --encoding=enc < input}) the code on a Unix-alike now
      ensures that each converted input line is terminated with a
      newline even if re-encoding fails.

      \item \code{as.matrix.data.frame()} now produces better strings from
      logicals, thanks to \PR{17548} from Gabe Becker.

      \item The S4 generic signature of \code{rowSums()},
      \code{rowMeans()}, \code{colSums()} and \code{colMeans()} is
      restricted to \code{"x"}.

      \item \code{match(x, tab)} now works for long \emph{character}
      vectors \code{x}, thanks to \PR{17552} by Andreas Kersting.

      \item Class unions are unloaded when their namespace is
      unloaded (\PR{17531}, adapted from a patch by Brodie Gaslam).

      \item \code{selectMethod()} is robust to ANY-truncation of method
      signatures (thanks to Herve Pages for the report).
    }
  }
}

\section{\Rlogo CHANGES IN R 3.5.3}{

  \subsection{INSTALLATION on a UNIX-ALIKE}{
    \itemize{
      \item Detection of flags for C++98/11/14/17 has been improved: in
      particular if CXX??STD is set, it is tried first with no
      additional flags.
    }
  }

  \subsection{PACKAGE INSTALLATION}{
    \itemize{
      \item New macro \samp{F_VISIBILITY} as an alternative to
      \samp{F77_VISIBILITY}.  This will become the preferred form in
      \R 3.6.0.
    }
  }

  \subsection{BUG FIXES}{
    \itemize{
      \item \code{writeLines(readLines(fnam), fnam)} now works as expected,
      thanks to Peter Meissner's \PR{17528}.

      \item \code{setClassUnion()} no longer warns, but uses \code{message()}
      for now, when encountering \dQuote{non local} subclasses of class members.

      \item \code{stopifnot(exprs = T)} no longer fails.
    }
  }
}

\section{\Rlogo CHANGES IN R 3.5.2}{

  \subsection{PACKAGE INSTALLATION}{
    \itemize{
      \item New macro \samp{CXX_VISIBILITY} analogous to
      \samp{C_VISIBILITY} (which several packages have been misusing for
      C++ code) for the default C++ compiler (but not necessarily one used
      for non-default C++ dialects like C++14).
    }
  }

  \subsection{TESTING}{
    \itemize{
      \item The random number generator tests in
      \file{tests/p-r-random-tests.R} no longer fail occasionally as
      they now randomly sample from \dQuote{certified} random seeds.
    }
  }

  \subsection{BUG FIXES}{
    \itemize{
      \item The \code{"glm"} method of \code{drop1()} miscalculated the
      score test (\code{test="Rao"}) when the model contained an offset.

      \item Linear multiple empty models such as \code{lm(y ~ 0)}
      now have a correctly dimensioned empty coefficient matrix;
      reported by Brett Presnell.

      \item \code{vcov(<empty mlm>)} and hence \code{confint()} now
      work (via a consistency change in \code{summary.lm()}).

      \item \code{confint(<multiple lm()>)} now works correctly;
      reported on R-devel by Steven Pav.

      \item \code{quade.test()} now also works correctly when its
      arguments are not yet sorted along \code{groups}, fixing \PR{15842}.

      \item Installation on a Unix-alike tries harder to link to the
      \samp{pthread} library where required (rather than relying on
      OpenMP to provide it: configuring with \option{--disable-openmp}
      was failing on some Linux systems).

      \item The \code{data.frame} method for \code{print(x)} is fast now
      also for large data frames \code{x} and got an optional argument
      \code{max}, thanks to suggestions by Juan Telleria.

      \item \code{hist()} no longer integer overflows in very rare
      cases, fixing \PR{17450}.

      \item \code{untar()} ignored a character \code{compressed}
      argument: however many external \command{tar} programs ignore
      the flags which should have been set and automagically choose the
      compression type, and if appropriate \command{gzip} or
      \command{bzip2} compression would have been chosen from the magic
      header of the tarball.

      \item \code{zapsmall(x)} now works for more \dQuote{number-like} objects.

      \item The tools-internal function called from \command{R CMD
	INSTALL} now gets a \code{warnOption = 1} argument and only sets
      \code{options(warn = warnOption)} when that increases the warning
      level (\PR{17453}).

      \item Analogously, the tools-internal function called from \command{R CMD
	check} gets a \code{warnOption = 1} argument and uses the larger
      of that and \code{getOption("warn")}, also allowing to be run with
      increased warning level.

      \item Parse data now have deterministic parent nodes (\PR{16041}).

      \item Calling \code{match()} with length one \code{x} and POSIXlt
      \code{table} gave a segfault (\PR{17459}).

      \item Fork clusters could hang due to a race condition in cluster
      initialization (\code{makeCluster()}).

      \item \code{nextn(n)} now also works for larger \code{n} and no
      longer loops infinitely for e.g, \code{n <- 214e7}.

      \item \code{cooks.distance()} and \code{rstandard()} now work
      correctly for multiple linear models (\code{"mlm"}).

      \item \code{polym()} and corresponding \code{lm()} prediction now
      also work for a boundary "vector" case fixing \PR{17474},
      reported by Alexandre Courtiol.

      \item With a very large number of variables \code{terms()} could
      segfault (\PR{17480}).

      \item \code{cut(rep(0, 7))} now works, thanks to Joey Reid and
      Benjamin Tyner (\PR{16802}).

      \item \code{download.file(*, method = "curl", cacheOK = FALSE)}
      should work now on Windows, thanks to Kevin Ushey's patch in
      \PR{17323}.

      \item \code{duplicated(<dataframe with 'f'>)} now works, too,
      thanks to Andreas Kersting's \PR{17485}; ditto for
      \code{anyDuplicated()}.

      \item \code{legend(*, cex = 1:2)} now works less badly.

      \item The \code{print()} method for \code{POSIXct} and
      \code{POSIXlt} now correctly obeys \code{getOption("max.print")},
      fixing a long-standing typo, and it also gets a corresponding
      optional \code{max} argument.

      \item Unserialization of raw vectors serialized in ASCII
      representation now works correctly.

      \item \code{<data frame>[TRUE, <new>] <- list(c1, c2)} now works
      correctly, thanks to Suharto Anggono's \PR{15362} and Emil Bode's
      patch in \PR{17504}.

      \item \code{seq.int(*, by=by, length=n)} no longer wrongly
      \dQuote{drops fractional parts} when \code{by} is integer, thanks
      to Suharto Anggono's report \PR{17506}.

      \item Buffering is disabled for \code{file()} connections to
      non-regular files (like sockets), as well as \code{fifo()} and
      \code{pipe()} connections.  Fixes \PR{17470}, reported by Chris
      Culnane.
    }
  }
}

\section{\Rlogo CHANGES IN R 3.5.1}{

  \subsection{BUG FIXES}{
    \itemize{
      \item \code{file("stdin")} is no longer considered seekable.

      \item \code{dput()} and \code{dump()} are no longer truncating
      when \code{options(deparse.max.lines = *)} is set.

      \item Calls with an S3 class are no longer evaluated when printed,
      fixing part of \PR{17398}, thanks to a patch from Lionel Henry.

      \item Allow \code{file} argument of \command{Rscript} to include space
      even when it is first on the command line.

      \item \code{callNextMethod()} uses the generic from the
      environment of the calling method. Reported by Hervé Pagès with
      well documented examples.

      \item Compressed file connections are marked as blocking.

      \item \code{optim(*, lower = c(-Inf, -Inf))} no longer warns (and
      switches the method), thanks to a suggestion by John Nash.

      \item \code{predict(fm, newdata)} is now correct also for models
      where the formula has terms such as \code{splines::ns(..)} or
      \code{stats::poly(..)}, fixing \PR{17414}, based on a patch from
      Duncan Murdoch.

      \item \code{simulate.lm(glm(*, gaussian(link = <non-default>)))}
      has been corrected, fixing \PR{17415} thanks to Alex Courtiol.

      \item \code{unlist(x)} no longer fails in some cases of nested
      empty lists.  Reported by Steven Nydick.

      \item \code{qr.coef(qr(<all 0, w/ colnames>))} now works.
      Reported by Kun Ren.

      \item The radix sort is robust to vectors with >1 billion elements
      (but long vectors are still unsupported). Thanks to Matt Dowle for
      the fix.

      \item Terminal connections (e.g., stdin) are no longer
      buffered. Fixes \PR{17432}.

      \item \code{deparse(x)}, \code{dput(x)} and \code{dump()} now
      respect \code{c()}'s argument names \code{recursive} and
      \code{use.names}, e.g., for \code{x <- setNames(0, "recursive")},
      thanks to Suharto Anggono's \PR{17427}.

      \item Unbuffered connections now work with encoding conversion.
      Reported by Stephen Berman.

      \item \file{.Renviron} on Windows with \command{Rgui} is again by
      default searched for in user documents directory when invoked
      \emph{via} the launcher icon.  Reported by Jeroen Ooms.

      \item \code{printCoefmat()} now also works with explicit
      \code{right=TRUE}.

      \item \code{print.noquote()} now also works with explicit
      \code{quote=FALSE}.

      \item The default method for \code{pairs(.., horInd=*, verInd=*)}
      now gets the correct order, thanks to reports by Chris Andrews and
      Gerrit Eichner.  Additionally, when \code{horInd} or \code{verInd}
      contain only a subset of variables, all the axes are labeled
      correctly now.

      \item \code{agrep("..|..", .., fixed=FALSE)} now matches when it
      should, thanks to a reminder by Andreas Kolter.

      \item \code{str(ch)} now works for more invalid multibyte strings.
    }
  }
}

\section{\Rlogo CHANGES IN R 3.5.0}{

  \subsection{SIGNIFICANT USER-VISIBLE CHANGES}{
    \itemize{
      \item All packages are by default byte-compiled on installation.
      This makes the installed packages larger (usually marginally so)
      and may affect the format of messages and tracebacks (which often
      exclude \code{.Call} and similar).
    }
  }

  \subsection{NEW FEATURES}{
    \itemize{
      \item \code{factor()} now uses \code{order()} to sort its levels,
      rather than \code{sort.list()}.  This allows \code{factor()} to
      support custom vector-like objects if methods for the appropriate
      generics are defined.  It has the side effect of making
      \code{factor()} succeed on empty or length-one non-atomic
      vector(-like) types (e.g., \code{"list"}), where it failed before.

      \item \code{diag()} gets an optional \code{names} argument: this
      may require updates to packages defining S4 methods for it.

      \item \code{chooseCRANmirror()} and \code{chooseBioCmirror()} no
      longer have a \code{useHTTPS} argument, not needed now all \R
      builds support \samp{https://} downloads.

      \item New \code{summary()} method for \code{warnings()} with a
      (somewhat experimental) \code{print()} method.

      \item (\pkg{methods} package.) \code{.self} is now automatically
      registered as a global variable when registering a reference class
      method.

      \item \code{tempdir(check = TRUE)} recreates the \code{tempdir()}
      directory if it is no longer valid (e.g.\sspace{}because some
      other process has cleaned up the \file{/tmp} directory).

      \item New \code{askYesNo()} function and \code{"askYesNo"} option
      to ask the user binary response questions in a customizable but
      consistent way.  (Suggestion of \PR{17242}.)

      \item New low level utilities \code{...elt(n)} and \code{...length()}
      for working with \code{...} parts inside a function.

      \item \code{isTRUE()} is more tolerant and now true
      in  \preformatted{   x <- rlnorm(99)
   isTRUE(median(x) == quantile(x)["50\%"])}
      New function \code{isFALSE()} defined analogously to \code{isTRUE()}.

      \item The default symbol table size has been increased from 4119
      to 49157; this may improve the performance of symbol resolution
      when many packages are loaded.  (Suggested by Jim Hester.)

      \item \code{line()} gets a new option \code{iter = 1}.

      \item Reading from connections in text mode is buffered,
      significantly improving the performance of \code{readLines()}, as
      well as \code{scan()} and \code{read.table()}, at least when
      specifying \code{colClasses}.

      \item \code{order()} is smarter about picking a default sort
      \code{method} when its arguments are objects.

      \item \code{available.packages()} has two new arguments which
      control if the values from the per-session repository cache are
      used (default true, as before) and if so how old cached values can
      be used (default one hour).

      These arguments can be passed from \code{install.packages()},
      \code{update.packages()} and functions calling that: to enable
      this \code{available.packages()}, \code{packageStatus()} and
      \code{download.file()} gain a \code{\dots} argument.

      \item \code{packageStatus()}'s \code{upgrade()} method no longer
      ignores its \code{\dots} argument but passes it to
      \code{install.packages()}.

      \item \code{installed.packages()} gains a \code{\dots} argument to
      allow arguments (including \code{noCache}) to be passed from
      \code{new.packages()}, \code{old.packages()},
      \code{update.packages()} and \code{packageStatus()}.

      \item \code{factor(x, levels, labels)} now allows duplicated
      \code{labels} (not duplicated \code{levels}!).  Hence you can map
      different values of \code{x} to the same level directly.

      \item Attempting to use \code{names<-()} on an S4 derivative of a
      basic type no longer emits a warning.

      \item The \code{list} method of \code{within()} gains an option
      \code{keepAttrs = FALSE} for some speed-up.

      \item \command{system()} and \command{system2()} now allow the
      specification of a maximum elapsed time (\sQuote{timeout}).

      \item \code{debug()} supports debugging of methods on any object
      of S4 class \code{"genericFunction"}, including group generics.

      \item Attempting to increase the length of a variable containing
      \code{NULL} using \code{length()<-} still has no effect on the
      target variable, but now triggers a warning.

      \item \code{type.convert()} becomes a generic function, with
      additional methods that operate recursively over list and
      \code{data.frame} objects. Courtesy of Arni Magnusson
      (\PR{17269}).

      \item \code{lower.tri(x)} and \code{upper.tri(x)} only needing
      \code{dim(x)} now work via new functions \code{.row()} and
      \code{.col()}, so no longer call \code{as.matrix()} by default in
      order to work efficiently for all kind of matrix-like objects.

      \item \code{print()} methods for \code{"xgettext"} and
      \code{"xngettext"} now use \code{encodeString()} which keeps,
      e.g.\sspace{}\code{"\n"}, visible.  (Wish of \PR{17298}.)

      \item \code{package.skeleton()} gains an optional \code{encoding}
      argument.

      \item \code{approx()}, \code{spline()}, \code{splinefun()} and
      \code{approxfun()} also work for long vectors.

      %% one item with several paragraphs
      \item \code{deparse()} and \code{dump()} are more useful for S4
      objects, \code{dput()} now using the same internal C code instead
      of its previous imperfect workaround R code.  S4 objects now
      typically deparse perfectly, i.e., can be recreated identically
      from deparsed code.

      \code{dput()}, \code{deparse()} and \code{dump()} now print the
      \code{names()} information only once, using the more readable
      \code{(tag = value)} syntax, notably for \code{list()}s, i.e.,
      including data frames.

      These functions gain a new control option \code{"niceNames"} (see
      \code{.deparseOpts()}), which when set (as by default) also uses
      the \code{(tag = value)} syntax for atomic vectors.  On the other
      hand, without deparse options \code{"showAttributes"} and
      \code{"niceNames"}, names are no longer shown also for lists.
      \code{as.character(list(  c (one = 1)))} now includes the name, as
      \code{as.character(list(list(one = 1)))} has always done.

      \code{m:n} now also deparses nicely when \eqn{m > n}.

      The \code{"quoteExpressions"} option, also part of \code{"all"},
      no longer \code{quote()}s formulas as that may not re-parse
      identically.  (\PR{17378})

      \item If the option \code{setWidthOnResize} is set and \code{TRUE},
      \R run in a terminal using a recent \code{readline} library will
      set the \code{width} option when the terminal is
      resized.  Suggested by Ralf Goertz.

      \item If multiple \code{on.exit()} expressions are set using
      \code{add = TRUE} then all expressions will now be run even if one
      signals an error.

      \item \code{mclapply()} gets an option \code{affinity.list} which
      allows more efficient execution with heterogeneous processors,
      thanks to Helena Kotthaus.

      \item The \code{character} methods for \code{as.Date()} and
      \code{as.POSIXlt()} are more flexible \emph{via} new arguments
      \code{tryFormats} and \code{optional}: see their help pages.

      \item \code{on.exit()} gains an optional argument \code{after}
      with default \code{TRUE}. Using \code{after = FALSE} with
      \code{add = TRUE} adds an exit expression before any
      existing ones. This way the expressions are run in a first-in
      last-out fashion. (From Lionel Henry.)

      \item On Windows, \code{file.rename()} internally retries the
      operation in case of error to attempt to recover from possible
      anti-virus interference.

      \item Command line completion on \samp{::} now also includes
      lazy-loaded data.

      \item If the \env{TZ} environment variable is set when date-time
      functions are first used, it is recorded as the session default
      and so will be used rather than the default deduced from the OS
      if \env{TZ} is subsequently unset.

      \item There is now a \code{[} method for class \code{"DLLInfoList"}.

      \item \code{glm()} and \code{glm.fit} get the same
      \code{singular.ok = TRUE} argument that \code{lm()} has had
      forever.  As a consequence, in \code{glm(*, method = <your_own>)},
      user specified methods need to accept a \code{singular.ok}
      argument as well.

      \item \code{aspell()} gains a filter for Markdown (\file{.md} and
      \file{.Rmd}) files.

      \item \code{intToUtf8(multiple = FALSE)} gains an argument to
      allow surrogate pairs to be interpreted.

      \item The maximum number of DLLs that can be loaded into R
      e.g.\sspace{}\emph{via} \code{dyn.load()} has been increased up to 614
      when the OS limit on the number of open files allows.

      \item \code{Sys.timezone()} on a Unix-alike caches the value at
      first use in a session: \emph{inter alia} this means that setting
      \env{TZ} later in the session affects only the \emph{current} time
      zone and not the \emph{system} one.

      \code{Sys.timezone()} is now used to find the system timezone to
      pass to the code used when \R is configured with
      \option{--with-internal-tzcode}.

      \item When \code{tar()} is used with an external command which is
      detected to be GNU \command{tar} or libarchive \command{tar} (aka
      \command{bsdtar}), a different command-line is generated to
      circumvent line-length limits in the shell.

      \item \code{system(*, intern = FALSE)}, \code{system2()} (when not
      capturing output), \code{file.edit()} and \code{file.show()} now
      issue a warning when the external command cannot be executed.

      \item The \dQuote{default} (\code{"lm"} etc) methods of
      \code{vcov()} have gained new optional argument \code{complete =
      TRUE} which makes the \code{vcov()} methods more consistent with
      the \code{coef()} methods in the case of singular designs.  The
      former (back-compatible) behavior is given by \code{vcov(*,
	complete = FALSE)}.

      \item \code{coef()} methods (for \code{lm} etc) also gain a
      \code{complete = TRUE} optional argument for consistency with
      \code{vcov()}. \cr
      For \code{"aov"}, both \code{coef()} and \code{vcov()} methods
      remain back-compatibly consistent, using the \emph{other} default,
      \code{complete = FALSE}.

      \item \code{attach(*, pos = 1)} is now an error instead of a
      warning.

      \item New function \code{getDefaultCluster()} in package
      \pkg{parallel} to get the default cluster set via
      \code{setDefaultCluster()}.

      \item \code{str(x)} for atomic objects \code{x} now treats both
      cases of \code{is.vector(x)} similarly, and hence much less often
      prints \code{"atomic"}.  This is a slight non-back-compatible
      change producing typically both more informative and shorter
      output.

      \item \code{gc()} gets new argument \code{full}.

      \item \code{write.dcf()} gets optional argument \code{useBytes}.

      \item New, partly experimental \code{packageDate()} which tries to
      get a valid \code{"Date"} object from a package \file{DESCRIPTION}
      file, thanks to suggestions in \PR{17324}.

      \item \code{tools::resaveRdaFiles()} gains a \code{version}
      argument, for use when packages should remain compatible with
      earlier versions of \R.

      \item \code{ar.yw(x)} and hence by default \code{ar(x)} now work
      when \code{x} has \code{NA}s, mostly thanks to a patch by Pavel
      Krivitsky in \PR{17366}.   The \code{ar.yw.default()}'s AIC
      computations have become more efficient by using
      \code{determinant()}.

      \item New \code{warnErrList()} utility (from package \pkg{nlme},
      improved).

      \item By default the (arbitrary) signs of the loadings from
      \code{princomp()} are chosen so the first element is non-negative.

      \item If \option{--default-packages} is not used, then
      \command{Rscript} now checks the environment variable
      \env{R_SCRIPT_DEFAULT_PACKAGES}. If this is set, then it takes
      precedence over \env{R_DEFAULT_PACKAGES}. If default packages are
      not specified on the command line or by one of these environment
      variables, then \command{Rscript} now uses the same default
      packages as \command{R}. For now, the previous behavior of not
      including \pkg{methods} can be restored by setting the environment
      variable \env{R_SCRIPT_LEGACY} to \samp{yes}.

      \item When a package is found more than once, the warning from
      \code{find.package(*, verbose=TRUE)} lists all library locations.

      \item POSIXt objects can now also be rounded or truncated to month
      or year.

      \item \code{stopifnot()} can be used alternatively via new argument
      \code{exprs} which is nicer and useful when testing several
      expressions in one call.

      \item The environment variable \env{R_MAX_VSIZE} can now be used
      to specify the maximal vector heap size. On macOS, unless specified
      by this environment variable, the maximal vector heap size is set to
      the maximum of 16GB and the available physical memory. This is to
      avoid having the \command{R} process killed when macOS over-commits
      memory.

      \item \code{sum(x)} and \code{sum(x1, x2, .., x<N>)} with many or
      long logical or integer vectors no longer overflows (and returns
      \code{NA} with a warning), but returns \code{double} numbers in
      such cases.

      \item Single components of \code{"POSIXlt"} objects can now be
      extracted and replaced via \code{[} indexing with 2 indices.

      \item S3 method lookup now searches the namespace registry after
      the top level environment of the calling environment.

      \item Arithmetic sequences created by \code{1:n},
      \code{seq_along}, and the like now use compact internal
      representations via the \code{ALTREP} framework.  Coercing integer
      and numeric vectors to character also now uses the \code{ALTREP}
      framework to defer the actual conversion until first use.

      \item Finalizers are now run with interrupts suspended.

      \item \code{merge()} gains new option \code{no.dups} and by
      default suffixes the second of two duplicated column names,
      thanks to a proposal by Scott Ritchie (and Gabe Becker).

      \item \code{scale.default(x, center, scale)} now also allows
      \code{center} or \code{scale} to be \dQuote{numeric-alike}, i.e.,
      such that \code{as.numeric(.)} coerces them correctly.  This also
      eliminates a wrong error message in such cases.

      \item \code{par*apply} and \code{par*applyLB} gain an optional
      argument \code{chunk.size} which allows to specify the granularity of
      scheduling.

      \item Some \code{as.data.frame()} methods, notably the
      \code{matrix} one, are now more careful in not accepting
      duplicated or \code{NA} row names, and by default produce unique
      non-NA row names.  This is based on new function
      \code{.rowNamesDF(x, make.names = *) <- rNms} where the logical
      argument \code{make.names} allows to specify \emph{how} invalid
      row names \code{rNms} are handled.  \code{.rowNamesDF()} is a
      \dQuote{workaround} compatible default.

      \item R has new serialization format (version 3) which supports
      custom serialization of \code{ALTREP} framework objects.  These
      objects can still be serialized in format 2, but less efficiently.
      Serialization format 3 also records the current native encoding of
      unflagged strings and converts them when de-serialized in R
      running under different native encoding.  Format 3 comes with new
      serialization magic numbers (RDA3, RDB3, RDX3).  Format 3 can be
      selected by \code{version = 3} in \code{save()},
      \code{serialize()} and \code{saveRDS()}, but format 2 remains the
      default for all serialization and saving of the workspace.
      Serialized data in format 3 cannot be read by versions of \R prior
      to version 3.5.0.

      \item The \code{"Date"} and \dQuote{date-time} classes
      \code{"POSIXlt"} and \code{"POSIXct"} now have a working
      \code{length<-()} method, as wished in \PR{17387}.

      \item \code{optim(*, control = list(warn.1d.NelderMead = FALSE))}
      allows to turn off the warning when applying the default
      \code{"Nelder-Mead"} method to 1-dimensional problems.

      \item \code{matplot(.., panel.first = .)} etc now work, as
      \code{log} becomes explicit argument and \code{...} is passed to
      \code{plot()} unevaluated, as suggested by Sebastian Meyer in \PR{17386}.

      \item Interrupts can be suspended while evaluating an expression
      using \code{suspendInterrupts}.  Subexpression can be evaluated
      with interrupts enabled using \code{allowInterrupts}.  These
      functions can be used to make sure cleanup handlers cannot be
      interrupted.

      \item \R 3.5.0 includes a framework that allows packages to
      provide alternate representations of basic \R objects
      (\code{ALTREP}).  The framework is still experimental and may
      undergo changes in future \R releases as more experience is
      gained. For now, documentation is provided in
      \url{https://svn.r-project.org/R/branches/ALTREP/ALTREP.html}.
    }
  }

  \subsection{UTILITIES}{
    \itemize{
      \item \code{install.packages()} for source packages now has the
      possibility to set a \sQuote{timeout} (elapsed-time limit).  For
      serial installs this uses the \code{timeout} argument of
      \code{system2()}: for parallel installs it requires the
      \command{timeout} utility command from GNU \pkg{coreutils}.

      \item It is now possible to set \sQuote{timeouts} (elapsed-time
      limits) for most parts of \command{R CMD check} \emph{via}
      environment variables documented in the \sQuote{R Internals}
      manual.

      \item The \sQuote{BioC extra} repository which was dropped from
      Bioconductor 3.6 and later has been removed from
      \code{setRepositories()}.  This changes the mapping for 6--8 used
      by \code{setRepositories(ind=)}.

      \item \command{R CMD check} now also applies the settings of
      environment variables \env{_R_CHECK_SUGGESTS_ONLY_} and
      \env{_R_CHECK_DEPENDS_ONLY_} to the re-building of vignettes.

      \item \command{R CMD check} with environment variable
      \env{_R_CHECK_DEPENDS_ONLY_} set to a true value makes
      test-suite-management packages available and (for the time being)
      works around a common omission of \CRANpkg{rmarkdown} from the
      \samp{VignetteBuilder} field.
    }
  }

  \subsection{INSTALLATION on a UNIX-ALIKE}{
    \itemize{
      \item Support for a system Java on macOS has been
      removed --- install a fairly recent Oracle Java (see \sQuote{R
	Installation and Administration} §C.3.2).

      \item \command{configure} works harder to set additional flags in
      \samp{SAFE_FFLAGS} only where necessary, and to use flags which
      have little or no effect on performance.

      In rare circumstances it may be necessary to override the
      setting of \samp{SAFE_FFLAGS}.

      \item C99 functions \code{expm1}, \code{hypot}, \code{log1p} and
      \code{nearbyint} are now required.

      \item \command{configure} sets a \option{-std} flag for the C++
      compiler for all supported C++ standards (e.g., \option{-std=gnu++11}
      for the C++11 compiler).   Previously this was not done in a few
      cases where the default standard passed the tests made
      (e.g.\sspace\command{clang 6.0.0} for C++11).
    }
  }

  \subsection{C-LEVEL FACILITIES}{
    \itemize{
      \item \sQuote{Writing R Extensions} documents macros
      \code{MAYBE_REFERENCED}, \code{MAYBE_SHARED} and
      \code{MARK_NOT_MUTABLE} that should be used by package \code{C}
      code instead \code{NAMED} or \code{SET_NAMED}.

      \item The object header layout has been changed to support merging
      the \code{ALTREP} branch. This requires re-installing packages
      that use compiled code.

      \item \sQuote{Writing R Extensions} now documents the
      \code{R_tryCatch}, \code{R_tryCatchError}, and
      \code{R_UnwindProtect} functions.

      \item \code{NAMEDMAX} has been raised to 3 to allow protection of
      intermediate results from (usually ill-advised) assignments in
      arguments to \code{BUILTIN} functions. Package \code{C} code using
      \code{SET_NAMED} may need to be revised.
    }
  }

  \subsection{DEPRECATED AND DEFUNCT}{
    \itemize{
      \item \code{Sys.timezone(location = FALSE)} is defunct, and is
      ignored (with a warning).

      \item \code{methods:::bind_activation()} is defunct now; it
      typically has been unneeded for years.

      The undocumented \sQuote{hidden} objects \code{.__H__.cbind} and
      \code{.__H__.rbind} in package \pkg{base} are deprecated (in
      favour of \code{cbind} and \code{rbind}).

      \item The declaration of \code{pythag()} in \file{Rmath.h} has
      been removed --- the entry point has not been provided since \R
      2.14.0.
    }
  }

  \subsection{BUG FIXES}{
    \itemize{
      \item \code{printCoefmat()} now also works without column names.

      \item The S4 methods on \code{Ops()} for the \code{"structure"} class no
      longer cause infinite recursion when the structure is not an S4 object.

      \item \code{nlm(f, ..)} for the case where \code{f()} has a
      \code{"hessian"} attribute now computes
      \eqn{LL' = H + \mu I}{LL' = H + µI} correctly.  (\PR{17249}).

      \item An S4 method that \dQuote{rematches} to its generic and
      overrides the default value of a generic formal argument to
      \code{NULL} no longer drops the argument from its formals.

      \item \code{Rscript} can now accept more than one argument given on
      the \samp{#!} line of a script. Previously, one could only pass a
      single argument on the \samp{#!} line in Linux.

      \item Connections are now written correctly with encoding
      \code{"UTF-16LE"}.  (\PR{16737}).

      \item Evaluation of \code{..0} now signals an error.  When
      \code{..1} is used and \code{...} is empty, the error message is
      more appropriate.

      \item (Windows mainly.)  Unicode code points which require
      surrogate pairs in UTF-16 are now handled.  All systems should
      properly handle surrogate pairs, even those systems that do not
      need to make use of them.  (\PR{16098})

      \item \code{stopifnot(e, e2, ...)} now evaluates the
      expressions sequentially and in case of an error or warning shows
      the relevant expression instead of the full \code{stopifnot(..)}
      call.

      \item \code{path.expand()} on Windows now accepts paths specified as
      UTF-8-encoded character strings even if not representable in the
      current locale.  (\PR{17120})

      \item \code{line(x, y)} now correctly computes the medians of the
      left and right group's x-values and in all cases reproduces
      straight lines.

      \item Extending S4 classes with slots corresponding to special
      attributes like \code{dim} and \code{dimnames} now works.

      \item Fix for \code{legend()} when \code{fill} has multiple values
      the first of which is \code{NA} (all colours used to default to
      \code{par(fg)}).  (\PR{17288})

      \item \code{installed.packages()} did not remove the cached value
      for a library tree that had been emptied (but would not use the
      old value, just waste time checking it).

      \item The documentation for \code{installed.packages(noCache = TRUE)}
      incorrectly claimed it would refresh the cache.

      \item \code{aggregate(<data.frame>)} no longer uses spurious names
      in some cases.  (\PR{17283})

      \item \code{object.size()} now also works for long vectors.

      \item \code{packageDescription()} tries harder to solve
      re-encoding issues, notably seen in some Windows locales.  This
      fixes the \code{citation()} issue in \PR{17291}.

      \item \code{poly(<matrix>, 3)} now works, thanks to prompting by
      Marc Schwartz.

      \item \code{readLines()} no longer segfaults on very large files
      with embedded \code{'\0'} (aka \sQuote{nul}) characters.
      (\PR{17311})

      \item \code{ns()} (package \pkg{splines}) now also works for a
      single observation.  \code{interpSpline()} gives a more friendly
      error message when the number of points is less than four.

      \item \code{dist(x, method = "canberra")} now uses the correct
      definition; the result may only differ when \code{x} contains
      values of differing signs, e.g.\sspace{}not for 0-1 data.

      \item \code{methods:::cbind()} and \code{methods:::rbind()} avoid
      deep recursion, thanks to Suharto Anggono via \PR{17300}.

      \item Arithmetic with zero-column data frames now works more
      consistently; issue raised by Bill Dunlap.

      Arithmetic with data frames gives a data frame for \code{^} (which
      previously gave a numeric matrix).

      \item \code{pretty(x, n)} for large \code{n} or large
      \code{diff(range(x))} now works better (though
      it was never meant for large \code{n}); internally it uses the
      same rounding fuzz (1e-10) as \code{seq.default()} --- as it did up
      to 2010-02-03 when both were 1e-7.

      \item Internal C-level \code{R_check_class_and_super()} and hence
      \code{R_check_class_etc()} now also consider non-direct super
      classes and hence return a match in more cases.  This e.g., fixes
      behaviour of derived classes in package \CRANpkg{Matrix}.

      \item Reverted unintended change in behavior of \code{return}
      calls in \code{on.exit} expressions introduced by stack unwinding
      changes in \R 3.3.0.

      \item Attributes on symbols are now detected and prevented; attempt
      to add an attribute to a symbol results in an error.

      \item \code{fisher.test(*, workspace = <n>)} now may also increase
      the internal stack size which allows larger problem to be solved,
      fixing \PR{1662}.

      \item The \pkg{methods} package no longer directly copies slots
      (attributes) into a prototype that is of an \dQuote{abnormal}
      (reference) type, like a symbol.

      \item The \pkg{methods} package no longer attempts to call
      \code{length<-()} on \code{NULL} (during the bootstrap process).

      \item The \pkg{methods} package correctly shows methods when there
      are multiple methods with the same signature for the same generic
      (still not fully supported, but at least the user can see them).

      \item \code{sys.on.exit()} is now always evaluated in the right
      frame. (From Lionel Henry.)

      \item \code{seq.POSIXt(*, by = "<n> DSTdays")} now should work
      correctly in all cases and is faster.  (\PR{17342})

      \item \code{.C()} when returning a logical vector now always maps
      values other than FALSE and NA to TRUE (as documented).

      \item Subassignment with zero length vectors now coerces as
      documented (\PR{17344}).\cr
      Further, \code{x <- numeric(); x[1] <- character()} now signals
      an error \sQuote{\verb{replacement has length zero}} (or a
      translation of that) instead of doing nothing.

      \item (Package \pkg{parallel}.)  \code{mclapply()}, \code{pvec()}
      and \code{mcparallel()} (when \code{mccollect()} is used to
      collect results) no longer leave zombie processes behind.

      \item \code{R CMD INSTALL <pkg>} now produces the intended error
      message when, e.g., the \code{LazyData} field is invalid.

      \item \code{as.matrix(dd)} now works when the data frame \code{dd}
      contains a column which is a data frame or matrix, including a
      0-column matrix/d.f. .

      \item \code{mclapply(X, mc.cores)} now follows its documentation
      and calls \code{lapply()} in case \code{mc.cores = 1} also in the
      case \code{mc.preschedule} is false.  (\PR{17373})

      \item \code{aggregate(<data.frame>, drop=FALSE)} no longer calls
      the function on <empty> parts but sets corresponding results to
      \code{NA}.  (Thanks to Suharto Anggono's patches in \PR{17280}).

      \item The \code{duplicated()} method for data frames is now based
      on the \code{list} method (instead of string coercion).  Consequently
      \code{unique()} is better distinguishing data frame rows, fixing
      \PR{17369} and \PR{17381}.  The methods for matrices and arrays
      are changed accordingly.

      \item Calling \code{names()} on an S4 object derived from
      \code{"environment"} behaves (by default) like calling
      \code{names()} on an ordinary environment.

      \item \code{read.table()} with a non-default separator now
      supports quotes following a non-whitespace character, matching the
      behavior of \code{scan()}.

      \item \code{parLapplyLB} and \code{parSapplyLB} have been fixed to do
      load balancing (dynamic scheduling).  This also means that results of
      computations depending on random number generators will now really be
      non-reproducible, as documented.

      \item Indexing a list using dollar and empty string (\code{l$""})
      returns NULL.

      \item Using \code{ \\usage\{ data(<name>, package="<pkg>") \} }
      no longer produces \command{R CMD check} warnings.

      \item \code{match.arg()} more carefully chooses the environment
      for constructing default \code{choices}, fixing \PR{17401} as
      proposed by Duncan Murdoch.

      \item Deparsing of consecutive \code{!} calls is now consistent
      with deparsing unary \code{-} and \code{+} calls and creates code
      that can be reparsed exactly; thanks to a patch by Lionel Henry in
      \PR{17397}.  (As a side effect, this uses fewer parentheses in
      some other deparsing involving \code{!} calls.)
    }
  }
}

\section{\Rlogo CHANGES IN R 3.4.4}{

  \subsection{NEW FEATURES}{
    \itemize{
      \item \code{Sys.timezone()} tries more heuristics on Unix-alikes and so
      is more likely to succeed (especially on Linux).  For the slowest
      method, a warning is given recommending that \env{TZ} is set to
      avoid the search.

      \item The version of LAPACK included in the sources has been
      updated to 3.8.0 (for the routines used by \R, a very minor bug-fix
      change).

      \item \code{parallel::detectCores(logical = FALSE)} is ignored on
      Linux systems, since the information is not available with
      virtualized OSes.
    }
  }

  \subsection{INSTALLATION on a UNIX-ALIKE}{
    \itemize{
      \item \command{configure} will use \command{pkg-config} to find the
      flags to link to \code{jpeg} if available (as it should be for the
      recently-released \code{jpeg-9c} and \code{libjpeg-turbo}).  (This
      amends the code added in \R 3.3.0 as the module name in
      \code{jpeg-9c} is not what that tested for.)
    }
  }

  \subsection{DEPRECATED AND DEFUNCT}{
    \itemize{
      \item \code{Sys.timezone(location = FALSE)} (which was a stop-gap
      measure for Windows long ago) is deprecated.  It no longer returns
      the value of environment variable \env{TZ} (usually a location).

      \item Legacy support of \command{make} macros such as \samp{CXX1X}
      is formally deprecated: use the \samp{CXX11} forms instead.
    }
  }

  \subsection{BUG FIXES}{
    \itemize{
      \item \code{power.prop.test()} now warns when it cannot solve the
      problem, typically because of impossible constraints. (\PR{17345})

      \item \code{removeSource()} no longer erroneously removes
      \code{NULL} in certain cases, thanks to Dénes Tóth.

      \item \code{nls(`NO [mol/l]` ~ f(t))} and \code{nls(y ~ a)} now
      work.  (Partly from \PR{17367})

      \item \command{R CMD build} checks for GNU \command{cp} rather
      than assuming Linux has it.  (\PR{17370} says \sQuote{Alpine
	Linux} does not.)

      \item Non-UTF-8 multibyte character handling fixed more
      permanently (\PR{16732}).

      \item \code{sum(<large ints>, <stuff>)} is more consistent.
      (\PR{17372})

      \item \code{rf()} and \code{rbeta()} now also work correctly when
      \code{ncp} is not scalar, notably when (partly) \code{NA}.
      (\PR{17375})

      \item \code{is.na(NULL)} no longer warns.  (\PR{16107})

      \item \command{R CMD INSTALL} now correctly sets C++ compiler flags
      when all source files are in sub-directories of \file{src}.
    }
  }
}

\section{\Rlogo CHANGES IN R 3.4.3}{

  \subsection{INSTALLATION on a UNIX-ALIKE}{
    \itemize{
      \item A workaround has been added for the changes in location of
      time-zone files in macOS 10.13 \sQuote{High Sierra} and
      again in 10.13.1, so the default time zone is deduced
      correctly from the system setting when \R is configured with
      \option{--with-internal-tzcode} (the default on macOS).

      \item \command{R CMD javareconf} has been updated to recognize the
      use of a Java 9 SDK on macOS.
    }
  }

  \subsection{BUG FIXES}{
    \itemize{
      \item \code{raw(0) & raw(0)} and \code{raw(0) | raw(0)}
      again return \code{raw(0)} (rather than \code{logical(0)}).

      \item \code{intToUtf8()} converts integers corresponding to
      surrogate code points to \code{NA} rather than invalid UTF-8, as
      well as values larger than the current Unicode maximum of
      \code{0x10FFFF}.  (This aligns with the current RFC3629.)

      \item Fix calling of methods on S4 generics that dispatch on
      \code{\dots} when the call contains \code{\dots}.

      \item Following Unicode \sQuote{Corrigendum 9}, the UTF-8
      representations of U+FFFE and U+FFFF are now regarded as valid by
      \code{utf8ToInt()}.

      \item \code{range(c(TRUE, NA), finite = TRUE)} and similar no longer
      return \code{NA}. (Reported by Lukas Stadler.)

      \item The self starting function \code{attr(SSlogis, "initial")}
      now also works when the y values have exact minimum zero and is
      slightly changed in general, behaving symmetrically in the y range.

      \item The printing of named raw vectors is now formatted nicely as
      for other such atomic vectors, thanks to Lukas Stadler.
    }
  }
}

\section{\Rlogo CHANGES IN R 3.4.2}{

  \subsection{NEW FEATURES}{
    \itemize{
      \item Setting the \code{LC_ALL} category in \code{Sys.setlocale()}
      invalidates any cached locale-specific day/month names and the
      AM/PM indicator for \code{strptime()} (as setting \code{LC_TIME}
      has since \R 3.1.0).

      \item The version of LAPACK included in the sources has been
      updated to 3.7.1, a bug-fix release.

      \item The default for \code{tools::write_PACKAGES(rds_compress=)}
      has been changed to \code{"xz"} to match the compression used by
      \acronym{CRAN}.

      \item \code{c()} and \code{unlist()} are now more efficient in
      constructing the \code{names(.)} of their return value, thanks to
      a proposal by Suharto Anggono.  (\PR{17284})
    }
  }

  \subsection{UTILITIES}{
    \itemize{
      \item \command{R CMD check} checks for and \command{R CMD build}
      corrects CRLF line endings in shell scripts \command{configure}
      and \command{cleanup} (even on Windows).
    }
  }

  \subsection{INSTALLATION on a UNIX-ALIKE}{
    \itemize{
      \item The order of selection of OpenMP flags has been changed:
      Oracle Developer Studio 12.5 accepts \option{-fopenmp} and
      \option{-xopenmp} but only the latter enables OpenMP so it is now
      tried first.
    }
  }

  \subsection{BUG FIXES}{
    \itemize{
      \item \code{within(List, rm(x1, x2))} works correctly again,
      including when \code{List[["x2"]]} is \code{NULL}.

      \item \code{regexec(pattern, text, *)} now applies
      \code{as.character(.)} to its first two arguments, as documented.

      \item \code{write.table()} and related functions,
      \code{writeLines()}, and perhaps other functions writing text to
      connections did not signal errors when the writes failed,
      e.g.\sspace{}due to a disk being full.  Errors will now be
      signalled if detected during the write, warnings if detected when
      the connection is closed.  (\PR{17243})

      \item \code{rt()} assumed the \code{ncp} parameter was a scalar.  (\PR{17306})

      \item \code{menu(choices)} with more than 10 choices which easily
      fit into one \code{getOption("width")}-line no longer erroneously
      repeats choices.  (\PR{17312})

      \item \code{length()<-} on a pairlist succeeds.
      (\url{https://stat.ethz.ch/pipermail/r-devel/2017-July/074680.html})

      \item Language objects such as \code{quote(("\n"))} or \R
      functions are correctly printed again, where \R 3.4.1 accidentally
      duplicated the backslashes.

      \item Construction of \code{names()} for very large objects in
      \code{c()} and \code{unlist()} now works, thanks to Suharto
      Anggono's patch proposals in \PR{17292}.

      \item Resource leaks (and similar) reported by Steve Grubb fixed.
      (\PR{17314}, \PR{17316}, \PR{17317}, \PR{17318}, \PR{17319}, \PR{17320})

      \item \code{model.matrix(~1, mf)} now gets the row names from
      \code{mf} also when they differ from \code{1:nrow(mf)}, fixing
      \PR{14992} thanks to the suggestion by Sebastian Meyer.

      \item \code{sigma(fm)} now takes the correct denominator degrees
      of freedom for a fitted model with \code{NA} coefficients.  (\PR{17313})

      \item \code{hist(x, "FD")} no longer \dQuote{dies} with a somewhat
      cryptic error message when \code{x} has extreme outliers or
      \code{IQR()} zero:  \code{nclass.FD(x)} tries harder to find a
      robust bin width \eqn{h} in the latter case, and
      \code{hist.default(*, breaks)} now checks and corrects a too large
      \code{breaks} number.  (\PR{17274})

      \item \code{callNextMethod()} works for \code{\dots} methods.

      \item \code{qr.coef(qd, y)} now has correct names also when
      \code{qd} is a complex QR or stems from \code{qr(*, LAPACK=TRUE)}.

      \item Setting \code{options(device = *)} to an invalid function no
      longer segfaults when plotting is initiated.  (\PR{15883})

      \item \code{encodeString(<very large string>)} no longer
      segfaults.  (\PR{15885})

      \item It is again possible to use \command{configure
	--enable-maintainer-mode} without having installed
      \command{notangle} (it was required in \R 3.4.[01]).

      \item S4 method dispatch on \code{\dots} calls the method by name
      instead of \code{.Method} (for consistency with default dispatch),
      and only attempts to pass non-missing arguments from the generic.

      \item \code{readRDS(textConnection(.))} works again.  (\PR{17325})

      \item \code{(1:n)[-n]} no longer segfaults for \code{n <- 2.2e9}
      (on a platform with enough RAM).

      \item \code{x <- 1:2; tapply(x, list(x, x), function(x) "")[1,2]}
      now correctly returns \code{NA}.  (\PR{17333})

      \item Running of finalizers after explicit GC request moved from
      the \R interface \code{do_gc} to the \code{C} interface
      \code{R_gc}.  This helps with reclaiming inaccessible connections.

      \item \code{help.search(topic)} and \code{??topic} matching topics
      in vignettes with multiple file name extensions (e.g.,
      \file{*.md.rsp} but not \file{*.Rmd}) failed with an error
      when using \code{options(help_type = "html")}.

      \item The X11 device no longer uses the Xlib backing store (\PR{16497}).

      \item \code{array(character(), 1)} now gives (a 1D array with)
      \code{NA} as has been documented for a long time as in the other
      cases of zero-length array initialization and also compatibly with
      \code{matrix(character(), *)}.  As mentioned there, this also
      fixes \PR{17333}.

      \item \code{splineDesign(.., derivs = 4)} no longer segfaults.

      \item \code{fisher.test(*, hybrid=TRUE)} now (again) will use the
      hybrid method when Cochran's conditions are met, fixing \PR{16654}.
    }
  }
}

\section{\Rlogo CHANGES IN R 3.4.1}{

  \subsection{INSTALLATION on a UNIX-ALIKE}{
    \itemize{
      \item The deprecated support for PCRE versions older than 8.20
      has been removed.
    }
  }

  \subsection{BUG FIXES}{
    \itemize{
      \item \code{getParseData()} gave incorrect column information when
      code contained multi-byte characters.  (\PR{17254})

      \item Asking for help using expressions like \code{?stats::cor()}
      did not work.  (\PR{17250})

      \item \code{readRDS(url(....))} now works.

      \item \command{R CMD Sweave} again returns \samp{status = 0} on
      successful completion.

      \item Vignettes listed in \file{.Rbuildignore} were not being
      ignored properly.  (\PR{17246})

      \item \code{file.mtime()} no longer returns NA on Windows when the
      file or directory is being used by another process.  This affected
      \code{installed.packages()}, which is now protected against this.

      \item \command{R CMD INSTALL} Windows .zip file obeys \code{--lock}
      and \code{--pkglock} flags.

      \item (Windows only)  The \code{choose.files()} function could
      return incorrect results when called with \code{multi = FALSE}.
      (\PR{17270})

      \item \code{aggregate(<data.frame>, drop = FALSE)} now also works in
      case of near-equal numbers in \code{by}.  (\PR{16918})

      \item \code{fourfoldplot()} could encounter integer overflow when
      calculating the odds ratio. (\PR{17286})

      \item \code{parse()} no longer gives spurious warnings when
      extracting srcrefs from a file not encoded in the current locale.

      This was seen from \command{R CMD check} with \file{inst/doc/*.R}
      files, and \code{check} has some additional protection for such files.

      \item \code{print.noquote(x)} now always returns its argument
      \code{x} (invisibly).

      \item Non-UTF-8 multibyte character sets were not handled properly
      in source references.  (\PR{16732})
    }
  }
}

\section{\Rlogo CHANGES IN R 3.4.0}{

  \subsection{SIGNIFICANT USER-VISIBLE CHANGES}{
    \itemize{
      \item (Unix-alike)
      The default methods for \code{download.file()} and \code{url()}
      now choose \code{"libcurl"} except for \samp{file://}
      URLs.  There will be small changes in the format and wording of
      messages, including in rare cases if an issue is a warning or an
      error.  For example, when HTTP re-direction occurs, some messages
      refer to the final URL rather than the specified one.

      Those who use proxies should check that their settings are
      compatible (see \code{?download.file}: the most commonly used
      forms work for both \code{"internal"} and \code{"libcurl"}).

      \item \code{table()} has been amended to be more internally
      consistent and become back compatible to \R \eqn{\le}{<=} 2.7.2
      again.  Consequently, \code{table(1:2, exclude = NULL)} no longer
      contains a zero count for \code{<NA>}, but \code{useNA = "always"}
      continues to do so.

      \item \code{summary.default()} no longer rounds, but its print
      method does resulting in less extraneous rounding, notably of
      numbers in the ten thousands.

      \item \code{factor(x, exclude = L)} behaves more rationally when
      \code{x} or \code{L} are character vectors.  Further,
      \code{exclude = <factor>} now behaves as documented for long.

      \item Arithmetic, logic (\code{&}, \code{|}) and comparison (aka
      \sQuote{relational}, e.g., \code{<}, \code{==}) operations with
      arrays now behave consistently, notably for arrays of length zero.

      Arithmetic between length-1 arrays and longer non-arrays had
      silently dropped the array attributes and recycled.  This now
      gives a warning and will signal an error in the future, as it has
      always for logic and comparison operations in these cases (e.g.,
      compare \code{matrix(1,1) + 2:3} and \code{matrix(1,1) < 2:3}).

      \item The JIT (\sQuote{Just In Time}) byte-code compiler is now
      enabled by default at its level 3. This means functions will be
      compiled on first or second use and top-level loops will be
      compiled and then run.  (Thanks to Tomas Kalibera for extensive
      work to make this possible.)

      For now, the compiler will not compile code containing explicit
      calls to \code{browser()}: this is to support single stepping from
      the \code{browser()} call.

      JIT compilation can be disabled for the rest of the session using
      \code{compiler::enableJIT(0)} or by setting environment variable
      \env{R_ENABLE_JIT} to \code{0}.

      \item \code{xtabs()} works more consistently with \code{NA}s, also
      in its result no longer setting them to \code{0}.  Further, a new
      logical option \code{addNA} allows to count \code{NA}s where
      appropriate.  Additionally, for the case \code{sparse = TRUE}, the
      result's \code{dimnames} are identical to the default case's.

      \item Matrix products now consistently bypass BLAS when the inputs
      have \code{NaN}/\code{Inf} values. Performance of the check of
      inputs has been improved. Performance when BLAS is used is
      improved for matrix/vector and vector/matrix multiplication (DGEMV
      is now used instead of DGEMM).

      One can now choose from alternative matrix product implementations
      \emph{via} \code{options(matprod = )}.  The \code{"internal"}
      implementation is not optimized for speed but consistent in
      precision with other summations in R (using \code{long double}
      accumulators where available).  \code{"blas"} calls BLAS directly
      for best speed, but usually with undefined behavior for inputs
      with \code{NaN}/\code{Inf}.
    }
  }

  \subsection{NEW FEATURES}{
    \itemize{
      \item User errors such as \code{integrate(f, 0:1, 2)} are now caught.

      \item Add \code{signature} argument to \code{debug()},
      \code{debugonce()}, \code{undebug()} and \code{isdebugged()} for
      more conveniently debugging S3 and S4 methods.  (Based on a patch by
      Gabe Becker.)

      \item Add \code{utils::debugcall()} and
      \code{utils::undebugcall()} for debugging the function that would
      be called by evaluating the given expression. When the call is to
      an S4 generic or standard S3 generic, \code{debugcall()} debugs the
      method that would be dispatched. A number of internal utilities
      were added to support this, most notably
      \code{utils::isS3stdGeneric()}.  (Based on a patch by Gabe Becker.)

      \item Add \code{utils::strcapture()}. Given a character vector and
      a regular expression containing capture expressions,
      \code{strcapture()} will extract the captured tokens into a
      tabular data structure, typically a \code{data.frame}.

      \item \code{str()} and \code{strOptions()} get a new option
      \code{drop.deparse.attr} with improved but \emph{changed} default
      behaviour for expressions.  For \code{expression} objects
      \code{x}, \code{str(x)} now may remove extraneous white space and
      truncate long lines.

      \item \code{str(<looooooooong_string>)} is no longer very slow;
      inspired by Mikko Korpela's proposal in \PR{16527}.

      \item \code{str(x)}'s default method is more \dQuote{accurate} and
      hence somewhat more generous in displaying character vectors; this
      will occasionally change \R outputs (and need changes to some
      \file{*.Rout(.save)} files).
      \cr %% same \item
      For a classed integer vector such as \code{x <- xtabs(~ c(1,9,9,9))},
      \code{str(x)} now shows both the class and \code{"int"}, instead
      of only the latter.

      \item \code{isSymmetric(m)} is much faster for large asymmetric
      matrices \code{m} \emph{via} pre-tests and a new option \code{tol1}
      (with which strict back compatibility is possible but not the
      default).

      \item The result of \code{eigen()} now is of class \code{"eigen"}
      in the default case when eigenvectors are computed.

      \item Zero-length date and date-time objects (of classes
      \code{"POSIX[cl]?t"}) now \code{print()} \dQuote{recognizably}.

      \item \code{xy.coords()} and \code{xyz.coords()} get a new
      \code{setLab} option.

      \item The \code{method} argument of \code{sort.list()},
      \code{order()} and \code{sort.int()} gains an \code{"auto"} option
      (the default) which should behave the same as before when
      \code{method} was not supplied.

      \item \code{stopifnot(E, ..)} now reports differences when
      \code{E} is a call to \code{all.equal()} and that is not true.

      \item \code{boxplot(<formula>, *)} gain optional arguments
      \code{drop}, \code{sep}, and \code{lex.order} to pass to
      \code{split.default()} which itself gains an argument
      \code{lex.order} to pass to \code{interaction()} for more
      flexibility.

      \item The \code{plot()} method for \code{ppr()} has enhanced
      default labels (\code{xmin} and \code{main}).

      \item \code{sample.int()} gains an explicit \code{useHash} option
      (with a back compatible default).

      \item \code{identical()} gains an \code{ignore.srcref} option
      which drops \code{"srcref"} and similar attributes when true (as
      by default).

      \item \code{diag(x, nrow = n)} now preserves \code{typeof(x)}, also
      for logical, integer and raw \code{x} (and as previously for
      complex and numeric).

      \item \code{smooth.spline()} now allows direct specification of
      \code{lambda}, gets a \code{hatvalues()} method and keeps
      \code{tol} in the result, and optionally parts of the internal
      matrix computations.

      \item \code{addNA()} is faster now, e.g.\sspace{}when applied twice.
      (Part of \PR{16895}.)

      \item New option \code{rstandard(<lm>, type = "predicted")}
      provides the \dQuote{PRESS}--related leave-one-out
      cross-validation errors for linear models.

      \item After seven years of deprecation, duplicated factor levels
      now produce a warning when printed and an error in \code{levels<-}
      instead of a warning.

      \item Invalid factors, e.g., with duplicated levels (invalid but
      constructable) now give a warning when printed, \emph{via} new
      function \code{.valid.factor()}.

      \item \code{sessionInfo()} has been updated for Apple's change in
      OS naming as from \sQuote{10.12} (\sQuote{macOS Sierra} \emph{vs}
      \sQuote{OS X El Capitan}).

      Its \code{toLatex()} method now includes the \code{running}
      component.

      \item \code{options(interrupt=)} can be used to specify a default
      action for user interrupts.  For now, if this option is not set
      and the \code{error} option is set, then an unhandled user
      interrupt invokes the \code{error} option.  (This may be dropped in
      the future as \code{interrupt} conditions are not \code{error}
      conditions.)

      \item In most cases user interrupt handlers will be called with a
      \code{"resume"} restart available.  Handlers can invoke this
      restart to resume computation. At the browser prompt the \code{r}
      command will invoke a \code{"resume"} restart if one is
      available. Some read operations cannot be resumed properly when
      interrupted and do not provide a \code{"resume"} restart.

      \item Radix sort is now chosen by \code{method = "auto"} for
      \code{sort.int()} for double vectors (and hence used for
      \code{sort()} for unclassed double vectors), excluding
      \sQuote{long} vectors.

      \code{sort.int(method = "radix")} no longer rounds double vectors.

      \item The \code{default} and \code{data.frame} methods for
      \code{stack()} preserve the names of empty elements in the levels
      of the \code{ind} column of the return value.  Set the new
      \code{drop} argument to \code{TRUE} for the previous behavior.

      \item Speedup in \code{simplify2array()} and hence \code{sapply()} and
      \code{mapply()} (for the case of names and common length > 1),
      thanks to Suharto Anggono's \PR{17118}.

      \item \code{table(x, exclude = NULL)} now sets
      \code{useNA = "ifany"} (instead of \code{"always"}).
      Together with the bug fixes for this case, this recovers more
      consistent behaviour  compatible to older versions of \R.  As
      a consequence, \code{summary()} for a logical vector no longer
      reports (zero) counts for \code{NA} when there are no \code{NA}s.

      \item \code{dump.frames()} gets a new option
      \code{include.GlobalEnv} which allows to also dump the global
      environment, thanks to Andreas Kersting's proposal in \PR{17116}.

      \item \code{system.time()} now uses \code{message()} instead of
      \code{cat()} when terminated early, such that
      \code{suppressMessages()} has an effect; suggested by Ben Bolker.

      \item \code{citation()} supports \file{inst/CITATION} files from
      package source trees, with \code{lib.loc} pointing to the
      directory containing the package.

      %% Would need more work (notably in  ../src/main/sort.c )
      %% \item New \code{orderL(lst, ....)} for use in \code{do.call()} to
      %% avoid the name problem seen in \PR{17119}.

      \item \code{try()} gains a new argument \code{outFile} with a
      default that can be modified \emph{via}
      \code{options(try.outFile = .)}, useful notably for \code{Sweave}.

      \item The unexported low-level functions in package \pkg{parallel}
      for passing serialized \R objects to and from forked children now
      support long vectors on 64-bit platforms.  This removes some
      limits on higher-level functions such as \code{mclapply()} (but
      returning gigabyte results from forked processes \emph{via}
      serialization should be avoided if at all possible).

      \item Connections now \code{print()} without error even if
      invalid, e.g.\sspace{}after having been destroyed.

      \item \code{apropos()} and \code{find(simple.words = FALSE)} no
      longer match object names starting with \samp{.} which are known
      to be internal objects (such as \code{.__S3MethodsTable__.}).

      \item Convenience function \code{hasName()} has been added; it
      is intended to replace the common idiom \code{!is.null(x$name)}
      without the usually unintended partial name matching.

      \item \code{strcapture()} no longer fixes column names nor coerces
      strings to factors (suggested by Bill Dunlap).

      \item \code{strcapture()} returns \code{NA} for non-matching
      values in \code{x} (suggested by Bill Dunlap).

      \item \code{source()} gets new optional arguments, notably
      \code{exprs}; this is made use of in the new utility function
      \code{withAutoprint()}.

      \item \code{sys.source()} gets a new \code{toplevel.env} argument.
      This argument is useful for frameworks running package tests;
      contributed by Tomas Kalibera.

      \item \code{Sys.setFileTime()} and
      \code{file.copy(copy.date = TRUE)} will set timestamps with
      fractions of seconds on platforms/filesystems which support this.

      \item (Windows only.) \code{file.info()} now returns file
      timestamps including fractions of seconds; it has done so on other
      platforms since \R 2.14.0.  (NB: some filesystems do not
      record modification and access timestamps to sub-second
      resolution.)

      \item The license check enabled by
      \code{options(checkPackageLicense = TRUE)} is now done when the
      package's namespace is first loaded.

      \item \code{ppr()} and \code{supsmu()} get an optional
      \code{trace} argument, and \code{ppr(.., sm.method = ..spline)} is
      no longer limited to sample size \eqn{n \le 2500}{n <= 2500}.

      \item The \code{POSIXct} method for \code{print()} gets optional
      \code{tz} and \code{usetz} arguments, thanks to a report from
      Jennifer S. Lyon.

      \item New function \code{check_packages_in_dir_details()} in
      package \pkg{tools} for analyzing package-check log files to
      obtain check details.

      \item Package \pkg{tools} now exports function
      \code{CRAN_package_db()} for obtaining information about current
      packages in the \acronym{CRAN} package repository, and several
      functions for obtaining the check status of these packages.

      \item The (default) Stangle driver \code{Rtangle} allows
      \code{annotate} to be a function and gets a new
      \code{drop.evalFALSE} option.

      \item The default method for \code{quantile(x, prob)} should now
      be monotone in \code{prob}, even in border cases, see \PR{16672}.

      \item \code{bug.report()} now tries to extract an email address
      from a \samp{BugReports} field, and if there is none, from a
      \samp{Contacts} field.

      \item The \code{format()} and \code{print()} methods for
      \code{object.size()} results get new options \code{standard} and
      \code{digits}; notably, \code{standard = "IEC"} and
      \code{standard = "SI"} allow more standard (but less common)
      abbreviations than the default ones, e.g.\sspace{}for kilobytes.
      (From contributions by Henrik Bengtsson.)

      \item If a reference class has a validity method,
      \code{validObject} will be called automatically from the
      default initialization method for reference classes.

      \item \code{tapply()} gets new option \code{default = NA}
      allowing to change the previously hardcoded value.

      \item \code{read.dcf()} now consistently interprets any
      \sQuote{whitespace} to be stripped to include newlines.

      \item The maximum number of DLLs that can be loaded into R
      e.g.\sspace{}\emph{via} \code{dyn.load()} can now be increased by
      setting the environment variable \code{R_MAX_NUM_DLLS} before
      starting R.

      \item Assigning to an element of a vector beyond the current
      length now over-allocates by a small fraction. The new vector is
      marked internally as growable, and the true length of the new
      vector is stored in the \code{truelength} field. This makes
      building up a vector result by assigning to the next element
      beyond the current length more efficient, though pre-allocating is
      still preferred.  The implementation is subject to change and not
      intended to be used in packages at this time.

      \item Loading the \pkg{parallel} package namespace no longer sets
      or changes the \code{.Random.seed}, even if \env{R_PARALLEL_PORT}
      is unset.

      NB: This can break reproducibility of output, and did for a
      \acronym{CRAN} package.

      \item Methods \code{"wget"} and \code{"curl"} for
      \code{download.file()} now give an \R error rather than a non-zero
      return value when the external command has a non-zero status.

      \item Encoding name \code{"utf8"} is mapped to \code{"UTF-8"}.
      Many implementations of \code{iconv} accept \code{"utf8"}, but not
      GNU \pkg{libiconv} (including the late 2016 version 1.15).

      \item \code{sessionInfo()} shows the full paths to the library or
      executable files providing the BLAS/LAPACK implementations
      currently in use (not available on Windows).

      \item The binning algorithm used by bandwidth selectors
      \code{bw.ucv()}, \code{bw.bcv()} and \code{bw.SJ()} switches to a
      version linear in the input size \code{n} for \code{n > nb/2}.
      (The calculations are the same, but for larger \code{n/nb} it is
      worth doing the binning in advance.)

      \item There is a new option \code{PCRE_study} which controls when
      \code{grep(perl = TRUE)} and friends \sQuote{study} the compiled
      pattern.  Previously this was done for 11 or more input strings:
      it now defaults to 10 or more (but most examples need many more
      for the difference from studying to be noticeable).

      \item \code{grep(perl = TRUE)} and friends can now make use of
      PCRE's Just-In-Time mechanism, for PCRE \eqn{\ge}{>=} 8.20 on
      platforms where JIT is supported.  It is used by default whenever
      the \code{pattern} is studied (see the previous item).  (Based on
      a patch from Mikko Korpela.)

      This is controlled by a new option \code{PCRE_use_JIT}.

      Note that in general this makes little difference to the speed,
      and may take a little longer: its benefits are most evident on
      strings of thousands of characters.  As a side effect it reduces
      the chances of C stack overflow in the PCRE library on very long
      strings (millions of characters, but see next item).

      Warning: segfaults were seen using PCRE with JIT enabled on 64-bit
      Sparc builds.

      \item There is a new option \code{PCRE_limit_recursion} for
      \code{grep(perl = TRUE)} and friends to set a recursion limit
      taking into account \R's estimate of the remaining C stack space
      (or 10000 if that is not available).  This reduces the chance of C
      stack overflow, but because it is conservative may report a
      non-match (with a warning) in examples that matched before.  By
      default it is enabled if any input string has 1000 or more bytes.
      (\PR{16757})

      \item \code{getGraphicsEvent()} now works on
      \code{X11(type = "cairo")} devices.  Thanks to Frederick Eaton (for
      reviving an earlier patch).

      \item There is a new argument \code{onIdle} for
      \code{getGraphicsEvent()}, which allows an R function to be run
      whenever there are no pending graphics events.  This is currently
      only supported on X11 devices.  Thanks to Frederick Eaton.

      \item The \code{deriv()} and similar functions now can compute
      derivatives of \code{log1p()}, \code{sinpi()} and similar
      one-argument functions, thanks to a contribution by Jerry Lewis.

      \item \code{median()} gains a formal \code{\dots} argument, so
      methods with extra arguments can be provided.

      \item \code{strwrap()} reduces \code{indent} if it is more than
      half \code{width} rather than giving an error.  (Suggested by Bill
      Dunlap.)

      \item When the condition \code{code} in \code{if(.)} or
      \code{while(.)} is not of length one, an error instead of a
      warning may be triggered by setting an environment variable, see
      the help page.

      \item Formatting and printing of bibliography entries
      (\code{bibentry}) is more flexible and better documented.  Apart
      from setting \code{options(citation.bibtex.max = 99)} you can also
      use \code{print(<citation>, bibtex=TRUE)} (or \code{format(..)})
      to get the BibTeX entries in the case of more than one entry.
      This also affects \code{citation()}.
      Contributions to enable \code{style = "html+bibtex"} are welcome.
    }
  }

  \subsection{C-LEVEL FACILITIES}{
    \itemize{
      \item Entry points \code{R_MakeExternalPtrFn} and
      \code{R_ExternalPtrFn} are now declared in header
      \file{Rinternals.h} to facilitate creating and retrieving an \R{}
      external pointer from a C function pointer without ISO C warnings
      about the conversion of function pointers.

      \item There was an exception for the native Solaris C++ compiler
      to the dropping (in \R 3.3.0) of legacy C++ headers from headers
      such as \file{R.h} and \file{Rmath.h} --- this has now been
      removed.  That compiler has strict C++98 compliance hence does not
      include extensions in its (non-legacy) C++ headers: some
      packages will need to request C++11 or replace non-C++98 calls
      such as \code{lgamma}: see §1.6.4 of \sQuote{Writing R Extensions}.

      Because it is needed by about 70 CRAN packages, headers \file{R.h}
      and \file{Rmath.h} still declare \preformatted{use namespace std;}
      when included on Solaris.

      \item When included from C++, the \R headers now use forms such as
      \code{std::FILE} directly rather than including the line \preformatted{using std::FILE;}
      C++ code including these headers might be relying on the latter.

      \item Headers \file{R_ext/BLAS.h} and \file{R_ext/Lapack.h} have
      many improved declarations including \code{const} for
      double-precision complex routines. \emph{Inter alia} this avoids
      warnings when passing \sQuote{string literal} arguments from C++11
      code.

      \item Headers for Unix-only facilities \file{R_ext/GetX11Image.h},
      \file{R_ext/QuartzDevice.h} and \file{R_ext/eventloop.h} are no
      longer installed on Windows.

      \item No-longer-installed headers \file{GraphicsBase.h},
      \file{RGraphics.h}, \file{Rmodules/RX11.h} and
      \file{Rmodules/Rlapack.h} which had a LGPL license no longer do so.

      \item \code{HAVE_UINTPTR_T} is now defined where appropriate by
      \code{Rconfig.h} so that it can be included before
      \code{Rinterface.h} when \code{CSTACK_DEFNS} is defined and a C
      compiler (not C++) is in use.  \code{Rinterface.h} now includes C
      header \file{stdint.h} or C++11 header \file{cstdint} where
      needed.

      \item Package \pkg{tools} has a new function
      \code{package_native_routine_registration_skeleton()} to assist
      adding native-symbol registration to a package.  See its help and
      §5.4.1 of \sQuote{Writing R Extensions} for how to use it.  (At
      the time it was added it successfully automated adding
      registration to over 90\% of \acronym{CRAN} packages which lacked
      it.  Many of the failures were newly-detected bugs in the
      packages, e.g.\sspace50 packages called entry points with varying
      numbers of arguments and 65 packages called entry points not in
      the package.)
      %% 235 fail out of 2460, with 15 others working with
      %% character_only = FALSE.
    }
  }

  \subsection{INSTALLATION on a UNIX-ALIKE}{
    \itemize{
      \item \code{readline} headers (and not just the library) are
      required unless configuring with \option{--with-readline=no}.

      \item \command{configure} now adds a compiler switch for C++11
      code, even if the compiler supports C++11 by default.  (This
      ensures that \command{g++} 6.x uses C++11 mode and not its default
      mode of C++14 with \sQuote{GNU extensions}.)

      The tests for C++11 compliance are now much more comprehensive.
      For gcc < 4.8, the tests from R 3.3.0 are used in order to maintain
      the same behaviour on Linux distributions with long-term support.

      \item An alternative compiler for C++11 is now specified with
      \samp{CXX11}, not \samp{CXX1X}. Likewise C++11 flags are specified
      with \samp{CXX11FLAGS} and the standard (e.g., \samp{-std=gnu++11})
      is specified with \samp{CXX11STD}.

      \item \command{configure} now tests for a C++14-compliant compiler
      by testing some basic features.  This by default tries flags for
      the compiler specified by \samp{CXX11}, but an alternative compiler,
      options and standard can be specified by variables \samp{CXX14},
      \samp{CXX14FLAGS} and \samp{CXX14STD} (e.g., \option{-std=gnu++14}).

      \item There is a new macro \code{CXXSTD} to help specify the
      standard for C++ code, e.g.\sspace{}\option{-std=c++98}.  This
      makes it easier to work with compilers which default to a later
      standard: for example, with \code{CXX=g++6 CXXSTD=-std=c++98}
      \command{configure} will select commands for \command{g++} 6.x
      which conform to C++11 and C++14 where specified but otherwise use
      C++98.

      \item Support for the defunct IRIX and OSF/1 OSes and Alpha CPU
      has been removed.

      \item \command{configure} checks that the compiler specified by
      \samp{$CXX $CXXFLAGS} is able to compile C++ code.

      \item \command{configure} checks for the required header
      \file{sys/select.h} (or \file{sys/time.h} on legacy systems) and
      system call \code{select} and aborts if they are not found.

      \item If available, the POSIX 2008 system call \code{utimensat}
      will be used by \code{Sys.setFileTime()} and
      \code{file.copy(copy.date = TRUE)}.  This may result in slightly
      more accurate file times.  (It is available on Linux and FreeBSD
      but not macOS.)
      %% My Solaris 10 system does not have it, but Oracle's online help does

      \item The minimum version requirement for \code{libcurl} has been
      reduced to 7.22.0, although at least 7.28.0 is preferred and
      earlier versions are little tested.  (This is to support Debian 7
      \sQuote{Wheezy} LTS and Ubuntu \sQuote{Precise} 12.04 LTS,
      although the latter is close to end-of-life.)
      %% Debian Wheezy LTS is supported until 2018-05-31

      \item \command{configure} tests for a C++17-compliant compiler.
      The tests are experimental and subject to change in the future.
    }
  }

  \subsection{INCLUDED SOFTWARE}{
    \itemize{
      \item (Windows only) Tcl/Tk version 8.6.4 is now included in the
      binary builds.  The \file{tcltk*.chm} help file is no longer
      included; please consult the online help at
      \url{http://www.tcl.tk/man/} instead.

      \item The version of LAPACK included in the sources has been
      updated to 3.7.0: no new routines have been added to \R{}.
    }
  }

  \subsection{PACKAGE INSTALLATION}{
    \itemize{
      \item There is support for compiling C++14 or C++17 code in
      packages on suitable platforms: see \sQuote{Writing R Extensions}
      for how to request this.

      \item The order of flags when \samp{LinkingTo} other packages has
      been changed so their include directories come earlier, before
      those specified in \code{CPPFLAGS}.  This will only have an effect
      if non-system include directories are included with \option{-I}
      flags in \code{CPPFLAGS} (and so not the default
      \code{-I/usr/local/include} which is treated as a system include
      directory on most platforms).
      %% This ensures that BH's 'boost' directory gets precedent over
      %% any Boost headers in, say, /opt/csw/include .

      \item Packages which register native routines for \code{.C} or
      \code{.Fortran} need to be re-installed for this version (unless
      installed with R-devel SVN revision r72375 or later).

      \item Make variables with names containing \code{CXX1X} are
      deprecated in favour of those using \code{CXX11}, but for the time
      being are still made available \emph{via} file
      \file{etc/Makeconf}.  Packages using them should be converted to
      the new forms and made dependent on \samp{R (>= 3.4.0)}.
    }
  }

  \subsection{UTILITIES}{
    \itemize{
      \item Running \command{R CMD check --as-cran} with
      \env{_R_CHECK_CRAN_INCOMING_REMOTE_} false now skips tests that
      require remote access.  The remaining (local) tests typically
      run quickly compared to the remote tests.

      \item \command{R CMD build} will now give priority to vignettes
      produced from files in the \file{vignettes} directory over those
      in the \file{inst/doc} directory, with a warning that the latter
      are being ignored.

      \item \command{R CMD config} gains a \option{--all} option for
      printing names and values of all basic configure variables.

      It now knows about all the variables used for the C++98, C++11 and
      C++14 standards.

      \item \command{R CMD check} now checks that output files in
      \file{inst/doc} are newer than the source files in \file{vignettes}.

      \item For consistency with other package subdirectories, files
      named \file{*.r} in the \file{tests} directory are now recognized
      as tests by \command{R CMD check}. (Wish of \PR{17143}.)

      \item \command{R CMD build} and \command{R CMD check} now use the
      \emph{union} of \env{R_LIBS} and \code{.libPaths()}.  They may not
      be equivalent, e.g., when the latter is determined by
      \env{R_PROFILE}.

      \item \command{R CMD build} now preserves dates when it copies files
      in preparing the tarball.  (Previously on Windows it changed the
      dates on all files; on Unix, it changed some dates when installing
      vignettes.)

      \item The new option \command{R CMD check --no-stop-on-test-error}
      allows running the remaining tests (under \file{tests/}) even if
      one gave an error.

      \item Check customization \emph{via} environment variables to
      detect side effects of \code{.Call()} and \code{.External()} calls
      which alter their arguments is described in §8 of the \sQuote{R
      Internals} manual.

      \item \command{R CMD check} now checks any \samp{BugReports} field
      to be non-empty and a suitable single URL.

      \item \command{R CMD check --as-cran} now NOTEs if the package
      does not register its native routines or does not declare its
      intentions on (native) symbol search.  (This will become a WARNING
      in due course.)
    }
  }

  \subsection{DEPRECATED AND DEFUNCT}{
    \itemize{
      \item (Windows only) Function \code{setInternet2()} is defunct.

      \item Installation support for \code{readline} emulations based on
      \code{editline} (aka \code{libedit}) is deprecated.

      \item Use of the C/C++ macro \samp{NO_C_HEADERS} is defunct and
      silently ignored.

      \item \code{unix.time()}, a traditional synonym for
      \code{system.time()}, has been deprecated.

      \item \code{structure(NULL, ..)} is now deprecated as you cannot
      set attributes on \code{\link{NULL}}.

      \item Header \file{Rconfig.h} no longer defines
      \samp{SUPPORT_OPENMP}; instead use \samp{_OPENMP} (as documented
      for a long time).

      \item (C-level Native routine registration.)  The deprecated
      \code{styles} member of the \code{R_CMethodDef} and
      \code{R_FortranMethodDef} structures has been removed.
      Packages using these will need to be re-installed for \R 3.4.0.

      \item The deprecated support for PCRE versions older than 8.20
      will be removed in \R 3.4.1. (Versions 8.20--8.31 will still be
      accepted but remain deprecated.)
    }
  }

  \subsection{BUG FIXES}{
    \itemize{
      \item Getting or setting \code{body()} or \code{formals()} on
      non-functions for now signals a warning and may become an error
      for setting.

      \item \code{match(x, t)}, \code{duplicated(x)} and
      \code{unique(x)} work as documented for complex numbers with \code{NA}s
      or \code{NaN}s, where all those containing \code{NA} do match,
      whereas in the case of \code{NaN}'s both real and imaginary parts
      must match, compatibly with how \code{print()} and \code{format()}
      work for complex numbers.

      \item \code{deparse(<complex>, options = "digits17")} prints more
      nicely now, mostly thanks to a suggestion by Richie Cotton.

      \item Rotated symbols in plotmath expressions are now positioned
      correctly on \code{x11(type = "Xlib")}. (\PR{16948})

      \item \code{as<-()} avoids an infinite loop when a virtual class
      is interposed between a subclass and an actual superclass.

      \item Fix level propagation in \code{unlist()} when the list
      contains zero-length lists or factors.

      \item Fix S3 dispatch on S4 objects when the \pkg{methods} package is
      not attached.

      \item Internal S4 dispatch sets \code{.Generic} in the method
      frame for consistency with \code{standardGeneric()}.  (\PR{16929})

      \item Fix \code{order(x, decreasing = TRUE)} when \code{x} is an
      integer vector containing \code{MAX_INT}.  Ported from a fix Matt
      Dowle made to \CRANpkg{data.table}.

      \item Fix caching by \code{callNextMethod()}, resolves \PR{16973}
      and \PR{16974}.

      \item \code{grouping()} puts NAs last, to be consistent with the
      default behavior of \code{order()}.

      \item Point mass limit cases: \code{qpois(-2, 0)} now gives
      \code{NaN} with a warning and \code{qgeom(1, 1)} is \code{0}.
      (\PR{16972})

      \item \code{table()} no longer drops an \code{"NaN"} factor level,
      and better obeys \code{exclude = <chr>}, thanks to Suharto Anggono's
      patch for \PR{16936}.  Also, in the case of \code{exclude = NULL}
      and \code{NA}s, these are tabulated correctly (again).

      Further, \code{table(1:2, exclude = 1, useNA = "ifany")} no longer
      erroneously reports \code{<NA>} counts.

      Additionally, all cases of empty \code{exclude} are equivalent,
      and \code{useNA} is not overwritten when specified (as it was by
      \code{exclude = NULL}).

      \item \code{wilcox.test(x, conf.int=TRUE)} no longer errors out in
      cases where the confidence interval is not available, such as for
      \code{x = 0:2}.

      \item \code{droplevels(f)} now keeps <NA> levels when present.

      \item In integer arithmetic, \code{NULL} is now treated as
      \code{integer(0)} whereas it was previously treated as \code{double(0)}.

      \item The radix sort considers \code{NA_real_} and \code{NaN} to
      be equivalent in rank (like the other sort algorithms).

      \item When \code{index.return=TRUE} is passed to \code{sort.int()},
      the radix sort treats \code{NA}s like \code{sort.list()} does
      (like the other sort algorithms).

      \item When in \code{tabulate(bin, nbin)} \code{length(bin)} is
      larger than the maximal integer, the result is now of type
      \code{double} and hence no longer silently overflows to wrong
      values.  (\PR{17140})

      \item \code{as.character.factor()} respects S4 inheritance when
      checking the type of its argument. (\PR{17141})

      \item The \code{factor} method for \code{print()} no longer sets
      the class of the factor to \code{NULL}, which would violate a
      basic constraint of an S4 object.

      \item \code{formatC(x, flag = f)} allows two new flags, and
      signals an error for invalid flags also in the case of character
      formatting.

      \item Reading from \code{file("stdin")} now also closes the
      connection and hence no longer leaks memory when reading from a
      full pipe, thanks to Gábor Csárdi, see thread starting at
      \url{https://stat.ethz.ch/pipermail/r-devel/2016-November/073360.html}.

      \item Failure to create file in \code{tempdir()} for compressed
      \code{pdf()} graphics device no longer errors (then later
      segfaults).  There is now a warning instead of error and
      compression is turned off for the device.  Thanks to Alec
      Wysoker (\PR{17191}).

      \item Asking for \code{methods()} on \code{"|"} returns only S3
      methods. See
      \url{https://stat.ethz.ch/pipermail/r-devel/2016-December/073476.html}.

      \item \code{dev.capture()} using Quartz Cocoa device (macOS) returned
      invalid components if the back-end chose to use ARGB instead of RGBA
      image format. (Reported by Noam Ross.)

      \item \code{seq("2", "5")} now works too, equivalently to
      \code{"2":"5"} and \code{seq.int()}.

      \item \code{seq.int(to = 1, by = 1)} is now correct, other cases are
      integer (instead of \code{double}) when \code{seq()} is integer
      too, and the "non-finite" error messages are consistent between
      \code{seq.default()} and \code{seq.int()}, no longer mentioning
      \code{NaN} etc.

      \item \code{rep(x, times)} and \code{rep.int(x, times)} now work
      when \code{times} is larger than the largest value representable
      in an integer vector.  (\PR{16932})

      \item \code{download.file(method = "libcurl")} does not check for
      URL existence before attempting downloads; this is more robust to
      servers that do not support HEAD or range-based retrieval, but may
      create empty or incomplete files for aborted download requests.

      \item Bandwidth selectors \code{bw.ucv()}, \code{bw.bcv()} and
      \code{bw.SJ()} now avoid integer overflow for large sample sizes.

      \item \code{str()} no longer shows \code{"list output truncated"},
      in cases that list was not shown at all.  Thanks to Neal Fultz
      (\PR{17219})

      \item Fix for \code{cairo_pdf()} (and \code{svg()} and
      \code{cairo_ps()}) when replaying a saved display list that contains
      a mix of \pkg{grid} and \pkg{graphics} output.
      (Report by Yihui Xie.)

      %% It never failed in a released version of R: pre-3.3.3 did
      %% \item \code{rep(3, list(4))} works again, as \code{\link{list}}s
      %% \emph{are} vectors.

      \item The \code{str()} and \code{as.hclust()} methods for
      \code{"dendrogram"} now also work for deeply nested dendrograms
      thanks to non-recursive implementations by Bradley Broom.

      \item \code{sample()} now uses two uniforms for added precision
      when the uniform generator is \code{Knuth-TAOCP},
      \code{Knuth-TAOCP-2002}, or a user-defined generator and the
      population size is \eqn{2^{25}}{2^25} or greater.

      \item If a vignette in the \file{vignettes} directory is listed in
      \file{.Rbuildignore}, \command{R CMD build} would not include it
      in the tarball, but would include it in the vignette database,
      leading to a check warning.  (\PR{17246})

      \item \code{tools::latexToUtf8()} infinite looped on certain
      inputs.  (\PR{17138})

      \item \code{terms.formula()} ignored argument names when determining
      whether two terms were identical.  (\PR{17235})

      \item \code{callNextMethod()} was broken when called from a method
      that augments the formal arguments of a primitive generic.

      \item Coercion of an S4 object to a vector during sub-assignment
      into a vector failed to dispatch through the \code{as.vector()}
      generic (often leading to a segfault).

      \item Fix problems in command completion:  Crash (\PR{17222})
      and junk display in Windows, handling special characters in
      filenames on all systems.
    }
  }
}

\section{\Rlogo CHANGES IN R 3.3.3}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item Changes when redirection of a \samp{http://} URL to a
      \samp{https://} URL is encountered:

      \itemize{
	\item The internal methods of \code{download.file()} and
	\code{url()} now report that they cannot follow this (rather
	than failing silently).

	\item (Unix-alike) \code{download.file(method = "auto")} (the
	default) re-tries with \code{method = "libcurl"}.

	\item (Unix-alike) \code{url(method = "default")} with an
	explicit \code{open} argument re-tries with
	\code{method = "libcurl"}.  This covers many of the usages,
	e.g.\sspace{}\code{readLines()} with a URL argument.
      }
    }
  }

  \subsection{INSTALLATION on a UNIX-ALIKE}{
    \itemize{
      \item The \command{configure} check for the \code{zlib} version is
      now robust to versions longer than 5 characters, including
      \code{1.2.11}.
    }
  }

  \subsection{UTILITIES}{
    \itemize{
      \item Environmental variable \env{_R_CHECK_TESTS_NLINES_} controls
      how \command{R CMD check} reports failing tests (see §8 of the
      \sQuote{R Internals} manual).
    }
  }

  \subsection{DEPRECATED AND DEFUNCT}{
    \itemize{
      \item (C-level Native routine registration.)  The undocumented
      \code{styles} field of the components of \code{R_CMethodDef} and
      \code{R_FortranMethodDef} is deprecated.
    }
  }

  \subsection{BUG FIXES}{
    \itemize{
      \item \code{vapply(x, *)} now works with long vectors \code{x}.
      (\PR{17174})

      \item \code{isS3method("is.na.data.frame")} and similar are
      correct now.  (\PR{17171})

      \item \code{grepRaw(<long>, <short>, fixed = TRUE)} now works,
      thanks to a patch by Mikko Korpela.  (\PR{17132})

      \item Package installation into a library where the package exists
      \emph{via} symbolic link now should work wherever
      \code{Sys.readlink()} works, resolving \PR{16725}.

      \item \code{"Cincinnati"} was missing an \code{"n"} in the
      \code{precip} dataset.

      \item Fix buffer overflow vulnerability in \code{pdf()} when
      loading an encoding file.  Reported by Talos (TALOS-2016-0227).

      \item \code{getDLLRegisteredRoutines()} now produces its warning
      correctly when multiple DLLs match, thanks to Matt Dowle's \PR{17184}.

      \item \code{Sys.timezone()} now returns non-NA also on platforms
      such as \samp{Ubuntu 14.04.5 LTS}, thanks to Mikko Korpela's
      \PR{17186}.

      \item \code{format(x)} for an illegal \code{"POSIXlt"} object
      \code{x} no longer segfaults.

      \item \code{methods(f)} now also works for \code{f} \code{"("}
      or \code{"{"}.

      \item (Windows only)  \code{dir.create()} did not check the length
      of the path to create, and so could overflow a buffer and crash
      \R.  (\PR{17206})

      \item On some systems, very small hexadecimal numbers in hex notation
      would underflow to zero.  (\PR{17199})

      \item \code{pmin()} and \code{pmax()} now work again for
      \code{ordered} factors and 0-length S3 classed objects, thanks to
      Suharto Anggono's \PR{17195} and \PR{17200}.

      \item \code{bug.report()} did not do any validity checking on a
      package's \samp{BugReports} field.  It now ignores an empty field,
      removes leading whitespace and only attempts to open
      \samp{http://} and \samp{https://} URLs, falling back to emailing
      the maintainer.

      \item Bandwidth selectors \code{bw.ucv()} and
      \code{bw.SJ()} gave incorrect answers or incorrectly reported an
      error (because of integer overflow) for inputs longer than
      46341.  Similarly for \code{bw.bcv()} at length 5793.

      Another possible integer overflow is checked and may result in an
      error report (rather than an incorrect result) for much longer
      inputs (millions for a smooth distribution).

      \item \code{findMethod()} failed if the active signature had
      expanded beyond what a particular package used. (Example with
      packages \CRANpkg{XR} and \CRANpkg{XRJulia} on \acronym{CRAN}.)

      \item \code{qbeta()} underflowed too early in some very asymmetric
      cases.  (\PR{17178})

      \item \command{R CMD Rd2pdf} had problems with packages with
      non-ASCII titles in \file{.Rd} files (usually the titles were
      omitted).
    }
  }
}

\section{\Rlogo CHANGES IN R 3.3.2}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item \code{extSoftVersion()} now reports the version (if any) of
      the \code{readline} library in use.

      \item The version of LAPACK included in the sources has been
      updated to 3.6.1, a bug-fix release including a speedup for the
      non-symmetric case of \code{eigen()}.

      \item Use \code{options(deparse.max.lines=)} to limit the number
      of lines recorded in \code{.Traceback} and other deparsing
      activities.

      \item \code{format(<AsIs>)} looks more regular, also for
      non-character atomic matrices.

      \item \code{abbreviate()} gains an option \code{named = TRUE}.

      \item The online documentation for package \pkg{methods} is
      extensively rewritten.  The goals are to simplify
      documentation for basic use, to note old features not
      recommended and to correct out-of-date information.

      \item Calls to \code{setMethod()} no longer print a message when
      creating a generic function in those cases where that is
      natural: S3 generics and primitives.
    }
  }

  \subsection{INSTALLATION and INCLUDED SOFTWARE}{
    \itemize{
      \item Versions of the \code{readline} library >= 6.3 had been
      changed so that terminal window resizes were not signalled to
      \code{readline}: code has been added using a explicit signal
      handler to work around that (when \R is compiled against readline
      >= 6.3).  (\PR{16604})

      \item \command{configure} works better with Oracle Developer
      Studio 12.5.
    }
  }

  \subsection{UTILITIES}{
    \itemize{
      \item \command{R CMD check} reports more dubious flags in files
      \file{src/Makevars[.in]}, including \option{-w} and \option{-g}.

      \item \command{R CMD check} has been set up to filter important
      warnings from recent versions of \command{gfortran} with
      \option{-Wall -pedantic}: this now reports non-portable GNU
      extensions such as out-of-order declarations.

      \item \command{R CMD config} works better with paths containing
      spaces, even those of home directories (as reported by Ken Beath).
    }
  }

  \subsection{DEPRECATED AND DEFUNCT}{
    \itemize{
      \item Use of the C/C++ macro \samp{NO_C_HEADERS} is deprecated
      (no C headers are included by \R headers from C++ as from \R
      3.3.0, so it should no longer be needed).
    }
  }

  \subsection{BUG FIXES}{
    \itemize{
      \item The check for non-portable flags in \command{R CMD check}
      could be stymied by \file{src/Makevars} files which contained
      targets.

      \item (Windows only) When using certain desktop themes in Windows
      7 or higher, \kbd{Alt-Tab} could cause \command{Rterm} to stop
      accepting input.  (\PR{14406}; patch submitted by Jan Gleixner.)

      \item \code{pretty(d, ..)} behaves better for date-time \code{d}
      (\PR{16923}).

      \item When an S4 class name matches multiple classes in the S4
      cache, perform a dynamic search in order to obey namespace
      imports.  This should eliminate annoying messages about multiple
      hits in the class cache.  Also, pass along the package from the
      \code{ClassExtends} object when looking up superclasses in the
      cache.

      \item \code{sample(NA_real_)} now works.

      \item Packages using non-ASCII encodings in their code did not
      install data properly on systems using different encodings.

      \item \code{merge(df1, df2)} now also works for data frames with
      column names \code{"na.last"}, \code{"decreasing"}, or
      \code{"method"}.  (\PR{17119})

      \item \code{contour()} caused a segfault if the \code{labels}
      argument had length zero.  (Reported by Bill Dunlap.)

      \item \code{unique(warnings())} works more correctly, thanks to a
      new \code{duplicated.warnings()} method.

      \item \code{findInterval(x, vec = numeric(), all.inside = TRUE)}
      now returns \code{0}s as documented.  (Reported by Bill Dunlap.)

      \item (Windows only) \command{R CMD SHLIB} failed when a symbol
      in the resulting library had the same name as a keyword in
      the \file{.def} file.  (\PR{17130})

      \item \code{pmax()} and \code{pmin()} now work with (more ?)
      classed objects, such as \code{"Matrix"} from the \CRANpkg{Matrix}
      package, as documented for a long time.

      \item \code{axis(side, x = D)} and hence \code{Axis()} and
      \code{plot()} now work correctly for \code{"Date"} and time
      objects \code{D}, even when \dQuote{time goes backward}, e.g.,
      with decreasing \code{xlim}.  (Reported by William May.)

      \item \code{str(I(matrix(..)))} now looks as always intended.

      \item \code{plot.ts()}, the \code{plot()} method for time series,
      now respects \code{cex}, \code{lwd} and \code{lty}.  (Reported by
      Greg Werbin.)

      \item \code{parallel::mccollect()} now returns a named list (as
      documented) when called with \code{wait = FALSE}.  (Reported by
      Michel Lang.)

      \item If a package added a class to a class union in another
      package, loading the first package gave erroneous warnings
      about \dQuote{undefined subclass}.

      \item \code{c()}'s argument \code{use.names} is documented now, as
      belonging to the (C internal) default method.   In
      \dQuote{parallel}, argument \code{recursive} is also moved from
      the generic to the default method, such that the formal argument list
      of \pkg{base} generic \code{c()} is just \code{(...)}.

      \item \code{rbeta(4, NA)} and similarly \code{rgamma()} and
      \code{rnbinom()} now return \code{NaN}'s with a warning, as other
      r<dist>(), and as documented.  (\PR{17155})

      \item Using \code{options(checkPackageLicense = TRUE)} no longer
      requires acceptance of the licence for non-default standard
      packages such as \pkg{compiler}.  (Reported by Mikko Korpela.)

      \item \code{split(<very_long>, *)} now works even when the split
      off parts are long. (\PR{17139})

      \item \code{min()} and \code{max()} now also work correctly when
      the argument list starts with \code{character(0)}.  (\PR{17160})

      \item Subsetting very large matrices (\code{prod(dim(.)) >= 2^31})
      now works thanks to Michael Schubmehl's \PR{17158}.

      \item \code{bartlett.test()} used residual sums of squares
      instead of variances, when the argument was a list of \code{lm} objects.
      (Reported by Jens Ledet Jensen).

      \item \code{plot(<lm>, which = *)} now correctly labels the
      contour lines for the standardized residuals for \code{which = 6}.
      It also takes the correct \eqn{p} in case of singularities (also
      for \code{which = 5}).  (\PR{17161})

      \item \code{xtabs(~ exclude)} no longer fails from wrong scope,
      thanks to Suharto Anggono's \PR{17147}.

      \item Reference class calls to \code{methods()} did not
      re-analyse previously defined methods, meaning that calls to
      methods defined later would fail. (Reported by Charles Tilford).

      \item \code{findInterval(x, vec, left.open = TRUE)} misbehaved in some
      cases. (Reported by Dmitriy Chernykh.)
    }
  }
}

\section{\Rlogo CHANGES IN R 3.3.1}{
  \subsection{BUG FIXES}{
    \itemize{
      \item \command{R CMD INSTALL} and hence \code{install.packages()}
      gave an internal error installing a package called
      \pkg{description} from a tarball on a case-insensitive file system.

      \item \code{match(x, t)} (and hence \code{x \%in\% t}) failed
      when \code{x} was of length one, and either \code{character} and \code{x}
      and \code{t} only differed in their \code{Encoding} or when
      \code{x} and \code{t} where \code{complex} with \code{NA}s or \code{NaN}s.
      (\PR{16885}.)

      \item \code{unloadNamespace(ns)} also works again when \code{ns} is a
      \sQuote{namespace}, as from \code{getNamespace()}.

      \item \code{rgamma(1, Inf)} or \code{rgamma(1, 0, 0)} no longer give
      \code{NaN} but the correct limit.

      \item \code{length(baseenv())} is correct now.

      \item \code{pretty(d, ..)} for date-time \code{d} rarely failed
      when \code{"halfmonth"} time steps were tried (\PR{16923}) and
      on \sQuote{inaccurate} platforms such as 32-bit Windows or
      a configuration with \command{--disable-long-double}; see comment
      #15 of \PR{16761}.

      \item In \code{text.default(x, y, labels)}, the rarely(?) used
      default for \code{labels} is now correct also for the case of a
      2-column matrix \code{x} and missing \code{y}.

      \item \code{as.factor(c(a = 1L))} preserves \code{names()} again
      as in \R < 3.1.0.

      \item \code{strtrim(""[0], 0[0])} now works.

      \item Use of \code{Ctrl-C} to terminate a reverse incremental
      search started by \code{Ctrl-R} in the \code{readline}-based Unix
      terminal interface is now supported when \R{} was compiled against
      \code{readline} >= 6.0 (\code{Ctrl-G} always worked).
      (\PR{16603})

      \item \code{diff(<difftime>)} now keeps the \code{"units"}
      attribute, as subtraction already did, \PR{16940}.
    }
  }
}

\section{\Rlogo CHANGES IN R 3.3.0}{
  \subsection{SIGNIFICANT USER-VISIBLE CHANGES}{
    \itemize{
      \item \code{nchar(x, *)}'s argument \code{keepNA} governing how
      the result for \code{NA}s in \code{x} is determined, gets a new
      default \code{keepNA = NA} which returns \code{NA} where
      \code{x} is \code{NA}, except for \code{type = "width"} which still
      returns \code{2}, the formatting / printing width of \code{NA}.

      \item All builds have support for \samp{https:} URLs in the
      default methods for \code{download.file()}, \code{url()} and code
      making use of them.

      Unfortunately that cannot guarantee that any particular
      \samp{https:} URL can be accessed.  For example, server and client
      have to successfully negotiate a cryptographic protocol (TLS/SSL,
      \dots) and the server's identity has to be verifiable \emph{via}
      the available certificates.  Different access methods may allow
      different protocols or use private certificate bundles: we
      encountered a \samp{https:} CRAN mirror which could be accessed by
      one browser but not by another nor by \code{download.file()} on
      the same Linux machine.
    }
  }

  \subsection{NEW FEATURES}{
    \itemize{
      \item The \code{print} method for \code{methods()} gains a
      \code{byclass} argument.

      \item New functions \code{validEnc()} and \code{validUTF8()} to
      give access to the validity checks for inputs used by
      \code{grep()} and friends.

      \item Experimental new functionality for S3 method checking, notably
      \code{isS3method()}.

      Also, the names of the \R \sQuote{language elements} are exported
      as character vector \code{tools::langElts}.

      \item \code{str(x)} now displays \code{"Time-Series"} also for
      matrix (multivariate) time-series, i.e. when \code{is.ts(x)} is true.

      \item (Windows only)  The GUI menu item to install local packages
      now accepts \file{*.tar.gz} files as well as \file{*.zip} files
      (but defaults to the latter).

      \item New programmeR's utility function \code{chkDots()}.

      \item \code{D()} now signals an error when given invalid input,
      rather than silently returning \code{NA}.  (Request of John Nash.)

      \item \code{formula} objects are slightly more \dQuote{first class}:
      e.g., \code{formula()} or \code{new("formula", y ~ x)} are now
      valid.  Similarly, for \code{"table"}, \code{"ordered"} and
      \code{"summary.table"}.  Packages defining S4 classes with the above
      S3/S4 classes as slots should be reinstalled.
      % notably if their classes are reused in other
      % packages. e.g. re-install arules for arulesSequence, flexmix for betareg

      \item New function \code{strrep()} for repeating the elements of a
      character vector.

      \item \code{rapply()} preserves attributes on the list when
      \code{how = "replace"}.

      \item New S3 generic function \code{sigma()} with methods for
      extracting the estimated standard deviation aka \dQuote{residual
	standard deviation} from a fitted model.

      \item \code{news()} now displays \R and package news files within
      the HTML help system if it is available.  If no news file is found,
      a visible \code{NULL} is returned to the console.

      \item \code{as.raster(x)} now also accepts \code{raw} arrays
      \code{x} assuming values in \code{0:255}.

      \item Subscripting of matrix/array objects of type
      \code{"expression"} is now supported.

      \item \code{type.convert("i")} now returns a factor instead of a
      complex value with zero real part and missing imaginary part.

      \item Graphics devices \code{cairo_pdf()} and \code{cairo_ps()}
      now allow non-default values of the cairographics \sQuote{fallback
	resolution} to be set.

      This now defaults to 300 on all platforms: that is the default
      documented by cairographics, but apparently was not used by all
      system installations.

      \item \code{file()} gains an explicit \code{method} argument
      rather than implicitly using
      \code{getOption("url.method", "default")}.

      \item Thanks to a patch from Tomas Kalibera, \code{x[x != 0]} is
      now typically faster than \code{x[which(x != 0)]} (in the case
      where \code{x} has no NAs, the two are equivalent).

      \item \code{read.table()} now always uses the names for a named
      \code{colClasses} argument (previously names were only used when
      \code{colClasses} was too short). (In part, wish of \PR{16478}.)

      \item (Windows only) \code{download.file()} with default
      \code{method = "auto"} and a \samp{ftps://} URL chooses
      \code{"libcurl"} if that is available.

      \item The out-of-the box Bioconductor mirror has been changed to
      one using \samp{https://}: use \code{chooseBioCmirror()} to choose
      a \samp{http://} mirror if required.

      \item The data frame and formula methods for \code{aggregate()}
      gain a \code{drop} argument.

      \item \code{available.packages()} gains a \code{repos} argument.

      \item The undocumented switching of methods for \code{url()} on
      \samp{https:} and \samp{ftps:} URLs is confined to
      \code{method = "default"} (and documented).

      \item \code{smoothScatter()} gains a \code{ret.selection} argument.

      \item \code{qr()} no longer has a \code{\dots} argument to pass
      additional arguments to methods.

      \item \code{[} has a method for class \code{"table"}.

      \item It is now possible (again) to \code{replayPlot()} a
      display list snapshot that was created by \code{recordPlot()}
      in a different \R session.

      It is still not a good idea to use snapshots as a persistent
      storage format for \R plots, but it is now not completely silly to
      use a snapshot as a format for transferring an R plot between two
      R sessions.

      The underlying changes mean that packages providing graphics
      devices (e.g., \CRANpkg{Cairo}, \CRANpkg{RSvgDevice},
      \CRANpkg{cairoDevice}, \CRANpkg{tikzDevice}) will need to be
      reinstalled.

      Code for restoring snapshots was contributed by Jeroen Ooms
      and JJ Allaire.

      Some testing code is available at
      \url{https://github.com/pmur002/R-display-list}.

      \item \code{tools::undoc(dir = D)} and \code{codoc(dir = D)} now
      also work when \code{D} is a directory whose \code{normalizePath()}ed
      version does not end in the package name, e.g.\sspace{}from a symlink.

      \item \code{abbreviate()} has more support for multi-byte
      character sets -- it no longer removes bytes within characters and
      knows about Latin vowels with accents.  It is still only
      really suitable for (most) European languages, and still warns on
      non-ASCII input.

      \code{abbreviate(use.classes = FALSE)} is now implemented, and
      that is more suitable for non-European languages.

      \item \code{match(x, table)} is faster (sometimes by an order of
      magnitude) when \code{x} is of length one and \code{incomparables}
      is unchanged, thanks to Peter Haverty (\PR{16491}).

      \item More consistent, partly not back-compatible behavior of
      \code{NA} and \code{NaN} coercion to complex numbers, operations
      less often resulting in complex \code{NA} (\code{NA_complex_}).

      \item \code{lengths()} considers methods for \code{length} and
      \code{[[} on \code{x}, so it should work automatically on any
      objects for which appropriate methods on those generics are
      defined.

      \item The logic for selecting the default screen device on OS X
      has been simplified: it is now \code{quartz()} if that is
      available even if environment variable \env{DISPLAY} has been set
      by the user.

      The choice can easily be overridden \emph{via} environment
      variable \env{R_INTERACTIVE_DEVICE}.

      \item On Unix-like platforms which support the \command{getline} C
      library function, \code{system(*, intern = TRUE)} no longer
      truncates (output) lines longer than 8192 characters, thanks to
      Karl Millar. (\PR{16544})

      \item \code{rank()} gains a \code{ties.method = "last"} option,
      for convenience (and symmetry).

      %% \item \code{getRcode(vignette(..))} is the \R source file name of
      %% a vignette.

      \item \code{regmatches(invert = NA)} can now be used to extract
      both non-matched and matched substrings.

      \item \code{data.frame()} gains argument \code{fix.empty.names};
      \code{as.data.frame.list()} gets new \code{cut.names},
      \code{col.names} and \code{fix.empty.names}.

      \item \code{plot(x ~ x, *)} now warns that it is the same as
      \code{plot(x ~ 1, *)}.

      \item \code{recordPlot()} has new arguments \code{load} and
      \code{attach} to allow package names to be stored as part of a
      recorded plot.  \code{replayPlot()} has new argument
      \code{reloadPkgs} to load/attach any package names that were
      stored as part of a recorded plot.

      \item S4 dispatch works within calls to \code{.Internal()}. This
      means explicit S4 generics are no longer needed for
      \code{unlist()} and \code{as.vector()}.

      \item Only font family names starting with \samp{"Hershey"} (and
      not \samp{"Her"} as before) are given special treatment by the
      graphics engine.

      \item S4 values are automatically coerced to vector (\emph{via}
      \code{as.vector}) when subassigned into atomic vectors.

      \item \code{findInterval()} gets a \code{left.open} option.

      \item The version of LAPACK included in the sources has been
      updated to 3.6.0, including those \sQuote{deprecated} routines
      which were previously included.  \emph{Ca} 40 double-complex
      routines have been added at the request of a package maintainer.

      As before, the details of what is included are in
      \file{src/modules/lapack/README} and this now gives information on
      earlier additions.

      \item \code{tapply()} has been made considerably more efficient
      without changing functionality, thanks to proposals from Peter
      Haverty and Suharto Anggono.  (\PR{16640})

      \item \code{match.arg(arg)} (the one-argument case) is faster; so
      is \code{sort.int()}.  (\PR{16640})

      \item The \code{format} method for \code{object_size} objects now
      also accepts \dQuote{binary} units such as \code{"KiB"} and e.g.,
      \code{"Tb"}.  (Partly from \PR{16649}.)

      \item Profiling now records calls of the form \code{foo::bar} and
      some similar cases directly rather than as calls to
      \code{<Anonymous>}.  Contributed by Winston Chang.

      \item New string utilities \code{startsWith(x, prefix)} and
      \code{endsWith(x, suffix)}.  Also provide speedups for some
      \code{grepl("^...", *)} uses (related to proposals in \PR{16490}).

      \item Reference class finalizers run at exit, as well as on
      garbage collection.

      \item Avoid \pkg{parallel} dependency on \pkg{stats} for port
      choice and random number seeds.  (\PR{16668})

      \item The radix sort algorithm and implementation from
      \CRANpkg{data.table} (\code{forder}) replaces the previous radix
      (counting) sort and adds a new method for \code{order()}.
      Contributed by Matt Dowle and Arun Srinivasan, the new algorithm
      supports logical, integer (even with large values), real, and
      character vectors.  It outperforms all other methods, but there
      are some caveats (see \code{?sort}).

      \item The \code{order()} function gains a \code{method} argument
      for choosing between \code{"shell"} and \code{"radix"}.

      \item New function \code{grouping()} returns a permutation that
      stably rearranges data so that identical values are adjacent.  The
      return value includes extra partitioning information on the
      groups.  The implementation came included with the new radix sort.

      \item \code{rhyper(nn, m, n, k)} no longer returns \code{NA} when
      one of the three parameters exceeds the maximal integer.

      \item \code{switch()} now warns when no alternatives are provided.

      \item \code{parallel::detectCores()} now has default \code{logical
	= TRUE} on all platforms -- as this was the default on Windows,
      this change only affects Sparc Solaris.

      Option \code{logical = FALSE} is now supported on Linux and recent
      versions of OS X (for the latter, thanks to a suggestion of Kyaw
      Sint).

      \item \code{hist()} for \code{"Date"} or \code{"POSIXt"} objects
      would sometimes give misleading labels on the breaks, as they
      were set to the day before the start of the period being
      displayed.  The display format has been changed, and the shift of the
      start day has been made conditional on \code{right = TRUE} (the
      default).  (\PR{16679})

      \item \R now uses a new version of the logo (donated to the R
      Foundation by RStudio).  It is defined in \file{.svg} format, so
      will resize without unnecessary degradation when displayed on
      HTML pages---there is also a vector PDF version.  Thanks to
      Dirk Eddelbuettel for producing the corresponding X11 icon.

      \item New function \code{.traceback()} returns the stack trace
      which \code{traceback()} prints.

      \item \code{lengths()} dispatches internally.

      \item \code{dotchart()} gains a \code{pt.cex} argument to control
      the size of points separately from the size of plot labels.
      Thanks to Michael Friendly and Milan Bouchet-Valat for ideas
      and patches.

      \item \code{as.roman(ch)} now correctly deals with more diverse
      character vectors \code{ch}; also arithmetic with the resulting
      roman numbers works in more cases.  (\PR{16779})

      \item \code{prcomp()} gains a new option \code{rank.} allowing to
      directly aim for less than \code{min(n,p)} PC's.  The
      \code{summary()} and its \code{print()} method have been amended,
      notably for this case.

      \item \code{gzcon()} gains a new option \code{text}, which marks
      the connection as text-oriented (so e.g.\sspace{}\code{pushBack()}
      works).  It is still always opened in binary mode.

      \item The \code{import()} namespace directive now accepts an
      argument \code{except} which names symbols to exclude from the
      imports. The \code{except} expression should evaluate to a
      character vector (after substituting symbols for strings). See
      Writing R Extensions.

      \item New convenience function \code{Rcmd()} in package
      \pkg{tools} for invoking \command{R CMD} tools from within \R.

      \item New functions \code{makevars_user()} and
      \code{makevars_site()} in package \pkg{tools} to determine the
      location of the user and site specific \file{Makevars} files for
      customizing package compilation.
    }
  }

  \subsection{UTILITIES}{
    \itemize{
      \item \command{R CMD check} has a new option
      \option{--ignore-vignettes} for use with non-Sweave vignettes
      whose \samp{VignetteBuilder} package is not available.

      \item \command{R CMD check} now by default checks code usage
      (\emph{via} \CRANpkg{codetools}) with only the base package
      attached.  Functions from default packages other than \pkg{base}
      which are used in the package code but not imported are reported
      as undefined globals, with a suggested addition to the
      \code{NAMESPACE} file.

      \item \command{R CMD check --as-cran} now also checks DOIs in
      package \file{CITATION} and Rd files.

      \item \command{R CMD Rdconv} and \command{R CMD Rd2pdf} each have
      a new option \option{--RdMacros=pkglist} which allows Rd macros
      to be specified before processing.
    }
  }

  \subsection{DEPRECATED AND DEFUNCT}{
    \itemize{
      \item The previously included versions of \code{zlib},
      \code{bzip2}, \code{xz} and PCRE have been removed, so suitable
      external (usually system) versions are required (see the
      \sQuote{R Installation and Administration} manual).

      \item The unexported and undocumented Windows-only devices
      \code{cairo_bmp()}, \code{cairo_png()} and \code{cairo_tiff()}
      have been removed.  (These devices should be used as
      e.g.\sspace{}\code{bmp(type = "cairo")}.)

      \item (Windows only)  Function \code{setInternet2()} has no effect
      and will be removed in due course.  The choice between methods
      \code{"internal"} and \code{"wininet"} is now made by the
      \code{method} arguments of \code{url()} and \code{download.file()}
      and their defaults can be set \emph{via} options.  The
      out-of-the-box default remains \code{"wininet"} (as it has been
      since \R 3.2.2).

      \item \code{[<-} with an S4 value into a list currently embeds the
      S4 object into its own list such that the end result is roughly
      equivalent to using \code{[[<-}.  That behavior is deprecated.  In
      the future, the S4 value will be coerced to a list with
      \code{as.list()}.

      \item Package \pkg{tools}' functions
      \code{package.dependencies()}, \code{pkgDepends()}, etc are
      deprecated now, mostly in favor of \code{package_dependencies()}
      which is both more flexible and efficient.
    }
  }

  \subsection{INSTALLATION and INCLUDED SOFTWARE}{
    \itemize{
      \item Support for very old versions of \command{valgrind}
      (e.g., 3.3.0) has been removed.

      \item The included \command{libtool} script (generated by
      \command{configure}) has been updated to version 2.4.6 (from 2.2.6a).

      \item \code{libcurl} version 7.28.0 or later with support for the
      \code{https} protocol is required for installation (except on
      Windows).

      \item BSD networking is now required (except on Windows) and so
      \code{capabilities("http/ftp")} is always true.

      \item \command{configure} uses \command{pkg-config} for PNG, TIFF
      and JPEG where this is available.  This should work better with
      multiple installs and with those using static libraries.

      \item The minimum supported version of OS X is 10.6 (\sQuote{Snow
	Leopard}): even that has been unsupported by Apple since 2012.

      \item The \command{configure} default on OS X is
      \option{--disable-R-framework}: enable this if you intend to
      install under \file{/Library/Frameworks} and use with \code{R.app}.

      \item The minimum preferred version of PCRE has since \R 3.0.0
      been 8.32 (released in Nov 2012).  Versions 8.10 to 8.31 are now
      deprecated (with warnings from \command{configure}), but will
      still be accepted until \R 3.4.0.

      \item \command{configure} looks for C functions \code{__cospi},
      \code{__sinpi} and \code{__tanpi} and uses these if \code{cospi}
      \emph{etc} are not found.  (OS X is the main instance.)

      \item (Windows) R is now built using \command{gcc}
      4.9.3.  This build will require recompilation of at least those packages
      that include C++ code, and possibly others.  A build of R-devel using
      the older toolchain will be temporarily available for comparison
      purposes.

      During the transition, the environment variable
      \env{R_COMPILED_BY} has been defined to indicate which toolchain
      was used to compile R (and hence, which should be used to compile
      code in packages).  The \code{COMPILED_BY} variable described below
      will be a permanent replacement for this.

      \item (Windows) A \command{make} and \command{R CMD config} variable
      named \code{COMPILED_BY} has been added.  This indicates
      which toolchain was used to compile R (and hence, which should be
      used to compile code in packages).
    }
  }

  \subsection{PACKAGE INSTALLATION}{
    \itemize{
      \item The \command{make} macro \code{AWK} which used to be made
      available to files such as \file{src/Makefile} is no longer set.
    }
  }

  \subsection{C-LEVEL FACILITIES}{
    \itemize{
      \item The API call \code{logspace_sum} introduced in \R 3.2.0 is
      now remapped as an entry point to \code{Rf_logspace_sum}, and its
      first argument has gained a \code{const} qualifier.  (\PR{16470})

      Code using it will need to be reinstalled.

      Similarly, entry point \code{log1pexp} also defined in
      \file{Rmath.h} is remapped there to \code{Rf_log1pexp}

      \item \code{R_GE_version} has been increased to \code{11}.

      \item New API call \code{R_orderVector1}, a faster
      one-argument version of \code{R_orderVector}.

      \item When \R{} headers such as \file{R.h} and \file{Rmath.h} are
      called from C++ code in packages they include the C++ versions of
      system headers such as \file{<cmath>} rather than the legacy
      headers such as \file{<math.h>}.  (Headers \file{Rinternals.h} and
      \file{Rinterface.h} already did, and inclusion of system headers
      can still be circumvented by defining \code{NO_C_HEADERS},
      including as from this version for those two headers.)

      The manual has long said that \R{} headers should \bold{not} be
      included within an \code{extern "C"} block, and almost all the
      packages affected by this change were doing so.

      \item Including header \file{S.h} from C++ code would fail on some
      platforms, and so gives a compilation error on all.

      \item The deprecated header \file{Rdefines.h} is now compatible
      with defining \code{R_NO_REMAP}.

      \item The connections interface now includes a function
      \code{R_GetConnection()} which allows packages implementing
      connections to convert R \code{connection} objects to
      \code{Rconnection} handles. Code which previously
      used the low-level R-internal \code{getConnection()} entry point
      should switch.
    }
  }

  \subsection{BUG FIXES}{
    \itemize{
      \item C-level \code{asChar(x)} is fixed for when \code{x} is not a
      vector, and it returns \code{"TRUE"}/\code{"FALSE"} instead of
      \code{"T"}/\code{"F"} for logical vectors.

      \item The first arguments of \code{.colSums()} etc (with an
      initial dot) are now named \code{x} rather than \code{X} (matching
      \code{colSums()}): thus error messages are corrected.

      \item A \code{coef()} method for class \code{"maov"} has been
      added to allow \code{vcov()} to work with multivariate
      results. (\PR{16380})

      \item \code{method = "libcurl"} connections signal errors rather
      than retrieving HTTP error pages (where the ISP reports the
      error).

      \item \code{xpdrows.data.frame()} was not checking for unique
      row names; in particular, this affected assignment to non-existing
      rows \emph{via} numerical indexing. (\PR{16570})

      \item \code{tail.matrix()} did not work for zero rows matrices,
      and could produce row \dQuote{labels} such as \code{"[1e+05,]"}.

      \item Data frames with a column named \code{"stringsAsFactors"}
      now format and print correctly.  (\PR{16580})

      \item \code{cor()} is now guaranteed to return a value with
      absolute value less than or equal to 1. (\PR{16638})

      \item Array subsetting now keeps \code{names(dim(.))}.

      \item Blocking socket connection selection recovers more
      gracefully on signal interrupts.

      \item The \code{data.frame} method of \code{rbind()} construction
      \code{row.names} works better in borderline integer cases, but
      may change the names assigned.  (\PR{16666})

      \item (X11 only)  \code{getGraphicsEvent()} miscoded buttons and
      missed mouse motion events.  (\PR{16700})

      \item \code{methods(round)} now also lists \code{round.POSIXt}.

      \item \code{tar()} now works with the default \code{files = NULL}.
      (\PR{16716})

      \item Jumps to outer contexts, for example in error recovery, now
      make intermediate jumps to contexts where \code{on.exit()} actions
      are established instead of trying to run all \code{on.exit()}
      actions before jumping to the final target. This unwinds the stack
      gradually, releases resources held on the stack, and significantly
      reduces the chance of a segfault when running out of C stack
      space. Error handlers established using
      \code{withCallingHandlers()} and \code{options("error")}
      specifications are ignored when handling a C stack overflow error
      as attempting one of these would trigger a cascade of C stack
      overflow errors.  (These changes resolve \PR{16753}.)

      \item The spacing could be wrong when printing a complex array.
      (Report and patch by Lukas Stadler.)

      \item \code{pretty(d, n, min.n, *)} for date-time objects \code{d}
      works again in border cases with large \code{min.n}, returns a
      \code{labels} attribute also for small-range dates and in such cases
      its returned length is closer to the desired \code{n}.  (\PR{16761})
      Additionally, it finally does cover the range of \code{d}, as it
      always claimed.

      \item \code{tsp(x) <- NULL} did not handle correctly objects
      inheriting from both \code{"ts"} and \code{"mts"}.  (\PR{16769})

      \item \code{install.packages()} could give false errors when
      \code{options("pkgType")} was \code{"binary"}.  (Reported by
      Jose Claudio Faria.)

      \item A bug fix in \R 3.0.2 fixed problems with \code{locator()}
      in X11, but introduced problems in Windows.  Now both should be
      fixed.  (\PR{15700})

      \item \code{download.file()} with \code{method = "wininet"}
      incorrectly warned of download file length difference when
      reported length was unknown. (\PR{16805})

      \item \code{diag(NULL, 1)} crashed because of missed type
      checking.  (\PR{16853})
    }
  }
}

\section{\Rlogo CHANGES IN R 3.2.5}{
  \subsection{BUG FIXES}{
    \itemize{
      \item \code{format.POSIXlt()} behaved incorrectly in \R 3.2.4.
      E.g.\sspace{}the output of
      \code{format(as.POSIXlt(paste0(1940:2000, "-01-01"), tz = "CET"),
	usetz = TRUE)} ended in two \code{"CEST"} time formats.
    }
  }
}

\section{\Rlogo CHANGES IN R 3.2.4}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item \code{install.packages()} and related functions now give a
      more informative warning when an attempt is made to install a base
      package.

      \item \code{summary(x)} now prints with less rounding when
      \code{x} contains infinite values. (Request of \PR{16620}.)

      \item \code{provideDimnames()} gets an optional \code{unique} argument.

      \item \code{shQuote()} gains \code{type = "cmd2"} for quoting
      in \command{cmd.exe} in Windows.  (Response to \PR{16636}.)

      \item The \code{data.frame} method of \code{rbind()} gains an
      optional argument \code{stringsAsFactors} (instead of only
      depending on \code{getOption("stringsAsFactors")}).

      \item \code{smooth(x, *)} now also works for long vectors.

      \item \code{tools::texi2dvi()} has a workaround for problems with
      the \command{texi2dvi} script supplied by \pkg{texinfo 6.1}.

      It extracts more error messages from the LaTeX logs when in
      emulation mode.
    }
  }

  \subsection{UTILITIES}{
    \itemize{
      \item \command{R CMD check} will leave a log file
      \file{build_vignettes.log} from the re-building of vignettes in
      the \file{.Rcheck} directory if there is a problem, and always if
      environment variable \env{_R_CHECK_ALWAYS_LOG_VIGNETTE_OUTPUT_} is
      set to a true value.
    }
  }

  \subsection{DEPRECATED AND DEFUNCT}{
    \itemize{
      \item Use of \samp{SUPPORT_OPENMP} from header \file{Rconfig.h} is
      deprecated in favour of the standard OpenMP define \samp{_OPENMP}.

      (This has been the recommendation in the manual for a while now.)

      \item The \command{make} macro \code{AWK} which is long unused by
      \R itself but recorded in file \file{etc/Makeconf} is deprecated
      and will be removed in \R 3.3.0.

      \item The C header file \file{S.h} is no longer documented: its
      use should be replaced by \file{R.h}.
    }
  }

  \subsection{BUG FIXES}{
    \itemize{
      \item \code{kmeans(x, centers = <1-row>)} now works. (\PR{16623})

      \item \code{Vectorize()} now checks for clashes in argument names.
      (\PR{16577})

      \item \code{file.copy(overwrite = FALSE)} would signal a successful
      copy when none had taken place.  (\PR{16576})

      \item \code{ngettext()} now uses the same default domain as
      \code{gettext()}.  (\PR{14605})

      \item \code{array(.., dimnames = *)} now warns about
      non-\code{list} dimnames and, from \R 3.3.0, will signal the same
      error for invalid dimnames as \code{matrix()} has always done.

      \item \code{addmargins()} now adds dimnames for the extended
      margins in all cases, as always documented.

      \item \code{heatmap()} evaluated its \code{add.expr} argument
      in the wrong environment.  (\PR{16583})

      \item \code{require()} etc now give the correct entry of
      \code{lib.loc} in the warning about an old version of a package
      masking a newer required one.

      \item The internal deparser did not add parentheses when
      necessary,  e.g. before \code{[]} or \code{[[]]}.  (Reported by
      Lukas Stadler; additional fixes included as well).

      \item \code{as.data.frame.vector(*, row.names=*)} no longer
      produces \sQuote{corrupted} data frames from row names of incorrect
      length, but rather warns about them.  This will become an error.

      \item \code{url} connections with \code{method = "libcurl"} are
      destroyed properly. (\PR{16681})

      \item \code{withCallingHandler()} now (again) handles warnings
      even during S4 generic's argument evaluation.  (\PR{16111})

      \item \code{deparse(..., control = "quoteExpressions")}
      incorrectly quoted empty expressions.  (\PR{16686})

      \item \code{format()}ting datetime objects (\code{"POSIX[cl]?t"})
      could segfault or recycle wrongly.  (\PR{16685})

      \item \code{plot.ts(<matrix>, las = 1)} now does use \code{las}.

      \item \code{saveRDS(*, compress = "gzip")} now works as
      documented.  (\PR{16653})

      \item (Windows only) The \command{Rgui} front end did not
      always initialize the console properly, and could cause
      \R to crash.  (\PR{16698})

      \item \code{dummy.coef.lm()} now works in more cases, thanks to a
      proposal by Werner Stahel (\PR{16665}).  In addition, it now works
      for multivariate linear models (\code{"mlm"}, \code{\link{manova}})
      thanks to a proposal by Daniel Wollschlaeger.

      \item The \code{as.hclust()} method for \code{"dendrogram"}s
      failed often when there were ties in the heights.

      \item \code{reorder()} and \code{midcache.dendrogram()} now are
      non-recursive and hence applicable to somewhat deeply nested
      dendrograms, thanks to a proposal by Suharto Anggono in \PR{16424}.

      \item \code{cor.test()} now calculates very small p values
      more accurately (affecting the result only in extreme not
      statistically relevant cases).  (\PR{16704})

      \item \code{smooth(*, do.ends=TRUE)} did not always work correctly
      in \R versions between 3.0.0 and 3.2.3.

      \item \code{pretty(D)} for date-time objects \code{D} now also
      works well if \code{range(D)} is (much) smaller than a second.  In
      the case of only one unique value in \code{D}, the pretty range
      now is more symmetric around that value than previously.
      \cr
      Similarly, \code{pretty(dt)} no longer returns a length 5 vector
      with duplicated entries for \code{Date} objects \code{dt} which
      span only a few days.

      \item The figures in help pages such as \code{?points} were
      accidentally damaged, and did not appear in \R 3.2.3.
      (\PR{16708})

      \item \code{available.packages()} sometimes deleted the wrong
      file when cleaning up temporary files.  (\PR{16712})

      \item The \code{X11()} device sometimes froze on Red Hat
      Enterprise Linux 6. It now waits for \code{MapNotify} events
      instead of \code{Expose} events, thanks to Siteshwar
      Vashisht. (\PR{16497})

      \item \code{[dpqr]nbinom(*, size=Inf, mu=.)} now works as limit
      case, for \sQuote{dpq} as the Poisson.  (\PR{16727})\cr
      \code{pnbinom()} no longer loops infinitely in border cases.

      \item \code{approxfun(*, method="constant")} and hence \code{ecdf()}
      which calls the former now correctly \dQuote{predict} \code{NaN}
      values as \code{NaN}.

      \item \code{summary.data.frame()} now displays \code{NA}s in
      \code{Date} columns in all cases.  (\PR{16709})
    }
  }
}

\section{\Rlogo CHANGES IN R 3.2.3}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item Some recently-added Windows time zone names have been added
      to the conversion table used to convert these to Olson names.
      (Including those relating to changes for Russia in Oct 2014, as in
      \PR{16503}.)

      \item (Windows) Compatibility information has been added to the
      manifests for \file{Rgui.exe}, \file{Rterm.exe} and
      \file{Rscript.exe}.  This should allow \code{win.version()} and
      \code{Sys.info()} to report the actual Windows version up to
      Windows 10.

      \item Windows \code{"wininet"} FTP first tries EPSV / PASV mode
      rather than only using active mode (reported by Dan Tenenbaum).
      %% https://stat.ethz.ch/pipermail/r-devel/2015-August/071595.html

      \item \code{which.min(x)} and \code{which.max(x)} may be much
      faster for logical and integer \code{x} and now also work for long
      vectors.

      \item The \sQuote{emulation} part of \code{tools::texi2dvi()} has
      been somewhat enhanced, including supporting \code{quiet = TRUE}.
      It can be selected by \code{texi2dvi = "emulation"}.

      (Windows) MiKTeX removed its \code{texi2dvi.exe} command in Sept
      2015: \code{tools::texi2dvi()} tries \code{texify.exe} if it is
      not found.

      \item (Windows only) Shortcuts for printing and saving have
      been added to menus in \command{Rgui.exe}.  (Request of \PR{16572}.)

      \item \code{loess(..., iterTrace=TRUE)} now provides diagnostics
      for robustness iterations, and the \code{print()} method for
      \code{summary(<loess>)} shows slightly more.

      \item The included version of PCRE has been updated to 8.38, a
      bug-fix release.

      \item \code{View()} now displays nested data frames in a more
      friendly way.  (Request with patch in \PR{15915}.)
    }
  }

  \subsection{INSTALLATION and INCLUDED SOFTWARE}{
    \itemize{
      \item The included configuration code for \code{libintl} has been
      updated to that from \code{gettext} version 0.19.5.1 --- this
      should only affect how an external library is detected (and the
      only known instance is under OpenBSD).  (Wish of \PR{16464}.)

      \item \command{configure} has a new argument
      \option{--disable-java} to disable the checks for Java.

      \item The \command{configure} default for \code{MAIN_LDFLAGS} has
      been changed for the FreeBSD, NetBSD and Hurd OSes to one more
      likely to work with compilers other than \command{gcc} (FreeBSD 10
      defaults to \command{clang}).

      \item \command{configure} now supports the OpenMP flags
      \option{-fopenmp=libomp} (clang) and \option{-qopenmp} (Intel C).

      \item Various macros can be set to override the default
      behaviour of \command{configure} when detecting OpenMP: see file
      \file{config.site}.

      \item Source installation on Windows has been modified to allow
      for MiKTeX installations without \command{texi2dvi.exe}.  See file
      \file{MkRules.dist}.
    }
  }

  \subsection{BUG FIXES}{
    \itemize{
      \item \code{regexpr(pat, x, perl = TRUE)} with Python-style named
      capture did not work correctly when \code{x} contained \code{NA}
      strings.  (\PR{16484})

      \item The description of dataset \code{ToothGrowth} has been
      improved/corrected.  (\PR{15953})

      \item \code{model.tables(type = "means")} and hence
      \code{TukeyHSD()} now support \code{"aov"} fits without an
      intercept term.  (\PR{16437})

      \item \code{close()} now reports the status of a \code{pipe()}
      connection opened with an explicit \code{open} argument.
      (\PR{16481})

      \item Coercing a list without names to a data frame is faster if
      the elements are very long. (\PR{16467})

      \item (Unix-only) Under some rare circumstances piping the output
      from \command{Rscript} or \command{R -f} could result in
      attempting to close the input file twice, possibly crashing the
      process.  (\PR{16500})

      \item (Windows) \code{Sys.info()} was out of step with
      \code{win.version()} and did not report Windows 8.

      \item \code{topenv(baseenv())} returns \code{baseenv()} again as
      in \R 3.1.0 and earlier.  This also fixes \code{compilerJIT(3)}
      when used in \file{.Rprofile}.

      \item \code{detach()}ing the \pkg{methods} package keeps
      \code{.isMethodsDispatchOn()} true, as long as the methods
      namespace is not unloaded.% This keeps a sane state, see the reg-S4 test

      \item Removed some spurious warnings from \command{configure} about the
      preprocessor not finding header files.  (\PR{15989})

      \item \code{rchisq(*, df=0, ncp=0)} now returns \code{0} instead
      of \code{NaN}, and \code{dchisq(*, df=0, ncp=*)} also no longer
      returns \code{NaN} in limit cases (where the limit is unique).
      (\PR{16521})

      \item \code{pchisq(*, df=0, ncp > 0, log.p=TRUE)} no longer
      underflows (for ncp > ~60).

      \item \code{nchar(x, "w")} returned -1 for characters it did not
      know about (e.g.\sspace{}zero-width spaces): it now assumes 1.
      It now knows about most zero-width characters and a few more
      double-width characters.

      \item Help for \code{which.min()} is now more precise about
      behavior with logical arguments.  (\PR{16532})

      \item The print width of character strings marked as
      \code{"latin1"} or \code{"bytes"} was in some cases computed
      incorrectly.

      \item \code{abbreviate()} did not give names to the return value
      if \code{minlength} was zero, unlike when it was positive.

      \item (Windows only) \code{dir.create()} did not always warn
      when it failed to create a directory.  (\PR{16537})

      \item When operating in a non-UTF-8 multibyte locale
      (e.g.\sspace{}an East Asian locale on Windows), \code{grep()} and
      related functions did not handle UTF-8 strings properly.
      (\PR{16264})

      \item \code{read.dcf()} sometimes misread lines longer than 8191
      characters.  (Reported by Hervé Pagès with a patch.)

      \item \code{within(df, ..)} no longer drops columns whose name
      start with a \code{"."}.

      \item The built-in \code{HTTP} server converted entire
      \code{Content-Type} to lowercase including parameters which
      can cause issues for multi-part form boundaries (\PR{16541}).

      \item Modifying slots of S4 objects could fail when the
      \pkg{methods} package was not attached. (\PR{16545})

      \item \code{splineDesign(*, outer.ok=TRUE)} (\pkg{splines}) is better now
      (\PR{16549}), and \code{interpSpline()} now allows \code{sparse=TRUE}
      for speedup with non-small sizes.

      \item If the expression in the traceback was too long,
      \code{traceback()} did not report the source line number.  (Patch
      by Kirill Müller.)

      \item The browser did not truncate the display of the function when
      exiting with \code{options("deparse.max.lines")} set.
      (\PR{16581})

      \item When \code{bs(*, Boundary.knots=)} had boundary knots inside
      the data range, extrapolation was somewhat off.  (Patch by Trevor
      Hastie.)

      \item \code{var()} and hence \code{sd()} warn about
      \code{factor} arguments which are deprecated now. (\PR{16564})

      \item \code{loess(*, weights = *)} stored wrong weights and hence
      gave slightly wrong predictions for \code{newdata}.  (\PR{16587})

      \item \code{aperm(a, *)} now preserves \code{names(dim(a))}.

      \item \code{poly(x, ..)} now works when either \code{raw=TRUE} or
      \code{coef} is specified.  (\PR{16597})

      \item \code{data(package=*)} is more careful in determining the path.

      \item \code{prettyNum(*, decimal.mark, big.mark)}: fixed bug
      introduced when fixing \PR{16411}.
    }
  }
}

\section{\Rlogo CHANGES IN R 3.2.2}{
  \subsection{SIGNIFICANT USER-VISIBLE CHANGES}{
    \itemize{
      \item It is now easier to use secure downloads from
      \samp{https://} URLs on builds which support them: no longer do
      non-default options need to be selected to do so.  In particular,
      packages can be installed from repositories which offer
      \samp{https://} URLs, and those listed by \code{setRepositories()}
      now do so (for some of their mirrors).

      Support for \samp{https://} URLs is available on Windows, and on
      other platforms if support for \code{libcurl} was compiled in and
      if that supports the \code{https} protocol (system installations
      can be expected to do).  So \samp{https://} support can be
      expected except on rather old OSes (an example being OS X
      \sQuote{Snow Leopard}, where a non-system version of
      \code{libcurl} can be used).

      (Windows only) The default method for accessing URLs \emph{via}
      \code{download.file()} and \code{url()} has been changed to be
      \code{"wininet"} using Windows API calls.  This changes the way
      proxies need to be set and security settings made: there have been
      some reports of \samp{ftp:} sites being inaccessible under the new default
      method (but the previous methods remain available).
    }
  }

  \subsection{NEW FEATURES}{
    \itemize{
      \item \code{cmdscale()} gets new option \code{list.} for increased
      flexibility when a list should be returned.

      \item \command{configure} now supports \code{texinfo} version 6.0,
      which (unlike the change from 4.x to 5.0) is a minor update.
      (Wish of \PR{16456}.)

      \item (Non-Windows only) \code{download.file()} with default
      \code{method = "auto"} now chooses \code{"libcurl"} if that is
      available and a \samp{https://} or \samp{ftps://} URL is used.

      \item (Windows only) \code{setInternet2(TRUE)} is now the default.
      The command-line option \command{--internet2} and environment
      variable \env{R_WIN_INTERNET2} are now ignored.

      Thus by default the \code{"internal"} method for
      \code{download.file()} and \code{url()} uses the \code{"wininet"}
      method: to revert to the previous default use
      \code{setInternet2(FALSE)}.

      This means that \samp{https://} URLs can be read by default by
      \code{download.file()} (they have been readable by \code{file()}
      and \code{url()} since \R 3.2.0).

      There are implications for how proxies need to be set (see
      \code{?download.file}).

      \item \code{chooseCRANmirror()} and \code{chooseBioCmirror()} now
      offer HTTPS mirrors in preference to HTTP mirrors.  This changes
      the interpretation of their \code{ind} arguments: see their help
      pages.

      \item \code{capture.output()} gets optional arguments \code{type}
      and \code{split} to pass to \code{sink()}, and hence can be used to
      capture messages.
    }
  }

  \subsection{C-LEVEL FACILITIES}{
    \itemize{
      \item Header \file{Rconfig.h} now defines \code{HAVE_ALLOCA_H} if
      the platform has the \file{alloca.h} header (it is needed to
      define \code{alloca} on Solaris and AIX, at least: see
      \sQuote{Writing R Extensions} for how to use it).
    }
  }

  \subsection{INSTALLATION and INCLUDED SOFTWARE}{
    \itemize{
      \item The \command{libtool} script generated by
      \command{configure} has been modified to support FreeBSD >= 10
      (\PR{16410}).
    }
  }

  \subsection{BUG FIXES}{
    \itemize{
      \item The HTML help page links to demo code failed due to a change
      in \R 3.2.0.  (\PR{16432})

      \item If the \code{na.action} argument was used in
      \code{model.frame()}, the original data could be
      modified. (\PR{16436})

      \item \code{getGraphicsEvent()} could cause a crash if a graphics
      window was closed while it was in use. (\PR{16438})

      \item \code{matrix(x, nr, nc, byrow = TRUE)} failed if \code{x}
      was an object of type \code{"expression"}.

      \item \code{strptime()} could overflow the allocated storage on
      the C stack when the timezone had a non-standard format much
      longer than the standard formats. (Part of \PR{16328}.)

      \item \code{options(OutDec = s)} now signals a warning (which will
      become an error in the future) when \code{s} is not a string with
      exactly one character, as that has been a documented requirement.

      \item \code{prettyNum()} gains a new option \code{input.d.mark}
      which together with other changes, e.g., the default for
      \code{decimal.mark}, fixes some \code{format()}ting variants with
      non-default \code{getOption("OutDec")} such as in \PR{16411}.

      \item \code{download.packages()} failed for \code{type} equal to
      either \code{"both"} or \code{"binary"}.  (Reported by Dan
      Tenenbaum.)

      \item The \code{dendrogram} method of \code{labels()} is much more
      efficient for large dendrograms, now using \code{rapply()}.
      (Comment #15 of \PR{15215})

      \item The \code{"port"} algorithm of \code{nls()} could give
      spurious errors.  (Reported by Radford Neal.)

      \item Reference classes that inherited from reference classes in
      another package could invalidate methods of the inherited
      class. Fixing this requires adding the ability for methods to be
      \dQuote{external}, with the object supplied explicitly as the first
      argument, named \code{.self}. See "Inter-Package Superclasses"
      in the documentation.

      \item \code{readBin()} could fail on the SPARC architecture
      due to alignment issues.  (Reported by Radford Neal.)

      \item \code{qt(*, df=Inf, ncp=.)} now uses the natural
      \code{qnorm()} limit instead of returning \code{NaN}. (\PR{16475})

      \item Auto-printing of S3 and S4 values now searches for
      \code{print()} in the base namespace and \code{show()} in the
      \pkg{methods} namespace instead of searching the global
      environment.

      \item \code{polym()} gains a \code{coefs = NULL} argument and
      returns class \code{"poly"} just like \code{poly()} which gets a
      new \code{simple=FALSE} option.  They now lead to correct
      \code{predict()}ions, e.g., on subsets of the original data.
      (\PR{16239})

      \item \code{rhyper(nn, <large>)} now works correctly. (\PR{16489})

      \item \code{ttkimage()} did not (and could not) work so was
      removed. Ditto for \code{tkimage.cget()} and
      \code{tkimage.configure()}. Added two Ttk widgets and missing
      subcommands for Tk's \code{image} command:
      \code{ttkscale()},
      \code{ttkspinbox()},
      \code{tkimage.delete()},
      \code{tkimage.height()},
      \code{tkimage.inuse()},
      \code{tkimage.type()},
      \code{tkimage.types()},
      \code{tkimage.width()}. (\PR{15372}, \PR{16450})

      \item \code{getClass("foo")} now also returns a class definition when it is
      found in the cache more than once.
    }
  }
}

\section{\Rlogo CHANGES IN R 3.2.1}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item \code{utf8ToInt()} now checks that its input is valid UTF-8
      and returns \code{NA} if it is not.

      \item \code{install.packages()} now allows \code{type = "both"}
      with \code{repos = NULL} if it can infer the type of file.

      \item \code{nchar(x, *)} and \code{nzchar(x)} gain a new argument
      \code{keepNA} which governs how the result for \code{NA}s in
      \code{x} is determined.  For \code{nzchar()} in general and
      \code{nchar()} in the \R 3.2.x series, the default
      remains \code{FALSE} which is fully back compatible.
      From \R 3.3.0, \code{nchar()}'s default will change to
      \code{keepNA = NA} and you are advised to consider this for code
      portability.

      \item \code{news()} more flexibly extracts dates from package
      \file{NEWS.Rd} files.

      \item \code{lengths(x)} now also works (trivially) for atomic
      \code{x} and hence can be used more generally as an efficient
      replacement of \code{sapply(x, length)} and similar.

      \item The included version of PCRE has been updated to 8.37, a
      bug-fix release.

      \item \code{diag()} no longer duplicates a matrix when extracting
      its diagonal.

      \item \code{as.character.srcref()} gains an argument to allow
      characters corresponding to a range of source references to be
      extracted.
    }
  }

  \subsection{BUG FIXES}{
    \itemize{
      \item \code{acf()} and \code{ccf()} now guarantee values strictly
      in \eqn{[-1,1]} (instead of sometimes very slightly outside). \PR{15832}.

      \item \code{as.integer("111111111111")} now gives NA (with a warning) as
      it does for the corresponding numeric or negative number
      coercions.  Further, \code{as.integer(M + 0.1)} now gives \code{M}
      (instead of NA) when M is the maximal representable integer.

      \item On some platforms \code{nchar(x, "c")} and \code{nchar(x, "w")}
      would return values (possibly \code{NA}) for inputs which were
      declared to be UTF-8 but were not, or for invalid strings without a
      marked encoding in a multi-byte locale, rather than give an
      error.  Additional checks have been added to mitigate this.

      \item \code{apply(a, M, function(u) c(X = ., Y = .))} again
      has dimnames containing "X" and "Y" (as in R < 3.2.0).

      \item (Windows only) In some cases, the \command{--clean} option
      to \command{R CMD INSTALL} could fail.  (\PR{16178})

      \item (Windows only) \code{choose.files()} would occasionally
      include characters from the result of an earlier call in
      the result of a later one.  (\PR{16270})

      \item A change in \code{RSiteSearch()} in \R 3.2.0 caused it to
      submit invalid URLs.  (\PR{16329})

      \item \command{Rscript} and command line \command{R} silently
      ignored incomplete statements at the end of a script; now they
      are reported as parse errors.  (\PR{16350})

      \item Parse data for very long strings was not stored.  (\PR{16354})

      \item \code{plotNode()}, the workhorse of the \code{plot} method
      for \code{"dendrogram"}s is no longer recursive, thanks to Suharto
      Anggono, and hence also works for deeply nested dendrograms.  (\PR{15215})

      \item The parser could overflow internally when given numbers
      in scientific format with extremely large exponents.  (\PR{16358})

      \item If the CRAN mirror was not set, \code{install.packages(type = "both")}
      and related functions could repeatedly query the user for it.
      (Part of \PR{16362})

      \item The low-level functions \code{.rowSums()} etc. did not
      check the length of their argument, so could segfault. (\PR{16367})

      \item The \code{quietly} argument of \code{library()} is now
      correctly propagated from \code{.getRequiredPackages2()}.

      \item Under some circumstances using the internal PCRE when
      building \R from source would cause external libs such as
      \code{-llzma} to be omitted from the main link.

      \item The .Primitive default methods of the logic operators, i.e.,
      \code{!}, \code{&} and \code{|}, now give correct error messages
      when appropriate, e.g., for \code{`&`(TRUE)} or \code{`!`()}.
      (\PR{16385})

      \item \code{cummax(x)} now correctly propagates \code{NA}s also
      when \code{x} is of type \code{integer} and begins with an \code{NA}.

      \item \code{summaryRprof()} could fail when the profile contained
      only two records.  (\PR{16395})

      \item HTML vignettes opened using \code{vignette()} did not support
      links into the rest of the HTML help system.  (Links worked properly
      when the vignette was opened using \code{browseVignettes()} or from
      within the help system.)

      \item \code{arima(*, xreg = .)} (for \eqn{d \ge 1}{d >= 1})
      computes estimated variances based on a the number of effective
      observations as in \R version 3.0.1 and earlier.  (\PR{16278})

      \item \code{slotNames(.)} is now correct for \code{"signature"}
      objects (mostly used internally in \pkg{methods}).

      \item On some systems, the first string comparison after
      a locale change would result in \code{NA}.
    }
  }
}

\section{\Rlogo CHANGES IN R 3.2.0}{
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

      \item Some unusual arguments to \code{embedFonts()} can now be
      specified as character vectors, and the defaults have been changed
      accordingly.

      \item Functions in the \code{Summary} group duplicate less.
      (\PR{15798})

      \item (Unix-alikes.) \code{system(cmd, input = )} now uses
      \sQuote{shell-execution-environment} redirection, which will be
      more natural if \code{cmd} is not a single command (but requires a
      POSIX-compliant shell). (Wish of \PR{15508})

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
      of columns to be computed on Unix-alikes: on some systems without
      properly-configured caching this can be significantly faster with
      large file lists.

      \item New function \code{dir.exists()} in package \pkg{base} to
      test efficiently whether one or more paths exist and are
      directories.

      \item \code{dput()} and friends gain new controls
      \samp{hexNumeric} and \samp{digits17} which output double and
      complex quantities as, respectively, binary fractions (exactly,
      see \code{sprintf("\%a")}) and as decimals with up to 17
      significant digits.

      \item \code{save()}, \code{saveRDS()} and \code{serialize()} now
      support \code{ascii = NA} which writes ASCII files using
      \code{sprintf("\%a")} for double/complex quantities.  This is
      read-compatible with \code{ascii = TRUE} but avoids
      binary->decimal->binary conversions with potential loss of
      precision.  Unfortunately the Windows C runtime's lack of C99
      compliance means that the format cannot be read correctly there in
      \R before 3.1.2.

      \item The default for \code{formatC(decimal.mark =)} has been
      changed to be \code{getOption("OutDec")}; this makes it more
      consistent with \code{format()} and suitable for use in print
      methods, e.g.\sspace{}those for classes \code{"density"}, \code{"ecdf"},
      \code{"stepfun"} and \code{"summary.lm"}.

      \code{getOption("OutDec")} is now consulted by the print method
      for class \code{"kmeans"}, by \code{cut()}, \code{dendrogram()},
      \code{plot.ts()} and \code{quantile()} when constructing labels
      and for the report from \code{legend(trace = TRUE)}.

      (In part, wish of \PR{15819}.)

      \item \code{printNum()} and hence \code{format()} and
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

      \item Packages may now use a file of common macro definitions in
      their help files, and may import definitions from other packages.

      \item A number of macros have been added in the new
      \file{share/Rd} directory for use in package overview help pages,
      and \code{promptPackage()} now makes use of them.

      \item \code{tools::parse_Rd()} gains a new \code{permissive}
      argument which converts unrecognized macros into text.  This is
      used by \code{utils:::format.bibentry} to allow LaTeX markup to be
      ignored.

      \item \code{options(OutDec =)} can now specify a multi-byte
      character, e.g., \code{options(OutDec = "\u00b7")} in a UTF-8
      locale.

      \item \code{is.recursive(x)} is no longer true when \code{x} is an
      external pointer, a weak reference or byte code; the first enables
      \code{all.equal(x, x)} when \code{x <- getClass(.)}.

      \item \code{ls()} (aka \code{objects()}) and
      \code{as.list.environment()} gain a new argument \code{sorted}.

      \item The \code{"source"} attribute (which has not been added to
      functions by \R since before \R version 2.14.0) is no longer
      treated as special.

      \item Function \code{returnValue()} has been added to give
      \code{on.exit()} code access to a function's return value for
      debugging purposes.

      \item \code{crossprod(x, y)} allows more matrix coercions when
      \code{x} or \code{y} are vectors, now equalling
      \code{t(x) \%*\% y} in these cases (also reported by Radford Neal).
      Similarly, \code{tcrossprod(x,y)} and \code{\%*\%} work in more
      cases with vector arguments.

      \item Utility function \code{dynGet()} useful for detecting
      cycles, aka infinite recursions.

      \item The byte-code compiler and interpreter include new
      instructions that allow many scalar subsetting and assignment and
      scalar arithmetic operations to be handled more efficiently. This
      can result in significant performance improvements in scalar
      numerical code.

      \item \code{apply(m, 2, identity)} is now the same as the matrix
      \code{m} when it has \emph{named} row names.

      \item A new function \code{debuggingState()} has been added,
      allowing to temporarily turn off debugging.

      \item \code{example()} gets a new optional argument
      \code{run.donttest} and \code{tools::Rd2ex()} a corresponding
      \code{commentDonttest}, with a default such that
      \code{example(..)} in help examples will run \code{\\donttest}
      code only if used interactively (a change in behaviour).

      \item \code{rbind.data.frame()} gains an optional argument
      \code{make.row.names}, for potential speedup.

      \item New function \code{extSoftVersion()} to report on the
      versions of third-party software in use in this session.
      Currently reports versions of \code{zlib}, \code{bzlib}, the
      \code{liblzma} from \code{xz}, PCRE, ICU, TRE and the \code{iconv}
      implementation.

      A similar function \code{grSoftVersion()} in package \pkg{grDevices}
      reports on third-party graphics software.

      Function \code{tcltk::tclVersion()} reports the Tcl/Tk version.

      \item Calling \code{callGeneric()} without arguments now works
      with primitive generics to some extent.

      \item \code{vapply(x, FUN, FUN.VALUE)} is more efficient notably
      for large \code{length(FUN.VALUE)}; as extension of \PR{16061}.

      \item \code{as.table()} now allows tables with one or more
      dimensions of length 0 (such as \code{as.table(integer())}).

      \item \code{names(x) <- NULL} now clears the names of call
      and \code{\dots} objects.

      \item \code{library()} will report a warning when an insufficient
      dependency version is masking a sufficient one later on the
      library search path.

      \item A new \code{plot()} method for class \code{"raster"} has
      been added.

      \item New \code{check_packages_in_dir_changes()} function in
      package \pkg{tools} for conveniently analyzing how changing
      sources impacts the check results of their reverse dependencies.

      \item Speed-up from Peter Haverty for \code{ls()} and
      \code{methods:::.requirePackage()} speeding up package loading.
      (\PR{16133})

      \item New \code{get0()} function, combining \code{exists()} and
      \code{get()} in one call, for efficiency.

      \item \code{match.call()} gains an \code{envir} argument for
      specifying the environment from which to retrieve
      the \code{\dots} in the call, if any; this environment was wrong
      (or at least undesirable) when the \code{definition} argument was
      a function.

      \item \code{topenv()} has been made \code{.Internal()} for
      speedup, based on Peter Haverty's proposal in \PR{16140}.

      \item \code{getOption()} no longer calls \code{options()} in the main
      case.

      \item Optional use of \code{libcurl} (version 7.28.0 from Oct 2012
      or later) for Internet access:
      \itemize{
	\item \code{capabilities("libcurl")} reports if this is
	available.

	\item \code{libcurlVersion()} reports the version in use, and
	other details of the \code{"libcurl"} build including which URL
	schemes it supports.

	\item \code{curlGetHeaders()} retrieves the headers for
	\samp{http://}, \samp{https://}, \samp{ftp://} and
	\samp{ftps://} URLs: analysis of these headers can provide
	insights into the `existence' of a URL (it might for example be
	permanently redirected) and is so used in \command{R CMD
	  check --as-cran}.

	\item \code{download.file()} has a new optional method
	\code{"libcurl"} which will handle more URL schemes, follow
	redirections, and allows simultaneous downloads of multiple
	URLs.

	\item \code{url()} has a new method \code{"libcurl"} which
	handles more URL schemes and follows redirections.  The default
	method is controlled by a new option \code{url.method}, which
	applies also to the opening of URLs \emph{via} \code{file()}
	(which happens implicitly in functions such as
	\code{read.table}.)

	\item When \code{file()} or \code{url()} is invoked with a
	\code{https://} or \code{ftps://} URL which the current method
	cannot handle, it switches to a suitable method if one is
	available.
      }

      \item (Windows.) The DLLs \file{internet.dll} and
      \file{internet2.dll} have been merged.  In this version it is safe
      to switch (repeatedly) between the internal and Windows internet
      functions within an \R session.

      The Windows internet functions are still selected by flag
      \option{--internet2} or \code{setInternet2()}.  This can be
      overridden for an \code{url()} connection \emph{via} its new
      \code{method} argument.

      \code{download.file()} has new method \code{"wininet"}, selected
      as the default by \option{--internet2} or \code{setInternet2()}.

      \item \code{parent.env<-} can no longer modify the parent of a
      locked namespace or namespace imports environment.  Contributed by
      Karl Millar.

      \item New function \code{isNamespaceLoaded()} for readability and speed.

      \item \code{names(env)} now returns all the object names of an
      \code{environment} \code{env}, equivalently to
      \code{ls(env, all.names = TRUE, sorted = FALSE)} and also to
      the names of the corresponding list,
      \code{names(as.list(env, all.names = TRUE))}.
      Note that although \code{names()} returns a character vector, the
      names have no particular ordering.

      \item The memory manager now grows the heap more aggressively. This
      reduces the number of garbage collections, in particular while
      data or code are loaded, at the expense of slightly increasing the
      memory footprint.

      \item New function \code{trimws()} for removing leading/trailing
      whitespace.

      \item \code{cbind()} and \code{rbind()} now consider S4 inheritance
      during S3 dispatch and also obey \code{deparse.level}.

      \item \code{cbind()} and \code{rbind()} will delegate recursively
      to \code{methods::cbind2} (\code{methods::rbind2}) when at least
      one argument is an S4 object and S3 dispatch fails (due to
      ambiguity).

      \item (Windows.)  \code{download.file(quiet = FALSE)} now uses
      text rather than Windows progress bars in non-interactive use.

      \item New function \code{hsearch_db()} in package \pkg{utils} for
      building and retrieving the help search database used by
      \code{help.search()}, along with functions for inspecting the
      concepts and keywords in the help search database.

      \item New function \code{.getNamespaceInfo()}, a no-check version
      of \code{getNamespaceInfo()} mostly for internal speedups.

      \item The help search system now takes \samp{\keyword} entries in
      Rd files which are not standard keywords (as given in
      \file{KEYWORDS} in the \R documentation directory) as concepts.
      For standard keyword entries the corresponding descriptions are
      additionally taken as concepts.

      \item New \code{lengths()} function for getting the lengths of all
      elements in a list.

      \item New function \code{toTitleCase()} in package \pkg{tools},
      tailored to package titles.

      \item The matrix methods of \code{cbind()} and \code{rbind()}
      allow matrices as inputs which have \eqn{2^{31}}{2^31} or more
      elements.  (For \code{cbind()}, wish of \PR{16198}.)

      \item The default method of \code{image()} has an explicit check
      for a numeric or logical matrix (which was always required).

      \item \code{URLencode()} will not by default encode further URLs
      which appear to be already encoded.

      \item \code{BIC(mod)} and \code{BIC(mod, mod2)} now give non-NA
      numbers for \code{arima()} fitted models, as \code{nobs(mod)} now
      gives the number of \dQuote{used} observations for such models.
      This fixes \PR{16198}, quite differently than proposed there.

      \item The \code{print()} methods for \code{"htest"},
      \code{"pairwise.htest"} and \code{"power.htest"} objects now have
      a \code{digits} argument defaulting to (a function of)
      \code{getOption("digits")}, and influencing all printed numbers
      coherently.  Unavoidably, this changes the display of such test
      results in some cases.

      \item Code completion for namespaces now recognizes all loaded
      namespaces, rather than only the ones that are also attached.

      \item The code completion mechanism can now be replaced by a
      user-specified completer function, for (temporary) situations
      where the usual code completion is inappropriate.

      \item \code{unzip()} will now warn if it is able to detect truncation
      when unpacking a file of 4GB or more (related to \PR{16243}).

      \item \code{methods()} reports S4 in addition to S3 methods;
      output is simplified when the \code{class} argument is
      used.  \code{.S3methods()} and \code{methods::.S4methods()} report
      S3 and S4 methods separately.

      \item Higher order functions such as the \code{apply} functions
      and \code{Reduce()} now force arguments to the functions they apply
      in order to eliminate undesirable interactions between lazy
      evaluation and variable capture in closures.  This resolves
      \PR{16093}.
    }
  }

  \subsection{INSTALLATION and INCLUDED SOFTWARE}{
    \itemize{
      \item The \code{\\donttest} sections of \R's help files can be
      tested by\cr
      \command{make check TEST_DONTTEST=TRUE} .

      \item It is possible to request the use of system
      \command{valgrind} headers \emph{via} \command{configure} option
      \option{--with-system-valgrind-headers}: note the possible future
      incompatibility of such headers discussed in the 'R Installation
      and Administration' manual. (Wish of \PR{16068}.)

      \item The included version of \code{liblzma} has been updated to
      \code{xz-utils} 5.0.7 (minor bug fixes from 5.0.5).

      \item \command{configure} options \option{--with-system-zlib},
       \option{--with-system-bzlib} and \option{--with-system-pcre} are
       now the default.  For the time being there is fallback to the
       versions included in the \R sources if no system versions are
       found or (unlikely) if they are too old.

       Linux users should check that the \code{-devel} or \code{-dev}
       versions of packages \pkg{zlib}, \pkg{bzip2}/\pkg{libbz2} and
       \pkg{pcre} as well as \pkg{xz-devel}/\pkg{liblzma-dev} (or
       similar names) are installed.

       \item \command{configure} by default looks for the
       \command{texi2any} script from \pkg{texinfo} 5.1 or later, rather
       than the \command{makeinfo} program.  (\command{makeinfo} is a
       link to the Perl script \command{texi2any} in \pkg{texinfo} 5.x.)

       \item \command{R CMD INSTALL} gains an  option
       \option{--built-timestamp=STAMP} allowing 100\% reproducible
       package building, thanks to Dirk Eddelbuettel.
     }
  }

  \subsection{UTILITIES}{
    \itemize{
      \item There is support for testing the \code{\\dontrun} and
      \code{\\donttest} parts of examples in packages.

      \code{tools::testInstalledPackage()} accepts new arguments
      \code{commentDontrun = FALSE} and \code{commentDonttest = FALSE}.

      \command{R CMD check} gains options \option{--run-dontrun} and
      \option{--run-donttest}.

      \item The \acronym{HTML} generated by \code{tools::Rd2HTML()} and
      \code{tools::toHTML()} methods is now
      \sQuote{\acronym{XHTML} 1.0 Strict}.

      \item The \pkg{compiler} package's utility function
      \code{setCompilerOptions()} now returns the old values
      invisibly. The initial optimization level can also be set with the
      environment variable \env{R_COMPILER_OPTIMIZE}.

      \item \command{R CMD build} adds a \samp{NeedsCompilation} field
      if one is not already present in the \file{DESCRIPTION} file.

      \item \command{R CMD check} gains option \option{--test-dir} to
      specify an alternative set of tests to run.

      \item \command{R CMD check} will now by default continue with
      testing after many types of errors, and will output a summary
      count of errors at the end if any have occurred.

      \item \command{R CMD check} now checks that the \samp{Title} and
      \samp{Description} fields are correctly terminated.

      \item \command{R CMD check --as-cran} now:
      \itemize{
	\item checks a \file{README.md} file can be processed: this
	needs \command{pandoc} installed.

	\item checks the existence and accessibility of URLs in the
	\file{DESCRIPTION}, \file{CITATION}, \file{NEWS.Rd} and
	\file{README.md} files and in the help files (provided the build
	has \code{libcurl} support).

	\item reports non-ASCII characters in R source files when there
	is no package encoding declared in the \file{DESCRIPTION} file.

	\item reports (apparent) S3 methods exported but not registered.

	\item reports overwriting registered S3 methods from
	base/recommended packages.  (Such methods are replaced in the
	affected package for the rest of the session, even if the
	replacing namespace is unloaded.)

	\item reports if the \code{Title} field does not appear to be in
	title case (see \sQuote{Writing R Extensions}: there may be
	false positives, but note that technical words should be
	single-quoted and will then be accepted).
      }

      Most of these checks can also be selected by environment
      variables: see the \sQuote{R Internals} manual.
    }
  }

  \subsection{C-LEVEL FACILITIES}{
    \itemize{
      \item New C API utility \code{logspace_sum(logx[], n)}.

      \item Entry points \code{rbinom_mu}, \code{rnbinom_mu} and
      \code{rmultinom} are remapped (by default) to \code{Rf_rbinom_mu}
      etc.  This requires packages using them to be re-installed.

      \item \code{.C(DUP = FALSE)} and \code{.Fortran(DUP = FALSE)} are
      now ignored, so arguments are duplicated if \code{DUP = TRUE}
      would do so.  As their help has long said, \code{.Call()} is much
      preferred.

      \item New entry point \code{R_allocLD}, like \code{R_alloc} but
      guaranteed to have sufficient alignment for \code{long double}
      pointers.

      \item \code{isPairList()} now returns \code{TRUE} for DOTSXP.
    }
  }

  \subsection{WINDOWS BUILD CHANGES}{
    A number of changes to the Windows build system are in development.
    The following are currently in place.
    \itemize{
      \item Installation using external binary distributions
      of \pkg{zlib}, \pkg{bzip2}, \pkg{liblzma}, \pkg{pcre},
      \pkg{libpng}, \pkg{jpeglib} and \pkg{libtiff} is now required,
      and the build instructions have been revised.

      \item A new \command{make} target \code{rsync-extsoft} has been
      added to obtain copies of the external libraries from
      \acronym{CRAN}.

      \item Building the manuals now requires \command{texi2any} from
      \pkg{texinfo} 5.1 or later.  \acronym{CRAN} binary builds include
      the manuals, but by default builds from source will not, and they
      will be accessed from \acronym{CRAN}.  See the comments in
      \file{src/gnuwin32/MkRules.dist} for how to specify the location
      of \command{texi2any}.

      \item (Windows) Changes have been made to support an experimental
      Windows toolchain based on GCC 4.9.2.  The default toolchain
      continues to be based on GCC 4.6.3, as the new toolchain is not
      yet stable enough.  A change to a new toolchain is expected during
      the R 3.2.x lifetime.
    }
  }

  \subsection{PACKAGE INSTALLATION}{
    \itemize{
      \item  (Windows) The use of macro \code{ZLIB_LIBS} in file
      \file{src/Makevars.win} (which has not been documented for a long
      time) now requires an external \file{libz.a} to be available (it is
      part of the \sQuote{goodies} used to compile Windows binary
      packages).  It would be simpler to use \code{-lz} instead.

      \item The default for option \code{pkgType} on platforms using
      binary packages is now \code{"both"}, so source packages will be
      tried if binary versions are not available or not up to date.

      There are options for what \code{install.packages(type = "both")}
      (possibly called \emph{via} \code{update.packages()}) will do if
      compilation of a source package is desirable: see \code{?options}
      (under \pkg{utils}).

      If you intend not to accept updates as source packages, you should
      use \code{update.packages(type = "binary")}.
    }
  }

  \subsection{DEPRECATED AND DEFUNCT}{
    \itemize{
      \item \code{download.file(method = "lynx")} is defunct.

      \item Building \R using the included versions of \code{zlib},
      \code{bzip2}, \code{xz} and PCRE is deprecated: these are frozen
      (bar essential bug-fixes) and will be removed for \R 3.3.0.

      \item The \command{configure} option
      \option{--with-valgrind-instrumentation=3} has been withdrawn, as
      it did not work with recent \command{valgrind} headers: it is now
      treated as level \code{2}.

      \item The \code{MethodsList} class in package \pkg{methods} had
      been deprecated in \R 2.11.0 and is defunct now.  Functions using
      it are defunct if they had been deprecated in \R 2.11.0, and are
      deprecated now, otherwise.
    }
  }

  \subsection{BUG FIXES}{
    \itemize{
      \item Fixed two obscure bugs in pairlist subassignment, reported by
      Radford Neal as part of pqR issue 16.

      \item Fixes for bugs in handling empty arguments and argument
      matching by name in \code{log()}.

      \item \code{all.equal()} gains methods for \code{environment}s and
      \code{refClass}es.

      \item \code{[<-} and \code{[[<-} gain S4 \code{data.frame} methods
      to avoid corruption of S4 class information by the S3 methods.

      \item \code{callNextMethod()} should now work within a \code{.local}
      call when \code{\dots} is absent from \code{formals(.local)}.

      \item \code{dput(pairlist(x))} generates a call to the
      \code{pairlist} constructor instead of the \code{list}
      constructor.

      \item Fix \code{missing()} when arguments are propagated through
      \code{\dots} . (\PR{15707})

      \item \code{eigen(m)} now defaults to \code{symmetric = TRUE} even
      when the dimnames are asymmetric if the matrix is otherwise
      symmetric.  (\PR{16151})

      \item Fix issues with forwarding \code{\dots} through
      \code{callGeneric()} and \code{callNextMethod()}.  (\PR{16141})

      \item \code{callGeneric()} now works after a \code{callNextMethod()}.

      \item Subclass information is kept consistent when replacing an
      ordinary S4 class with an \dQuote{old class} \emph{via} the
      \code{S4Class} argument to \code{setOldClass()}. Thus, for
      example, a \code{data.frame} is valid for a \code{list} argument
      in the signature, and a \code{factor} is valid for \code{vector}
      arguments.

      \item In \code{qbeta()} the inversion of \code{pbeta()} is much
      more sophisticated.  This works better in corner cases some of
      which failed completely previously (\PR{15755}), or were using
      too many iterations.

      \item Auto-printing no longer duplicates objects when printing is
      dispatched to a method.

      \item \code{kmeans(x, k)} would fail when \code{nrow(x) >= 42949673}.
      (Comment 6 of \PR{15364})

      \item \sQuote{Abbreviated} locale-specific day and month names
      could have been truncated in those rare locales where there are
      the same as the full names.

      \item An irrelevant warning message from updating subclass
      information was silenced (the namespace would not be writable
      in this case).
    }
  }
}

\section{\Rlogo CHANGES IN R 3.1.3}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item The internal method of \code{download.file()} can now handle
      files larger than 2GB on 32-bit builds which support such files
      (tested on 32-bit \R running on 64-bit Windows).

      \item \code{kruskal.test()} warns on more types of suspicious input.

      \item The \code{as.dendrogram()} method for \code{"hclust"}
      objects gains a \code{check} argument protecting against memory
      explosion for invalid inputs.

      \item \code{capabilities()} has a new item \code{long.double}
      which indicates if the build uses a \code{long double} type which
      is longer than \code{double}.

      \item \code{nlm()} no longer modifies the callback argument in
      place (a new vector is allocated for each invocation, which mimics
      the implicit duplication that occurred in R < 3.1.0); note that
      this is a change from the previously documented
      behavior. (\PR{15958})

      \item \code{icuSetCollate()} now accepts \code{locale = "ASCII"}
      which uses the basic C function \code{strcmp} and so collates
      strings byte-by-byte in numerical order.

      \item \code{sessionInfo()} tries to report the OS version in use
      (not just that compiled under, and including details of Linux
      distributions).

      \item \code{model.frame()} (used by \code{lm()} and many other
      modelling functions) now warns when it drops contrasts
      from factors.  (Wish of \PR{16119})

      \item \code{install.packages()} and friends now accept the value
      \code{type = "binary"} as a synonym for the native binary type on
      the platform (if it has one).

      \item Single source or binary files can be supplied for
      \code{install.packages(type = "both")} and the appropriate type
      and \code{repos = NULL} will be inferred.

      \item New function \code{pcre_config()} to report on some of the
      configuration options of the version of PCRE in use.  In
      particular, this reports if regular expressions using
      \samp{\p{xx}} are supported.

      \item (Windows.) \code{download.file(cacheOK = FALSE)} is now
      supported when \file{internet2.dll} is used.

      \item \code{browseURL()} has been updated to work with Firefox
      36.0 which has dropped support for the \option{-remote} interface.
    }
  }

  \subsection{INSTALLATION and INCLUDED SOFTWARE}{
    \itemize{
      \item The included version of PCRE has been updated to 8.36.

      \item \command{configure} accepts \samp{MAKEINFO=texi2any} as
      another way to ensure \pkg{texinfo} 5.x is used when both 5.x and
      4.x are installed.
    }
  }

  \subsection{UTILITIES}{
    \itemize{
      \item \command{R CMD check} now checks the packages used in
      \code{\\donttest} sections of the examples are specified in the
      \file{DESCRIPTION} file.
      (These are needed to run the examples interactively.)

      \item \command{R CMD check} checks for the undeclared use of GNU
      extensions in Makefiles, and for Makefiles with a missing final
      linefeed.

      \command{R CMD build} will correct line endings in all Makefiles,
      not just those in the \file{src} directory.

      \item \command{R CMD check} notes uses of \code{library()} and
      \code{require()} in package code: see the section
      \sQuote{Suggested packages} of \sQuote{Writing R Extensions} for
      good practice.
    }
  }
  \subsection{DEPRECATED AND DEFUNCT}{
    \itemize{
      \item The \command{configure} option
      \option{--with-valgrind-instrumentation=3} is deprecated and will
      be removed in \R 3.2.0.
    }
  }

  \subsection{BUG FIXES}{
    \itemize{
      \item (Windows.) \command{Rscript.exe} was missing a
      manifest specifying the modern style for common controls (e.g.,
      the download progress bar).

      \item If a package had extra documentation files but no vignette,
      the HTML help system produced an empty index page.

      \item The parser now gives an error if a null character is included
      in a string using Unicode escapes. (\PR{16046})

      \item \code{qr.Q()} failed on complex arguments due to
      pre-3.0(!) typo. (\PR{16054})

      \item \code{abs()} failed with named arguments when the argument
      was complex.  (\PR{16047})

      \item \code{"noquote"} objects may now be used as columns in
      data frames.  (\PR{15997})

      \item Some values with extremely long names were printed
      incorrectly.  (\PR{15999})

      \item Extremely large exponents on zero expressed in scientific
      notation (e.g. \code{0.0e50000}) could give \code{NaN}.  (\PR{15976})

      \item \code{download.file()} reported downloaded sizes as 0KB if
      less than 1MB, only for \R 3.1.2 and only on big-endian platforms.

      \item \code{prompt()} did not escape percent signs in the
      automatically generated usage section of help files.

      \item \code{drop.terms()} dropped some of the attributes of the
      object it was working with.  (\PR{16029})

      \item (Windows.) The command completion in \command{Rgui.exe}
      messed up the console.  (\PR{15791})

      \item (Windows.) The \code{choose.files()} command returned a
      blank string when the user asked for a single file but cancelled
      the request.  (\PR{16074})

      \item \code{Math2} S4 group generics failed to correctly dispatch
      \code{"structure"}- and \code{"nonStructure"}-derived classes.

      \item \code{loadNamespace()} imposed undocumented restrictions
      on the \code{versionCheck} parameter.  (Reported by Geoff Lee.)

      \item Rare over-runs detected by AddressSanitizer in
      \code{substr()} and its replacement version have been avoided.

      \emph{Inter alia} that fix gives the documented behaviour for
      \code{substr(x, 1, 2) <- ""} (subsequently reported as
      \PR{16214}).

      \item Loading packages incorrectly defining an S4 generic followed
      by a function of the same name caused an erroneous cyclic
      namespace dependency error.

      \item Declared vignette encodings are now always passed to the
      vignette engine.

      \item Port Tomas Kalibera's fix from R-devel that restores the
      \code{loadMethod()} fast path, effectively doubling the speed of
      S4 dispatch.

      \item \code{power.t.test()} and \code{power.prop.test()} now make
      use of the \code{extendInt} option of \code{uniroot()} and hence
      work in more extreme cases.  (\PR{15792})

      \item If a package was updated and attached when its namespace was
      already loaded, it could end up with parts from one version and
      parts from the other.  (\PR{16120})

      \item \code{tools:::.Rdconv()} didn't accept \code{--encoding=} due
      to a typo.  (\PR{16121})

      \item Unix-alike builds without a suitable \command{makeinfo} were
      documented to link the missing HTML manuals to CRAN, but did not.

      \item \code{save(*, ascii=TRUE)} and \code{load()} now correctly
      deal with \code{NaN}'s.  (\PR{16137})

      \item \code{split.Date()} retains fractional representations while
      avoiding incomplete class propagation.

      \item \file{R_ext/Lapack.h} had not been updated for changes made
      by LAPACK to the argument lists of its (largely internal)
      functions \code{dlaed2} and \code{dlaed3}.  (\PR{16157})

      \item \code{RShowDoc("NEWS", "txt")} had not been updated for the
      layout changes of \R 3.1.0.

      \item The \code{xtfrm()} method for class \code{"Surv"} has been
      corrected and its description expanded.

      \item \code{mode(x) <- y} would incorrectly evaluate \code{x} before
      changing its mode. (\PR{16215})

      \item \code{besselJ(1, 2^64)} and \code{besselY(..)} now signal a
      warning, returning \code{NaN} instead of typically
      segfaulting. (Issue 3 of \PR{15554})

      \item HTML conversion of \samp{\href} markup in \file{.Rd} files
      did not remove the backslash from \samp{\\\%} and so gave an
      invalid URL.  In a related change, the \samp{\\} escape is now
      required in such URLs.
    }
  }
}

\section{\Rlogo CHANGES IN R 3.1.2}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item \code{embedFonts()} now defaults to
      \code{format = "ps2write"} for \file{.ps} and \file{.eps} files.
      This is available in Ghostscript 9.x (since 2010) whereas the
      previous default, \code{format = "pswrite"}, was removed in
      Ghostscript 9.10.

      \item For consistency with \code{[dpqr]norm()},
      \code{[dp]lnorm(sdlog = 0)} model a point mass at
      \code{exp(mulog)} rather than return \code{NaN} (for an error).

      \item \code{capabilities()} now reports if ICU is compiled in for
      use for collation (it is only actually used if a suitable locale
      is set for collation, and never for a \code{C} locale).

      \item (OS X only.) Package \pkg{tcltk} checks when loaded if it is
      linked against the CRAN X11-based Tcl/Tk and if so that the Tcl/Tk
      component and the X11 libraries are installed.  This allows more
      informative error messages to be given advising the installation
      of the missing component or of XQuartz.

      The \code{X11()} device and X11-based versions of the data editor
      and viewer (invoked by \code{edit()} and \code{View()} for data
      frames and matrices from command-line \R) check that the X11
      libraries are installed and if not advises installing XQuartz.

      \item \code{icuSetCollate()} allows \code{locale = "default"}, and
      \code{locale = "none"} to use OS services rather than ICU for
      collation.

      Environment variable \env{R_ICU_LOCALE} can be used to set the
      default ICU locale, in case the one derived from the OS locale is
      inappropriate (this is currently necessary on Windows).

      \item New function \code{icuGetCollate()} to report on the ICU
      collation locale in use (if any).

      \item \code{utils::URLencode()} was updated to use unreserved and
      reserved characters from RFC 3986
      (\url{http://tools.ietf.org/html/rfc3986}) instead of RFC 1738.

      \item \code{unique(warnings())} and \code{c(warnings())} are now
      supported.

      \item The Bioconductor \sQuote{version} used by
      \code{setRepositories()} now defaults to \code{3.0}. (It can be
      set at runtime \emph{via} environment variable
      \env{R_BIOC_VERSION}.)

      \item Omegahat is no longer listed as providing Windows binary
      packages, e.g. by \code{setRepositories()}.  It has no binary
      packages available for \R 3.1.x and those for earlier versions
      were 32-bit only.
    }
  }

  \subsection{INSTALLATION and INCLUDED SOFTWARE}{
    \itemize{
      \item The \command{configure} script reports on the more important
      capabilities/options which will not be compiled in.

      More types of external BLAS are recognized by name in that report.

      \item When building \R as a shared library, the
      \option{-L${R_HOME}/lib${R_ARCH}} flag is placed earlier in the
      link commands used during installation and when packages are
      installed: this helps ensure that the current build has priority
      if an \R shared library has already been installed by
      e.g.\sspace{}\command{install-libR} in a library mentioned in
      \env{LDFLAGS} (and not in \sQuote{your system's library directory}
      as documented). (Wish of \PR{15790}.)

      \item LaTeX package \pkg{upquote} is no longer required for \R's
      use of \pkg{inconsolata}.

      \item (Windows only) If both 32- and 64-bit versions of \R are
      installed, the \file{bin/R.exe} and \file{bin/Rscript.exe} executables
      now run 64-bit \R.  (To run 32-bit \R, overwrite these files
      with copies of \file{bin/i386/Rfe.exe}.)
     }
  }

  \subsection{UTILITIES}{
    \itemize{
      \item Running \command{R CMD check} with
      \env{_R_CHECK_DEPENDS_ONLY_} true now makes the
      \samp{VignetteBuilder} packages available even if they are
      listed in \samp{Suggests}, since they are needed to recognise and
      process non-Sweave vignettes.

      \item \command{R CMD check} now reports empty \code{importFrom}
      declarations in a \file{NAMESPACE} file, as these are common
      errors (writing \code{importFrom(\var{Pkg})} where
      \code{import(\var{Pkg})} was intended).

      \item \command{R CMD check} now by default checks code usage
      directly on the package namespace without loading and attaching
      the package and its suggests and enhances.  For good practice with
      packages in the \samp{Suggests} field, see §1.1.3.1 of
      \sQuote{Writing R Extensions}.  For use of lazy-data objects in
      the package's own code, see \code{?data}.
    }
  }

  \subsection{BUG FIXES}{
    \itemize{
      \item \code{dmultinom()} did not handle non-finite probabilities
      correctly.

      \item \code{prettyNum(x, zero.print=*)} now also works when
      \code{x} contains \code{NA}s.

      \item A longstanding bug exhibited by \code{nlminb()} on Windows
      was traced to a compiler bug in gcc 4.6.3; a workaround has been
      put in place.  (\PR{15244} and \PR{15914}).

      \item Rendering of \code{\\command} in HTML versions of help pages
      has been improved: this is particularly evident on the help page
      for \code{INSTALL}.

      \item \code{as.hexmode(x)} and \code{as.octmode(x)} now behave
      correctly for some numeric \code{x}, e.g., \code{c(NA, 1)} or
      \code{c(1, pi)}.

      \item \code{drop1()} failed if the \code{scope} argument had
      no variables to drop.  (\PR{15935})

      \item \code{edit()} (and hence \code{fix()}) failed if
      an object had a non-character attribute named \code{"source"}
      (an attribute that had been used in \R prior to version 2.14.0).

      \item \code{callGeneric()} could fail if the generic had
      \code{\dots} as a formal argument. (\PR{15937}).

      \item Forking in package \pkg{parallel} called C entry point
      \code{exit} in the child.  This was unsafe (\code{_exit} should
      have been called), and could flush \code{stdin} of the main \R
      process (seen most often on Solaris).

      As good practice, \code{stdout} is now flushed before forking a child.

      \item \R objects such as \code{list(`a\\b` = 1)} now print correctly.

      \item \code{getAnywhere("C_pbinom")} now returns correctly a
      single object (rather than unlisting it).

      \item The \code{confint()} method for \code{nls()} fits failed it
      these has specified parameter limits despite using an algorithm
      other than \code{"port"}.  (\PR{15960})

      \item Subclassing an S4 class failed if the class required
      arguments to the generator, through its \code{initialize()} method.

      \item \code{removeSource()} did not properly handle expressions
      containing arguments that were supplied as missing, e.g.
      \code{x[i,]}. (\PR{15957})

      \item \code{as.environment(list())} now works, and
      \code{as.list()} of such an environment is now the same as
      \code{list()}. (\PR{15926})

      \item Several \pkg{tcltk} functions failed when run in unusual
      environments. (\PR{15970})

      \item \code{options(list())} now works (trivially). (\PR{15979})

      \item \code{merge(<dendrogram>, ..)} now works correctly for two
      `independent' dendrograms (\PR{15648}), and still compatibly
      \emph{via} \code{adjust = "auto"} e.g. for two branches of an
      existing dendrogram.

      \item The \code{plot} method for \code{"hclust"} objects gets an
      optional argument \code{check}; when that is true (the default) it
      checks more carefully for valid input.

      \item (Windows only) If a user chose to install 64 bit \R but not
      32 bit \R, the \file{bin/R} and \file{bin/Rscript} executables
      failed to run.  (\PR{15981})

      \item Various possible buffer overruns have been prevented,
      and missed memory protection added. (\PR{15990})

      \item \command{Rscript} no longer passes \code{--args} to
      \command{R} when there are no extra (\dQuote{user}) arguments.

      \item objects like \code{getClass("refClass")@prototype} now
      \code{print()} and \code{str()} without error.

      \item \code{identical()} now also looks at the S4 bit.

      \item \code{hist(x, breaks)} is more robust in adding a small fuzz
      to few breaks when some are very large. (\PR{15988})

      \item \code{sub()} and \code{gsub()} did not handle regular
      expressions like \code{"\\s{2,}"} properly if the text
      contained \code{NA} or non-ASCII elements in a UTF-8
      locale.  Part of this was due to a bug in the TRE library.
      (\PR{16009})

      \item \code{RShowDoc("NEWS")} now displays the PDF version.

      \item Matrices and arrays with last dimension zero did not print
      at all or incompletely.  (\PR{16012})

      \item \code{plot.histogram()} and hence \code{hist()} now respect
      the \code{xaxs}, \code{yaxs} and \code{lab} graphics parameters.
      (\PR{16021})

      \item \code{bw.SJ(x)} and other bw.*() no longer segfault when
      \code{x} contains non-finite values. (\PR{16024})

      \item \command{R CMD Rd2pdf} unintentionally ignored its
      \option{--os} option.

      \item The internal method of \code{download.file()} was not
      reporting file sizes and progress correctly on files larger than
      2GB (inherited from \code{libxml2}).  This is corrected for 64-bit
      builds (32-bit platforms may not support such files, but where
      possible will be supported in future versions of \R).

      \item Work around a bug in OS X Yosemite where key environment
      variables may be duplicated causing issues in subprocesses. The
      duplicates are now removed on \R startup (\emph{via}
      Rprofile). (\PR{16042})

      \item Adjust X11 auto-launch detection in DISPLAY on OS X to
      recognize latest XQuartz.
    }
  }
}

\section{\Rlogo CHANGES IN R 3.1.1}{
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

      \item The function determining the default number of knots for
      \code{smooth.spline()} is now exported, as \code{.nknots.smspl()}.

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
      \samp{http://} or \samp{ftp://} URLs of package archives as well
      as file paths, and will download as required.  In most cases
      \code{repos = NULL} can be deduced from the extension of the URL.

      \item The warning when using partial matching with the \code{$}
      operator on data frames is now only given when
      \code{options("warnPartialMatchDollar")} is \code{TRUE}.

      \item Package help requests like \code{package?foo} now
      try the package \code{foo} whether loaded or not.

      \item General help requests now default to trying all loaded
      packages, not just those on the search path.

      \item Added a new function \code{promptImport()}, to generate a
      help page for a function that was imported from another package
      (and presumably re-exported, or help would not be needed).
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

      \item Compilers/linkers were handling the visibility control in
      \file{src/extra/xz} inconsistently (and apparently in some cases
      incorrectly), so it has been simplified.  (\PR{15327})

      \item (Windows)  There is updated support for the use of ICU for
      collation: see the \sQuote{R Installation and Administration Manual}.
    }
  }

  \subsection{BUG FIXES}{
    \itemize{
      \item \code{dbinom(x, n)}, \code{pbinom()}, \code{dpois()}, etc,
      are slightly less restrictive in checking if \code{n} is
      integer-valued.  (Wish of \PR{15734}.)

      \item \code{pchisq(x, df, ncp, log.p = TRUE)} is more accurate and
      no longer underflows for small \code{x} and \code{ncp < 80}, e.g,
      for \code{pchisq(1e-5, df = 100, ncp = 1, log = TRUE)}.
      (Based on \PR{15635} and a suggestion by Roby Joehanes.)

      \item The \code{s} (\dQuote{step into}) command in the debugger
      would cause \R to step into expressions evaluated there,
      not just into functions being debugged.  (\PR{15770})

      \item The C code used by \code{strptime()} rejected time-zone
      offsets of more than \code{+1200} (\code{+1245}, \code{+1300} and
      \code{+1400} can occur).  (\PR{15768})

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
      e.g.\sspace{}\code{parse(FALSE)} attempted to read from \code{stdin}.

      Nor did \code{dump()} and \code{dput()}.

      \item The \code{"help.try.all.packages"} option was ignored when
      the shortcut syntax for help was used, e.g.\sspace{}\code{?foo}.

      \item A potential segfault in string allocation has been fixed.
      (Found by Radford Neal.)

      \item Potential memory protection errors in \code{sort()} and
      \code{D()} have been fixed. (Found by Radford Neal.)

      \item Fixed a lack of error checking in graphics event functions.
      (Found by Radford Neal; a different patch used here than the
      one in pqR.)

      \item \code{numericDeriv()} sometimes miscalculated the gradient.
      (\PR{15849}, reported originally by Radford Neal)
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
      non-opaque colours and a ramp in opacity \emph{via} the new argument
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
      a component which will be reliable (e.g., \code{$cov.unscaled})
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
      \samp{CXX1XFLAGS} and \samp{CXX1XSTD} (e.g., \samp{-std=gnu++11}).

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
      \file{inst}: some packages use this to install e.g.\sspace{}C++ headers
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

      \item Error messages involving non-syntactic names (e.g., as produced
      by \code{`\\r`} when that object does not exist) now encode
      the control characters.  (Reported by Hadley Wickham.)

      \item \code{getGraphicsEvent()} caused 100\% usage of one CPU in
      Windows. (\PR{15500})

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
      containing them to be constructed, e.g.\sspace{}\code{`\\``}.  (\PR{15621})

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
      functions (e.g., OS X).

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

      \item Double-clicking outside of the list box (e.g., on the scrollbar)
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
      but the command was not found (e.g., it contained its arguments,
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

      \item \code{rowsum()} has more support for matrices and data frames
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

      \item \code{tail(warnings())} now works, \emph{via} the new
      \code{`[`} method.

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
      namespace scoping rules.  E.g., \code{stats::lm()} can now find
      \code{stats::model.frame()} even if \pkg{stats} is not on the
      search path or if some package defines a function of that name.

      \item If an invalid/corrupt \code{.Random.seed} object is
      encountered in the workspace it is ignored with a warning rather
      than giving an error.  (This allows \R itself to rely on a working
      RNG, e.g.\sspace{}to choose a random port.)

      \item \code{seq()} and \code{seq.int()} give more explicit error
      messages if called with invalid (e.g., \code{NaN}) inputs.

      \item When \code{parse()} finds a syntax error, it now makes
      partial parse information available up to the location of the
      error.  (Request of Reijo Sund.)

      \item Methods invoked by \code{NextMethod()} had a different
      dynamic parent to the generic. This was causing trouble where S3
      methods invoked \emph{via} lazy evaluation could lose track of
      their generic.  (\PR{15267})

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
      namespace (e.g., as a default method).  It now reports calls where
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
      hexadecimal constants (e.g., \code{0x1.1p0}), rather than returning
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

      \item Errors in the encoding specified on the command line
      \emph{via} \command{--encoding=foo} were not handled properly.
      (\PR{15405})

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
      not record information correctly, so replaying a graph (e.g., by
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
      facilities (e.g., \command{ulimit} in a \command{bash} shell,
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

      \item Matrix indexing of data frames by two-column numeric indices
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
      unsafe in a shell (e.g., absolute paths containing spaces) now uses
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

      \item More matrix algebra functions (e.g., \code{chol()} and
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

      \item Profiling \emph{via} \code{Rprof()} now optionally records
      information at the statement level, not just the function level.

      \item The \code{Rprof()} function now quotes function names in
      in its output file on Windows, to be consistent with the quoting
      in Unix.

      \item Profiling \emph{via} \code{Rprof()} now optionally records
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
      with integer indices (e.g., \code{anyDuplicated()} and
      \code{match()}) or because other limits (e.g., of character strings
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
      the input(s) are not double (e.g., integer or logical).

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
      \R, e.g.\sspace{}on OS X.
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
      packages (e.g., \code{R_zeroin2}) or replaced by different non-API
      entry points (e.g., \code{R_tabulate}).

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
      control in the OS by its own bookkeeping \emph{via} \code{mcaffinity()}
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

      \item Field assignments in reference class objects \emph{via}
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

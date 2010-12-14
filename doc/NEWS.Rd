\newcommand{\PR}{\Sexpr[results=rd]{tools:::Rd_expr_PR(#1)}}

\name{NEWS}
\title{R News}
\encoding{UTF-8}

\section{CHANGES IN R VERSION 2.12.1}{
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

      \item New reference class utility methods \code{copy()}, \code{field()},
      \code{getRefClass()} and \code{getClass()} have been added.
     
      \item When a character value is used for the \code{EXPR}
      argument in \code{switch()}, a warning is given if more than
      one unnamed alternative value is given.  This will become an
      error in R 2.13.0.

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

      \item Rendering of raster images on Cairo X11 device was wrong,
      particularly when a small image was being scaled up using
      interpolation.

      With Cairo < 1.6, will be better than before, though still a
      little clunky.  With Cairo >= 1.6, should be sweet as.
            
      \item Several bugs fixed in \code{read.DIF()}:  single column
      inputs caused errors, cells marked as \code{"character"} could
      be converted to other types, and (in Windows) copying from the
      clipboard failed.      
    }
  }
}

\section{CHANGES IN R VERSION 2.12.0}{
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
       a number of packages, e.g. \pkg{MASS}.  (\PR{14250})

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
       (\command{R CMD INSTALL} does so), and \code{--force} removes
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

      \item \command{R CMD Sweave} now runs \R with \code{--vanilla} so
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


\section{CHANGES IN R VERSION 2.11.1 patched}{
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


\section{CHANGES IN R VERSION 2.11.1}{
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

      \item \code{splinefun(method = \code{"monoH.FC"})} was not
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

      \item \code{do.call()}ing \code{NextMethod} in erronous ways no
      longer segfaults. (\PR{13487})
    }
  }
}


\section{CHANGES IN R VERSION 2.11.0}{
  \subsection{SIGNIFICANT USER-VISIBLE CHANGES}{
    \itemize{
      \item Packages must have been installed under \R >= 2.10.0, as the
       current help system is the only one now supported.

      \item A port to 64-bit Windows is now available as well as binary
       package repositiories: see the \sQuote{R Administration and
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
       name spaces).  By default \code{detach()} prevents such
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
       matchArgs for the same function \emph{via} a finalizer.

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


\section{CHANGES IN R VERSION 2.10.1 patched}{
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


\section{CHANGES IN R VERSION 2.10.1}{
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
      \code{qchisq()} are more accurate for probablilities very close
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

      \item S4 methods are uncached whenever the name space containing
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


\section{CHANGES IN R VERSION 2.10.0}{
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
       now returns a sparse Matrix, using package \pkg{Matrix}.

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
       the function of that name in the \pkg{utils} name space is used.

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
       particularly effective for \pkg{survival}.)

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

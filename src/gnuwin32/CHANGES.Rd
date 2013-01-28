\newcommand{\PR}{\Sexpr[results=rd]{tools:::Rd_expr_PR(#1)}}
\newcommand{\CRANpkg}{\href{http://CRAN.R-project.org/package=#1}{\pkg{#1}}}

\name{CHANGES}
\title{Windows-specific changes to R}
\encoding{UTF-8}

\section{CHANGES IN R VERSIONS > 2.15.0}{
  \itemize{
    \item Windows-specific changes will now be announced in the
    common NEWS file.
  }
}

\section{CHANGES IN R VERSION 2.15.0}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item The 32-bit and 64-bit builds are now treated equally, as it
      is anticipated that 64-bit R will soon be (if not already) the
      more commonly used.  This entails:
      
      The \command{Rterm} or \command{Rgui} title mentions 32-bit as
      well as 64-bit.
      
      The desktop icons are labelled \code{R i386} and \code{R x64}.

      \command{R CMD INSTALL} for a package with compiled code will fail
      if compilation fails for any of the installed sub-architectures:
      use \option{--no-multiarch} to override this.

      \item \code{postscript(file = "|cmd")} now works on Windows.
      
      \item \code{file.symlink()} is now implemented on NTFS file
      systems on Vista or later, for accounts which have suitable
      permissions (and most will not).  It can link existing
      directories, and existing-or-not files.

      \item There is a new function \code{Sys.junction()} to create
      junction points on NTFS file systems.

      \item C-level error formats now use the trio library, and so for
      example output \samp{1.53e-11} (as required by C99) rather than
      \samp{1.53e-011}.
    }
  }
  
  \subsection{INSTALLER}{
    \itemize{
      \item The installer has many fewer options with more files always
      being installed, as a few Mb of file space is nowadays no longer
      an issue.

      \item It is now possible to select \sQuote{64-bit Files}
      from the standard installer even on a 32-bit version of Windows.
    }
  }
  
  \subsection{PACKAGE INSTALLATION}{
    \itemize{
      \item As the current toolchain uses only 32-bit executables, it is
      possible to compile a package for both architectures on 32-bit
      Windows provided both architectures of \R have been installed: use
      \command{Rcmd INSTALL --compile-both}.

      \item \code{Rcmd INSTALL --merge-multiarch} will work (but do a
      normal install) if only one architecture is installed.
    }
  }

  \subsection{BUG FIXES}{
    \itemize{
      \item Due to a race condition, some graphics changes were not
      flushed to the display if another graphics window was closed at
      nearly the same time.  (Reported by Michael Sumner.)      
    }
  }
}

\section{CHANGES IN R VERSION 2.14.2}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item \code{bitmap()} and \code{dev2bitmap()} look for
      \command{gswin64c.exe} (as used by 64-bit GhostScript), in
      preference to \command{gswin32c.exe}.
    }
  }
  
  \subsection{INSTALLATION}{
    \itemize{
      \item The sources (and packages) can now be compiled using the
      multilib toolchain developed for \R 2.15.x: see the
      \samp{MULTI} macro in \file{MkRules.dist}.  This toolchain is used
      for the CRAN binary distribution.
    }
  }
      
  \subsection{PACKAGE INSTALLATION}{
    \itemize{
      \item The preferred toolchain has changed since the one used for
      \R 2.12.0 to 2.14.1: see the\sQuote{R Installation and
      Administration manual}.  Compiled code (except DLLs) may be
      incompatible with previous toolchains (and compiled C++ code
      almost certainly will be: users of \CRANpkg{Rcpp} take care).

      Ensure that the settings in \file{MkRules.local} are appropriate
      to the toolchain you use.

      \item There is a new macro (aka make variable)
      \samp{SHLIB_PTHREAD_FLAGS}.  This is set to \option{-pthread} on
      builds using toolchains which support it, and should be included
      in both \samp{PKG_CPPFLAGS} (or the Fortran or F9x equivalents)
      and \samp{PKG_LIBS}.
    }
  }

  \subsection{BUG FIXES}{
    \itemize{
      \item Using a prompt of more than 80 characters in
      \code{readline()} could cause a buffer overflow in Rterm.
      (Reported by Henrik Bengtsson.)

      \item Some of the custom messages in the installer were corrupted:
      add a BOM mark to the file as now required by Unicode Inno Setup.
      (\PR{14816})
    }
  }
}

\section{CHANGES IN R VERSION 2.14.1}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item The \code{Save as} menu item on the script
      editor adds extension \file{.R} to a file name without an extension.

      \item In package \pkg{parallel}, \code{detectCores(logical =
	FALSE)} makes an OS-dependent attempt to find the number of
      physical cores.  It usually succeeds, even on XP.
    }
  }
  
  \subsection{PACKAGE INSTALLATION}{
    \itemize{
      \item The directory pointed to by \env{USER_LOCAL} can now have
      architecture-specific sub-directories \file{lib/i386} and
      \file{lib/x64}.
    }
  }
  
  \subsection{BUG FIXES}{
    \itemize{
      \item The fix for \PR{14543} caused stack problems with outputting
      large \R objects (e.g. data frames of 25,000 items).  (\PR{14698})

      \item In a double-byte locale (Chinese, Japanese, Korean),
      \code{grep()} and friends might have used byte-wise matching of
      strings in the native encoding.  (\PR{14622})
    }
  }
}

\section{CHANGES IN R VERSION 2.14.0}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item The \code{yLineBias} of the \code{windows()} family of
      devices has been changed from 0.1 to 0.2: this changes slightly
      the vertical positioning of text (including axis annotations).
      This can be overridden by setting the new \code{"ylbias"} graphical
      parameter.  This was done for consistency with other devices
      such as \code{pdf()}.

      \item \command{R CMD build} once again attempts to preserve file
      permissions on Windows.

      \item There is support for cairographics-based devices using the
      same code as on Unix-alikes.  This can be selected by the new
      \code{type} argument of the bitmap devices \code{bmp()},
      \code{jpeg()}, \code{png()} and \code{tiff()}, and devices
      \code{svg()}, \code{cairo_pdf()} and \code{cairo_ps()} are now
      available on Windows.

      These are not compiled in by default when building from source:
      see the instructions in the \sQuote{R Installation and
	Administration Manual}.

      \item All the Windows-specific graphics devices now have a
      \code{family} argument.  If non-empty this specifies an initial
      family to be used for fonts 1--4.  If empty the fonts specified in
      the \file{Rdevga} configuration file are used for the Windows GDI
      devices and \code{"sans"} for cairographics-based devices.

      This will generally be a Windows font name such as
      \code{"Lucida Bright"} or one of the device-independent names
      (\code{"sans"}, \code{"serif"} and \code{"mono"}).  Outside
      Western Europe you may need to select a family that better
      supports your locale such as \code{"Arial MS Unicode"} or one
      specific to Chinese/Korean/Thai \dots{}.

      \item There is a new \file{antialias} argument to
      \code{windows()}, \code{win.print()} and the bitmap devices.
      This is an option that can be set in \code{windows.options()} to
      set the default for \code{windows()} (and \code{win.graph()}).

      This gives a hint to the Windows plotting system.  Whether
      anti-aliasing is actually used principally depends on the OS
      settings: this argument should at least be able to turn it
      off.  The default behaviour (unchanged from before) is that Windows
      will use anti-aliasing for screen devices (and bitmap devices, as
      they plot on a hidden screen) if ClearType has been enabled.  For
      those not using ClearType, \code{windows.options(antialias =
	"cleartype")} will make this the default, and it will probably
      give more legible plots. 

      The argument can also be used for the cairographics-based versions
      of the bitmap devices.

      \item The \sQuote{Update packages ...} menu item now runs
      \code{update.packages(ask="graphics", checkBuilt=TRUE)}.

      \item \code{R CMD INSTALL} preserves the package-directory
      modification time when it restores an earlier install of the package.

      \item File extensions \code{.xz}, \code{.rda} and \code{.RData}
      have been added to those which default to binary transfer for
      \code{download.file()}.

      \item \code{install.packages()} and \command{R CMD check} have a
      small delay after removing a directory to counteract some
      interference from anti-virus software.

      \item There is a Danish translation of the RGui menus.
    }
  }

  \subsection{INSTALLATION}{
    \itemize{
      \item Compilation of C and Fortran code now uses the optimization
      flag \option{-mtune=core2}: this will improve performance a few
      percent on recent CPUs at the expense of those which are several
      years old.  Its effect is particularly evident on 64-bit builds.

      This can be overridden when building from the sources: see the
      \samp{EOPTS} macro defined in file \file{MkRules.dist}.

      \item Where both 32- and 64-bit versions of R are installed, the
      file association for \file{.RData} files defaults to 64-bit R (it
      defaulted to 32-bit in R 2.12.x and 2.13.x).

      \item There is preliminary support for \sQuote{multilib}
      toolchains which use options \option{--m32} or \option{--m64} to
      select the architecture; set the appropriate macros in
      \file{MkRules.local}.

      It is the intention to move to such a toolchain when they become
      mature enough.
    }
  }

  \subsection{PACKAGE INSTALLATION}{
    \itemize{
      \item \file{Rzlib.dll} (sometimes used in packages \emph{via}
      \samp{$(ZLIB_LIBS)}) does not include the buggy gzio   % for balance: $
      interface from zlib 1.2.5.
    }
  }

  \subsection{DEFUNCT}{
    \itemize{
      \item{zip.unpack()} (deprecated in \R 2.13.0) is defunct:
      use \code{unzip()} instead.
    }
  }

  \subsection{BUG FIXES}{
    \itemize{
      \item Raster drawing on \code{win.metafile()} (or copying a plot
      that includes a raster image from another device to a Metafile)
      now does not crash.  (Reported by Stefan Gelissen.)
    }
  }
}

\section{CHANGES IN R VERSION 2.13.2}{
  \subsection{BUG FIXES}{
    \itemize{
      \item The fix for \PR{14583} caused inconsistent behaviour in other
      areas, e.g. \PR{14628}, extra lines appearing in image displays,
      and uneven bases on histograms.  (\PR{14632}).
      
      \item Opening large numbers of \code{windows()} graphics devices
      could cause some of them to fail to redraw (\PR{14668}).
    }
  }
}

\section{CHANGES IN R VERSION 2.13.1}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item \command{R CMD build} no longer attempts to preserve file
      permissions on Windows, because Windows (unlike POSIX-compliant
      OSes) stops read-only files being deleted in version-control
      directories.
      
      \item \code{shell.exec()} now interprets files relative to the current
      working directory (rather than \samp{R_HOME}).

      \item \code{file.info()} now accepts \sQuote{file} names with
      trailing directory separators, even though such names are invalid
      on Windows.

      \item The \code{windows()} family of devices now supports()
      per-pixel alpha for raster images.
    }
  }
  
  \subsection{BUG FIXES}{
    \itemize{
      \item Launching the PDF manuals from the Rgui menus did not work
      on some versions of Windows.
      
      \item The windows() screen device would sometimes fail to plot
      (visibly) points with \code{pch="."}. \PR{14583}
      
      \item \code{system()} and related functions sometimes failed
      when run in \file{Rgui}.

      \item Saving to PDF from the menu of a \code{windows()} device
      used defaults for \code{family}, \code{fg} and \code{bg} from
      \code{ps.options()} rather than \code{pdf.options()} (but
      the factory-fresh defaults were the same).
      
      \item Shutting down the \R session cleaned up the temporary
      directory before closing all graphics devices.  On Windows this
      necessitated changing the working directory, so some devices
      (e.g. \code{tiff()}) could write files in the wrong directory if
      closed during shutdown.  The order has been reversed.      
    }
  }
}
  
\section{CHANGES IN R VERSION 2.13.0}{
  \subsection{WINDOWS VERSION}{
    \itemize{
      \item Windows 2000 is no longer supported.  (It went end-of-life in
      July 2010.)
    }
  }
    
  \subsection{NEW FEATURES}{
    \itemize{
      \item \code{win_iconv} has been updated: this version has a change
      in the behaviour with BOMs on UTF-16 and UTF-32 files -- it
      removes BOMs when reading and adds them when writing.  (This is
      consistent with Microsoft applications, but Unix versions of
      \code{iconv} usually ignore them.)

      \item Support for repository type \samp{win64.binary} (used for
      64-bit Windows binaries for \R 2.11.x only) has been removed.

      \item The installers no longer put an \sQuote{Uninstall} item on
      the start menu (to conform to current Microsoft UI guidelines).

      \item Running \R always sets the environment variable \env{R_ARCH}
      (as it does on a Unix-alike from the shell-script front-end).

      \item The defaults for \code{options("browser")} and
      \code{options("pdfviewer")} are now set from environment variables
      \env{R_BROWSER} and \env{R_PDFVIEWER} respectively (as on a
      Unix-alike).  A value of \code{"false"} suppresses display (even
      if there is no \command{false.exe} present on the path).
      
      \item If \code{options("install.lock")} is set to \code{TRUE},
      binary package installs are protected against failure similar to
      the way source package installs are protected.

      \item \code{file.exists()} and \code{unlink()} have more support
      for files > 2GB.

      \item The versions of \command{R.exe} in
      \file{R_HOME/bin/{i386,x64}/bin} now support options such as
      \command{R --vanilla CMD}: there is no comparable interface for
      \file{Rcmd.exe}.

      \item A few more file operations will now work with >2GB files.

      \item The environment variable \env{R_HOME} in an \R session now
      uses slash as the path separator (as it always has when set by
      \command{Rcmd.exe}).

      \item \command{Rgui} has a new menu item for the PDF
      \sQuote{Sweave User Manual}.
    }
  }

  \subsection{DEPRECATED}{
    \itemize{
      \item{zip.unpack()} is deprecated: use \code{unzip()}.
    }
  }
  \subsection{INSTALLATION}{
    \itemize{
      \item There is support for \samp{libjpeg-turbo} \emph{via}
      setting \code{JPEGDIR} to that value in \file{MkRules.local}.

      Support for \samp{jpeg-6b} has been removed.

      \item The sources now work with \samp{libpng-1.5.1}, 
      \samp{jpegsrc.v8c} (which are used in the CRAN builds) and
      \samp{tiff-4.0.0beta6} (CRAN builds use 3.9.1).  It is possible
      that they no longer work with older versions than 
      \samp{libpng-1.4.5}. 
    } 
  }

  \subsection{BUG FIXES}{
    \itemize{
      \item Workaround for the incorrect values given by Windows'
      \code{casinh} function on the branch cuts.

      \item Bug fixes for drawing raster objects on \code{windows()}.  The
      symptom was the occasional raster image not being drawn, especially
      when drawing multiple raster images in a single expression.
      Thanks to Michael Sumner for report and testing.
      
      \item Printing extremely long string values could overflow the
      stack and cause the GUI to crash.  (\PR{14543})
    }
  }
}

\section{CHANGES IN R VERSION 2.12.2}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item Building badly-written vignettes which used the default
      device (\code{pdf(file = "Rplots.pdf")} could leave the device
      open and hence the file \file{Rplots.pdf} could not be removed on
      Windows: all devices are now closed.
    }
  }

  \subsection{BUG FIXES}{
    \itemize{
      \item In some circumstances the caret (cursor) could be positioned 
      incorrectly. (\PR{14460})
      
      \item When loading \file{Rconsole} from disk, a spurious warning
      message was sometimes shown.
      
      \item In \R 2.12.1, \code{system()} sometimes passed an empty
      input stream to the new process rather than allowing user input.

      \item The internal method for \code{untar()} does a better job of
      unravelling symbolic links in the tarball (but still with many
      limitations).
      
      \item The graphical dialog version of \code{select.list()} could
      cause later dialogs to fail if Windows events were not processed
      between the calls.
    }
  }
}

\section{CHANGES IN R VERSION 2.12.1}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item \code{system()} and \code{system2()} with output redirected
      to a character vector now give a warning if the command returns a
      non-zero error code.  (This is what a Unix-alike does as from \R
      2.12.0.)

      \item \code{system()} and \code{system2()} no longer discard
      output from the command to stdout and stderr as often as they used
      to, and more cases of \code{system2()} are implemented.

      \item \code{stdout} is now flushed after echoing the input line.

      \item The internal implementation of \code{tar()} (package
      \pkg{utils}) is now supported on Windows.
    }
  }

  \subsection{INSTALLATION}{
    \itemize{
      \item The MSI installer is now made by an \R script, and there is
      now support for 64-bit builds.  (Unlike the Inno Setup installer,
      there are separate MSI installers for 32- and 64-bit Windows.)

      It is now built using \command{WiX 3.x} rather than \command{WiX
	2.0}.  (As a result it is about 25\% smaller.)
    }
  }

  \subsection{BUG FIXES}{
    \itemize{
      \item Some warning/error messages from \code{dyn.load()} could be
      masked in \command{Rgui} on Windows 7, as that OS can return
      messages containing carriage returns and \command{Rgui} does not
      support overwriting.
      
      \item Filenames could not be specified using backslashes in 
      \code{Sweave()}.

      \item In \R 2.12.0, \code{system(intern = TRUE)} captured
      \file{stderr} on \command{Rterm} when it was documented to only do
      so on \command{Rgui}.

      \item \command{Rgui} in \R 2.12.0 (only) did not always handle lines
      containing carriage returns (\samp{\r}) correctly: sometimes
      random characters where shown.

      Since carriage returns have been supported in \command{Rgui},
      the cursor positioning after outputting a line containing a
      carriage return had been unreliable.
    }
  }
}

\section{CHANGES IN R VERSION 2.12.0}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item A sub-architecture is used for 32-bit (i386) as well as
       64-bit (x64) builds.  32-bit packages containing compiled code
       need to re-installed for this release (64-bit packages built
       for 2.11.x will most likely still work).
      
       Architecture-specific files (such as \file{Rgui.exe} and
       \file{Rscript.exe}) are now in \file{R_HOME/bin/i386} or
       \file{R_HOME/bin/x64}.
      
       Because many people expect it, there is an executable
       \file{R_HOME/bin/R.exe}, which accepts \option{--arch} and
       launches either \file{R_HOME/bin/i386/R.exe} or
       \file{R_HOME/bin/x64/R.exe}, and analogously for
       \file{Rscript.exe}.  If not specified by their argument
       \option{--arch}, these check the environment variable
       \env{R_ARCH} for the architecture to use.
        
      \item The \command{Rgui} console now has an optional blinking
        caret (cursor) to assist screen readers for the visually
        impaired.
        
      \item \code{normalizePath()} now signals a warning instead of an
        error when the input path does not exist.  It attempts partial
        normalization, or if that fails, returns the original input.
      
      \item All the \command{Rcmd} sub-commands except \code{config} and
       \code{Sd2d} are now handled internally rather by shell scripts.
      
      \item \command{Rcmd} has a new sub-command \command{texify} to run
       MiKTeX's \file{texify.exe} with arguments to find files such as
       \file{Sweave.sty}.  See the examples in \code{?Sweave}.

       \item \code{system(intern = FALSE)} now returns \code{127} (the
       same as Unix-alikes) rather than \code{-1} if the command cannot
       be run, and (like Unix-alikes) no longer gives a warning.

       \item \command{Rscript -e} and similar use a temporary file, and
       now work harder to ensure that filename is not the same as some
       other simultaneous \R process.
    }
  }
  \subsection{INSTALLATION}{
    \itemize{
      \item The preferred toolchain for 32-bit builds (including
       packages) is now based on \command{gcc 4.5.0}: see the \sQuote{R
       Installation and Administration Manual}.
      
      \item It is now possible to build \R and the standard installer
        without Perl: only maintainer-mode tasks now need Perl.
    }
  }
  \subsection{64-bit NEWS}{
    \itemize{
      \item The 32- and 64-bit distributions have been merged:
      
        There is a combined installer (called
        \file{R-<version>-win.exe}).  When used under 32-bit Windows
        this works as before and installs 32-bit \R.  When used under
        64-bit Windows there are options to install either or both of
        32- and 64-bit \R: the default is to install both.
      
        The default package type is \code{"win.binary"} on both 32- and
        64-bit builds: a single repository contains binary packages for
        both architectures.  This is in place for CRAN, CRAN extras and
        BioC: \code{type = "win64.binary"} can still be used for any
        repositories which follow the \R 2.11.x convention.  With the
        default filters (see \code{?available.packages}) packages will
        only be offered if they are available for the current
        architecture.

	The default personal library, e.g. \file{~/R/win-library/2.12}
	is now the same for both 32- and 64-bit \R.
	
      \item The recommended 64-bit toolchain has been changed to one
      that does not add leading underscores.  See
      \url{http://www.murdoch-sutherland.com/Rtools/} or
      \url{http://www.stats.ox.ac.uk/pub/Rtools/}.  Any static or import
      libraries for external software used in installing packages will
      need to be re-compiled for this toolchain, but the use of DLLs is
      unaffected.

      \item For those building \R from the sources, the 64-bit
       \code{Tcl} binary distribution for 2.12.x is in file
       \file{R64a_Tcl_8-5-8.zip} and contains e.g. directory
       \file{Tcl/bin64} and not \file{Tcl/bin}.
    }
  }
  \subsection{PACKAGE INSTALLATION}{
    \itemize{
      \item The deprecated environment variable \env{RHOME} is no longer
      set when a package is installed: use \env{R_HOME} instead (and
      depend on \code{R (>= 2.9)} ).
      
      \item Package installation, including \file{configure.win}
       scripts, can use \code{$\{R_HOME\}/bin$\{R_ARCH_BIN\}} as a
       portable way to find the binaries such as \file{Rscript.exe}.
    }
  }
  \subsection{BUG FIXES}{
    \itemize{
      \item \command{R.exe} when used in place of \command{Rterm.exe}
      set the environment variables set by \command{R CMD}.  Conversely,
      \command{R CMD BATCH} did not.
    }
  }
}

\section{CHANGES IN R VERSION 2.11.1 patched}{
  \subsection{BUG FIXES}{
    \itemize{
      \item \code{file.rename()} now waits for the rename to complete
      before returning, so that operations on the new filename will
      not fail.
      
      \item \code{Sys.info()} and \code{win.version()} now report
      Windows 7 as such (and not as Vista), and \code{Sys.info()} now
      reports the machine type as \code{"x86-64"} on 64-bit platforms.

      \item \code{serialize()} to a raw vector works around the dismal
      performance of Windows' memory manager by allocating much larger
      chunks of memory less frequently.
      
      \item \code{readClipboard()} could cause a segfault with certain
      clipboard data.

      \item A fix intended for Mac OS X was incorrectly applied to
      Windows, so \code{iconv(x, from, "")} in a C locale was
      interpreted as requesting translation to UTF-8.

      \item \code{system(intern=TRUE)} often returned \code{0}
      rather than an error status if the command could not be found, if
      run from \code{Rterm} or from \code{Rgui} with
      \code{show.output.on.console = FALSE}.
      
      \item \code{memory.limit()} did not allow sizes greater than
      4095 on 64 bit builds. (\PR{14372})
    }
  }
}

\section{CHANGES IN R VERSION 2.11.1}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item The \command{Rgui} console, pagers and editor on the 64-bit
       build now have a title bar icon.
      
      \item The outer frame/console for \command{Rgui/Rterm} mentions
       '(64-bit)' to make it easier to distinguish running copies of \R.
      
      \item The package entries in listbox shown by the \code{Install
       package(s) ...} menu are now explicitly sorted in the current
       locale (as they were in most previous releases, but not in
       2.11.0).
      
      \item \command{R CMD INSTALL} can install zipfiles as binary
      packages.
        
      \item Errors in the \file{Rconsole} file are ignored: silently on
        startup, with a warning when \file{Rconsole} is loaded
        interactively.
    }
  }
}

\section{CHANGES IN R VERSION 2.11.0}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item There is now a native x64 Windows port, with a binary installer
       and binaries of packages available on CRAN.  See the 'R
       Administration and Installation Manual' and the 'R for Windows FAQ'.
      
      \item \command{Rcmd cmd} (or \command{R CMD cmd}) now runs an
       arbitrary '\option{cmd}' if '\option{cmd}' is not one of the
       builtin commands, and does not have extension '\file{.sh}' or
       '\file{.pl}' (when it is run as a shell or Perl script
       respectively).  This is done by passing '\option{cmd}' to the
       command interpreter determined by the \env{COMSPEC} environment
       variable (which in a vanilla Windows system points to
       \file{cmd.exe}).
      
      \item \code{X11()} and \code{x11()} gain additional arguments to
       be more similar to the Unix \code{X11()} device.
      
      \item \code{file.rename()} can work across volumes (by copy-and-delete).
        
      \item The main HTML help page now links to this \file{CHANGES} file.
       (Suggested by Henrik Bengtsson.) 
      
      \item \code{strftime(format = "\%z")} now gives the timezone names
       used by the Olson database and not those used by Windows.
      
      \item \code{iconv(to = "ASCII")} no longer transliterates, but
       \code{iconv(to = "ASCII//TRANSLIT")} will.  This makes
       \code{tools::showNonASCII()} work the same way as on other platforms.
      
      \item \code{readRegistry()} can look at either the 32- or 64-bit view
       of the Registry on 64-bit Windows.
    }
  }
  \subsection{INSTALLATION}{
    \itemize{
      \item The \code{Tcl} timezone files (3Mb on disc) are now an
       optional part of the installation: they would be very rarely used
       with \R.
      
      \item \file{RSetReg.exe} has been updated to (un)set the
       version-specific keys in the same way as the main installer, and
       to have the optional argument \option{/Personal} to (un)set the
       per-user rather than the per-machine keys (which need
       administrative access).
      
      \item When building \R, the recommended way is now to supply a file
       \file{R_HOME/src/gnuwin32/MkRules.local} instead of editing
       \file{MkRules}: there is now no \file{MkRules} file in the
       distribution, but one is generated during the build process.
       
       \file{MkRules.local} can be generated by copying and editing
       \file{MkRules.dist}.
      
      \item The MSI installer defaults to an all-user (per machine)
       install (by setting the \code{ALLUSERS} property to \code{1}).
       This can be changed at installation, and it can be built for a
       user-mode install (see \file{MkRules.dist}).
    }
  }
  \subsection{BUG FIXES}{
    \itemize{
      \item Using \code{ESC} to interrupt a \code{system()} call could
       cause \R to become unstable.
      
      \item \code{0^1i} (which should be complex \code{NAN}) was
      incorrect.
    }
  }
}

\section{CHANGES IN R VERSION 2.10.1 patched}{
  \subsection{BUG FIXES}{
    \itemize{
      \item Some of the MDI toolbar buttons (e.g. \code{"Open Script"})
       did not return the focus to the console if the user cancelled the
       action.
       
      \item \code{system()} (and hence \code{shell()}) did not always
        set the standard file handles properly, so some programs would
        not run.
      
      \item \code{setWinProgressBar()} did not check the type of its
       \code{'title'} and \code{'label'} arguments and might have
       crashed if they were mis-specified.  It now does, thanks to a
       report by Greg Snow.
    }
  }
}
\section{CHANGES IN R VERSION 2.10.1}{
  \subsection{BUG FIXES}{
    \itemize{
      \item The installer did not properly record help type choices in
       \file{R_HOME/etc/Rprofile.site}.
      
      \item \command{Rcmd INSTALL} once again uses
       \option{--no-restore}, for consistency with the Unix version and
       \R 2.9.2.
       
      \item The HTML package index was not constructed properly when the
       library path was specified with backslashes.  (Patch submitted
       by Gabor Grothendieck.)
    }
  }
}

\section{CHANGES IN R VERSION 2.10.0}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item \code{windows()} now has a parameter \code{fillOddEven}
       (default \code{TRUE}), which controls the mode used for polygon
       fills of self-intersecting shapes.
       
      \item The functions \code{arrangeWindows()} and
        \code{getWindowsHandles()} have been added to give more
        programmatic control to the appearance of windows in an R
        session.
        
      \item A \sQuote{Windows} menu has been added to the SDI version of
       the \R GUI, and the corresponding menu in the MDI version has
       been expanded slightly.
      
      \item The \code{libpng} library has been updated to version 1.2.40.
      
      \item The remaining Perl scripts \code{build}, \code{check},
      \code{Rprof}, \code{Sd2Rd} in \file{R_HOME/bin} now have extension
      \file{.pl}.
      
      \item \code{Rcmd} (or \code{R CMD}) can now run \file{.sh} (shell)
       and \file{.pl} (Perl) scripts anywhere on the path (as well as
       \file{.bat} and \file{.exe} files as before).
    }
  }
  \subsection{PACKAGE INSTALLATION}{
    \itemize{
      \item \command{Rcmd SHLIB} (which is used by \command{Rcmd
       INSTALL}) now calls \code{make} without a target, rather than
       target \code{'all'}.  This is now the same as on Unix, and was
       announced in the \file{NEWS} file for 2.9.0.
      
      \item Help files are no longer zipped: rendering as text, HTML,
       latex or examples is now done when needed.
      
       Options \option{--use-zip} and \option{--use-zip-help} in
       \command{R CMD INSTALL} are no longer available and
       \option{--auto-zip} only zips data (or not).
      
       Help in packages installed under 2.9.x may not work correctly,
       and a warning is given when such a package is attached by
       \code{library()}.  Re-installation of all packages is strongly
       recommended (use \code{update.packages(checkBuilt = TRUE)}).
      
       Binary packages are installed without CHM help if they contain
       it (as those intended for 2.9.x may).
      
      \item It is possible to install source packages without Rtools
       provided they have no compiled code and no \file{configure.win} or
       \file{cleanup.win} script.
    }
  }
  \subsection{INSTALLATION}{
    \itemize{
      \item Support for memory profiling is enabled in a standard build
       (this can be changed by editing \file{config.h}).
      
      \item CHM help is no longer supported.
      
      \item The help default for the standard installer is HTML help rather
       than CHM help.
      
      \item The categories in the installers have been simplified.  Now the
       files to support installation of source packages are always
       installed (they are now only 0.7Mb), and there are no longer
       categories 'chtml', 'html/help', 'latex' and 'Rd'.
      
      
    }
  }
  \subsection{BUG FIXES}{
    \itemize{
      \item \command{Rcmd open} now works as documented: previously
       \command{Rcmd open.exe} was required.
      
      \item \code{cat(x, file=)} in \command{Rgui} tried to interpret
       UTF-8-encoded strings for the console rather than converting them
       to the native encoding.
    }
  }
}

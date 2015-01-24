% File src/library/tcltk/man/tcltk-package.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2014 R Core Team
% Distributed under GPL 2 or later

\name{tcltk-package}
\alias{tcltk-package}
\alias{tcltk}
\docType{package}
\title{
Tcl/Tk Interface
}
\description{
Interface and language bindings to Tcl/Tk GUI elements.
}
\details{
This package provides access to the platform-independent Tcl scripting
language and Tk GUI elements.  See \link{TkWidgets} for a list of
supported widgets, \link{TkWidgetcmds} for commands to work with them,
and references in those files for more.

The Tcl/Tk documentation is in
#ifdef windows
the directory \file{\var{\link{R_HOME}}/Tcl/doc}.
#endif
#ifdef unix
the system man pages.
#endif

For a complete list of functions, use \code{ls("package:tcltk")}.

#ifdef unix
Note that Tk will not be initialized if there is no \code{DISPLAY}
variable set, but Tcl can still be used.  This is most useful to allow
the loading of a package which depends on \pkg{tcltk} in a session that
does not actually use it (e.g., during installation).
#endif
}
\author{
R Core Team

Maintainer: R Core Team \email{R-core@r-project.org}
}
\keyword{ package }

% File src/library/tcltk/man/tkStartGUI.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{tkStartGUI}
\alias{tkStartGUI}
\title{Tcl/Tk GUI startup}
\description{
  Starts up the Tcl/Tk GUI
}
\usage{
tkStartGUI()
}
\details{
  Starts a GUI console implemented via a Tk text widget. This should
  probably be called at most once per session. Also redefines the file
  pager (as used by \code{help()}) to be the Tk pager.
}
\author{Peter Dalgaard}
\note{
#ifdef windows
  This function is not usable under Windows.

#endif
  \code{tkStartGUI()} saves its evaluation environment as
  \code{.GUIenv}. This means that the user interface elements can be
  accessed in order to extend the interface. The three main objects are
  named \code{Term}, \code{Menu}, and \code{Toolbar}, and the various
  submenus and callback functions can be seen with
  \code{ls(envir = .GUIenv)}.
}


\keyword{misc}

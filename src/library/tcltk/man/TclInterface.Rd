\name{TclInterface}
\title{Low-level Tcl/Tk Interface}
%
\alias{.Tcl}
\alias{.Tcl.callback}
\alias{.Tcl.args}
\alias{$.tclvar}
\alias{$<-.tclvar}
\alias{tclvar}
\alias{.Tk.ID}
\alias{.Tk.newwin}
\alias{.Tk.subwin}
\alias{is.tkwin}
\alias{tkdestroy}
\alias{.TkWin}
\alias{.TkRoot}
\alias{addTclPath}
\alias{tclRequire}
\description{
  These functions and variables provide the basic glue between \R and the
  Tcl interpreter and Tk GUI toolkit. Tk
  windows may be represented via \R objects. Tcl variables can be accessed
  via the pseudo-list \code{tclvar}.
}
\usage{
.Tcl(...)
.Tcl.args(...)
.Tcl.callback(...)
.Tk.ID(win)
.Tk.newwin(ID)
.Tk.subwin(parent)
.TkWin
.TkRoot

tkdestroy(win)
is.tkwin(x)

tclvar$name
tclvar$name <- value

addTclPath(path)
tclRequire(package, warn = TRUE)
}
\arguments{
  \item{win}{a window structure}
  \item{x}{an object}
  \item{ID}{a window ID}
  \item{path}{path to a directory containing Tcl packages}
  \item{package}{a Tcl package name}
  \item{warn}{logical. Warn if not found?}
}
\details{
  Many of these functions are not intended for general use but are used
  internally by the commands that create and manipulate Tk widgets and
  Tcl objects.  At the lowest level \code{.Tcl} sends a command as a text
  string to the Tcl interpreter and returns the result as a text string.

  \code{.Tcl.args} converts an R argument list of \code{tag=value} pairs
  to the Tcl \code{-option value} style, thus
  enabling a simple translation between the two languages. To send a
  value with no preceding option flag to Tcl, just use an untagged
  argument. In the rare case one needs an option with no subsequent
  value \code{tag=NULL} can be used. Most values are just converted to
  character mode and inserted in the command string, but window objects
  are passed using their ID string, and functions are passed via the
  result of \code{.Tcl.callback}. Tags are converted to option flags
  simply by prepending a \code{-}

  \code{.Tcl.callback} converts \R functions to Tcl command strings.  The
  argument must be a function closure.  The
  return value is something of the form \code{\{ R_call 0x408b94d4 \}} in which
  the hexadecimal number is the memory address of the function.  
  \code{.Tcl.args} takes special precautions to ensure that the function
  will exist at that address by assigning the function into the relevant
  window environment (see below).

  Tk windows are represented as objects of class \code{tkwin} which are
  lists containing  a \code{ID} field and an \code{env} field which is
  an \R environments, enclosed in the global environment.  The value of
  the \code{ID} field is identical to the Tk window name. The \code{env}
  environment contains a \code{parent} variable and a \code{num.subwin}
  variable.  If the   window obtains subwindows and  callbacks, they are
  added as variables to the environment.   \code{.TkRoot} is the top
  window with ID "."; this window is not  displayed in order to avoid
  ill effects of closing it via window  manager controls. The
  \code{parent} variable is undefined for \code{.TkRoot}.

  \code{.Tk.ID} extracts the \code{ID} of a window,
  \code{.Tk.newwin} creates a new window environment with a given ID and
  \code{.Tk.subwin} creates a new window which is a subwindow of a given
  parent window.

  \code{tkdestroy} destroys a window and also removes the reference to a
  window from its parent.

  \code{is.tkwin} can be used to test whether a given object is a window
  environment.

  \code{tclvar} is used to access Tcl variables. The Tcl variable name
  is used as if it were a list element name in \code{tclvar}, but in
  reality \code{tclvar} is an object of class \code{tclvar} and \code{$}
  and  \code{$<-} have special methods for that class.

  Tcl packages can be loaded with \code{tclRequire}; it may be necessary
  to add the directory where they are found to the Tcl search path with
  \code{addTclPath}.
  
}
\note{ Strings containing unbalanced braces are currently not handled
  well in many circumstances.
}
\seealso{
  \code{\link{TkWidgets}},
  \code{\link{TkCommands}},
  \code{\link{TkWidgetcmds}}.

  \code{\link{capabilities}("tcltk"}} to see if Tcl/Tk support was
  compiled into this build of \R.
#ifdef windows
  (It is possible to build with Tcl/Tk support but not be able to run it,
  if the DLLs are not available or \code{TCL_LIBRARY)} is not set.)
#endif
}
\examples{
\dontrun{
## These cannot be run by example() but should be OK when pasted
## into an interactive R session with the tcltk package loaded
.Tcl("format \"\%s\\n\" \"Hello, World!\"")
f <- function()"HI!"
.Tcl.callback(f)
.Tcl.args(text="Push!", command=f) # NB: Different address

tclvar$xyzzy <- 7913 ; .Tcl("set xyzzy")
.Tcl("set xyzzy 3917") ; tclvar$xyzzy

top <- tktoplevel() # a Tk widget, see Tk-widgets
ls(envir=top$env, all=TRUE)
ls(envir=.TkRoot$env, all=TRUE)# .Tcl.args put a callback ref in here
}
}
\keyword{ misc }


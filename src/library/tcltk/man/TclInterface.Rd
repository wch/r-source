\name{TclInterface}
\title{Low-level Tcl/Tk Interface}
%
\alias{TclInterface}
\alias{.Tcl}
\alias{.Tcl.callback}
\alias{.Tcl.args}
\alias{$.tclvar}
\alias{$<-.tclvar}
\alias{tclvar}
\alias{tclVar}
\alias{as.character.tclVar}
\alias{tclvalue}
\alias{tclvalue<-}
\alias{tclvalue.default}
\alias{tclvalue<-.default}
\alias{tclvalue.tclVar}
\alias{tclvalue<-.tclVar}
\alias{tclvalue.tclObj}
\alias{tclvalue<-.tclObj}
\alias{as.character.tclObj}
\alias{as.integer.tclObj}
\alias{as.double.tclObj}
\alias{as.tclObj}
\alias{is.tclObj}
\alias{tclObj}
\alias{tclObj<-}
\alias{tclObj.tclVar}
\alias{tclObj<-.tclVar}
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
  via objects of class \code{tclVar} and the C level interface to Tcl
  objects is accessed via objects of class \code{tclObj}.
}
\synopsis{
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

tclVar(init="")
as.character(x) # x of class "tclVar" or "tclObj"

tclvalue(x) # x of class "tclVar" or "tclObj"
tclvalue(x) <- value

tclObj(x)  # x of class "tclVar"
tclObj(x) <- value

as.tclObj(x)
is.tclObj(x)

as.integer(x)   # x of class "tclObj"
as.double(x)    # x of class "tclObj"


addTclPath(path = ".")
tclRequire(package, warn = TRUE)
}
\arguments{
  \item{win}{a window structure}
  \item{x}{an object}
  \item{ID}{a window ID}
  \item{parent}{a window which becomes the parent of the resulting window}
  \item{path}{path to a directory containing Tcl packages}
  \item{package}{a Tcl package name}
  \item{warn}{logical. Warn if not found?}
  \item{...}{Additional arguments. See below.}
  \item{init}{initialization value}
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
  are passed using their ID string, and callbacks are passed via the
  result of \code{.Tcl.callback}. Tags are converted to option flags
  simply by prepending a \code{-}

  Callbacks can be either \emph{atomic callbacks} handled by
  \code{.Tcl.callback} or expressions. An expression is treated as a
  list of atomic callbacks, with the following exceptions: if an
  element is a name, it is first evaluated in the callers frame, and
  likewise if it is an explicit function definition; the \code{break}
  expression is translated directly to the Tcl counterpart.
  \code{.Tcl.callback} converts \R functions and unevaluated calls to
  Tcl command strings.  The argument must be either a function closure
  or an object of mode \code{"call"} followed by an environment.  The
  return value in the first case is of the form \code{R_call
  0x408b94d4} in which the hexadecimal number is the memory address of
  the function. In the second case it will be of the form
  \code{R_call_lang 0x8a95904 0x819bfd0}. For expressions, a sequence
  of similar items is generated, separated by
  semicolons. \code{.Tcl.args} takes special precautions to ensure
  that the function will exist at that address by assigning the
  callback into the relevant window environment (see below).

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

  \code{tclVar} creates a new Tcl variable and initializes it to
  \code{init}. An R object of class \code{tclVar} is created to
  represent it. Using \code{as.character} on the object returns the Tcl
  variable name. Accessing the Tcl variable from R is done using the 
  \code{tclvalue} function, which can also occur on the left sie of
  assignments. If \code{tclvalue} is passed an argument which is not a
  \code{tclVar} object, then it will assume that it is a character string
  explicitly naming global Tcl variable. Tcl variables created by 
  \code{tclVar}  are  uniquely named and automatically unset by the 
  garbage collector when the representing object is no longer in use. 

  It is possible to access Tcl's `dual-ported' objects directly, thus
  avoiding parsing and deparsing of their string representation. This
  works by using objects of class \code{tclObj}. The string
  representation of such objects can be extracted (but not set) using
  \code{tclvalue} and conversion to vectors of mode \code{"character"},
  \code{"double"}, or \code{"integer"}. Conversely, such vectors can be
  converted using \code{as.tclObj}. The object behind a Tcl variable
  is extracted using \code{tclObj(x)} which also allows an assignment
  form, in which the right hand side of the assignment is automatically
  converted using \code{as.tclObj}.  
  
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

  \code{\link{capabilities}("tcltk")}} to see if Tcl/Tk support was
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

xyzzy <- tclVar(7913)
tclvalue(xyzzy)
tclvalue(xyzzy) <- "foo"
as.character(xyzzy)
tkcmd("set", as.character(xyzzy))

top <- tktoplevel() # a Tk widget, see Tk-widgets
ls(envir=top$env, all=TRUE)
ls(envir=.TkRoot$env, all=TRUE)# .Tcl.args put a callback ref in here
}
}
\keyword{ misc }


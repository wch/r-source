\name{TclInterface}
\title{Low-level Tcl/Tk Interface}
%
\alias{TclInterface}
\alias{.Tcl}
\alias{.Tcl.objv}
\alias{.Tcl.callback}
\alias{.Tcl.args}
\alias{.Tcl.args.objv}
\alias{tclvar}
\alias{tclVar}
\alias{as.character.tclVar}
\alias{tclArray}
\alias{[[.tclArray}
\alias{[[<-.tclArray}
\alias{$.tclArray}
\alias{$<-.tclArray}
\alias{names.tclArray}
\alias{length.tclArray}
\alias{names<-.tclArray}
\alias{length<-.tclArray}
\alias{tclvalue}
\alias{tclvalue<-}
\alias{tclvalue.default}
\alias{tclvalue<-.default}
\alias{tclvalue.tclVar}
\alias{tclvalue<-.tclVar}
\alias{tclvalue.tclObj}
\alias{as.character.tclObj}
\alias{as.integer.tclObj}
\alias{as.double.tclObj}
\alias{as.tclObj}
\alias{is.tclObj}
\alias{tclObj}
\alias{print.tclObj}
\alias{tclObj<-}
\alias{tclObj.tclVar}
\alias{tclObj<-.tclVar}
\alias{.Tk.ID}
\alias{.Tk.newwin}
\alias{.Tk.subwin}
\alias{is.tkwin}
\alias{tkdestroy}
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
\usage{
.Tcl(...)
.Tcl.objv(objv)
.Tcl.args(...)
.Tcl.args.objv(...)
.Tcl.callback(...)
.Tk.ID(win)
.Tk.newwin(ID)
.Tk.subwin(parent)
.TkRoot

tkdestroy(win)
is.tkwin(x)

tclvalue(x)
tclvalue(x) <- value

tclVar(init="")
\method{as.character}{tclVar}(x, ...)
\method{tclvalue}{tclVar}(x)
\method{tclvalue}{tclVar}(x) <- value

tclArray()
\method{[[}{tclArray}(x,...)
\method{[[}{tclArray}(x,...) <- value
\method{$}{tclArray}(x, i)
\method{$}{tclArray}(x, i) <- value

\method{names}{tclArray}(x)
\method{length}{tclArray}(x)

tclObj(x)
tclObj(x) <- value
\method{tclObj}{tclVar}(x)
\method{tclObj}{tclVar}(x) <- value

as.tclObj(x, drop=FALSE)
is.tclObj(x)

\method{as.character}{tclObj}(x. ...)
\method{as.integer}{tclObj}(x, ...)
\method{as.double}{tclObj}(x, ...)
\method{tclvalue}{tclObj}(x)

\method{tclvalue}{default}(x)
\method{tclvalue}{default}(x) <- value


addTclPath(path = ".")
tclRequire(package, warn = TRUE)
}
\arguments{
  \item{objv}{a named vector of Tcl objects}
  \item{win}{a window structure}
  \item{x}{an object}
  \item{i}{character or (unquoted) name}
  \item{drop}{logical. Indicates whether a single-element vector should
    be made into a simple Tcl object or a list of length one}
  \item{value}{For \code{tclvalue} assignments, a character string. For
    \code{tclObj} assignments, an object of class \code{tclObj}}
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
  string to the Tcl interpreter and returns the result as an object of
  class \code{tclObj} (see below). A newer variant \code{.Tcl.objv}
  accepts arguments in the form of a named list of \code{tclObj}
  objects. 

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

  \code{.Tcl.args.objv} serves a similar purpose as \code{.Tcl.args} but
  produces a list of \code{tclObj} objects suitable for passing to
  \code{.Tcl.objv}. The names of the list are converted to Tcl option
  style internally by \code{.Tcl.objv}. 
  
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
  that functions or calls will continue to exist at the specified
  address by assigning the
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

  \code{tclArray} creates a new Tcl array and initalizes it to the empty
  array. An R object of class \code{tclArray} and inheriting from class
  \code{tclVar} is created to represent it. You can access elements of
  the Tcl array using indexing with \code{[[} or \code{$}, which also
  allow replacement forms. Notice that Tcl arrays are associative by
  nature and hence unordered; indexing with a numeric index \code{i}
  refers to the element with the \emph{name}
  \code{as.character(i)}. Multiple indices are pasted together separated
  by commas to form a single name. You can query the
  length and the set of names in an array using methods for
  \code{length} and \code{names}, respectively; these cannot
  meaningfully be set so assignment forms exist only to print an error
  message. 

  It is possible to access Tcl's \sQuote{dual-ported} objects directly,
  thus avoiding parsing and deparsing of their string representation.
  This works by using objects of class \code{tclObj}.  The string
  representation of such objects can be extracted (but not set) using
  \code{tclvalue} and conversion to vectors of mode \code{"character"},
  \code{"double"}, or \code{"integer"}. Conversely, such vectors can be
  converted using \code{as.tclObj}. There is an ambiguity as to what
  should happen for length one vectors, controlled by the \code{drop}
  argument; there are cases where the distiction matters to Tcl,
  although mostly it treats them equivalently. Notice that
  \code{tclvalue} and \code{as.character} differ on an object whose
  string representation has embedded spaces, the former is sometimes to
  be preferred, in particular when applied to the result of
  \code{tclread}, \code{tkgetOpenFile}, and similar functions. 

  The object behind a \code{tclVar} object
  is extracted using \code{tclObj(x)} which also allows an assignment
  form, in which the right hand side of the assignment is automatically
  converted using \code{as.tclObj}. There is a print method for
  \code{tclObj} objects; it prints \code{<Tcl>} followed by the string
  representation of the object. Notice that \code{as.character} on a
  \code{tclVar} object is the \emph{name} of the corresponding Tcl
  variable and not the value. 
  
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

  \code{\link{capabilities}("tcltk")} to see if Tcl/Tk support was
  compiled into this build of \R.
#ifdef windows
  (It is possible to build with Tcl/Tk support but not be able to run it,
  if the DLLs are not available or \code{TCL_LIBRARY} is not set.)
#endif
}
\examples{
\dontrun{
## These cannot be run by example() but should be OK when pasted
## into an interactive R session with the tcltk package loaded
.Tcl("format \"\%s\\n\" \"Hello, World!\"")
f <- function()cat("HI!\n")
.Tcl.callback(f)
.Tcl.args(text="Push!", command=f) # NB: Different address

xyzzy <- tclVar(7913)
tclvalue(xyzzy)
tclvalue(xyzzy) <- "foo"
as.character(xyzzy)
tcl("set", as.character(xyzzy))

top <- tktoplevel() # a Tk widget, see Tk-widgets
ls(envir=top$env, all=TRUE)
ls(envir=.TkRoot$env, all=TRUE)# .Tcl.args put a callback ref in here
}
}
\keyword{ misc }


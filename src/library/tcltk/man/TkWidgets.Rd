% File src/library/tcltk/man/TkWidgets.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2008 R Core Team
% Distributed under GPL 2 or later

\name{TkWidgets}
\alias{TkWidgets}
\alias{tkwidget}
\alias{tkbutton}
\alias{tkcanvas}
\alias{tkcheckbutton}
\alias{tkentry}
\alias{tkframe}
\alias{tklabel}
\alias{tklistbox}
\alias{tkmenu}
\alias{tkmenubutton}
\alias{tkmessage}
\alias{tkradiobutton}
\alias{tkscale}
\alias{tkscrollbar}
\alias{tktext}
\alias{tktoplevel}

\alias{ttkbutton}
\alias{ttkcheckbutton}
\alias{ttkcombobox}
\alias{ttkentry}
\alias{ttkframe}
\alias{ttklabel}
\alias{ttklabelframe}
\alias{ttkmenubutton}
\alias{ttknotebook}
\alias{ttkpanedwindow}
\alias{ttkprogressbar}
\alias{ttkradiobutton}
\alias{ttkscale}
\alias{ttkscrollbar}
\alias{ttkseparator}
\alias{ttksizegrip}
\alias{ttkspinbox}
\alias{ttktreeview}

\title{Tk widgets}
\description{
  Create Tk widgets and associated \R objects.
}
\usage{
tkwidget(parent, type, ...)

tkbutton(parent, ...)
tkcanvas(parent, ...)
tkcheckbutton(parent, ...)
tkentry(parent, ...)
ttkentry(parent, ...)
tkframe(parent, ...)
tklabel(parent, ...)
tklistbox(parent, ...)
tkmenu(parent, ...)
tkmenubutton(parent, ...)
tkmessage(parent, ...)
tkradiobutton(parent, ...)
tkscale(parent, ...)
tkscrollbar(parent, ...)
tktext(parent, ...)
tktoplevel(parent = .TkRoot, ...)

ttkbutton(parent, ...)
ttkcheckbutton(parent, ...)
ttkcombobox(parent, ...)
ttkframe(parent, ...)
ttklabel(parent, ...)
ttklabelframe(parent, ...)
ttkmenubutton(parent, ...)
ttknotebook(parent, ...)
ttkpanedwindow(parent, ...)
ttkprogressbar(parent, ...)
ttkradiobutton(parent, ...)
ttkscale(parent, ...)
ttkscrollbar(parent, ...)
ttkseparator(parent, ...)
ttksizegrip(parent, ...)
ttkspinbox(parent, ...)
ttktreeview(parent, ...)
}
\arguments{
  \item{parent}{Parent of widget window.}
  \item{type}{string describing the type of widget desired.}
  \item{\dots}{handled via \code{\link{.Tcl.args}}.}
}
\details{
  These functions create Tk widgets.  \code{tkwidget} creates a widget of
  a given type, the others simply call \code{tkwidget} with the
  respective \code{type} argument.

  The functions starting \code{ttk} are for the themed widget set for Tk
  8.5 or later.  A tutorial can be found at \url{http://www.tkdocs.com}.

  It is not possible to describe the widgets and their arguments in
  full.  Please refer to the Tcl/Tk documentation.
}
\seealso{\code{\link{TclInterface}}, \code{\link{TkCommands}},
  \code{\link{TkWidgetcmds}}}

\examples{
\dontrun{
## These cannot be run by examples() but should be OK when pasted
## into an interactive R session with the tcltk package loaded

tt <- tktoplevel()
label.widget <- tklabel(tt, text = "Hello, World!")
button.widget <- tkbutton(tt, text = "Push",
                          command = function()cat("OW!\n"))
tkpack(label.widget, button.widget) # geometry manager
                                    # see Tk-commands

## Push the button and then...

tkdestroy(tt)

## test for themed widgets
if(as.character(tcl("info", "tclversion")) >= "8.5") {
  # make use of themed widgets
  # list themes
  as.character(tcl("ttk::style", "theme", "names"))
  # select a theme -- here pre-XP windows
  tcl("ttk::style", "theme", "use", "winnative")
} else {
  # use Tk 8.0 widgets
}
}}
\keyword{ misc }

\name{Tk-widgets}
\title{Tk widgets}
\description{
  Create Tk widgets and associated \R objects.
}
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
\usage{
tkwidget(parent, type, \dots)
tkbutton(parent, \dots)

  ...many more...

tktext(parent, \dots)
tktoplevel(parent=.TkRoot, \dots)
}
\arguments{
  \item{parent}{ Parent of widget window }
  \item{type}{string describing the type of widget desired}
  \item{\dots}{handled via \code{.Tcl.args}}
}
\details{
  These functions create Tk widgets.  \code{tkwidget} creates a widget of
  a given type, the others simply call \code{tkwidget} with the
  respective \code{type} argument.

  It is not possible to describe the widgets and their arguments in
  full.  Please refer to the Tcl/Tk documentation.
}
\seealso{\code{\link{Tcl-interface}}, \code{\link{Tk-commands}},
  \code{\link{Tk-widgetcmd}}}

\examples{
\dontrun{
## These cannot be run by examples() but should be OK when pasted
## into an interactive R session with the tcltk package loaded

tt <- tktoplevel()
label.widget <- tklabel(tt, text="Hello, World!")
button.widget <- tkbutton(tt, text="Push",
                          command=function()cat("OW!\n"))
tkpack(label.widget, button.widget) # geometry manager
                                    # see Tk-commands

## Push the button and then...

tkdestroy(tt)
}
}
\keyword{ misc }%-- one or more ...

\name{Tk-commands}
\alias{tkcmd}
\alias{tktitle}
\alias{"tktitle<-"}
\alias{tkbell}
\alias{tkbind}
\alias{tkbindtags}
\alias{tkfocus}
\alias{tklower}
\alias{tkraise}
\alias{tkclipboard.append}
\alias{tkclipboard.clear}
\alias{tkevent.add}
\alias{tkevent.delete}
\alias{tkevent.generate}
\alias{tkevent.info}
\alias{tkfont.actual}
\alias{tkfont.configure}
\alias{tkfont.create}
\alias{tkfont.delete}
\alias{tkfont.families}
\alias{tkfont.measure}
\alias{tkfont.metrics}
\alias{tkfont.names}
\alias{tkgrab}
\alias{tkgrab.current}
\alias{tkgrab.release}
\alias{tkgrab.set}
\alias{tkgrab.status}
\alias{tkXselection.clear}
\alias{tkXselection.get}
\alias{tkXselection.handle}
\alias{tkXselection.own}
\alias{tkwinfo}
\alias{tkwm.aspect}
\alias{tkwm.client}
\alias{tkwm.colormapwindows}
\alias{tkwm.command}
\alias{tkwm.deiconify}
\alias{tkwm.focusmodel}
\alias{tkwm.frame}
\alias{tkwm.geometry}
\alias{tkwm.grid}
\alias{tkwm.group}
\alias{tkwm.iconbitmap}
\alias{tkwm.iconify}
\alias{tkwm.iconmask}
\alias{tkwm.iconname}
\alias{tkwm.iconposition}
\alias{tkwm.iconwindow}
\alias{tkwm.maxsize}
\alias{tkwm.minsize}
\alias{tkwm.overrideredirect}
\alias{tkwm.positionfrom}
\alias{tkwm.protocol}
\alias{tkwm.resizable}
\alias{tkwm.sizefrom}
\alias{tkwm.state}
\alias{tkwm.title}
\alias{tkwm.transient}
\alias{tkwm.withdraw}
\alias{tkgrid}
\alias{tkgrid.bbox}
\alias{tkgrid.columnconfigure}
\alias{tkgrid.configure}
\alias{tkgrid.forget}
\alias{tkgrid.info}
\alias{tkgrid.location}
\alias{tkgrid.propagate}
\alias{tkgrid.rowconfigure}
\alias{tkgrid.remove}
\alias{tkgrid.size}
\alias{tkgrid.slaves}
\alias{tkpack}
\alias{tkpack.configure}
\alias{tkpack.forget}
\alias{tkpack.info}
\alias{tkpack.propagate}
\alias{tkpack.slaves}
\alias{tkplace}
\alias{tkplace.configure}
\alias{tkplace.forget}
\alias{tkplace.info}
\alias{tkplace.slaves}
}

\title{Tk non-widget commands}
\description{
  These functions interface to Tk non-widget commands, such as the
  window manager interface commands and the geometry managers.
}
\usage{
tkcmd(...)
tkpack(...)
tkpack.configure(...)
...many more...
}
\arguments{
  \item{\dots}{Handled via \code{.Tcl.args}}
}
\details{
  \code{tkcmd} provides a generic interface to calling any Tk or Tcl
  command by simply running \code{.Tcl.args} on the arguments and passing the
  result to \code{.Tcl}
  
  There are far too many of these commands to describe them and their
  arguments in full. Please refer to the Tcl/Tk documentation for details.
  Notice the general pattern for converting Tcl subcommands like
  \code{pack configure} to function names. The naming pattern should be obvious
  with a few exceptions. E.g.  \code{tkXselection.clear}
  has been renamed to avoid confusion with a similarly named
  widget command.
}
\seealso{\code{\link{Tcl-interface}}, \code{\link{Tk-widgets}},
  \code{\link{Tk-widgetcmd}}} 

\examples{
\dontrun{
## These cannot be run by examples() but should be OK when pasted
## into an interactive R session with the tcltk package loaded

tt <- tktoplevel()
tkpack(l1<-tklabel(tt,text="Heave"),l2<-tklabel(tt,text="Ho"))
tkpack.config(l1,side=left)

## Try stretching the window and then

tkdestroy(tt)
}

}
\keyword{ misc }%-- one or more ...

\name{TkCommands}
\alias{TkCommands}
\alias{tkcmd}
\alias{tktitle}
\alias{tktitle<-}
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
\alias{tkwait.variable}
\alias{tkwait.visibility}
\alias{tkwait.window}
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
\alias{tkgetOpenFile}
\alias{tkgetSaveFile} 
\alias{tkchooseDirectory}        
\alias{tkmessageBox}  
\alias{tkdialog}        
\alias{tkpopup}        
\alias{tkfile.tail}
\alias{tkfile.dir}
\alias{tkopen}
\alias{tkclose}
\alias{tkputs}        
\alias{tkread}        

\title{Tk non-widget commands}
\description{
  These functions interface to Tk non-widget commands, such as the
  window manager interface commands and the geometry managers.
}
\usage{
tkcmd(...)
tktitle(x)

tktitle(x) <- value

tkbell(...)
tkbind(...)
tkbindtags(...)
tkfocus(...)
tklower(...)
tkraise(...)

tkclipboard.append(...)
tkclipboard.clear(...)

tkevent.add(...)
tkevent.delete(...)
tkevent.generate(...)
tkevent.info(...)

tkfont.actual(...)
tkfont.configure(...)
tkfont.create(...)
tkfont.delete(...)
tkfont.families(...)
tkfont.measure(...)
tkfont.metrics(...)
tkfont.names(...)

tkgrab(...)
tkgrab.current(...)
tkgrab.release(...)
tkgrab.set(...)
tkgrab.status(...)

## NB: some widgets also have a selection.clear command, hence the "X".

tkXselection.clear(...)
tkXselection.get(...)
tkXselection.handle(...)
tkXselection.own(...)

tkwait.variable(...)
tkwait.visibility(...)
tkwait.window(...)

## winfo actually has a large number of subcommands, but it's rarely
## used, so use tkwinfo("atom", ...) etc. instead.

tkwinfo(...)

# Window manager interface

tkwm.aspect(...)
tkwm.client(...)
tkwm.colormapwindows(...)
tkwm.command(...)
tkwm.deiconify(...)
tkwm.focusmodel(...)
tkwm.frame(...)
tkwm.geometry(...)
tkwm.grid(...)
tkwm.group(...)
tkwm.iconbitmap(...)
tkwm.iconify(...)
tkwm.iconmask(...)
tkwm.iconname(...)
tkwm.iconposition(...)
tkwm.iconwindow(...)
tkwm.maxsize(...)
tkwm.minsize(...)
tkwm.overrideredirect(...)
tkwm.positionfrom(...)
tkwm.protocol(...)
tkwm.resizable(...)
tkwm.sizefrom(...)
tkwm.state(...)
tkwm.title(...)
tkwm.transient(...)
tkwm.withdraw(...)


### Geometry managers

tkgrid(...)
tkgrid.bbox(...)
tkgrid.columnconfigure(...)
tkgrid.configure(...)
tkgrid.forget(...)
tkgrid.info(...)
tkgrid.location(...)
tkgrid.propagate(...)
tkgrid.rowconfigure(...)
tkgrid.remove(...)
tkgrid.size(...)
tkgrid.slaves(...)

tkpack(...)
tkpack.configure(...)
tkpack.forget(...)
tkpack.info(...)
tkpack.propagate(...)
tkpack.slaves(...)

tkplace(...)
tkplace.configure(...)
tkplace.forget(...)
tkplace.info(...)
tkplace.slaves(...)

## Standard dialogs
tkgetOpenFile(...)
tkgetSaveFile(...)
tkmessageBox(...)

## File handling functions
tkfile.tail(...)
tkfile.dir(...)
tkopen(...)
tkclose(...)
tkputs(...)
}
\arguments{
  \item{x}{A window object}
  \item{value}{For \code{tktitle} assignments, a character string.}
  \item{\dots}{Handled via \code{.Tcl.args}}
}
\details{
  \code{tkcmd} provides a generic interface to calling any Tk or Tcl
  command by simply running \code{.Tcl.args} on the arguments and passing the
  result to \code{.Tcl}. Most of the other commands simply call
  \code{tkcmd} with a particular
  first argument and sometimes also a second argument giving the
  subcommand.

  \code{tktitle} and its assignment form provides an alternate interface
  to Tk's \code{wm title}
  
  There are far too many of these commands to describe them and their
  arguments in full. Please refer to the Tcl/Tk documentation for details.
  Except for a few exceptions, the pattern is that  Tcl subcommands like
  \code{pack configure} are converted to function names like
  \code{tkpack.configure}. 
}
\seealso{\code{\link{TclInterface}}, \code{\link{TkWidgets}},
  \code{\link{TkWidgetcmds}}}

\examples{
\dontrun{
## These cannot be run by examples() but should be OK when pasted
## into an interactive R session with the tcltk package loaded

tt <- tktoplevel()
tkpack(l1<-tklabel(tt,text="Heave"),l2<-tklabel(tt,text="Ho"))
tkpack.configure(l1,side="left")

## Try stretching the window and then

tkdestroy(tt)
}

}
\keyword{misc}%-- one or more ...

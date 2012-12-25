% File src/library/tcltk/man/TkWidgetcmds.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{TkWidgetcmds}
\alias{TkWidgetcmds}
\alias{tkactivate}
\alias{tkadd}
\alias{tkaddtag}
\alias{tkbbox}
\alias{tkcanvasx}
\alias{tkcanvasy}
\alias{tkcget}
\alias{tkcompare}
\alias{tkconfigure}
\alias{tkcoords}
\alias{tkcreate}
\alias{tkcurselection}
\alias{tkdchars}
\alias{tkdebug}
\alias{tkdelete}
\alias{tkdelta}
\alias{tkdeselect}
\alias{tkdlineinfo}
\alias{tkdtag}
\alias{tkdump}
\alias{tkentrycget}
\alias{tkentryconfigure}
\alias{tkfind}
\alias{tkflash}
\alias{tkfraction}
\alias{tkget}
\alias{tkgettags}
\alias{tkicursor}
\alias{tkidentify}
\alias{tkindex}
\alias{tkinsert}
\alias{tkinvoke}
\alias{tkitembind}
\alias{tkitemcget}
\alias{tkitemconfigure}
\alias{tkitemfocus}
\alias{tkitemlower}
\alias{tkitemraise}
\alias{tkitemscale}
\alias{tkmark.gravity}
\alias{tkmark.names}
\alias{tkmark.next}
\alias{tkmark.previous}
\alias{tkmark.set}
\alias{tkmark.unset}
\alias{tkmove}
\alias{tknearest}
\alias{tkpost}
\alias{tkpostcascade}
\alias{tkpostscript}
\alias{tkscan.mark}
\alias{tkscan.dragto}
\alias{tksearch}
\alias{tksee}
\alias{tkselect}
\alias{tkselection.adjust}
\alias{tkselection.anchor}
\alias{tkselection.clear}
\alias{tkselection.from}
\alias{tkselection.includes}
\alias{tkselection.present}
\alias{tkselection.range}
\alias{tkselection.set}
\alias{tkselection.to}
\alias{tkset}
\alias{tksize}
\alias{tktoggle}
\alias{tktag.add}
\alias{tktag.bind}
\alias{tktag.cget}
\alias{tktag.configure}
\alias{tktag.delete}
\alias{tktag.lower}
\alias{tktag.names}
\alias{tktag.nextrange}
\alias{tktag.prevrange}
\alias{tktag.raise}
\alias{tktag.ranges}
\alias{tktag.remove}
\alias{tktype}
\alias{tkunpost}
\alias{tkwindow.cget}
\alias{tkwindow.configure}
\alias{tkwindow.create}
\alias{tkwindow.names}
\alias{tkxview}
\alias{tkxview.moveto}
\alias{tkxview.scroll}
\alias{tkyposition}
\alias{tkyview}
\alias{tkyview.moveto}
\alias{tkyview.scroll}


\title{Tk widget commands}
\description{
  These functions interface to Tk widget commands.
}
\usage{
tkactivate(widget, ...)
tkadd(widget, ...)
tkaddtag(widget, ...)
tkbbox(widget, ...)
tkcanvasx(widget, ...)
tkcanvasy(widget, ...)
tkcget(widget, ...)
tkcompare(widget, ...)
tkconfigure(widget, ...)
tkcoords(widget, ...)
tkcreate(widget, ...)
tkcurselection(widget, ...)
tkdchars(widget, ...)
tkdebug(widget, ...)
tkdelete(widget, ...)
tkdelta(widget, ...)
tkdeselect(widget, ...)
tkdlineinfo(widget, ...)
tkdtag(widget, ...)
tkdump(widget, ...)
tkentrycget(widget, ...)
tkentryconfigure(widget, ...)
tkfind(widget, ...)
tkflash(widget, ...)
tkfraction(widget, ...)
tkget(widget, ...)
tkgettags(widget, ...)
tkicursor(widget, ...)
tkidentify(widget, ...)
tkindex(widget, ...)
tkinsert(widget, ...)
tkinvoke(widget, ...)
tkitembind(widget, ...)
tkitemcget(widget, ...)
tkitemconfigure(widget, ...)
tkitemfocus(widget, ...)
tkitemlower(widget, ...)
tkitemraise(widget, ...)
tkitemscale(widget, ...)
tkmark.gravity(widget, ...)
tkmark.names(widget, ...)
tkmark.next(widget, ...)
tkmark.previous(widget, ...)
tkmark.set(widget, ...)
tkmark.unset(widget, ...)
tkmove(widget, ...)
tknearest(widget, ...)
tkpost(widget, ...)
tkpostcascade(widget, ...)
tkpostscript(widget, ...)
tkscan.mark(widget, ...)
tkscan.dragto(widget, ...)
tksearch(widget, ...)
tksee(widget, ...)
tkselect(widget, ...)
tkselection.adjust(widget, ...)
tkselection.anchor(widget, ...)
tkselection.clear(widget, ...)
tkselection.from(widget, ...)
tkselection.includes(widget, ...)
tkselection.present(widget, ...)
tkselection.range(widget, ...)
tkselection.set(widget, ...)
tkselection.to(widget, ...)
tkset(widget, ...)
tksize(widget, ...)
tktoggle(widget, ...)
tktag.add(widget, ...)
tktag.bind(widget, ...)
tktag.cget(widget, ...)
tktag.configure(widget, ...)
tktag.delete(widget, ...)
tktag.lower(widget, ...)
tktag.names(widget, ...)
tktag.nextrange(widget, ...)
tktag.prevrange(widget, ...)
tktag.raise(widget, ...)
tktag.ranges(widget, ...)
tktag.remove(widget, ...)
tktype(widget, ...)
tkunpost(widget, ...)
tkwindow.cget(widget, ...)
tkwindow.configure(widget, ...)
tkwindow.create(widget, ...)
tkwindow.names(widget, ...)
tkxview(widget, ...)
tkxview.moveto(widget, ...)
tkxview.scroll(widget, ...)
tkyposition(widget, ...)
tkyview(widget, ...)
tkyview.moveto(widget, ...)
tkyview.scroll(widget, ...)
}
\arguments{
  \item{widget}{The widget this applies to}
  \item{\dots}{Handled via \code{.Tcl.args}}
}
\details{
  There are far too many of these commands to describe them and their
  arguments in full. Please refer to the Tcl/Tk documentation for details.
  Except for a few exceptions, the pattern is that  Tcl widget commands
  possibly with subcommands like
  \code{.a.b selection clear} are converted to function names like
  \code{tkselection.clear} and the widget is given as the first argument.
}
\seealso{\code{\link{TclInterface}}, \code{\link{TkWidgets}},
  \code{\link{TkCommands}}}

\examples{
\dontrun{
## These cannot be run by examples() but should be OK when pasted
## into an interactive R session with the tcltk package loaded

tt <- tktoplevel()
tkpack(txt.w <- tktext(tt))
tkinsert(txt.w, "0.0", "plot(1:10)")

# callback function
eval.txt <- function()
   eval(parse(text = tclvalue(tkget(txt.w, "0.0", "end"))))
tkpack(but.w <- tkbutton(tt, text = "Submit", command = eval.txt))

## Try pressing the button, edit the text and when finished:

tkdestroy(tt)
}

}
\keyword{ misc }%-- one or more ...

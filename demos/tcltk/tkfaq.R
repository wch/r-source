require(tcltk) || stop("tcltk support is absent")

tt <- tktoplevel()
tkwm.title(tt, "R FAQ")
txt <- tktext(tt, bg="white", font="courier",
              yscrollcommand=function(...)tkset(scr,...))
scr <- tkscrollbar(tt, repeatinterval=5,
                   command=function(...)tkyview(txt,...))
tkpack(txt, side="left", fill="both", expand=TRUE)
tkpack(scr, side="right", fill="y")

chn <- tkcmd("open", file.path(Sys.getenv("R_HOME"),"FAQ"))
tkinsert(txt, "end", tkcmd("read", chn))
tkcmd("close", chn)

tkconfigure(txt, state="disabled")
tkmark.set(txt,"insert","0.0")
tkfocus(txt)

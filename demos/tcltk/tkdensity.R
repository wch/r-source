# Interactive density plots. Based on TCL version by Guido Masarotto

require(tcltk) || stop("tcltk support is absent")
local({
    y <- NULL
    xlim <-NULL
    bw <- 1 # in case replot.maybe is called too early

    replot <- function(...) {
        if (is.null(y)) return() # too early...
        bw <<- b <- as.numeric(tclvar$bw)
        k <- tclvar$kernel
        sz <- as.numeric(tclvar$size)
        eval(substitute(plot(density(y, bw=b,
                     kernel=k),xlim=xlim)))
        points(y,rep(0,sz))
    }

    replot.maybe <- function(...)
    {
        if (as.numeric(tclvar$bw) != bw) replot()
    }

    regen <- function(...) {
        if (tclvar$dist==1) y<<-rnorm(as.numeric(tclvar$size))
        else y<<-rexp(as.numeric(tclvar$size))
        xlim <<- range(y) + c(-2,2)
        replot()
    }



    base <- tktoplevel()
    tkwm.title(base, "Density")

    spec.frm <- tkframe(base,borderwidth=2)
    left.frm <- tkframe(spec.frm)
    right.frm <- tkframe(spec.frm)

    frame1 <- tkframe(left.frm, relief="groove", borderwidth=2)
    tkpack(tklabel(frame1, text="Distribution"))
    tkpack(tkradiobutton(frame1, command=regen, text="Normal",
                         value=1, variable="dist"), anchor="w")
    tkpack(tkradiobutton(frame1, command=regen, text="Exponential",
                         value=2, variable="dist"), anchor="w")

    frame2 <- tkframe(left.frm, relief="groove", borderwidth=2)
    tkpack(tklabel(frame2, text="Kernel"))
    for ( i in c("gaussian", "epanechnikov", "rectangular",
                 "triangular", "cosine") ) {
        tmp <- tkradiobutton(frame2, command=replot,
                             text=i, value=i, variable="kernel")
        tkpack(tmp, anchor="w")
    }

    frame3 <-tkframe(right.frm, relief="groove", borderwidth=2)
    tkpack(tklabel(frame3, text="Sample size"))
    for ( i in c(50,100,200,300) ) {
        tmp <- tkradiobutton(frame3, command=regen,
                             text=i,value=i,variable="size")
        tkpack(tmp, anchor="w")

    }

    frame4 <-tkframe(right.frm, relief="groove", borderwidth=2)
    tkpack(tklabel (frame4, text="Bandwidth"))
    tkpack(tkscale(frame4, command=replot.maybe, from=0.05, to=2.00,
                   showvalue=F, variable="bw",
                   resolution=0.05, orient="horiz"))


    tkpack(frame1, frame2, fill="x")
    tkpack(frame3, frame4, fill="x")
    tkpack(left.frm, right.frm,side="left", anchor="n")

    q.but <- tkbutton(base,text="Quit",
                      command=function()tkdestroy(base))

    tkpack(spec.frm, q.but)

    tclvar$size  <- 50
    tclvar$dist  <- 1
    tclvar$kernel<- "gaussian"
    tclvar$bw    <- 1
    regen()
})




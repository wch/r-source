# Interactive density plots. Based on TCL version by Guido Masarotto

require(tcltk)
local({
    y<-NULL

    replot <- function(...) {
        b <- as.numeric(tclvar$bw)/5
        k <- tclvar$kernel
        sz <- as.numeric(tclvar$size)
        eval(substitute(plot(density(y, bw=b * sd(y),
                     kernel=k))))
        points(y,rep(0,sz))
    }

    regen <- function(...) {
        if (tclvar$dist==1) y<<-rnorm(as.numeric(tclvar$size))
        else y<<-rexp(as.numeric(tclvar$size))
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
    tkpack(tkscale(frame4, command=replot, from=1, to=9,
                   showvalue=F, variable="bw",
                   resolution=0.2, orient="horiz"))


    tkpack(frame1, frame2, fill="x")
    tkpack(frame3, frame4, fill="x")
    tkpack(left.frm, right.frm,side="left", anchor="n")

    q.but <- tkbutton(base,text="Quit",
                      command=function()tkdestroy(base))

    tkpack(spec.frm, q.but)

    tclvar$size  <- 50
    tclvar$dist  <- 1
    tclvar$kernel<- "gaussian"
    tclvar$bw    <- 5
    regen()
})




require(tcltk) || stop("tcltk support is absent")
require(ctest)

local({

    dialog.t.test <- function(){
        tt <- tktoplevel()
        tkwm.title(tt,"t test")
        x.entry <- tkentry(tt, textvariable="xvar")
        y.entry <- tkentry(tt, textvariable="yvar")

        reset <- function()
        {
            tclvar$xvar<-""
            tclvar$yvar<-""
            tclvar$alt<-"two.sided"
            tclvar$eqvar<-"0"
        }
        reset.but <- tkbutton(tt, text="Reset", command=reset)
        submit.but <- tkbutton(tt, text="submit",
                               command=function()tclvar$done<-1)

        build <- function()
        {
            x  <- parse(text=tclvar$xvar)[[1]]
            y  <- parse(text=tclvar$yvar)[[1]]
            alt<- tclvar$alt
            vv <- as.logical(as.numeric(tclvar$eqvar))
            substitute(t.test(x,y,alternative=alt,var.equal=vv))
        }
        var.cbut <- tkcheckbutton(tt,text="Equal variance", variable="eqvar")
        alt.rbuts <- tkframe(tt)

        tkpack(tklabel(alt.rbuts, text="Alternative"))
        for ( i in c("two.sided", "less", "greater")){
            tmp<-tkradiobutton(alt.rbuts, text=i, variable="alt", value=i)
            tkpack(tmp,anchor="w")
        }

        tkgrid(tklabel(tt,text="t-test"),columnspan=2)
        tkgrid(tklabel(tt,text="x variable"), x.entry)
        tkgrid(tklabel(tt,text="y variable"), y.entry)
        tkgrid(var.cbut, alt.rbuts)
        tkgrid(submit.but, reset.but)

        if (tclvar$alt=="") tclvar$alt<-"two.sided"

        ## capture destroy (e.g. from window controls
        ## otherwise the tkwait hangs with nowhere to go
        tkbind(tt, "<Destroy>", function()tclvar$done<-2)

        tkwait.variable("done")

        if(tclvar$done=="2") stop("aborted")

        tkdestroy(tt)
        cmd <- build()
        cat("### Command executed via Tk ###\n")
        cat(deparse(build()),sep="\n")
        cat("### -----\n")
        eval(cmd)
    }

    cat("******************************************************\n",
        "The source for this demo can be found in the file:\n",
        file.path(system.file(package = "tcltk"), "demo", "tkttest.R"),
        "\n******************************************************\n")
    
    data(airquality)
    attach(airquality)
    tclvar$xvar <- "Ozone[Month==5]"
    tclvar$yvar <- "Ozone[Month==8]"
    dialog.t.test()
})

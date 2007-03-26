tkStartGUI <- function() {
    ## Philippe Grosjean: this is added for more explicit error message under Windows
    if (.Platform$OS.type == "windows")
    	stop("The tkGUI is not available under Windows")
    tcl("source", file.path(.Library, "tcltk", "exec", "console.tcl"))
    .C("RTcl_ActivateConsole", PACKAGE = "tcltk")
    Menu <- .Tk.newwin(".menu")
    Term <- .Tk.newwin(".tk-R.term")
    Toolbar <- .Tk.newwin(".tk-R.toolbar")
    options(pager=tkpager)

    fileMenu <- tkmenu(Menu)
    demoMenu <- tkmenu(Menu)
    packageMenu <- tkmenu(Menu)
    helpMenu <- tkmenu(Menu)
    quitMenu <- tkmenu(fileMenu)

    tkadd(Menu,"cascade",label=gettext("File"),menu=fileMenu)
    tkadd(Menu,"cascade",label=gettext("Demos"),menu=demoMenu)
    tkadd(Menu,"cascade",label=gettext("Packages"),menu=packageMenu)
    tkadd(Menu,"cascade",label=gettext("Help"),menu=helpMenu)

    tkadd(fileMenu,"command",label=gettext("Source R code"),
	  command=function(){f <- as.character(tkgetOpenFile())
                             if (length(f)) source(f)})
    tkadd(fileMenu,"cascade", label=gettext("Quit"), menu=quitMenu)

    tkadd(quitMenu,"command",label=gettext("Save workspace"),
          command=quote(q("yes")))
    tkadd(quitMenu,"command",label=gettext("Don't save workspace"),
          command=quote(q("no")))

    tkadd(demoMenu,"command",label=gettext("t test"),
          command=quote(demo(tkttest)))
    tkadd(demoMenu,"command",label=gettext("Density"),
          command=quote(demo(tkdensity)))
    tkadd(demoMenu,"command",label=gettext("R FAQ"),
          command=quote(demo(tkfaq)))

    loadpackageWidget <- function()
    {
	pkglist <- .packages(all=TRUE)
        lvar <- tclVar()
	tclObj(lvar) <- pkglist
	box <- tklistbox(tt<-tktoplevel(),
		listvariable=lvar, selectmode="multiple")
	load <- function() {
	   s <- as.integer(tkcurselection(box))
	   if (length(s) == 0) return
	   lapply(pkglist[s+1],require,character.only=TRUE)
	   tkdestroy(tt)
	}
	tkpack(box)
	tkpack(tkbutton(tt,text=gettext("Load"),command=load))
    }

    CRANpackageWidget <- function()
    {
        CRANurl <- utils::contrib.url(getOption("repos")["CRAN"])
	l <- utils::available.packages(CRANurl)[,1]
        lvar <- tclVar()
	tclObj(lvar) <- l
	box <- tklistbox(tt<-tktoplevel(),
                         listvariable=lvar, selectmode="multiple")
	gogetem <- function() {
            s <- as.integer(tkcurselection(box))
            if (length(s) == 0) return
            utils::install.packages(l[s+1])
            tkdestroy(tt)
	}
	tkpack(box)
	tkpack(tkbutton(tt,text=gettext("Go get them!"),command=gogetem))
    }

    tkadd(packageMenu,"command",label=gettext("Load packages"),
          command=loadpackageWidget)
    tkadd(packageMenu,"command",label=gettext("Install packages from CRAN"),
          command=CRANpackageWidget)


    local({
        label <- tklabel(Toolbar,text=gettext("Help topic:"))
        txtvar <- tclVar()
        entry <- tkentry(Toolbar,textvariable=txtvar)
        showhelp <-  function() {
            s <- as.character(tclObj(txtvar))[1]
            if (length(s) == 0) return
            nm <- as.name(s)
            print(eval(substitute(help(nm))))
            tclvalue(txtvar)<-""
        }
        tkpack(label,side="left")
        tkpack(entry,side="left")
        tkbind(entry, "<Return>", showhelp)
    })

    manuals <- matrix(c(
	"R-FAQ",     "Frequently asked questions",
	"R-intro",   "An Introduction to R",
	"R-admin",   "R Administrators Manual",
	"R-data",    "R Data Import/Export",
	"R-exts",    "Writing R extensions",
	"R-lang",    "R Language Reference",
	"refman",    "R Reference Manual"
    ), ncol=2, byrow=TRUE)

    helpPDFMenu <- tkmenu(helpMenu)
    tkadd(helpMenu,"cascade", label=gettext("Manuals in PDF format"),
          menu=helpPDFMenu)
    pdfBase <- file.path(R.home("doc"), "manual")
    apply(manuals, 1, function(x) {
	f <- file.path(pdfBase, paste(x[1], ".pdf", sep="") )
        cmd <- function() system(paste(shQuote(getOption("pdfviewer")),
                                       shQuote(f)),
                                 wait = FALSE)
	tkadd(helpPDFMenu, "command", label=x[2], command=cmd,
              state=if (file.exists(f)) "normal" else "disabled")
    })
    #tkadd(helpMenu,"command", label=gettext("Help on topic..."), command=topicHelp)
    assign(".GUIenv", environment(), envir=.GlobalEnv)
}

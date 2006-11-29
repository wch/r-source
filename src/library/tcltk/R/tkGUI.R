tkStartGUI <- function() {
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

    tkadd(Menu,"cascade",label="File",menu=fileMenu)
    tkadd(Menu,"cascade",label="Demos",menu=demoMenu)
    tkadd(Menu,"cascade",label="Packages",menu=packageMenu)
    tkadd(Menu,"cascade",label="Help",menu=helpMenu)

    tkadd(fileMenu,"command",label="Source R code",
	  command=function(){f <- as.character(tkgetOpenFile())
                             if (length(f)) source(f)})
    tkadd(fileMenu,"cascade", label="Quit", menu=quitMenu)

    tkadd(quitMenu,"command",label="Save workspace", command=quote(q("yes")))
    tkadd(quitMenu,"command",label="Don't save workspace", command=quote(q("no")))

    tkadd(demoMenu,"command",label="t test", command=quote(demo(tkttest)))
    tkadd(demoMenu,"command",label="Density", command=quote(demo(tkdensity)))
    tkadd(demoMenu,"command",label="R FAQ", command=quote(demo(tkfaq)))

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
	tkpack(tkbutton(tt,text="Load",command=load))
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
	tkpack(tkbutton(tt,text="Go get them!",command=gogetem))
    }

    tkadd(packageMenu,"command",label="Load packages",
          command=loadpackageWidget)
    tkadd(packageMenu,"command",label="Install packages from CRAN",
          command=CRANpackageWidget)


    local({
        label <- tklabel(Toolbar,text="Help topic:")
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
    tkadd(helpMenu,"cascade", label="Manuals in PDF format", menu=helpPDFMenu)
    pdfBase <- file.path(R.home("doc"), "manual")
    apply(manuals, 1, function(x) {
	f <- file.path(pdfBase, paste(x[1], ".pdf", sep="") )
        cmd <- function() system(paste(getOption("pdfviewer"), f, "&"))
	tkadd(helpPDFMenu, "command", label=x[2], command=cmd,
              state=if (file.exists(f)) "normal" else "disabled")
    })
    #tkadd(helpMenu,"command", label="Help on topic...", command=topicHelp)
    assign(".GUIenv", environment(), envir=.GlobalEnv)
}

tkStartGUI <- function() {
    tkcmd("source", file.path(Sys.getenv("R_HOME"),
                              "library","tcltk","exec","console.tcl"))
    .C("RTcl_ActivateConsole")
    Menu <- .Tk.newwin(".menu")
    Term <- .Tk.newwin(".tk-R.term") 
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
    
    tkadd(fileMenu,"cascade", label="Quit", menu=quitMenu)
    
    tkadd(quitMenu,"command",label="Save workspace", command=quote(q("yes")))
    tkadd(quitMenu,"command",label="Don't save workspace", command=quote(q("no")))
    
    tkadd(demoMenu,"command",label="t test", command=quote(demo(tkttest)))
    tkadd(demoMenu,"command",label="Density", command=quote(demo(tkdensity)))
    tkadd(demoMenu,"command",label="R FAQ", command=quote(demo(tkfaq)))

    loadpackageWidget <- function()
    {
	pkglist <- .packages(all=TRUE)
        loaded <- .packages()
        # might be a good idea to label packages that
        # have already been loaded, but then we can't just lapply() below
	#n <- match(loaded, pkglist)
	#pkglist[n] <- page(pkglist[n], "*")
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
	l <- CRAN.packages()[,1]
        lvar <- tclVar()
	tclObj(lvar) <- l
	box <- tklistbox(tt<-tktoplevel(),
                         listvariable=lvar, selectmode="multiple")
	gogetem <- function() {
            s <- as.integer(tkcurselection(box))
            if (length(s) == 0) return
            install.packages(l[s+1])
            tkdestroy(tt)
	}
	tkpack(box)
	tkpack(tkbutton(tt,text="Go get them!",command=gogetem))
    } 

    tkadd(packageMenu,"command",label="Load packages", 
          command=loadpackageWidget)
    tkadd(packageMenu,"command",label="Install packages from CRAN", 
          command=CRANpackageWidget)

    topicHelp <- function()
    {
        txtvar <- tclVar()
        entry <- tkentry(tt<-tktoplevel(),textvariable=txtvar)
        showhelp <-  function() {
            s <- as.character(tclObj(txtvar))[1]
            if (nchar(s) == 0) return
            nm <- as.name(s)
            eval(substitute(help(nm)))
            tkdestroy(tt)
        }
        tkpack(entry)
        tkbind(entry, "<Return>", showhelp)
    } 
    
    tkadd(helpMenu,"command", label="Help on topic...", command=topicHelp)
    assign(".GUIenv", environment(), envir=.GlobalEnv)
}

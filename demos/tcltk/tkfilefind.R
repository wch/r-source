require(tcltk) || stop("tcltk support is absent")
local({
    
    tkfilefind<-function(path=getwd(), all.names=FALSE, multiple=FALSE){
        tclRequire("::Utility")
        tclRequire("Hierarchy")
        .Tcl("namespace import -force ::Utility::*")
        done <- FALSE
        base <- tktoplevel()
        on.exit({if(!done) tkdestroy(base)})
        tkwm.title(base, "Directory Tree")
        dirtree<-tkwidget(base, "hierarchy_dir",
                          root=path,
                          showparent="Parent",
                          showfiles=1,
                          showall=all.names,
                          selectmode=if(multiple) "multiple" else "browse")
        tkpack(dirtree, fill="both", expand=1)

        .tclfilename <- NULL
        selected <- function(){
            index <- tkcurselection(dirtree)
            index <- strsplit(index," ")[[1]] # multiple selections
            fname <- tkget(dirtree, index)
            tkdestroy(base)
            if (fname==""){
                .tclfilename <<- NULL
                return()
            }
            fnamelist<-strsplit(fname,"}")[[1]]
            for (i in seq(along=fnamelist)){
                fnamelist[i] <- gsub("[ ]*{","",fnamelist[i])
                fnamelist[i] <- paste(strsplit(fnamelist[i]," ")[[1]],
                                      collapse=Platform()$file.sep)
            }
            .tclfilename <<- fnamelist
        }

        buttonframe<-tkframe(base)
        okbut<-tkbutton(buttonframe, text="Select", command=selected)
        qbut<-tkbutton(buttonframe, text="Quit",
                       command=function() tkdestroy(base))
        tkpack(okbut, qbut, side="left")
        tkpack(buttonframe)

        tkwait.window(base)
        done<-TRUE
        return(.tclfilename)
    }

    cat("\n\n    ------------------------------------------------------\n",
        "    This demo will put a tree widget to explore the filesystem",
        "    from the current working directory.",
        "",
        "    *Double* click on a folder marked with + or - to expand",
        "    or contract a branch.",
        "",
        "    Click on the Select button to pick a file,",
        "    or Quit to return without selecting",
        "\n    ------------------------------------------------------\n\n",
        sep="\n")

    cat("******************************************************\n",
        "The source for this demo can be found in the file:\n", 
        paste(R.home(),"demos","tcltk","tkfilefind.R", sep=.Platform$file.sep),
        "\n******************************************************\n")


    if(.Platform$OS.type == "windows") flush.console()

    tkfilefind(".", multiple=TRUE)

})

tkfilefind<-function(path=getwd(),all.names=FALSE, multiple=FALSE){
    tclRequire("::Utility")
    tclRequire("Hierarchy")
    .Tcl("namespace import -force ::Utility::*")
    done<-FALSE
    base<-tktoplevel()
    on.exit({if(!done) tkdestroy(base)})
    tkwm.title(base,"Directory Tree")
    dirtree<-tkwidget(base,"hierarchy_dir",root=path,showparent="Parent",showfiles=1,showall=as.integer(all.names),selectmode=if(multiple) "multiple" else "browse")
    tkpack(dirtree,fill="both",expand=1)
    
    .tclfilename<-NULL
    selected<-function(){
        index<-.Tcl(paste(.Tk.ID(dirtree),"curselection"))
        fname<-.Tcl(paste(.Tk.ID(dirtree)," get ", index))
        tkdestroy(base)
        if (fname==""){
            .tclfilename<<-NULL
            return()
        }
        fnamelist<-strsplit(fname,"}")[[1]]
        for (i in seq(along=fnamelist)){
            fnamelist[i]<-gsub("[ ]*{","",fnamelist[i])
            fnamelist[i]<-paste(strsplit(fnamelist[i]," ")[[1]],collapse=Platform()$file.sep)
        }
        .tclfilename<<-fnamelist
    }

    buttonframe<-tkframe(base)
    okbut<-tkbutton(buttonframe,text="Select",command=function() selected())
    qbut<-tkbutton(buttonframe,text="Quit",command=function() tkdestroy(base))
    tkpack(okbut,qbut,side="left")
    tkpack(buttonframe)

    tkwait.window(base)
    done<-TRUE
    return(.tclfilename)
}

tkfilefind(".", multiple=TRUE)

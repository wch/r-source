package.skeleton<-function(name="anRpackage",list,environment=.GlobalEnv,path=".",force=FALSE){
    if(missing(list))
        list<-ls(env=environment)

    cat("Creating directories\n")
    ##make the directories
    if (file.exists(file.path(path,name)) && !force)
        stop(paste("Directory",name,"exists."))
    dir.create(file.path(path,name))
    dir.create(file.path(path,name,"man"))
    dir.create(file.path(path,name,"src"))
    dir.create(file.path(path,name,"R"))
    dir.create(file.path(path,name,"data"))

    ## Structural files
   
    description<-file(file.path(path,name,"DESCRIPTION"),"wt")
    cat("Package: the_name_of_the_package\n",file=description)
    cat("Title: What the package does\n",file=description)
    cat("Version: 1.0\n",file=description)
    cat("Author: Who wrote it\n",file=description)
    cat("Description: More about what it does\n",file=description)
    cat("Maintainer: Who to complain to <yourfault@somewhere.net>\n",file=description)
    cat("License: What license is it under?\n",file=description)
    close(description)
    
    ##README
    src<-file(file.path(path,name,"src","README"),"wt")
    cat("Put C/Fortran code here\n",file=src)
    cat("If you have compiled code add a .First.lib() function\n",file=src)
    cat("in the R/ subdirectory to load it.\n",file=src)
    close(src)
    
    man<-file(file.path(path,name,"man","README"),"wt")
    cat("Edit these help files.\n",file=man)
    cat("You may want to combine the help files for multiple functions.\n",file=man)
    close(man)
    
    top<-file(file.path(path,name,"README"),"wt")
    cat("1. Put any C/Fortran code in src/ \n",file=top)
    cat("2. If you have compiled code, add a .First.lib() function in R/\n",file=top)
    cat("   to load the shared library\n",file=top)
    cat("3. Edit the help file skeletons in man/.\n",file=top)
    cat("4. Run R CMD build to create INDEX and data/00Index\n",file=top)
    cat("5. (Optionally) edit INDEX and data/00Index\n",file=top)
    cat("6. Run R CMD check to check the package\n",file=top)
    cat("7. Run R CMD build to make the package file\n",file=top)
    cat("\n\n Read \"Writing R Extensions\" for more information.\n",file=top)
    close(top)

    ## dump the items in data/ or R/
    cat("Saving functions and data\n")
    for(item in list){
        if (is.function(get(item)))
            dump(item,file=file.path(path,name,"R",paste(item,"R",sep=".")))
        else
            save(list=item,file=file.path(path,name,"data",paste(item,"rda",sep=".")))
    }

    ## make help file skeletons in man/
    cat("Making help files\n")
    for (item in list){
        ## work around bug in prompt()
        filename<-file.path(path,name,"man",paste(item,"Rd",sep="."))
        if (is.data.frame(get(item)))
            do.call("prompt.data.frame",list(item,filename=filename))
        else{
            do.call("prompt.default",list(item,filename=filename))
            if(!is.function(get(item))){
                dta<-file(filename,"at")
                cat("\\keyword{datasets}\n",file=dta)
                close(dta)
            }
        }
    }
    
    cat("Done.\n")
    cat(paste("Further steps are described in",file.path(path,name,"README"),"\n"))
}

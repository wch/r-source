
citEntry <- function(entry, textVersion, header=NULL, footer=NULL, ...)
{
    z <- list(...)
    names(z) <- tolower(names(z))

    if("author" %in% names(z))
        z$author <- as.personList(z$author)
    
    attr(z, "entry") <- entry
    attr(z, "textVersion") <- textVersion
    attr(z, "header") <- header
    attr(z, "footer") <- footer
    class(z) <- "citation"
    z
}

citHeader <- function(...)
{
    z <- paste(...)
    class(z) <- "citationHeader"
    z
}

citFooter <- function(...)
{
    z <- paste(...)
    class(z) <- "citationFooter"
    z
}

readCitationFile <- function(file)
{
    pcf <- parse(file)
    z <- list()
    k = 0
    envir = new.env()

    for(expr in pcf){
        
        x <- eval(expr, envir=envir)
        if(class(x)=="citation")
            z[[k <- k+1]] <- x
        else if(class(x)=="citationHeader")
            attr(z, "header") <- c(attr(z, "header"), x)
        else if(class(x)=="citationFooter")
            attr(z, "footer") <- c(attr(z, "footer"), x)
    }
    class(z) <- "citationList"
    z
}
        
###**********************************************************

print.citation <- function(x, bibtex=TRUE, ...){

    if(!is.null(attr(x, "header"))){
        writeLines(strwrap(attr(x, "header")))
        cat("\n")
    }

    if(!is.null(attr(x, "textVersion"))){
        writeLines(strwrap(attr(x, "textVersion"), prefix="  "))
        cat("\n")
    }

    if(bibtex){
        cat("A BibTeX entry for LaTeX users is\n\n")
        print(toBibtex(x), prefix="  ")
    }

    if(!is.null(attr(x, "footer"))){
        cat("\n")
        writeLines(strwrap(attr(x, "footer")))
    }
}

print.citationList <- function(x, bibtex=length(x)==1, ...)
{
    cat("\n")
    if(!is.null(attr(x, "header"))){
        writeLines(strwrap(attr(x, "header")))
        cat("\n")
    }
    for(y in x)
        print(y, bibtex=bibtex)

    if(!is.null(attr(x, "footer"))){
        cat("\n")
        writeLines(strwrap(attr(x, "footer")))
    }
    cat("\n")
}

###**********************************************************

person <- function(first="", last="", middle="", email="")
{
    z <- list(name=c(first=first, middle=middle, last=last),
              email=email)
    class(z) <- "person"
    z
}

as.person <- function(x) UseMethod("as.person")

as.person.default <- function(x)
{
    if(class(x)=="person") return(x)
    
    x <- as.character(x)

    if(length(grep("<.*>", x))>0)
        email <- sub(".*<([^>]*)>.*", "\\1", x)
    else
        email <- NULL
    
    name <- sub("[[:space:]]*<[^>]*>", "", x)
    name = unlist(strsplit(name, "[[:space:]]"))

    ## fix for email address only
    if(length(name)==0) name = ""

    ## and now create appropriate person objects
    if(length(name)==1)
        z <- person(last=name, email=email)
    else if(length(name)==2)
        z <- person(first=name[1], last=name[2], email=email)
    else
        z <- person(first=name[1],
                    last=name[length(name)],
                    middle=paste(name[-c(1, length(name))], collapse=" "),
                    email=email)
    z
}

personList <- function(...)
{
    z = list(...)
    if(any(sapply(z, function(x) class(x) != "person")))
        stop("All arguments must be of class", sQuote("person"))

    class(z) <- "personList"
    z
}

as.personList <- function(x) UseMethod("as.personList")

as.personList.person <- function(x) personList(x)

as.personList.default <- function(x)
{
    if(class(x)=="personList") return(x)
    
    x <- as.character(x)
    
    ## first split into individual persons
    x <- unlist(strsplit(x,"[[:space:]]?(,|and)[[:space:]]+"))
    x <- x[nchar(x)>0]
    
    z <- list()
    for(k in seq(along=x))
    {
        z[[k]] <- as.person(x[k])
    }
    class(z) <- "personList"
    z
}
        
as.character.person <- function(x, ...)
{
    paste(x$name[nchar(x$name)>0], collapse=" ")
}

as.character.personList <- function(x, ...)
{
    sapply(x, as.character)
}

###**********************************************************

toBibtex.person <- function(object, ...)
{
    if(length(grep(" ", object$name["last"]))>0)
        object$name["last"] <- paste("{", object$name["last"], "}", sep="")
    as.character(object)
}

toBibtex.personList <- function(object, ...)
{
    z <- sapply(object, toBibtex)
    paste(z, collapse = " and ")
}

toBibtex.citation <- function(object, ...)
{
    z <- paste("@", attr(object, "entry"), "{,", sep="")

    if("author" %in% names(object)){
        object$author <- toBibtex(object$author)
    }
    
    for(n in names(object))
        z <- c(z, paste("  ", n, " = {", object[[n]], "},", sep=""))

    z <- c(z, "}")
    class(z) <- "Bibtex"
    z
}

toBibtex.citationList <- function(object, ...)
{
    lapply(object, toBibtex)
}
              
###**********************************************************

citation <- function(package="base", lib.loc = NULL)
{
    ## if we have a CITATION file, use it
    citfile <- system.file("CITATION", package=package, lib.loc=lib.loc)[1]
    if(nchar(citfile)>0){
        return(readCitationFile(citfile))
    }
    else if(package=="base"){
        ## avoid infinite recursion for broken installation
        stop("Broken installation, no CITATION file in the base package.")
    }
        
    ## else auto-generate citation info
    desc <- packageDescription(pkg=package, lib.loc = lib.loc)

    ## base packages without a CITATION file use the base citation
    if((!is.null(desc$Priority)) && (desc$Priority=="base"))
        return(citation("base"))

    if(length(desc)==1 && is.na(desc))
       stop("Package ", sQuote(package), " not found\n")

    z <- list(title = paste(package, ": ", desc$Title, sep=""),
              author = as.personList(desc$Author),
              year = sub(".*((19|20)[[:digit:]]{2}).*", "\\1", desc$Date),
              note = paste("R package version", desc$Version)
              )

    if(is.null(desc$Date)){
        warning("No date field in DESCRIPTION of package ",
                sQuote(package), "\n")
    }
    else if(length(z$year)==0){
        warning("Could not determine year for ",
                sQuote(package), " from package DESCRIPTION\n")
    }
    
    z$url <- desc$URL

    class(z) <- "citation"
    attr(z, "entry") <- "Manual"
    attr(z, "package") <- package

    attr(z, "header") <-
        paste("To cite package", sQuote(package), "in publications use:")
    
    attr(z, "footer") <-
        paste("ATTENTION: This citation information has been auto-generated",
              "from the package DESCRIPTION file and may need manual editing,",
              "see ", sQuote("help(\"citation\")"), ".")

    author <- as.character(z$author)
    if(length(author)>1)
        author <- paste(paste(author[1:(length(author)-1)], collapse=", "),
                        author[length(author)], sep=" and ")
    
    attr(z, "textVersion") <-
        paste(author, " (", z$year, "). ",
              z$title, ". ", z$note, ". ", z$url, sep="")
    
    z
}

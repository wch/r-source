"promptClass" <-
function (clName, filename = paste(topicName(type, clName), ".Rd", sep = ""), type = "class",
          keywords = "classes", where = find(classMetaName(clName)))
{
    classesInSig <- function(g, where) {
    # given a generic g, obtain list of all classes
    # named among its signatures
        mlist <- getMethods(g, where)
        if(is.null(mlist))
            return(NULL)
        tmp <- listFromMlist(mlist)
        if (length(tmp[[1]]) == 0)
            NULL
        else if ((lt <- length(tmp[[1]])) == 1)
            unlist(tmp[[1]])
        else {
            o <- list()
            for (i in seq(length = lt))
                o[[i]] <- unlist(tmp[[1]][[i]])
            o
        }
    }
    genWithClass <- function(cl, where) {
    # given a class cl
    # obtain list of all generics with cl in
    # one of its signatures
        allgen <- getGenerics()
        o <- list()
        for (i in seq(along=allgen)) o[[allgen[i]]] <- classesInSig(allgen[i],
            where)
        genl <- NULL
        nmok <- names(o)
        for (i in seq(along = o)) {
            if (!all(is.na(match(unlist(o[[i]]), cl))))
                genl <- c(genl, nmok[i])
        }
        genl
    }

    sigsMatrix <- function (g, where)
    # given a generic g, obtain matrix
    # with one row per signature
    # it assumes at present that signatures for a given generic
    # are all of the same length.
    #
    # it would be simple to jettison the matrix construct
    # replace it by a list
    {
        tmp <- listFromMlist(getMethods(g, where))
        if (length(tmp[[1]]) == 0)
            NULL
        else if ((lt <- length(tmp[[1]])) == 1)
            matrix(unlist(tmp[[1]]), nr = 1)
        else {
            o <- matrix(" ", nc = length(tmp[[1]][[1]]), nr = length(tmp[[1]]))
            for (i in 1:lt) o[i, ] <- unlist(tmp[[1]][[i]])
            o
        }
    }

    sigsList <- function (g, where)
    # given a generic g, obtain list
    # with one element per signature
    #
    {
        tmp <- listFromMlist(getMethods(g, where))
        if (length(tmp[[1]]) == 0)
            NULL
        else
            tmp[[1]]
    }
    slotClassWithSource <- function(clname) {
        clDef <- getClassDef(clname)
        extds <- names(getExtends(clDef))
        allslots <- getSlots(clDef) ## establishes all slots, in the right order
        for(j in rev(seq(along=extds))) {
            i <- extds[[j]]
            slotsi <- getSlots(i)
            if(length(slotsi)>0)
                allslots[names(slotsi)] <- paste0("\"", as.character(slotsi),
                                                  "\", from class \"", i, "\"")
        }
        slotsi <- getSlots(clDef)
        if(length(slotsi)>0)
            allslots[names(slotsi)] <- paste0("\"", as.character(slotsi),"\"")
        allslots
    }
    paste0 <- function(...) paste(..., sep = "")
    pastePar <- function(x) {
        xn <- names(x); x <- as.character(x)
        if(length(xn) == length(x))
            xn <- paste(xn, "= ")
        else
            xn <- ""
        paste("(", paste(xn, "\"", x, "\"", sep="", collapse = ", "),
        ")", sep = "")
    }
    if(length(where) == 0)
        stop(paste0("No definition of class \"", clName,"\" found"))
    else if(length(where) > 1) {
        where <- where[[1]]
        warning("Multiple definitions of \"", clName, "\" found; using the one on ",
                where)
    }
    fullName <- topicName("class", clName)
    clDef <- getClass(clName)
    .name <- paste0("\\name{", fullName, "}")
    .type <- paste0("\\docType{", type, "}")
    .alias <- paste0("\\alias{", fullName, "}")
    .title <- paste0("\\title{Class \"", clName, "\" ~~~ }")
    .desc <- paste0("\\description{", "  ~~ A concise (1-5 lines) description of what the class is.  ~~",
        "}")
    slotclasses <- getSlots(clName)
    slotnames <- names(slotclasses)
    slotclasses <- as.character(slotclasses)
    nslots <- length(slotclasses)
    .usage <- "\\section{Objects from the Class}"
    if(isVirtualClass(clName)) {
        .usage <- paste0(.usage, "{A virtual Class: No objects may be created from it.}")
    }
    else {
        initMethod <- unRematchDefinition(selectMethod("initialize", clName))
        argNames <- formalArgs(initMethod)
        ## but for new() the first argument is the class name
        argNames[[1]] <- paste0('"', clName, '"')
        .usage <- c(paste0(.usage,"{"),
                    paste0("Objects can be created by calls of the form \\code{", .makeCallString(initMethod, "new", argNames), "}."),
                    "    ~~ describe objects here ~~ ", "}")
    }
    if (nslots > 0) {
        slotclasses <- slotClassWithSource(clName)
        slotnames <- names(slotclasses)
        .slots.head <- c("\\section{Slots}{", "  \\describe{")
        .slots.body <-  paste0("    \\item{\\code{", slotnames,
                "}:}", "{Object of class \\code{", slotclasses, "} ~~ }")
        .slots.tail <- c("  }","}")
        .slots <- c(.slots.head,  .slots.body,  .slots.tail)
    }
    else
        .slots <- character()
    .extends <- getExtends(clDef)
    if(length(.extends)>0) {
         .extends <- showExtends(.extends, print=FALSE)
        .extends <- c("\\section{Extends}{",
                      paste("Class \\code{\"", .extends$what, "\"}, ",
                            .extends$how, ".", sep=""),
                      "}")
    }
    else
        .extends <- character()
    nmeths <- length(methnms <- genWithClass(clName, where))
    .meths.head <- "\\section{Methods}{"
    if (nmeths > 0) {
        .meths.body <- "  \\describe{"
        for (i in 1:nmeths) {
            .sigmat <- sigsList(methnms[i], where)
            for (j in seq(along = .sigmat)) {
                if (!all(is.na(match(.sigmat[[j]],clName))))
                .meths.body <- c(.meths.body, paste0("    \\item{",
                  methnms[i], "}{\\code{signature", pastePar(.sigmat[[j]]), "}: ... }"))
            }
        }
        .meths.body <- c(.meths.body, "  }")
    }
    else {
        .meths.head <- "\\section{Methods}{"
        .meths.body <- paste0("No methods defined with class \"", clName,
                              "\" in the signature.")
    }
    .meths.tail <- "}"
    .keywords <- paste("\\keyword{", keywords, "}", sep = "")
    .boilerplate <- c( "\\references{ ~put references to the literature/web site here ~ }", 
            "\\author{ ~~who you are~~ }", "\\note{ ~~further notes~~ }", 
            "", " ~Make other sections like WARNING with \\section{WARNING }{....} ~", 
            "", "\\seealso{ ~~objects to SEE ALSO as \\code{\\link{~~fun~~}}, ~~~",
                  " or\\code{\\link{CLASSNAME-class}} for links to other classes }", 
            "", "\\examples{", "##---- Should be DIRECTLY executable !! ----","}")
    cat(.name, .type, .alias,  .title, .desc,
        .usage,
        .slots,  .extends,
        .meths.head,  .meths.body,  .meths.tail,
        .boilerplate, .keywords, sep ="\n",
        file = filename)
    if(is.character(filename))
        what <- paste0(" to the file \"", filename, "\"")
    else if(inherits(filename, "connection"))
        ##  Doesn't seem to be any way in R to get the description from the connection
        what <- " to the connection"
    else
        what <- "" # what, indeed?
    message("A shell of class documentation has been written",what,".\n")
    invisible(filename)
}

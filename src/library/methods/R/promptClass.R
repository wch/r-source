"promptClass" <-
function (clName, filename = NULL, type = "class",
          keywords = "classes", where = topenv(parent.frame()))
{
    if(is.null(filename))
        filename <- paste(topicName(type, clName), ".Rd", sep = "")

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
        allgen <- getGenerics(where=where)
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
        extds <- names(clDef@contains)
        allslots <- getSlots(clDef) ## establishes all slots, in the right order
        for(j in rev(seq(along=extds))) {
            i <- extds[[j]]
            slotsi <- getSlots(getClass(i))
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
    whereClass <- find(classMetaName(clName))
    if(length(whereClass) == 0)
        stop(gettextf("no definition of class \"%s\" found", clName),
             domain = NA)
    else if(length(whereClass) > 1) {
        if(identical(where, topenv(parent.frame()))) {
            whereClass <- whereClass[[1]]
            warning(gettextf("multiple definitions of \"%s\" found; using the one on %s", clName, whereClass), domain = NA)
        }
        else {
            if(exists(classMetaName(clName), where, inherits = FALSE))
                whereClass <- where
            else
                stop(gettextf("no definition of class \"%s\" in the specified position, %s, definition(s) on : %s",
                              clName, where,
                              paste(whereClass, collapse = ", ")),
                     domain = NA)
        }
    }
    fullName <- utils::topicName("class", clName)
    clDef <- getClass(clName)
    .name <- paste0("\\name{", fullName, "}")
    .type <- paste0("\\docType{", type, "}")
    .alias <- paste0("\\alias{", fullName, "}")
    .title <- paste0("\\title{Class \"", clName, "\" ~~~ }")
    .desc <- paste0("\\description{", "  ~~ A concise (1-5 lines) description of what the class is.  ~~",
        "}")
    slotclasses <- getSlots(getClass(clName))
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
    .extends <- clDef@contains
    if(length(.extends)>0) {
        .extends <- showExtends(.extends, print=FALSE)
        .extends <-
            c("\\section{Extends}{",
              paste("Class \\code{\"",
                    .extends$what,
                    "\"}, ",
                    ## Add Rd markup to 'by class "CLASS"' results
                    gsub("^(by class) (\".*\")$", "\\1 \\\\code{\\2}",
                         .extends$how),
                    ".",
                    sep = ""),
              "}")
    }
    else
        .extends <- character()
    nmeths <- length(methnms <- genWithClass(clName, where=whereClass))
    .meths.head <- "\\section{Methods}{"
    .methAliases <- ""
    if (nmeths > 0) {
        .meths.body <- "  \\describe{"
        for (i in 1:nmeths) {
            .sigmat <- sigsList(methnms[i], where)
            for (j in seq(along = .sigmat)) {
                if (!all(is.na(match(.sigmat[[j]],clName)))) {
                .meths.body <- c(.meths.body, paste0("    \\item{",
                  methnms[i], "}{\\code{signature", pastePar(.sigmat[[j]]), "}: ... }"))

                  cur <- paste(.sigmat[[j]], collapse = ",")
                  .methAliases <- paste(.methAliases, "\\alias{",
                    methnms[i], ",", cur, "-method}\n", sep = "")
                 }
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

    Rdtxt <-
        list(name = .name,
             type = .type,
             aliases = .alias,
        methAliases = .methAliases,
             title = .title,
             description = .desc,
             "section{Objects from the Class}" = .usage,
             "section{Slots}" = .slots,
             "section{Extends}" = .extends,
             "section{Methods}" =
             c(.meths.head, .meths.body, .meths.tail),
             references = paste("\\references{ ~put references to the",
             "literature/web site here ~ }"),
             author = "\\author{ ~~who you are~~ }",
             note = c("\\note{ ~~further notes~~ }",
             "",
             paste(" ~Make other sections like Warning with",
                   "\\section{Warning }{....} ~"),
             ""),
             seealso = c("\\seealso{",
             paste("  ~~objects to See Also as",
                   "\\code{\\link{~~fun~~}}, ~~~"),
             paste("  or \\code{\\link{CLASSNAME-class}}",
                   "for links to other classes"),
             "}"),
             examples = c("\\examples{",
             "##---- Should be DIRECTLY executable !! ----",
             "}"),
             keywords = .keywords)

    if(is.na(filename)) return(Rdtxt)

    cat(unlist(Rdtxt), file = filename, sep = "\n")
    if(is.character(filename)) {
        what <- if(nchar(filename))
            paste(" to the file", sQuote(filename))
        else
            " to the standard output connection"
    }
    else if(inherits(filename, "connection"))
        what <- paste(" to the connection",
                      sQuote(summary(filename)$description))
    else
        what <- ""                      # what, indeed?
    .message("A shell of class documentation has been written",what,".\n")
    invisible(filename)
}


.makeCallString <- function (def, name = substitute(def), args = formalArgs(def))
{
#
# need this for experimentation because the function is not exported
#
    if (is.character(def)) {
        if (missing(name))
            name <- def
        def <- getFunction(def)
    }
    if (is(def, "function"))
        paste(name, "(", paste(args, collapse = ", "), ")", sep = "")
    else ""
}

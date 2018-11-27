#  File src/library/methods/R/promptClass.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2014 The R Core Team
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  https://www.R-project.org/Licenses/

promptClass <-
function (clName, filename = NULL, type = "class",
	  keywords = "classes", where = topenv(parent.frame()),
          generatorName = clName)
{
    classInSig <- function(g, where, cl) {
        ## given a generic g, is class cl in one of the method
        ## signatures for the class?
	cl %in% unique(unlist(findMethods(g, where)@signatures))
    }
    genWithClass <- function(cl, where) {
    ## given a class cl
    ## obtain list of all generics with cl in
    ## one of its signatures
	allgen <- getGenerics(where = where)
	ok <- as.logical(unlist(lapply(allgen, classInSig, cl = cl, where = where)))
	allgen[ok]
    }

    sigsList <- function (g, where)
      ## given a generic g, obtain list with one element per signature,
      ## with argument names inserted
    {
        methods <- findMethods(g, where)
	value <- methods@signatures
        args <- methods@arguments
        if(length(value)) {
            ## name the individual signature elements for output
            length(args) <- length(value[[1]]) # all sigs are same length
            value <- lapply(value, function(x){names(x) <- args; x})
        }
        value
    }
    slotClassWithSource <- function(clname) {
	clDef <- getClassDef(clname)
	extds <- names(clDef@contains)
	allslots <- getSlots(clDef) ## establishes all slots, in the right order
	for(j in rev(seq_along(extds))) {
	    i <- extds[[j]]
	    slotsi <- getSlots(getClass(i))
	    if(length(slotsi))
		allslots[names(slotsi)] <- paste0("\"", as.character(slotsi),
						  "\", from class \"", i, "\"")
	}
	slotsi <- getSlots(clDef)
	if(length(slotsi))
	    allslots[names(slotsi)] <- paste0("\"", as.character(slotsi),"\"")
	allslots
    }
    cleanPrompt <- function(object, name) {
        ## get the prompt() result and clean out the junk
        ## lines that prompt() creates
        value <- utils::prompt(object, name = name, filename = NA)
        for(i in seq_along(value)) {
            item <- value[[i]]
            bad <- grepl("^ *%", item)
            if(any(bad))
                value[[i]] <- item[!bad]
        }
        value
    }
    pastePar <- function(x) {
        xn <- names(x)
	x <- as.character(x)
	xn <- if(length(xn) == length(x)) paste(xn, "= ") else ""
	paste0("(", paste0(xn, "\"", x, "\"", collapse = ", "), ")")
    }
    escape <- function(txt) gsub("%", "\\\\%", txt)

    if(is.null(filename))
	filename <- paste0(utils:::topicName(type, clName), ".Rd")
    if(!missing(where) && !is.na(match(clName, getClasses(where))))
      whereClass <- where
    else {
        whereClass <- utils::find(classMetaName(clName))
        if(length(whereClass) == 0L)
            stop(gettextf("no definition of class %s found",
                          dQuote(clName)), domain = NA)
        else if(length(whereClass) > 1L) {
            if(identical(where, topenv(parent.frame()))) {
                whereClass <- whereClass[[1L]]
                warning(gettextf("multiple definitions of %s found; using the one on %s",
                                 dQuote(clName), whereClass), domain = NA)
            }
            else {
                if(exists(classMetaName(clName), where, inherits = FALSE))
                    whereClass <- where
                else
                    stop(sprintf(ngettext(length(whereClass),
                                          "no definition of class %s in the specified position, %s, definition on : %s",
                                          "no definition of class %s in the specified position, %s, definitions on : %s"),
                                 dQuote(clName), where,
                                 paste(whereClass, collapse = ", ")),
                         domain = NA)
            }
        }
    }
    fullName <- utils:::topicName("class", clName)
    clDef <- getClass(clName, where = whereClass)
    .name <- paste0("\\name{", fullName, "}")
    .type <- paste0("\\docType{", type, "}")
    .alias <- paste0("\\alias{", fullName, "}")
    .title <- sprintf("\\title{Class \\code{\"%s\"}}", clName)
    .desc <- paste0("\\description{",
                    "\n%%  ~~ A concise (1-5 lines) description of what the class is. ~~",
                    "\n}")
    slotclasses <- getSlots(clDef)
    slotnames <- names(slotclasses)
    slotclasses <- as.character(slotclasses)
    nslots <- length(slotclasses)
    clNameQ <- paste0('"', clName, '"')
    .usage <- "\\section{Objects from the Class}"
    virtualClass <- isVirtualClass(clName)
    if(virtualClass) {
	.usage <- paste0(.usage, "{A virtual Class: No objects may be created from it.}")
        generator <- NULL # regardless of what exists
    }
    else {
        if(exists(generatorName, where, inherits = FALSE))
            generator <- get(generatorName, where, inherits = FALSE)
        else
            generator <- NULL
        if(is(generator, "classGeneratorFunction")) {
            promptGenerator <- cleanPrompt(generator, generatorName)
            callString <- .makeCallString(generator, generatorName)
            .alias <- c(.alias, promptGenerator$aliases)
            ## the rest of the promptGenerator will be added later
        }
        else {
            initMethod <- unRematchDefinition(selectMethod("initialize", clName))
            argNames <- formalArgs(initMethod)
            ## but for new() the first argument is the class name
            argNames[[1L]] <- clNameQ
            callString <- .makeCallString(initMethod, "new", argNames)
        }
	.usage <-
            c(paste0(.usage,"{"),
              paste0("Objects can be created by calls of the form \\code{",
                     callString,
                     "}."),
              "%%  ~~ describe objects here ~~ ",
              "}")
    }
    .slots <- if (nslots > 0) {
	slotclasses <- slotClassWithSource(clName)
	slotnames <- names(slotclasses)
	.slots.head <- c("\\section{Slots}{", "  \\describe{")
	.slots.body <-	paste0("    \\item{\\code{", slotnames,
                               "}:}", "{Object of class \\code{",
                               slotclasses, "} ~~ }")
	.slots.tail <- c("  }","}")
	c(.slots.head,  .slots.body,	.slots.tail)
    } else character()
    .extends <- clDef@contains
## FIXME: the superclass slots should be marked as such
##       and left *optional* to be documented
    if(length(.extends)) {
	.extends <- showExtends(.extends, printTo = FALSE)
	.extends <-
	    c("\\section{Extends}{",
	      paste0("Class \\code{\"\\linkS4class{",
		    .extends$what,
		    "}\"}, ",
		    ## Add Rd markup to 'by class "CLASS"' results
		    gsub("^(by class) (\".*\")$", "\\1 \\\\code{\\2}",
			 .extends$how),
		    "."),
	      "}")
    }
    else
	.extends <- character()
    nmeths <- length(methnms <- genWithClass(clName, where = whereClass))
    .meths.head <- "\\section{Methods}{"
    .methAliases <- ""
    if (nmeths > 0) {
	.meths.body <- "  \\describe{"
	for (i in 1L:nmeths) {
	    .sig <- sigsList(methnms[i], where = whereClass)
	    for (j in seq_along(.sig)) {
		if (!all(is.na(match(.sig[[j]],clName)))) {
		    methn.i <- escape(methnms[i])
		    .meths.body <-
			c(.meths.body,
			  paste0("    \\item{",
				 methn.i, "}{\\code{signature",
				 pastePar(.sig[[j]]), "}: ... }"))

		    cur <- paste(.sig[[j]], collapse = ",")
		    .methAliases <- paste0(.methAliases, "\\alias{",
					   methn.i, ",", cur, "-method}\n")
		}
	    }
	}
	.meths.body <- c(.meths.body, "	 }")
    }
    else {
	.meths.head <- "\\section{Methods}{"
	.meths.body <- paste("No methods defined with class", clNameQ,
                             "in the signature.")
    }
    .meths.tail <- "}"
    .keywords <- paste0("\\keyword{", keywords, "}")

    Rdtxt <-
	list(name = .name,
             version = "\\Rdversion{1.1}",
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
	     references = paste("\\references{\n%%  ~~put references to the",
	     "literature/web site here~~\n}"),
	     author = "\\author{\n%%  ~~who you are~~\n}",
	     note =
	     c("\\note{\n%%  ~~further notes~~\n}",
	       "",
	       paste("%% ~Make other sections like Warning with",
		     "\\section{Warning }{....} ~"),
	       ""),
	     seealso =
	     c("\\seealso{",
	       paste("%%  ~~objects to See Also as",
		     "\\code{\\link{~~fun~~}}, ~~~"),
	       paste("%%  ~~or \\code{\\linkS4class{CLASSNAME}}",
		     "for links to other classes ~~~"),
	       "}"),
	     examples = c("\\examples{",
	     paste0("showClass(", clNameQ, ")"),
	     "}"),
	     keywords = .keywords)

    if(is(clDef, "refClassRepresentation"))
        Rdtxt <- refClassPrompt(clDef, Rdtxt, nmeths, nslots, .meths.head)
    else if(is(generator, "classGeneratorFunction")) {
        ## add in the actual usage, arguments sections, mostly to make
        ## CMD check happy
        what <-  c("usage", "arguments")
        Rdtxt[what] <- promptGenerator[what]
    }

    if(is.na(filename)) return(Rdtxt)

    cat(unlist(Rdtxt), file = filename, sep = "\n")
    .message("A shell of class documentation has been written",
             .fileDesc(filename), ".\n")
    invisible(filename)
}

## used in promptClass() above and in promptMethods() :
.fileDesc <- function(file) {
    if(is.character(file)) {
	if(nzchar(file))
	    paste(" to the file", sQuote(file))
	else
	    " to the standard output connection"
    }
    else if(inherits(file, "connection"))
	paste(" to the connection",
              sQuote(summary(file)$description))
    else "" # what, indeed?
}

refClassPrompt <- function(clDef, Rdtxt, nmeths, nslots, .meths.head) {
    ## exclude some sections that are usually irrelevant
    sections <- names(Rdtxt)
    envRefX <- paste0("{",extends("envRefClass"), "}")
    exclude <- grep("Objects from the Class", sections)
    if(nmeths < 1)
        exclude <- c(exclude, grep("Methods", sections))
    else
        .meths.head <- "\\section{Class-Based Methods}{"
    if(nslots < 2) # just the data slot, usually
        exclude <- c(exclude, grep("Slots", sections))
    Rdtxt <- Rdtxt[-exclude]
    extdsthead <- "section{Extends}" # has to be there
    extds <- Rdtxt[[extdsthead]]
    drop <- rep(FALSE, length(extds))
    for(class in envRefX) #drop the envRefClass & its superclasses
        drop <- drop | grepl(class, extds, fixed = TRUE)
    extds <- extds[!drop]
    extds <- append(extds, "\nAll reference classes extend and inherit methods from \\code{\"\\linkS4class{envRefClass}\"}.\n", length(extds)-1)
    Rdtxt[[extdsthead]] <- extds
    fieldClasses <- refClassFields(clDef)
    nfields <- length(fieldClasses)
    .fields <- if (nfields > 0) {
	fieldnames <- names(fieldClasses)
	.fields.head <- c("\\section{Fields}{", "  \\describe{")
	.fields.body <-	paste0("    \\item{\\code{", fieldnames,
                               "}:}", "{Object of class \\code{",
                               fieldClasses, "} ~~ }")
	.fields.tail <- c("  }","}")
	c(.fields.head,  .fields.body,	.fields.tail)
    } else character()
    methodDefs <- as.list(clDef@refMethods)
    nmethods <- length(methodDefs)
    if(nmethods > 0) {
        thisClassDefs <- match(vapply(methodDefs, function(x) x@refClassName, ""), clDef@className, 0) > 0
        otherMethods <- methodDefs[!thisClassDefs]
        methodDefs <- methodDefs[thisClassDefs]
        .methods <-
            c(.meths.head, .refMethodDescription(methodDefs, fieldnames, otherMethods), "}")
    }
    else
        .methods <- character()
    c(Rdtxt,
      list("section{Fields}" = .fields,
           "section{ClassMethods}" = .methods)
      )
}

.refMethodDescription <- function(methodDefs, fieldnames, otherMethods) {
    methodnames <- names(methodDefs)
    methodargs <- vapply(methodDefs, function(x)
			 paste0("(", paste(formalArgs(x), collapse=", "), ")"), "")
    if(length(methodnames) > 0) {
        .methods.head <- "  \\describe{"
        .methods.body <-
            paste0("    \\item{\\code{",
                   methodnames, methodargs,
                   "}:}", "{ ~~ }")
        .methods <- c(.methods.head,  .methods.body, "  }")
    }
    else
        .methods <- character()
    methodclasses <- vapply(otherMethods,
	      function(x) if(is(x, "refMethodDef")) x@refClassName else "<unknown>", "")
    ## don't report the standard methods from envRefClass
    superclass <- methodclasses != "envRefClass"
    otherMethods <- otherMethods[superclass]
    methodclasses <- methodclasses[superclass]
    if(length(otherMethods)) {
        methodnames <- names(otherMethods)
        methodnames <- gsub("[#].*","", methodnames)
        .methods <- c(.methods,
                      "\nThe following methods are inherited (from the corresponding class):",
                      paste0(methodnames, ' ("', methodclasses,
                             '")', collapse = ", ")
                      )
    }
    .methods
}

.makeCallString <- function (def, name = substitute(def), args = formalArgs(def))
{
##
## need this for experimentation because the function is not exported
##
    if (is.character(def)) {
	if (missing(name))
	    name <- def
	def <- getFunction(def)
    }
    if (is.function(def))
	paste0(name, "(", paste(args, collapse = ", "), ")")
    else ""
}

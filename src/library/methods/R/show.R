#  File src/library/methods/R/show.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2019 The R Core Team
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

showDefault <- function(object, oldMethods = TRUE)
{
    clDef <- getClass(cl <- class(object), .Force=TRUE)
    if(!missing(oldMethods))
        .Deprecated(msg =
	"the 'oldMethods' argument is deprecated, as it has been unused since R 1.7.0")
    if(!is.null(clDef) && isS4(object) && is.na(match(clDef@className, .BasicClasses)) ) {
        cat("An object of class ", classLabel(cl), "\n", sep="")
        slots <- slotNames(clDef)
        dataSlot <- .dataSlot(slots)
        if(length(dataSlot) > 0) { # show data part and remove it from slots :
            show(slot(object, dataSlot))
            slots <- slots[is.na(match(slots, dataSlot))]
        }
        else if(length(slots) == 0L)
            show(unclass(object))
        for(what in slots) {
            if(what == ".Data")
                next ## should have been done above
	    cat("Slot ", deparse(what), ":\n", sep="")
            print(slot(object, what))
            cat("\n")
        }
    }
    else
        ## NBB:  This relies on the delicate fact
        ## that print will NOT recursively call show if it gets more than one argument!
        print(object, useS4 = FALSE)
    invisible() # documented return for show().
}

.extraSlotsDone <- new.env() # any unique reference value would do

showExtraSlots <- function(object, ignore) {
    if(is(ignore, "classRepresentation"))
      ignore <- slotNames(ignore)
    else if(!is(ignore, "character"))
      stop(gettextf("invalid 'ignore' argument; should be a class definition or a character vector, got an object of class %s", dQuote(class(ignore))),
           domain = NA)
    slots <- slotNames(class(object))
    for(s in slots[is.na(match(slots, ignore))]) {
        cat("Slot ",s, ":\n", sep="")
        show(slot(object, s))
    }
    .extraSlotsDone # a signal not to call this function (again)
}

## temporary definition of show, to become the default method
## when .InitShowMethods is called
show <- function(object) showDefault(object)

.InitShowMethods <- function(envir) {
    if(!isGeneric("show", envir))
        setGeneric("show", where = envir, simpleInheritanceOnly = TRUE)
    setMethod("show", "MethodDefinition",
              function(object) {
                  cl <- class(object)
		  nonStandard <-
		      if(.identC(cl, "MethodDefinition"))
			  "" else paste0(" (Class ", classLabel(cl),")")
                  cat("Method Definition",nonStandard,":\n\n", sep = "")
                  show(object@.Data)
                  mm <- methodSignatureMatrix(object)
                  cat("\nSignatures:\n")
                  print(mm)
              },
              where = envir)
    setMethod("show", "MethodWithNext",
              function(object)  {
                  callNextMethod()
                  cat("\nExcluded from nextMethod:\n")
                  print(unlist(object@excluded))
              },
              where = envir)
    setMethod("show", "genericFunction",
              function(object)  {
                  cat(class(object)," for \"", object@generic,
                      "\" defined from package \"", object@package,
                      "\"\n", sep = "")
                  if(length(object@group))
                      cat("  belonging to group(s):",
                          paste(unlist(object@group), collapse =", "), "\n")
                  if(length(object@valueClass))
                      cat("  defined with value class: \"", object@valueClass,
                          "\"\n", sep="")
                  cat("\n")
                  show(object@.Data)
                  cat("Methods may be defined for arguments: ",
                      paste(object@signature, collapse=", "), "\n",
			    "Use  showMethods(\"", object@generic,
			    "\")  for currently available ones.\n", sep="")
                  if(.simpleInheritanceGeneric(object))
                      cat("(This generic function excludes non-simple inheritance; see ?setIs)\n");
              },
              where = envir)
    setMethod("show", "classRepresentation",
              function(object){
                  if(!.identC(class(object), "classRepresentation"))
                    cat("Extended class definition (", classLabel(class(object)),
                        ")\n")
                  printClassRepresentation(object)
              },
              where = envir)

    ## a show() method for the signature class
    setMethod("show", "signature", function(object) {
        message(gettextf("An object of class %s", dQuote(class(object))),
                domain = NA)
        val <- object@.Data
        names(val) <- object@names
        callNextMethod(val)
    } ,
              where = envir)
}

.showPackage <- function(className) {
    if(is.logical(opt <- getOption("showPackageForClass")))
        opt
    else
        is.list(.Call(C_R_getClassFromCache, as.character(className), .classTable))
}
## an informative string label for a class
classLabel <- function(Class) {
    if(is.character(Class) && length(Class)) {
        className <- Class[[1L]]
        packageName <- attr(Class, "package")
        if(is.null(packageName))
            packageName <- ""
    }
    else {
        if(is(Class, "classRepresentation")) {
            className <- Class@className
            packageName <- Class@package
        }
        else stop(gettextf("invalid call to 'classLabel': expected a name or a class definition, got an object of class %s", classLabel(class(Class))), domain = NA)
    }
    if(.showPackage(className)) {
	packageName <-
	    if(identical(packageName, ".GlobalEnv"))
		" (from the global environment)"
	    else
		paste0(" (from package \"", packageName, "\")")
       paste0('"', className, '"', packageName)
   }
   else
       paste0('"', className, '"')
}

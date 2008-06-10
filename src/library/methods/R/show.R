#  File src/library/methods/R/show.R
#  Part of the R package, http://www.R-project.org
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
#  http://www.r-project.org/Licenses/

showDefault <- function(object, oldMethods = TRUE)
{
    clDef <- getClass(cl <- class(object), .Force=TRUE)
    cl <- classLabel(cl)
    if(!is.null(clDef) && is.na(match(clDef@className, .BasicClasses)) &&
       !extends(clDef, "oldClass")) {
        cat("An object of class ", cl, "\n", sep="")
        slots <- slotNames(clDef)
        if(!is.na(match(".Data", slots))) {
            dataPart <- object@.Data
            show(dataPart)
            slots <- slots[is.na(match(slots, ".Data"))]
        }
        else if(length(slots) == 0)
            show(unclass(object))
        for(what in slots) {
            if(identical(what, ".Data"))
                next ## should have been done above
            cat("Slot \"",what, "\":\n", sep="")
            print(slot(object, what))
            cat("\n")
        }
    }
    else if(isClass(clDef) && extends(clDef, "oldClass") &&
            length(slotNames(clDef)) > 0) {
        ## print the old-style object
        cat("An object of class ", cl, "\n", sep="")
        for( cl2 in rev(extends(clDef)))
            if(!.identC(cl2, "oldClass") && extends(cl2, "oldClass")) {
                print(as(object, cl2), useS4 = FALSE) # see comment NBB below
                break
            }
        for(what in slotNames(clDef)) {
            cat("Slot \"",what, "\":\n", sep="")
            print(slot(object, what))
            cat("\n")
        }
    }
    else
        ## NBB:  This relies on the delicate fact (as of version 1.7 at least)
        ## that print will NOT recursively call show if it gets more than one argument!
        print(object, useS4 = FALSE)
}

## temporary definition of show, to become the default method
## when .InitShowMethods is called
show <- function(object)
    showDefault(object, FALSE)

.InitShowMethods <- function(envir) {
    if(!isGeneric("show", envir))
        setGeneric("show", where = envir)
    setMethod("show", "MethodDefinition",
              function(object) {
                  cl <- class(object)
                  if(.identC(cl, "MethodDefinition"))
                      nonStandard <- ""
                  else
                      nonStandard <-  paste(" (Class ", classLabel(cl),")", sep="")
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
                  if(length(object@group) > 0)
                      cat("  belonging to group(s):",
                          paste(unlist(object@group), collapse =", "), "\n")
                  if(length(object@valueClass) > 0)
                      cat("  defined with value class: \"", object@valueClass,
                          "\"\n", sep="")
                  cat("\n")
                  show(object@.Data)
                  cat("Methods may be defined for arguments: ",
                      paste(object@signature, collapse=", "), "\n",
			    "Use  showMethods(\"", object@generic,
			    "\")  for currently available ones.\n", sep="")
              },
              where = envir)
    setMethod("show", "classRepresentation",
              function(object){
                  if(!.identC(class(object), "classRepresentation"))
                    cat("Extended class definition (", classLabel(class(object)),
                        ")\n")
                  print.classRepresentation(object)
              },
              where = envir)

    ## a show() method for the signature class
    setMethod("show", "signature", function(object) {
        message("An object of class \"", class(object), "\"")
        val <- object@.Data
        names(val) <- object@names
        callNextMethod(val)
    } ,
              where = envir)
}

## an informative string label for a class
classLabel <- function(Class) {
    if(is.character(Class) && length(Class) > 0) {
        className <- Class[[1]]
        packageName <- attr(Class, "package")
    }
    else {
        if(is(Class, "classRepresentation")) {
            className <- Class@className
            packageName <- Class@package
        }
        else stop(gettextf("invalid call to 'classLabel': expected a name or a class definition, got an object of class \"%s\"", classLabel(class(Class))), domain = NA)
    }
### TODO:  implement a test for the class NOT directly visible OR multiple versions
### and include the from... phrase in this case
#     if(....) {
#         if(identical(packageName, ".GlobalEnv"))
#             fromString <- " (from the global environment)"
#         else
#             fromString <- paste(" (from package \"", packageName, "\")", sep="")
#         className <- paste("\"", className, "\"", fromString, sep="")
#     }
#     else
    dQuote(className)
}

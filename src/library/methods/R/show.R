showDefault <-
  function(object, oldMethods = TRUE)
{
    printNoClass <- function(x)
        .Internal(print.default(x, NULL, TRUE, NULL, NULL, FALSE, FALSE))

    cl <- .class1(object)
    if(isClass(cl) && is.na(match(cl, .BasicClasses)) && !extends(cl, "oldClass")) {
        cat("An object of class \"", cl, "\"\n", sep="")
        slots <- slotNames(cl)
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
    else if(isClass(cl) && extends(cl, "oldClass") && length(slotNames(cl)) > 0) {
        ## print the old-style object
        cat("An object of class \"", cl, "\"\n", sep="")
        for( cl2 in rev(extends(cl)))
            if(!identical(cl2, "oldClass") && extends(cl2, "oldClass")) {
                print(as(object, cl2), useS4 = FALSE) # see comment NBB below
                break
            }
        for(what in slotNames(cl)) {
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
    if(!isGeneric("show"))
        setGeneric("show", where = envir)
    setMethod("show", "MethodDefinition",
              function(object) {
                  cat("Method Definition (Class \"", class(object), "\"):\n\n", sep = "")
                  show(object@.Data)
                  mm <- .methodSignatureMatrix(object)
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
                  cat("Methods may be defined for arguments:",
                      paste(object@signature, collapse=", "), "\n\n")
              },
              where = envir)
}

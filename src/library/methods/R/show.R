showDefault <-
  function(object, oldMethods = TRUE)
{
    cl <- .class1(object)
    if(isClass(cl) && is.na(match(cl, .BasicClasses))) {
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
    else {
        printFun <- printNoClass
        # Try to honor old-style methods for basic classes & undefined classes.
        if(oldMethods) {
            oldMethod <- paste("print", cl, sep=".")
            if(existsFunction(oldMethod))
                printFun <- getFunction(oldMethod, generic = FALSE)
        }
        printFun(object)
     }
}

## temporary definition of show, to become the default method
## when .InitShowMethods is called
show <- function(object)
    showDefault(object, FALSE)


printNoClass <- get("print.default", "package:base")

print.default <- function(x, ...) {
    cl <- attr(x, "class") # pick off old-style objects
    if(length(cl) == 1 && isClass(cl) && length(list(...)) == 0)
        show(x)
    else
        printNoClass(x, ...)
}

.InitShowMethods <- function(envir) {
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
                  cat("Method Definition (Class \"", class(object), "\"):\n\n", sep = "")
                  show(object@.Data)
                  mm <- rbind(.methodSignatureMatrix(object),
                              NextMethod = object@nextMethod@defined)
                  cat("\nSignatures:\n")
                  print(mm)
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

showDefault <-
  function(object, oldMethods = TRUE)
{
    cl <- data.class(object)
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

printNoClass <- get("print.default", "package:base")

print.default <- function(x, ...) {
    cl <- attr(x, "class") # pick off old-style objects
    if(length(cl) == 1 && isClass(cl) && length(list(...)) == 0)
        show(x)
    else
        printNoClass(x, ...)
}

.InitShowMethods <- function(envir) {
    setGeneric("show", function(object)standardGeneric("show"),
               where = envir)
    setMethod("show", "ANY",
              function(object)
              showDefault(object, oldMethods = FALSE), where = envir)
    setMethod("show", "MethodDefinition",
              function(object) {
                  cat("Method Definition (Class \"", class(object), "\"):\n\n", sep = "")
                  show(object@.Data)
                  mm <- methodSignatureMatrix(object)
                  cat("\nSignatures:\n")
                  print(mm)
              },
              where = envir)
    setMethod("show", "MethodWithNext",
              function(object)  {
                  cat("Method Definition (Class \"", class(object), "\"):\n\n", sep = "")
                  show(object@.Data)
                  mm <- rbind(methodSignatureMatrix(object),
                              NextMethod = object@nextMethod@defined)
                  cat("\nSignatures:\n")
                  print(mm)
              },
              where = envir)
}

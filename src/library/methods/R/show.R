show <-
  ## display the object, by printing, plotting or whatever suits its class.
  ##
  ## This function exists to be specialized by methods; the definition stored here
  ## is intended as the default method.  In S-Plus, but not currently in R, `show'
  ## is called for automatic display of the result of an evaluated expression.
    function(object)
    showDefault(object, oldMethods = FALSE)

showDefault <-
  function(object, printTo = stdout(), oldMethods = TRUE)
{
      if(identical(printTo, FALSE)) {
          tmp <- tempfile()
          con <- file(tmp, "w")
      }
      else
          con <- printTo
    cl <- data.class(object)
    if(isClass(cl) && is.na(match(cl, .BasicClasses))) {
        cat(file = con, "An object of class \"", cl, "\"\n", sep="")
        for(what in slotNames(cl)) {
            cat(file = con, "Slot ",what, ":\n", sep="")
            print(slot(object, what))
            cat(file = con, "\n")
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
        if(identical(printTo, FALSE)) {
            sink(con)
            printFun(object)
            sink()
        }
        else
            printFun(object)
     }

    if(identical(printTo, FALSE)) {
        close(con)
        value <- readLines(tmp)
        unlink(tmp)
        value
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

show <-
  ## display the object, by printing, plotting or whatever suits its class.
  ##
  ## This function exists to be specialized by methods; the definition stored here
  ## is intended as the default method.  In S-Plus, but not currently in R, `show'
  ## is called for automatic display of the result of an evaluated expression.
  function(object) {
    cl <- data.class(object)
    if(isClass(cl)) {
      cat("An object of class \"", cl, "\"\n", sep="")
      for(what in slotNames(cl)) {
        cat("Slot ",what, ":\n", sep="")
        show(slot(object, what))
        cat("\n")
      }
    }
    else
      print.default(unclass(object))
  }

class <-
  function(x) {
    value <- attr(x, "class")
    if(is.null(value)) {
      ## similar to data.class
        l <- length(dim(x))
        if (l == 2) 
            "matrix"
        else if (l > 0) 
            "array"
        else mode(x)
      }
    else
      value
  }
  .BasicVectorClasses <- c("logical", "numeric", "character",
                           "complex", "integer", "single",
                           "expression", "list")
  .BasicClasses <- c(.BasicVectorClasses,
                            "function", "environment")

"class<-" <-
  function(x, value) {
    ## a definition of match that avoids messing with inheritance
    ## (The version on base calls is.factor, is.table)
    Match <-
      function(x, table, nomatch = NA)
        .Internal(match(x, table, nomatch))
    if(identical(class(x), value))
      x
    else if(length(value) != 1) {## must be an old-style class:  just set the attribute
      attr(x, "class") <- value
      x
    }
    else if(is.na(Match(value, .BasicClasses))) {
      ## don't do the coerce here: that's the job of the as() function
      attr(x, "class") <- value
      x
    }
    else {
      ## must be something for which an as.<class> function exists,
      ## or else this will generate an error
      mode(x) <- value
      x
    }
  }

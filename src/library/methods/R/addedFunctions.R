
functionBody <- get("body", mode = "function")

"functionBody<-" <- get("body<-", mode = "function")

allNames <-
  ## the character vector of names (unlike names(), never returns NULL)
  function(x) {
    value <- names(x)
    if(is.null(value))
      character(length(x))
    else
      value
  }

getFunction <-
  ## find the object as a function.
  function(name, generic = T, mustFind = T, where = find(name, mode = "function", numeric=TRUE)) {
    isGenericFunction <- function(obj) exists(".Generic", envir = environment(obj), inherits=F)
    found <- F
    ## unfortunately, if `where' turns out to be an environment, the for
    ## loop will generate an error.
    if(is.environment(where)) where <- list(where)
    for(i in where)
      if(exists(name, i, mode = "function", inherits = F)) {
        f <- get(name, i)
        if(generic || !isGenericFunction(f)) {
          found <- T
          break
        }
      }
    if(found)
      f
    else if(mustFind)
      stop(paste("no function \"", name, "\" as requested", sep=""))
    else
      NULL
  }

el <-
  function(object, where)
  ## element of a vector; numeric index only.
  ##
  ## the definition allows indexing beyond current length of vector
  ## (consistent with [[]] in S but not in R).
  object[where][[1]]

"el<-" <-
  ## set the element of a vector; numeric index only.
  .Primitive("[[<-")

elNamed <-
  ## get the element of the vector corresponding to name.  No partial matching.
  function(x, name, mustFind=F) {
    i <- match(name, names(x))
    if(is.na(i)) {
      if(mustFind)
        stop(paste("\"", name, "\" is not one of the element names", sep=""))
      else NULL
    }
    else
      el(x,i)
  }

"elNamed<-" <-
  ## set the element of the vector corresponding to name.
  function(x, name, value) {
    x[[name]] <- value
    x
  }

formalArgs <-
  ## Returns the names of the formal arguments of this function.
  function(def)
    names(formals(def))

existsFunction <-
  ## Is there a function of this name. If `generic==FALSE', generic functions are not counted.
  function(name, generic=TRUE, where = find(name, mode="function", numeric=T)) {
    if(generic && missing(where))
      return(length(where) > 0)
    if(is.environment(where)) {
      if(!exists(name, where))
        return(FALSE)
      obj <- get(name, where)
      return(is.function(obj) &&
             (generic || !isGeneric(name, fdef = obj)))
    }
    for(wherei in where) {
      if(!exists(name, where, inherits=F))
        next
      obj <- get(name, wherei)
      if(is(obj, "function")) {
        if(!generic &&
           isGeneric(name, fdef = obj))
          next
        return(TRUE)
      }
    }
    return(FALSE)
  }

findFunction <-
  ## return all the indices of the search list on which a function
  ## definition for `name' exists.  If `generic' is FALSE, ignore generic
  ## functions.
  function(name, generic = TRUE)  {
    allWhere <- find(name, mode = "function", numeric=T)
    if(generic)
       allWhere
    else
      sapply(as.list(allWhere), function(x)isGeneric(get(name, x)))
  }

Quote <- get("quote" , mode = "function")


message <-
  ## output all the arguments, pasted together with no intervening spaces.
  function(...)
  cat(paste(..., collapse="", sep=""), "\n")

warning <-
  function(...) {
    if(nargs() == 0)
      message <- NULL
    else
      message <- paste(..., sep="")
    .Internal(warning(message))
  }


functionBody <- get("body", mode = "function")

.ff <- function(f, value, envir = environment(f)) f

body(.ff, envir = .GlobalEnv) <- body(get("body<-"))

"functionBody<-" <- .ff

rm(.ff)

allNames <-
  ## the character vector of names (unlike names(), never returns NULL)
  function(x)
{
    value <- names(x)
    if(is.null(value))
        character(length(x))
    else
        value
}

getFunction <-  function(name, generic = TRUE, mustFind = TRUE,
           where = -1)
      ## find the object as a function.
{
    isGenericFunction <- function(obj) exists(".Generic", envir = environment(obj), inherits=FALSE)
    found <- FALSE
    if(identical(where, -1))
      where <- 1:length(search())
    else if(is.environment(where)) where <- list(where)
    ## unfortunately, if `where' turns out to be an environment, the for
    ## loop will generate an error.
    for(i in where)
        if(exists(name, i, inherits = FALSE)) {
            f <- get(name, i)
            if(is.function(f) && (generic || !isGenericFunction(f))) {
                found <- TRUE
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
  function(x, name, mustFind=FALSE)
{
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
  function(x, name, value)
{
    x[[name]] <- value
    x
}

formalArgs <-
  ## Returns the names of the formal arguments of this function.
  function(def)
    names(formals(def))

existsFunction <-
  ## Is there a function of this name. If `generic==FALSE', generic functions are not counted.
  function(f, generic=TRUE, where)
{
  if(missing(where))
    return(length(findFunction(f, generic)) > 0)
  if(is.environment(where) || length(where == 1)) {
        if(!exists(f, where, inherits = FALSE))
          return(FALSE)
        obj <- get(f, where)
        return(is.function(obj) &&
               (generic || !isGeneric(f, fdef = obj)))
    }
  ## the case of multiple databases supplied.  Unusual, but supported.
    for(wherei in where) {
        if(!exists(f, where, inherits=FALSE))
            next
        obj <- get(f, wherei)
        if(is.function(obj) &&
           generic || is.primitive(obj)
           || !isGeneric(f, wherei, obj))
          return(TRUE)
    }
    return(FALSE)
}

findFunction <-
  ## return all the indices of the search list on which a function
  ## definition for `name' exists.  If `generic' is FALSE, ignore generic
  ## functions.
  function(f, generic = TRUE)
{
    allWhere <- integer()
    for(i in seq(along=search()))
      if(exists(f, i, inherits = FALSE)) {
        fdef <-get(f, i)
        if(generic || is.primitive(fdef)
           || !isGeneric(f, i, fdef))
          allWhere <- c(allWhere, i)
      }
    allWhere
  }

Quote <- get("quote" , mode = "function")


message <-
  ## output all the arguments, pasted together with no intervening spaces.
  function(...) {
      ## the junk below is just til cat honors fill=TRUE on a single string.
      text <- paste(..., collapse="", sep="")
      pos <- max(2 * getOption("width") %/% 3, 20)
      while(nchar(text) > pos) {
          line <- substr(text, 1, pos)
          text <- substr(text, pos+1, nchar(text))
          word <- regexpr(" ", text)
          if(word < 0) {
              line <- paste(line, text, sep="")
              text <- ""
          }
          else {
              line <- paste(line, substr(text, 1, word -1), sep="")
              text <- substr(text, word+1, nchar(text))
          }
          cat(line, "\n")
      }
      if(nchar(text) > 0)
          cat(text, "\n")
  }

hasArg <- function(name) {
    aname <- as.character(substitute(name))
    fnames <- names(formals(sys.function(1)))
    if(is.na(match(aname, fnames))) {
        if(is.na(match("...", fnames)))
            FALSE
        else {
            dotsCall <- eval(quote(substitute(list(...))), sys.parent())
            !is.na(match(aname, names(dotsCall)))
        }
    }
    else
        eval(substitute(!missing(name)), sys.frame(sys.parent()))
}

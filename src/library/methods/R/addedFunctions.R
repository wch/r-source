
functionBody <- get("body", mode = "function")

.ff <- function(fun, envir = environment(fun), value) fun

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
           where = topenv(parent.frame()))
      ## find the object as a function.
{
    found <- FALSE
    where <- as.environment(where)
    f <- NULL
    ## parent.env sequence of a namespace ends in the base package namespace,
    ## of a non-namespace ends in NULL (equiv. to base environment) [sigh]
    lastEnv <- if(isNamespace(where)) function(where) isBaseNamespace(where) else
    function(where) is.null(where)
    repeat {
        if(exists(name, envir = where, mode = "function", inherits = FALSE)) {
            f <- get(name, envir = where)
            found <- generic || !is(f, "genericFunction")
        }
        if(found || lastEnv(where))
            break
        where <- parent.env(where)
    }
    if(!found && mustFind)
        stop("no ", if(generic) "" else "non-generic ", "function \"", name, "\" found")
    f
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


findFunction <-
  ## return a list of all the places where a function
  ## definition for `name' exists.  If `generic' is FALSE, ignore generic
  ## functions.
  function(f, generic = TRUE, where = topenv(parent.frame()))
{
    allWhere <- .findAll(f, where)
    ok <- rep(FALSE, length(allWhere))
    for(i in seq(along = ok)) {
        wherei <- allWhere[[i]]
        if(exists(f, wherei, inherits = FALSE)) {
            fdef <-get(f, wherei)
            if(generic || is.primitive(fdef)
               || !isGeneric(f, wherei, fdef))
                ok[[i]] <- TRUE
        }
    }
    allWhere[ok]
  }

existsFunction <- function(f, generic=TRUE, where = topenv(parent.frame()))
    length(findFunction(f, generic, where))>0

Quote <- get("quote" , mode = "function")


.message <-
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
    fnames <- names(formals(sys.function(sys.parent())))
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

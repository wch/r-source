addTaskCallback <- function(f, data = NULL, name = character(0))
{
    if(!is.function(f))
        stop("handler must be a function")
    val <- .Call("R_addTaskCallback", f, data, !missing(data),
                 as.character(name), PACKAGE="base")

    val + 1
}

removeTaskCallback <- function(id)
{
    if(!is.character(id))
        id <- as.integer(id)

    .Call("R_removeTaskCallback", id, PACKAGE="base")
}

getTaskCallbackNames <-
function()
{
    .Call("R_getTaskCallbackNames", PACKAGE="base")
}


taskCallbackManager <-
  #
  #
  #
function(handlers = list(), registered = FALSE, verbose = FALSE)
{
    suspended <- FALSE
    .verbose <- verbose

    add <-
    #
    # this is used to register a callback.
    # It has the same call sequence and semantics
    # as addTaskCallback but provides an optional
    # name by which to identify the element.
    # This can be used to remove the value in the future.
    # The default name is the next available position in the
    # list.
    # The result is stored in the `handlers' list using the
    # name.
    #
    # The element in the list contains the function
    # in the `f' slot,  and optionally a data field
    # to store the `data' argument.
    #
    # This could arrange to register itself using
    # addTaskCallback() if the size of the handlers list
    # becomes 1.
        function(f, data = NULL, name = NULL, register = TRUE)
        {

      # generate default name if none supplied
            if(is.null(name))
                name <- as.character(length(handlers) + 1)

      # Add to handlers, replacing any element with that name
      # if needed.
            handlers[[name]] <<- list(f = f)

      # If data was specified, add this to the new element
      # so that it will be included in the call for this function
            if(!missing(data))
                handlers[[name]][["data"]] <<- data

      # We could arrange to register the evaluate function
      # so that the handlers list would be active. However,
      # we would have to unregister it in the remove()
      # function when there were no handlers.
            if(!registered && register) {
                register()
            }

            name
        }

    remove <- function(which)
    {
        if(is.character(which)) {
            tmp <- (1:length(handlers))[!is.na(match(which, names(handlers)))]
            if(length(tmp))
                stop("No such element ", sQuote(which))
            which <- tmp
        } else
        which <- as.integer(which)

        handlers <<- handlers[-which]

        return(TRUE)
    }


    evaluate <-
    #
    # This is the actual callback that is registered with the C-level
    # mechanism. It is invoked by R when a top-level task is completed.
    # It then calls each of the functions in the handlers list
    # passing these functions the arguments it received and any
    # user-level data for those functions registered in the call to
    # add() via the `data' argument.
    #
    # At the end of the evaluation, any function that returned FALSE
    # is discarded.
        function(expr, value, ok, visible)
        {
            if(suspended)
                return(TRUE)
            discard <- character(0)
            for(i in names(handlers)) {
                h <- handlers[[i]]
                if(length(h) > 1) {
                    val <- h[["f"]](expr, value, ok, visible, i[["data"]])
                } else {
                    val <- h[["f"]](expr, value, ok, visible)
                }
                if(!val) {
                    discard <- c(discard, i)
                }
            }
            if(length(discard) > 0) {
                if(.verbose)
                    cat(gettext("Removing"), paste(discard, collapse=", "), "\n")
                idx <- is.na(match(names(handlers), discard))
                if(length(idx))
                    handlers <<- handlers[idx]
                else
                    handlers <<- list()
            }
            return(TRUE)
        }

    suspend <-
        function(status = TRUE) {
            suspended <<- status
        }

    register <-
        function(name = "R-taskCallbackManager", verbose = .verbose)
        {
            if(verbose)
                cat(gettext("Registering evaluate as low-level callback\n"))
            id <- addTaskCallback(evaluate, name = name)
            registered <<- TRUE
            id
        }

    list(add = add,
         evaluate = evaluate,
         remove = remove,
         register = register,
         suspend = suspend,
         callbacks = function()
         handlers
         )
}


#  File src/library/base/R/taskCallback.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2012 The R Core Team
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  https://www.R-project.org/Licenses/

addTaskCallback <- function(f, data = NULL, name = character())
{
    if(!is.function(f))
        stop("handler must be a function")
    val <- .Call(.C_R_addTaskCallback, f, data, !missing(data),
                 as.character(name))
    val + 1L
}

removeTaskCallback <- function(id)
{
    if(!is.character(id))
        id <- as.integer(id)

    .Call(.C_R_removeTaskCallback, id)
}

getTaskCallbackNames <- function() .Call(.C_R_getTaskCallbackNames)


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
                name <- as.character(length(handlers) + 1L)

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
            tmp <- seq_along(handlers)[!is.na(match(which, names(handlers)))]
            if(length(tmp))
                stop(gettextf("no such element '%s'", which), domain = NA)
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
            discard <- character()
            for(i in names(handlers)) {
                h <- handlers[[i]]
                if(length(h) > 1L) {
                    val <- h[["f"]](expr, value, ok, visible, i[["data"]])
                } else {
                    val <- h[["f"]](expr, value, ok, visible)
                }
                if(!val) {
                    discard <- c(discard, i)
                }
            }
            if(length(discard)) {
                if(.verbose)
                    cat(gettextf("Removing %s", paste(discard, collapse=", ")), "\n")
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
                cat(gettext("Registering 'evaluate' as low-level callback\n"))
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

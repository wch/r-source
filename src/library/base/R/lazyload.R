## note that there is a version in ../baseloader.R that should be kept in step
lazyLoad <- function(filebase, envir = parent.frame(), filter)
{
    ##
    ## bootstrapping definitions so we can load base
    ## - not that this version is actually used to load base
    ##
    glue <- function (..., sep = " ", collapse = NULL)
        .Internal(paste(list(...), sep, collapse))
    readRDS <- function (file) {
        halt <- function (message) .Internal(stop(TRUE, message))
        gzfile <- function (description, open)
            .Internal(gzfile(description, open, "", 6))
        close <- function (con) .Internal(close(con, "rw"))
        if (! is.character(file)) halt("bad file name")
        con <- gzfile(file, "rb")
        on.exit(close(con))
        .Internal(unserializeFromConn(con, baseenv()))
    }
    `parent.env<-` <-
        function (env, value) .Internal(`parent.env<-`(env, value))
    existsInFrame <- function (x, env) .Internal(exists(x, env, "any", FALSE))
    getFromFrame <- function (x, env) .Internal(get(x, env, "any", FALSE))
    set <- function (x, value, env) .Internal(assign(x, value, env, FALSE))
    environment <- function () .Internal(environment(NULL))
    mkenv <- function() .Internal(new.env(TRUE, baseenv(), 29L))

    ##
    ## main body
    ##
    mapfile <- glue(filebase, "rdx", sep = ".")
    datafile <- glue(filebase, "rdb", sep = ".")
    env <- mkenv()
    map <- readRDS(mapfile)
    vars <- names(map$variables)
    rvars <- names(map$references)
    compressed <- map$compressed
    for (i in seq_along(rvars))
        set(rvars[i], map$references[[i]], env)
    envenv <- mkenv()
    envhook <- function(n) {
        if (existsInFrame(n, envenv))
            getFromFrame(n, envenv)
        else {
            e <- mkenv()
            set(n, e, envenv)           # MUST do this immediately
            key <- getFromFrame(n, env)
            data <- lazyLoadDBfetch(key, datafile, compressed, envhook)
            ## comment from r41494
            ## modified the loading of old environments, so that those
            ## serialized with parent.env NULL are loaded with the
            ## parent.env=emptyenv(); and yes an alternative would have been
            ## baseenv(), but that was seldom the intention of folks that
            ## set the environment to NULL.
            if (is.null(data$enclos))
                parent.env(e) <- emptyenv()
            else
                parent.env(e) <- data$enclos
            vars <- names(data$bindings)
            for (i in seq_along(vars))
                set(vars[i], data$bindings[[i]], e)
            e
        }
    }
    if (!missing(filter)) {
        use <- filter(vars)
        vars <- vars[use]
        vals <- map$variables[use]
        use <- NULL
    } else
        vals <-  map$variables

    expr <- quote(lazyLoadDBfetch(key, datafile, compressed, envhook))
    this <- environment()
    .Internal(makeLazy(vars, vals, expr, this, envir))

    ## reduce memory use
    map <- NULL
    vars <- NULL
    vals <- NULL
    rvars <- NULL
    mapfile <- NULL
    readRDS <- NULL
}

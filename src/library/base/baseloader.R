lazyLoad <- function(filebase, envir = parent.frame(), filter) {
    #
    # bootstrapping definitions so we can load base
    #
    glue <- function (..., sep = " ", collapse = NULL)
        .Internal(paste(list(...), sep, collapse))
    readRDS <- function (file) {
        halt <- function (message) .Internal(stop(TRUE, message))
        gzfile <- function (description, open)
            .Internal(gzfile(description, open, 0:255, 6))
        close <- function (con) .Internal(close(con, "rw"))
        if (! is.character(file)) halt("bad file name")
        con <- gzfile(file, "rb")
        on.exit(close(con))
        .Internal(unserializeFromConn(con, NULL))
    }
    "parent.env<-" <-
        function (env, value) .Internal("parent.env<-"(env, value))
    along <- function(x) { n <- length(x); if (n) 1 : n else NULL }
    existsInFrame <- function (x, env) .Internal(exists(x, env, "any", FALSE))
    getFromFrame <- function (x, env) .Internal(get(x, env, "any", FALSE))
    set <- function (x, value, env) .Internal(assign(x, value, env, FALSE))
    environment <- function () .Internal(environment(NULL))
    mkpromise <- function(expr, env) .Internal(delay(expr, env))
    mkenv <- function() .Internal(new.env(TRUE, NULL))
    names <- function(x) .Internal(names(x))
    lazyLoadDBfetch <- function(key, file, compressed, hook)
        .Call("R_lazyLoadDBfetch", key, file, compressed, hook, PACKAGE="base")

    #
    # main body
    #
    mapfile <- glue(filebase, "rdx", sep = ".")
    datafile <- glue(filebase, "rdb", sep = ".")
    env <- mkenv()
    map <- readRDS(mapfile)
    vars <- names(map$variables)
    rvars <- names(map$references)
    compressed <- map$compressed
    for (i in along(rvars))
        set(rvars[i], map$references[[i]], env)
    envenv <- mkenv()
    envhook <- function(n) {
       if (existsInFrame(n, envenv))
           getFromFrame(n, envenv)
       else {
           e <- mkenv()
           set(n, e, envenv) # MUST do this immediately
           key <- getFromFrame(n, env)
           data <- lazyLoadDBfetch(key, datafile, compressed, envhook)
           parent.env(e) <- data$enclos
           vars <- names(data$bindings)
           for (i in along(vars))
               set(vars[i], data$bindings[[i]], e)
           e
        }
    }
    expr <- quote(lazyLoadDBfetch(key, datafile, compressed, envhook))
    wrap<-function(key) {
        key <- key # force evaluation
        mkpromise(expr, environment())
    }
    if (! missing(filter)) {
        for (i in along(vars))
            if (filter(vars[i]))
                set(vars[i], wrap(map$variables[[i]]), envir)
    }
    else {
        for (i in along(vars))
            set(vars[i], wrap(map$variables[[i]]), envir)
    }

    # reduce memory use **** try some more trimming
    map <- NULL
    vars <- NULL
    rvars <- NULL
    mapfile <- NULL
    readRDS <- NULL
}
.Internal(eval(quote({

    existsInBase <- function (x)
        .Internal(exists(x, .BaseNamespaceEnv, "any", TRUE))
    assignInBase <- function (x, value)
        .Internal(assign(x, value, .BaseNamespaceEnv, FALSE))
    glue <- function (..., sep = " ", collapse = NULL)
        .Internal(paste(list(...), sep, collapse))

    filter <- function(n)
       ! existsInBase(n) || n == "is.ts" || n == "is.factor"

    basedb <- glue(.Internal(R.home()), "library", "base", "R",
                   "base", sep= .Platform$file.sep)

    lazyLoad(basedb, NULL, filter)

}), .Internal(new.env(FALSE, NULL)), NULL))

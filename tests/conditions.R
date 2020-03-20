
saveTo <- function(to) function(cond) assign(to, cond, envir = globalenv())
isCond <- function(x, class, msg) {
    inherits(x, "condition") && inherits(x, class) && identical(x$message, msg)
}

## Handler stack starts empty
handlers <- withVisible(globalCallingHandlers())
stopifnot(
    identical(handlers$value, list()),
    isTRUE(handlers$visible)
)

## Handlers must be functions
err <- tryCatch(globalCallingHandlers(foo = identity, bar = NA),
                error = identity)
stopifnot(identical(err$message, "condition handlers must be functions"))

## Can register and inspect handler
globalCallingHandlers(error = saveTo(".Last.error"))
handlers <- withVisible(globalCallingHandlers())

stopifnot(
    length(handlers$value) == 1,
    names(handlers$value) == "error",
    handlers$visible
)

## Handlers are invoked based on class
.Last.error <- NULL
signalCondition(simpleCondition("foo"))
stopifnot(is.null(.Last.error))
signalCondition(simpleError("foo"))
stopifnot(isCond(.Last.error, "error", "foo"))

## Can register multiple handlers
globalCallingHandlers(
    condition = saveTo(".Last.condition"),
    warning = saveTo(".Last.warning")
)
handlers <- globalCallingHandlers()
stopifnot(length(handlers) == 3,
          all(names(handlers) == c("condition", "warning", "error")))

## Multiple handlers are invoked
.Last.error <- NULL
signalCondition(simpleWarning("bar"))
stopifnot(
    is.null(.Last.error),
    isCond(.Last.condition, "warning", "bar"),
    isCond(.Last.warning, "warning", "bar")
)
signalCondition(simpleError("baz"))
stopifnot(
    isCond(.Last.error, "error", "baz"),
    isCond(.Last.condition, "error", "baz"),
    isCond(.Last.warning, "warning", "bar")
)

## Handlers are not invoked if error is caught
.Last.error <- NULL
try(stop("baz"), TRUE)
stopifnot(is.null(.Last.error))

## Can remove handlers
handlers <- globalCallingHandlers()
old <- withVisible(globalCallingHandlers(NULL))
stopifnot(
    identical(old$value, handlers),
    !old$visible,
    identical(globalCallingHandlers(), list())
)
signalCondition(simpleError("foo"))
stopifnot(is.null(.Last.error))

## Can pass list of handlers
foobars <- list(foo = function() "foo", bar = function() "bar")
globalCallingHandlers(foobars)
stopifnot(identical(globalCallingHandlers(), foobars))
globalCallingHandlers(NULL)

## Local handlers are not returned
handlers <- withCallingHandlers(foo = function(...) NULL,
                                globalCallingHandlers())
stopifnot(identical(handlers, list()))
globalCallingHandlers(foobars)
handlers <- withCallingHandlers(foo = function(...) NULL,
                                globalCallingHandlers())
stopifnot(identical(handlers, foobars))
globalCallingHandlers(NULL)


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

## Registering a handler again moves it to the top of the stack
globalCallingHandlers(
    warning = function(...) "foo",
    error = function(...) "foo",
    condition = function(...) "foo",
    error = function(...) "bar"
)
globalCallingHandlers(
    error = function(...) "foo"
)
bumped <- list(
    error = function(...) "foo",
    warning = function(...) "foo",
    condition = function(...) "foo",
    error = function(...) "bar"
)
stopifnot(identical(globalCallingHandlers(), bumped))
## Fails in R 4.0
globalCallingHandlers(
    warning = function(...) "foo",
    error = function(...) "foo"
)
bumped <- list(
    warning = function(...) "foo",
    error = function(...) "foo",
    condition = function(...) "foo",
    error = function(...) "bar"
)
stopifnot(identical(globalCallingHandlers(), bumped))
globalCallingHandlers(NULL)

## Attributes and closure environments are detected in the duplicate
## handlers check
hnd1 <- function(...) "foo"
hnd2 <- structure(function(...) "foo", bar = TRUE)
hnd3 <- local(function(...) "foo")
expectedList <- list(
    error = hnd1,
    error = hnd2,
    error = hnd3
)
globalCallingHandlers(error = hnd1, error = hnd2, error = hnd3)
stopifnot(identical(globalCallingHandlers(), expectedList))
globalCallingHandlers(NULL)
## and removeSource() now retains attributes:
stopifnot( identical(attributes(removeSource(hnd2)), list(bar = TRUE)) )


## Source references do not cause handlers to be treated as distinct
withSource <- function(src, envir = parent.frame(), file = NULL) {
    if (is.null(file)) {
	file <- tempfile("sourced", fileext = ".R")
    }
    on.exit(unlink(file))
    writeLines(src, file)
    source(file, local = envir, keep.source = TRUE)
}
withSource("hnd1 <- structure(function(...) { NULL })")
withSource("hnd2 <- structure(function(...) { NULL })")
globalCallingHandlers(NULL)
globalCallingHandlers(error = hnd1)
globalCallingHandlers(error = hnd2) # message "pushing duplicate .."
stopifnot(identical(globalCallingHandlers(), list(error = hnd2)))
globalCallingHandlers(NULL)

sys.call <-function(which = 0)
    .Internal(sys.call(which))

sys.calls <-function()
    .Internal(sys.calls())

sys.frame <-function(which = 0)
    .Internal(sys.frame(which))

sys.function <-function(which = 0)
    .Internal(sys.function(which))

sys.frames <-function()
    .Internal(sys.frames())

sys.nframe <- function()
    .Internal(sys.nframe())

sys.parent <- function(n = 1)
    .Internal(sys.parent(n))

sys.parents <- function()
    .Internal(sys.parents())

sys.status <- function()
    list(sys.calls=sys.calls(), sys.parents=sys.parents(),
         sys.frames=sys.frames())

sys.on.exit <- function()
    .Internal(sys.on.exit())


# Define a convenience function that is easy to call from C code
grid.top.level.vp <- function() {
  pushedvp(viewport(clip=TRUE, name="ROOT"))
}

# An accessor for getting at the grid global state structure
# to make debugging easier for me;  all I have to type is grid:::STATE()
STATE <- function() {
  get(".GRID.STATE", envir=.GridEvalEnv)
}

is.odd <- function(x) {
  (x %% 2 == 0)
}

is.even <- function(x) {
  !is.odd(x)
}

grid.pretty <- function(range) {
  if (!is.numeric(range))
    stop("'range' must be numeric")
  .Call("L_pretty", range, PACKAGE="grid")
}


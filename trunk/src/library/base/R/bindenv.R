lockEnvironment <- function(env, bindings = FALSE)
    .Internal(lockEnvironment(env, bindings))

environmentIsLocked <- function(env)
    .Internal(environmentIsLocked(env))

lockBinding <- function(sym, env) {
    if (is.character(sym)) sym <- as.name(sym)
    .Internal(lockBinding(sym, env))
}

bindingIsLocked <- function(sym, env) {
    if (is.character(sym)) sym <- as.name(sym)
    .Internal(bindingIsLocked(sym, env))
}

makeActiveBinding <- function(sym, fun, env) {
    if (is.character(sym)) sym <- as.name(sym)
    .Internal(makeActiveBinding(sym, fun, env))
}

bindingIsActive <- function(sym, env) {
    if (is.character(sym)) sym <- as.name(sym)
    .Internal(bindingIsActive(sym, env))
}

unlockBinding <- function(sym, env) {
    if (is.character(sym)) sym <- as.name(sym)
    .Internal(unlockBinding(sym, env))
}


## Must match order in src/include/R_ext/GraphicsEngine.h
compositingOperators <- c(## Porter-Duff
                          "clear", "source", "over",
                          "in", "out", "atop",
                          "dest", "dest.over", "dest.in",
                          "dest.out", "dest.atop", "xor",
                          "add", "saturate",
                          ## PDF "blend modes"
                          "multiply", "screen", "overlay",
                          "darken", "lighten", "color.dodge", "color.burn",
                          "hard.light", "soft.light", "difference", "exclusion"
                          )

.opIndex <- function(x) {
    op <- match(x, compositingOperators)
    if (is.na(op))
        stop("Invalid compositing operator")
    as.integer(op)
}

.defineGroup <- function(source, op, destination) {
    .External(C_defineGroup, source, .opIndex(op), destination)
}

.useGroup <- function(ref, trans) {
    .External(C_useGroup, ref, trans)
}

.devUp <- function() {
    .External(C_devUp)
}

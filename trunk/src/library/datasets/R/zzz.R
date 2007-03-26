.noGenerics <- TRUE
.First.lib <- function(...) {
    lockEnvironment(as.environment("package:datasets"), TRUE)
}

.First.lib <- function(lib, pkg) {
    library.dynam("eda", pkg, lib)
    provide(eda)
}

.First.lib <- function(lib, pkg) {
    library.dynam("tcltk", pkg, lib)
    .C("tcltk_init")
    provide(tcltk)
}

### dumping for standard packages is with R_DEFAULT_PACKAGES=NULL
require(graphics); require(stats); require(methods)

.onLoad <- function(lib, pkg) {
    require(graphics, save=FALSE) || stop("package 'graphics' is missing")
    require(stats, save=FALSE) || stop("package 'graphics' is missing")
    require(methods, save=FALSE) || stop("package 'graphics' is missing")
}

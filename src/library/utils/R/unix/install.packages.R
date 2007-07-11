link.html.help <- function(verbose=FALSE, lib.loc=.libPaths())
{
    if(verbose)
        message("Updating HTML index of packages in '.Library'")
    tools:::unix.packages.html(.Library)
}

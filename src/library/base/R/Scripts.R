.Script <- function(interpreter, script, args, ...)
{
    if(.Platform$OS.type == "windows") {
        cmd <- paste(file.path(R.home(), "bin", "Rcmd"),
                     file.path("..", "share", interpreter, script),
                     args)
        system(cmd, invisible = TRUE)
    }
    else
        system(paste(file.path(R.home(), "bin", "Rcmd"),
                     interpreter,
                     file.path(R.home(), "share", interpreter, script),
                     args),
               ...)
}

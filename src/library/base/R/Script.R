.Script <- function(interpreter, script, args, ...) {
    system(paste(file.path(R.home(), "bin", "Rcmd"),
                 interpreter,
                 file.path(R.home(), "share", interpreter, script),
                 args), ...)
}

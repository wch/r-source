.Script <-
function(interpreter, script, args, ...)
{
    ## <FIXME>
    ## Keep in sync with tools:::.shell_quote, or do something smarter
    ## eventually (such as using utils::shQuote) ...
    .shell_quote <- sQuote
    ## Also, what about Windows?
    ## </FIXME>
    
    if(.Platform$OS.type == "windows") {
        cmd <- paste(file.path(R.home(), "bin", "Rcmd"),
                     file.path("..", "share", interpreter, script),
                     args)
        system(cmd, invisible = TRUE)
    }
    else
        system(paste(.shell_quote(file.path(R.home(), "bin", "Rcmd")),
                     interpreter,
                     .shell_quote(file.path(R.home(), "share",
                                            interpreter, script)),
                     args),
               ...)
}

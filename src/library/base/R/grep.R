grep <-
    function(pattern, x, ignore.case=FALSE, extended=TRUE, perl=FALSE, value=FALSE)
{
    if(perl)
        .Internal(grep.perl(pattern, x, ignore.case, value))
    else
        .Internal(grep(pattern, x, ignore.case, extended, value))
}

sub <-
    function(pattern, replacement, x, ignore.case=FALSE, extended=TRUE, perl=FALSE)
{
    if(perl)
        .Internal(sub.perl(pattern, replacement, x, ignore.case))
    else
        .Internal(sub(pattern, replacement, x, ignore.case, extended))
}

gsub <-
    function(pattern, replacement, x, ignore.case=FALSE, extended=TRUE, perl=FALSE)
{
    if(perl)
        .Internal(gsub.perl(pattern, replacement, x, ignore.case))
    else
        .Internal(gsub(pattern, replacement, x, ignore.case, extended))
}

regexpr <- function(pattern, text, extended=TRUE, perl=FALSE)
{
    if(perl)
        .Internal(regexpr.perl(pattern, text))
    else
        .Internal(regexpr(pattern, text, extended))
}

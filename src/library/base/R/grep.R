grep <-
    function(pattern, x, ignore.case=FALSE, extended=TRUE, value=FALSE)
{
    .Internal(grep(pattern, x, ignore.case, extended, value))
}

sub <-
    function(pattern, replacement, x, ignore.case=FALSE, extended=TRUE)
{
    .Internal(sub(pattern, replacement, x, ignore.case, extended))
}

gsub <-
    function(pattern, replacement, x, ignore.case=FALSE, extended=TRUE)
{
    .Internal(gsub(pattern, replacement, x, ignore.case, extended))
}

regexpr <- function(pattern, text, extended=TRUE)
{
    .Internal(regexpr(pattern, text, extended))
}

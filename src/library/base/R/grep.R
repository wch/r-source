grep <-
    function(pattern, x, ignore.case=FALSE, extended=TRUE, perl=FALSE, value=FALSE)
{
    .Internal(grep(pattern, x, ignore.case, extended, perl, value))
}

sub <-
    function(pattern, replacement, x, ignore.case=FALSE, extended=TRUE, perl=FALSE)
{
    .Internal(sub(pattern, replacement, x, ignore.case, extended, perl))
}

gsub <-
    function(pattern, replacement, x, ignore.case=FALSE, extended=TRUE, perl=FALSE)
{
    .Internal(gsub(pattern, replacement, x, ignore.case, extended, perl))
}

regexpr <- function(pattern, text, extended=TRUE, perl=FALSE)
{
    .Internal(regexpr(pattern, text, extended, perl))
}

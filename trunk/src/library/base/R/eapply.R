eapply <- function (env, FUN, ..., all.names = FALSE)
{
    FUN <- match.fun(FUN)
    .Internal(eapply(env, FUN, all.names))
}

ifelse <-
    function (test, yes, no)
{
    storage.mode(test) <- "logical"
    ans <- test
    nas <- is.na(test)
    if (any(test[!nas]))
        ans[test & !nas] <- rep(yes, length.out = length(ans))[test & !nas]
    if (any(!test[!nas]))
        ans[!test & !nas] <- rep(no, length.out = length(ans))[!test & !nas]
    ans[nas] <- NA
    ans
}

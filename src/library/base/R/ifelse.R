ifelse <-
    function (test, yes, no)
{
    storage.mode(test) <- "logical"
    ans <- test
    nas <- is.na(test)
    if (any(test[!nas])) {
        ans[test] <- rep(yes, length = length(ans))[test]
    }
    if (any(!test[!nas])) {
        ans[!test] <- rep(no, length = length(ans))[!test]
    }
    ans[nas] <- NA
    ans
}

ifelse <- 
function (test, yes, no) 
{
        ans <- test
        test <- as.logical(test)
        nas <- is.na(test)
        ans[test] <- rep(yes, length = length(ans))[test]
        ans[!test] <- rep(no, length = length(ans))[!test]
        ans[nas] <- NA
        ans
}

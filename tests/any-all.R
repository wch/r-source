all_tests <- list(
                  list(TRUE, all(logical(0))),
                  list(TRUE, all(c(TRUE, TRUE))),
                  list(TRUE, all(rep(TRUE, 2), rep(TRUE, 10))),
                  list(NA, all(rep(TRUE, 2), c(TRUE, NA))),
                  list(TRUE, all(rep(TRUE, 2), c(TRUE, NA), na.rm = TRUE)),
                  list(FALSE, all(FALSE)),
                  list(NA, all(c(NA, TRUE))),
                  list(FALSE, all(c(NA, FALSE))),
                  list(FALSE, all(rep(TRUE, 2), c(TRUE, FALSE))),
                  list(FALSE, all(c(TRUE, FALSE), c(TRUE, NA)), na.rm = TRUE))

any_tests <- list(
                  list(FALSE, any(logical(0))),
                  list(TRUE, any(TRUE)),
                  list(TRUE, any(FALSE, TRUE)),
                  list(TRUE, any(c(FALSE, TRUE))),
                  list(FALSE, any(c(FALSE, FALSE))),
                  list(TRUE, any(c(TRUE, NA, FALSE))),
                  list(TRUE, any(c(TRUE, NA, FALSE), na.rm = TRUE)),
                  list(NA, any(c(NA, FALSE))),
                  list(TRUE, any(c(NA, TRUE))),
                  list(FALSE, any(c(NA, FALSE), na.rm = TRUE)))

test_list <- function(L)
{
    lab <- deparse(substitute(L))
    for (i in seq_along(L)) {
        case <- L[[i]]
        if (!identical(case[[1]], case[[2]])) {
            stop("test failed at ", lab, "[[", i, "]]")
        }
    }
}

test_list(all_tests)
test_list(any_tests)

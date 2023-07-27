## check that all S3 methods in base are registered.
(function() {
  old <-  Sys.getlocale("LC_COLLATE")
  on.exit(Sys.setlocale("LC_COLLATE", old))
          Sys.setlocale("LC_COLLATE", "C")
  stopifnot(identical(base:::.S3_methods_table, # >>> end of ../../base/R/zzz.R ; update *there* !
                      tools:::.make_S3_methods_table_for_base()))
})()


## check that all .internalGenerics  have .Internal :
(iGens <- .internalGenerics)
names(iGens) <- iGens
str(bdI <- lapply(iGens, body))
stopifnot(lengths(bdI) >= 2L)

is.qI  <- function(.) identical(., quote(.Internal))
has.qI <- function(E) is.qI(E) || is.qI(E[[1L]])
str(l1 <- lapply(bdI, \(bd) if(bd[[1]] == quote(`{`)) bd[[length(bd)]] else bd[[1]]))
(r <- vapply(l1, \(b) has.qI(b) || has.qI(b[[length(b)]]), NA))
stopifnot(r)

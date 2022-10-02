## check that all S3 methods in base are registered.
(function() {
  old <-  Sys.getlocale("LC_COLLATE")
  on.exit(Sys.setlocale("LC_COLLATE", old))
          Sys.setlocale("LC_COLLATE", "C")
  stopifnot(identical(base:::.S3_methods_table, # >>> end of ../../base/R/zzz.R ; update *there* !
                      tools:::.make_S3_methods_table_for_base()))
})()

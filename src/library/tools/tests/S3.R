## check that all S3 methods in base are registered.
## See the end of src/library/base/R/zzz.R ...
stopifnot(identical(base:::.S3_methods_table,
                    tools:::.make_S3_methods_table_for_base()))

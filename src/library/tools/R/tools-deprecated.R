filePathAsAbsolute <-
function(x)
{
    .Deprecated("file_path_as_absolute", package = "tools")
    file_path_as_absolute(x)
}

filePathSansExt <-
function(x)
{
    .Deprecated("file_path_sans_ext", package = "tools")
    file_path_sans_ext(x)
}    

fileTest <-
function(op, x, y)
{
    .Deprecated("file_test", package = "tools")
    file_test(op, x, y)
}

listFilesWithExts <-
function(dir, exts, all.files = FALSE, full.names = TRUE)
{
    .Deprecated("list_files_with_exts", package = "tools")
    list_files_with_exts(dir, exts,
                         all.files = all.files, full.names = full.names)

}

listFilesWithType <-
function(dir, type, all.files = FALSE, full.names = TRUE)
{
    .Deprecated("list_files_with_type", package = "tools")
    list_files_with_type(dir, type,
                         all.files = all.files, full.names = full.names)
}

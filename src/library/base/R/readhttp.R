url.show <-
    function (url,  title = url, file = tempfile(),
              delete.file = TRUE, method, ...)
{
    if (download.file(url, dest = file, method = method) != 0)
        stop("transfer failure")
    file.show(file, delete.file = delete.file, title = title, ...)
}

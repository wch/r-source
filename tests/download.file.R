## 2023-04 this relied on eu.httpbin.org which had become slow/unreliable.
## switched to httpbin,org which seems more reliable if sometimes slow.

## original commit
# r75890 | maechler | 2018-12-23 17:39:19 +0000 (Sun, 23 Dec 2018) | 1 line
# provide `headers` option to url() and download.file(), thanks to Gábor Csárdi

site <- "developer.R-project.org"
prefix <- "/inet-tests"
rx <- paste0("Host.*", site)

## Tests for HTTP headers -----------------------------------------------

## header names are case-insensitive - we could to better, but this
## should be good enough for tests.
grep_hdr <- function(regex, h) any(grepl(regex, h, TRUE))

is_online <- function() {
    tryCatch({
        ## 8.8.8.8 is Google DNS
        ## FIXME: many local networks block DNS requests to any non-local DNS
        ##        servers so this almost never works. Do we really need this?
        ## con <- suppressWarnings(socketConnection("8.8.8.8", port = 53))
        ## close(con)
        ## We may as well use socket to the site instead as a test ...
        con <- suppressWarnings(socketConnection(site, port = 443))
        close(con)
        URL <- paste0("http://", site, prefix, "/anything")
        con <- url(URL)
        lines <- readLines(con)
        close(con)
        stopifnot(grep_hdr(rx, lines))
        TRUE
    }, error = function(e) FALSE)
}

get_headers <- function(path = "anything", quiet = TRUE, ...,
                        protocol = "http") {
  url <- get_path(path, protocol)
  tmp <-  tempfile()
  on.exit(try(unlink(tmp)), add = TRUE)
  download.file(url, tmp, quiet = quiet, ...)
  readLines(tmp)
}

get_headers_url <- function(path = "anything", ..., protocol = "http") {
  con <- url(get_path(path, protocol), ...)
  on.exit(try(close(con)), add = TRUE)
  readLines(con)
}

get_path <- function(path = "anything", protocol = "http") {
  paste0(protocol, "://", site, prefix, "/", path)
}

with_options <- function(opts, expr) {
  old <- do.call(options, as.list(opts))
  on.exit(options(old), add = TRUE)
  expr
}

tests <- function() {
  cat("- User agent is still set\n")
  with_options(list(HTTPUserAgent = "foobar"), {
    h <- get_headers()
    stopifnot(grep_hdr("User-Agent.*foobar", h))
  })

  with_options(list(HTTPUserAgent = "foobar"), {
      h <- get_headers(headers = c(foo = "bar", zzzz = "bee"))
    stopifnot(grep_hdr("User-Agent.*foobar", h))
    stopifnot(grep_hdr("Foo.*bar", h))
    stopifnot(grep_hdr("Zzzz.*bee", h))
  })

  cat("- Can supply headers\n")
  h <- get_headers(headers = c(foo = "bar", zzzz = "bee"))
  stopifnot(grep_hdr("Foo.*bar", h))
  stopifnot(grep_hdr("Zzzz.*bee", h))

  cat("- Basic auth\n")
  ret <- tryCatch({
    h <- suppressWarnings(get_headers(
      "basic-auth/Aladdin/OpenSesame",
      headers = c(Authorization = "Basic QWxhZGRpbjpPcGVuU2VzYW1l")))
    TRUE
  }, error = function(e) FALSE)
  stopifnot(any(grepl("authenticated.*true", h)))

  if (getOption("download.file.method") == "libcurl") {
    cat("- Multiple urls (libcurl only)\n")
    urls <- get_path(c("anything", "headers"))
    tmp1 <- tempfile()
    tmp2 <- tempfile()
    on.exit(unlink(c(tmp1, tmp2)), add = TRUE)
    status <- download.file(urls, c(tmp1, tmp2), quiet = TRUE,
                            headers = c(foo = "bar", zzzz = "bee"))
    if (status == 0L) {
        h1 <- readLines(tmp1)
        h2 <- readLines(tmp2)
        stopifnot(grep_hdr("Foo.*bar", h1))
        stopifnot(grep_hdr("Zzzz.*bee", h1))
        stopifnot(grep_hdr("Foo.*bar", h2))
        stopifnot(grep_hdr("Zzzz.*bee", h2))
    }
  }

  cat("- HTTPS\n")
  h <- get_headers(headers = c(foo = "bar", zzzz = "bee"), protocol = "https")
  stopifnot(grep_hdr("Foo.*bar", h))
  stopifnot(grep_hdr("Zzzz.*bee", h))

  cat("- If headers not named, then error\n")
  ret <- tryCatch(
    download.file(get_path(), headers = c("foo", "xxx" = "bar")),
    error = function(err) TRUE)
  stopifnot(isTRUE(ret))
  ret <- tryCatch(
    download.file(get_path(), headers = "foobar"),
    error = function(err) TRUE)
  stopifnot(isTRUE(ret))

  cat("- If headers are NA, then error\n")
  ret <- tryCatch(
    download.file(get_path(), headers = c("foo" = NA, "xxx" = "bar")),
    error = function(err) TRUE)
  stopifnot(isTRUE(ret))
  ret <- tryCatch(
    download.file(
      get_path(), quiet = TRUE,
      headers = structure(c("foo", "bar", names = c("foo", NA)))),
    error = function(err) TRUE)
  stopifnot(isTRUE(ret))

  cat("- user agent is set in url()\n")
  with_options(list(HTTPUserAgent = "foobar"), {
    h <- get_headers_url()
    stopifnot(grep_hdr("User-Agent.*foobar", h))
  })

  cat("- file() still works with URLs\n")
  con <- file(get_path("anything", "http"))
  on.exit(close(con), add = TRUE)
  h <- readLines(con)
  stopifnot(grep_hdr(rx, h))

  cat("- If headers not named, then url() errors\n")
  ret <- tryCatch(
    url(get_path(), headers = c("foo", "xxx" = "bar")),
    error = function(err) TRUE)
  stopifnot(isTRUE(ret))

  cat("- If headers are NA, then url() errors\n")
  ret <- tryCatch(
    url(get_path(), headers = c("foo" = "bar", "xxx" = NA)),
    error = function(err) TRUE)
  stopifnot(isTRUE(ret))
  ret <- tryCatch(
    url(get_path(),
        headers = structure(c("1", "2"), names = c("foo", NA))),
    error = function(err) TRUE)
  stopifnot(isTRUE(ret))

  cat("- Can supply headers in url()\n")
  h <- get_headers_url(headers = c(foo = "bar", zzzz = "bee"))
  stopifnot(grep_hdr("Foo.*bar", h))
  stopifnot(grep_hdr("Zzzz.*bee", h))

  cat("- HTTPS with url()\n")
  h <- get_headers_url(headers = c(foo = "bar", zzzz = "bee"),
                       protocol = "https")
  stopifnot(grep_hdr("Foo.*bar", h))
  stopifnot(grep_hdr("Zzzz.*bee", h))
}

main <- function() {
    ## all builds have libcurl as from R 4.2.0.
    cat("\nlibcurl method\n")
    with_options(c(download.file.method = "libcurl"), tests())

    if (.Platform$OS.type == "windows")  {
        ## This is deprecated and will give warnings.
        cat("\nwininet method\n")
        with_options(c(download.file.method = "wininet"), tests())
    }
}

options(warn = 1L)

## if URL is unresponsive or times out, this silently skips all the checks
if (is_online()) main()

proc.time()

read_symbols_from_object_file <-
function(f)
{
    if(!nzchar(nm <- Sys.which("nm"))) return()
    f <- file_path_as_absolute(f)
    s <- strsplit(system(sprintf("%s -Pg %s", shQuote(nm), shQuote(f)),
                         intern = TRUE),
                  " +")
    ## Cannot simply rbind() this because elements may have 2-4
    ## entries.
    n <- length(s)
    tab <- matrix("", nrow = n, ncol = 4L)
    colnames(tab) <- c("name", "type", "value", "size")
    ## Compute desired i and j positions in tab.
    i <- rep.int(seq_len(n), sapply(s, length))
    j <- unlist(lapply(s, seq_along))

    tab[n * (j - 1L) + i] <- unlist(s)
    
    tab
}

get_system_ABI <-
function()
{
    s <- Sys.getenv("R_SYSTEM_ABI")
    if((s == "") || (substring(s, 1L, 1L) %in% c("@", "?")))
        return(character())
    s <- unlist(strsplit(s, ",", fixed = TRUE))
    names(s) <- c("system", "CC", "CXX", "F77", "FC")
    s
}

system_ABI <- get_system_ABI()

so_symbol_names_table <-
    c("linux, C, gcc, abort, abort",
      ## http://refspecs.freestandards.org/LSB_4.0.0/LSB-Core-generic/LSB-Core-generic/baselib---assert-fail-1.html
      "linux, C, gcc, assert, __assert_fail",
      "linux, C, gcc, exit, exit",
      "linux, C, gcc, printf, printf",      
      "linux, C, gcc, printf, puts",
      "linux, C, gcc, puts, puts",
      "linux, C, gcc, putchar, putchar",
      "linux, C, gcc, stderr, stderr",
      "linux, C, gcc, stdout, stdout",
      "linux, C, gcc, vprintf, vprintf",
      "linux, C++, gxx, std::cout, _ZSt4cout",
      "linux, C++, gxx, std::cerr, _ZSt4cerr",
      "linux, Fortran, gfortran, write, _gfortran_st_write",
      "linux, Fortran, gfortran, print, _gfortran_st_write",
      "linux, Fortran, gfortran, stop, _gfortran_stop_numeric_f08",
      "linux, Fortran, gfortran, stop, _gfortran_stop_string"
      )
so_symbol_names_table <-
    do.call(rbind,
            strsplit(so_symbol_names_table,
                     split = ", ", fixed = TRUE))
colnames(so_symbol_names_table) <-
    c("system", "language", "compiler", "ssname", "osname")

## Subscript according to system and compiler types here, rather than
## repeatedly doing this at run time.
so_symbol_names_table <-
    so_symbol_names_table[(so_symbol_names_table[, "system"] ==
                           system_ABI["system"]) &
                          (so_symbol_names_table[, "compiler"] %in%
                           system_ABI[c("CC", "CXX", "F77", "FC")]),
                          c("language", "ssname", "osname"),
                          drop = FALSE]

so_symbol_names_handlers_db <- list()
## <NOTE>
## As we record the low-level (possibly mangled) symbol names for
## each system/compiler combination, there is no need for handlers to
## demangle into user-level names (e.g., using c++filt).
## </NOTE>
so_symbol_names_handlers_db$linux <-
function(x)
{
    ## Linux ELF symbol versioning, see
    ##  http://lists.debian.org/lsb-spec/1999/12/msg00017.html:
    ## name@version for alternatives, name@@version for the default.
    sub("@.*", "", x)
}

check_so_symbols <-
function(so)
{
    if(!length(system_ABI)) return()
    tab <- read_symbols_from_object_file(so)
    nms <- tab[tab[, "type"] == "U", "name"]
    sys <- system_ABI["system"]
    if(!is.null(snh <- so_symbol_names_handlers_db[[sys]]))
        nms <- snh(nms)
    ind <- so_symbol_names_table[, "osname"] %in% nms
    tab <- so_symbol_names_table[ind, , drop = FALSE]
    attr(tab, "file") <- so
    class(tab) <- "check_so_symbols"
    tab
}

format.check_so_symbols <-
function(x, ...)
{
    if(!length(x)) return(character())
    entries <- split.data.frame(x, x[, "osname"])
    c(gettextf("File %s:", sQuote(attr(x, "file"))),
      unlist(Map(function(u, v)
                 strwrap(gettextf("Found %s, possibly from %s",
                                  sQuote(v),
                                  paste(sprintf("%s (%s)",
                                                sQuote(u[, "ssname"]),
                                                u[, "language"]),
                                        collapse = ", ")),
                         indent = 2L, exdent = 4L),
                 entries, names(entries))))
}

print.check_so_symbols <-
function(x, ...)
{
    writeLines(format(x))
    invisible(x)
}

check_compiled_code <-
function(dir)
{
    ## Check compiled code in the shared objects of an installed
    ## package.
    so_files <- Sys.glob(file.path(dir, "libs",
                                   sprintf("*%s", 
                                           .Platform$dynlib.ext)))
    bad <- Filter(length, lapply(so_files, check_so_symbols))
    class(bad) <- "check_compiled_code"
    bad
}

format.check_compiled_code <-
function(x, ...)
{
    if(!length(x)) return(character())
    paste(sapply(x, format), collapse = "\n")
}

print.check_compiled_code <-
function(x, ...)
{
    writeLines(format(x))
    invisible(x)
}

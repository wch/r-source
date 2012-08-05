if(.Platform$OS.type == "windows") {
    read_symbols_from_dll <- function(f, rarch)
#
#  Copyright (C) 1995-2012 The R Core Team
    {
        ## reasonable to assume this on the path
        DLL_nm <- "objdump.exe"
        if(!nzchar(Sys.which(DLL_nm))) return()
        f <- file_path_as_absolute(f)
        s0 <- suppressWarnings(system2(DLL_nm, c("-x", shQuote(f)),
                                       stdout = TRUE, stderr = TRUE))
        status <- attr(s0, "status")
        if (length(status) && status != 0) return()
        l1 <- grep("^\tDLL Name:", s0)
        l2 <- grep("^The Export Tables", s0)
        if (!length(l1) || !length(l2)) return()
        s1 <- s0[(l1[1L] + 3L):(l2 - 4L)]
        s2 <- grep("\t[0-9a-f]+\t +[0-9]+", s1, value = TRUE)
        sub(".* ([_A-Za-z0-9]+)$", "\\1", s2)
    }
}

read_symbols_from_object_file <- function(f)
{
    ## reasonable to assume this on the path
    if(!nzchar(nm <- Sys.which("nm"))) return()
    f <- file_path_as_absolute(f)
    if(!(file.info(f)$size)) return()
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

get_system_ABI <- if(.Platform$OS.type == "windows") {
    function() c(system = "windows", CC = "gcc", CXX = "g++",
                 F77 = "gfortran", FC = "gfortran")
} else {
    function()
    {
        s <- Sys.getenv("R_SYSTEM_ABI")
        if((s == "") || (substring(s, 1L, 1L) %in% c("@", "?")))
            return(character())
        s <- unlist(strsplit(s, ",", fixed = TRUE))
        names(s) <- c("system", "CC", "CXX", "F77", "FC")
        s
    }
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
      "linux, Fortran, gfortran, stop, _gfortran_stop_string",

      "osx, C, gcc, abort, _abort",
      "osx, C, gcc, assert, ___assert_rtn",
      "osx, C, gcc, exit, _exit",
      "osx, C, gcc, printf, _printf",
      "osx, C, gcc, printf, _puts",
      "osx, C, gcc, puts, _puts",
      "osx, C, gcc, putchar, _putchar",
      "osx, C, gcc, stderr, ___stderrp",
      "osx, C, gcc, stdout, ___stdoutp",
      "osx, C, gcc, vprintf, _vprintf",
      "osx, C++, gxx, std::cout, __ZSt4cout",
      "osx, C++, gxx, std::cerr, __ZSt4cerr",
      "osx, Fortran, gfortran, write, __gfortran_st_write",
      "osx, Fortran, gfortran, print, __gfortran_st_write",
      "osx, Fortran, gfortran, stop, __gfortran_stop_numeric",
      "osx, Fortran, gfortran, stop, __gfortran_stop_string",

      "freebsd, C, gcc, abort, abort",
      "freebsd, C, gcc, assert, __assert",
      "freebsd, C, gcc, exit, exit",
      "freebsd, C, gcc, printf, printf",
      "freebsd, C, gcc, printf, puts",
      "freebsd, C, gcc, puts, puts",
      "freebsd, C, gcc, putchar, putchar",
      "freebsd, C, gcc, stderr, __stderrp",
      "freebsd, C, gcc, stdout, __stdoutp",
      "freebsd, C, gcc, vprintf, vprintf",
      "freebsd, C++, gxx, std::cout, _ZSt4cout",
      "freebsd, C++, gxx, std::cerr, _ZSt4cerr",
      "freebsd, Fortran, gfortran, write, _gfortran_st_write",
      "freebsd, Fortran, gfortran, print, _gfortran_st_write",
      "freebsd, Fortran, gfortran, stop, _gfortran_stop_numeric_f08",
      "freebsd, Fortran, gfortran, stop, _gfortran_stop_string",

      ## stdout, stderr do not show up on Solaris
      "solaris, C, solcc, abort, abort",
      "solaris, C, solcc, assert, __assert_c99",
      "solaris, C, solcc, exit, exit",
      "solaris, C, solcc, printf, printf",
      "solaris, C, solcc, putchar, putchar",
      "solaris, C, solcc, puts, puts",
      "solaris, C, solcc, vprintf, vprintf",
      "solaris, C++, solCC, std::cout, __1cDstdEcout_",
      "solaris, C++, solCC, std::cerr, __1cDstdEcerr_",
      "solaris, Fortran, solf95, print, __f90_eslw",
      "solaris, Fortran, solf95, write, __f90_eslw",
      "solaris, Fortran, solf95, print, __f90_esfw",
      "solaris, Fortran, solf95, write, __f90_esfw",
      "solaris, Fortran, solf95, write, __f90_esuw",
      "solaris, Fortran, solf95, stop, __f90_stop",
      "solaris, Fortran, solf95, stop, __f90_stop_int",
      "solaris, Fortran, solf95, stop, __f90_stop_char",
      "solaris, Fortran, solf95, runtime, abort",

      ## Windows statically links libstdc++, libgfortran
      ## only in .o, positions hard-coded in check_so_symbols
      "windows, C++, g++, std::cout, _ZSt4cout",
      "windows, C++, g++, std::cerr, _ZSt4cerr",
      "windows, Fortran, gfortran, write, _gfortran_st_write",
      "windows, Fortran, gfortran, print, _gfortran_st_write",
      ## in DLL
      "windows, C, gcc, abort, abort",
      "windows, C++, gxx, runtime, abort",
      "windows, Fortran, gfortran, runtime, abort",
      "windows, C, gcc, assert, _assert",
      "windows, C, gcc, exit, exit",
      "windows, C, gcc, printf, printf",
      "windows, C, gcc, printf, puts",
      "windows, C, gcc, puts, puts",
      "windows, C, gcc, putchar, putchar",
      "windows, C, gcc, vprintf, vprintf",
      "windows, Fortran, gfortran, stop, exit"
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

so_symbol_names_handlers_db$freebsd <-
function(x)
{
    ## same as Linux, most likely, lots of name@@VERSION
    sub("@.*", "", x)
}

check_so_symbols <- if(.Platform$OS.type == "windows") {
    function(so, rarch, have_tables = FALSE)
    {
        if(!length(system_ABI)) return()
        nms <- read_symbols_from_dll(so, rarch)
        ind <- so_symbol_names_table[, "osname"] %in% nms
        if(have_tables) ind[1:4] <- TRUE
        tab <- so_symbol_names_table[ind, , drop = FALSE]
        attr(tab, "file") <- so
        class(tab) <- "check_so_symbols"
        tab
    }
} else {
    function(so)
    {
        if(!length(system_ABI)) return()
        tab <- read_symbols_from_object_file(so)
        # nms <- tab[tab[, "type"] == "U", "name"]
	nms <- tab[, "name"]
        sys <- system_ABI["system"]
        if(!is.null(snh <- so_symbol_names_handlers_db[[sys]]))
            nms <- snh(nms)
        ind <- so_symbol_names_table[, "osname"] %in% nms
        tab <- so_symbol_names_table[ind, , drop = FALSE]
        attr(tab, "file") <- so
        class(tab) <- "check_so_symbols"
        tab
    }
}

format.check_so_symbols <-
function(x, ...)
{
    if(!length(x)) return(character())
    entries <- split.data.frame(x, x[, "osname"])
    objects <- vector("list", length(entries))
    names(objects) <- names(entries)
    if(length(objs <- attr(x, "objects")))
        objects[names(objs)] <- objs
    c(gettextf("File %s:", sQuote(attr(x, "file"))),
      unlist(Map(function(u, v, w)
                 c(strwrap(gettextf("Found %s, possibly from %s",
                                    sQuote(v),
                                    paste(sprintf("%s (%s)",
                                                  sQuote(u[, "ssname"]),
                                                  u[, "language"]),
                                          collapse = ", ")),
                           indent = 2L, exdent = 4L),
                   if(length(w) > 1L) {
                       strwrap(sprintf("Objects: %s",
                                       paste(sQuote(w), collapse =
                                             ", ")),
                               indent = 4L, exdent = 6L)
                   } else if(length(w)) {
                       strwrap(sprintf("Object: %s", sQuote(w)),
                               indent = 4L, exdent = 6L)
                   }),
                 entries, names(entries), objects)))
}

print.check_so_symbols <-
function(x, ...)
{
    writeLines(format(x))
    invisible(x)
}

check_compiled_code <-
if(.Platform$OS.type == "windows") {
    function(dir)
    {
        ## Check compiled code in the DLL(s) of an installed package.

        r_arch <- .Platform$r_arch
        useST <- config_val_to_logical(Sys.getenv("_R_SHLIB_BUILD_OBJECTS_SYMBOL_TABLES_", "FALSE"))

        compare <- function(x, strip_ = FALSE) {
            ## Compare symbols in the DLL and in objects:
            symbols <-
                Filter(length,
                       lapply(tables,
                              function(tab) {
                                  nm <- tab[, "name"]
                                  if (strip_) nm <- sub("^_", "", nm)
                                  nm <- sub("_gfortran_stop.*", "exit", nm)
                                  intersect(x[, "osname"], nm)
                              }))
            ## Drop the DLL symbols not in any object.
            so <- attr(x, "file")
            osnames_in_objects <- unique(as.character(unlist(symbols)))
            x <- x[!is.na(match(x[, "osname"], osnames_in_objects)), , drop = FALSE]
            attr(x, "file") <- so

            attr(x, "objects") <-
                split(rep.int(names(symbols), sapply(symbols, length)),
                      unlist(symbols))
            class(x) <- "check_so_symbols"
            x
        }

        so_files <-
            Sys.glob(file.path(dir, "libs/i386",
                               sprintf("*%s", .Platform$dynlib.ext)))
        bad <- if(length(so_files)) {
            objects_symbol_tables_file <-
                file.path(dir, "libs/i386", "symbols.rds")
            if(file_test("-f", objects_symbol_tables_file)) {
                bad <- Filter(length, lapply(so_files, check_so_symbols,
                                             rarch = "i386", have_tables = TRUE))
                tables <- readRDS(objects_symbol_tables_file)
                Filter(length, lapply(bad, compare, strip_ = TRUE))
            } else {
                if(useST)
                    cat("Note: information on .o files for i386 is not available\n")
                Filter(length, lapply(so_files, check_so_symbols, rarch="i386"))
            }
        } else NULL

        so_files <-
            Sys.glob(file.path(dir, "libs/x64",
                               sprintf("*%s", .Platform$dynlib.ext)))
        bad2 <- if(length(so_files)) {
            objects_symbol_tables_file <- file.path(dir, "libs/x64", "symbols.rds")
            if(file_test("-f", objects_symbol_tables_file)) {
                bad2 <- Filter(length, lapply(so_files, check_so_symbols,
                                              rarch = "x64", have_tables = TRUE))
                tables <- readRDS(objects_symbol_tables_file)
                Filter(length, lapply(bad2, compare))
            } else {
                if(useST)
                    cat("Note: information on .o files for x64 is not available\n")
                Filter(length, lapply(so_files, check_so_symbols, rarch="x64"))
            }
        } else NULL

        if(!length(bad) && !length(bad2)) return(invisible(NULL))

        bad <- if(length(bad) && length(bad2)) rbind(bad, bad2)
        else if(length(bad2)) bad2 else bad
        class(bad) <- "check_compiled_code"
        bad
    }
} else {
    function(dir)
    {
        ## Check compiled code in the shared objects of an installed package.

        r_arch <- .Platform$r_arch
        useST <- config_val_to_logical(Sys.getenv("_R_SHLIB_BUILD_OBJECTS_SYMBOL_TABLES_", "FALSE"))

        compare <- function(x) {
            ## Compare symbols in the so and in objects:
            symbols <-
                Filter(length,
                       lapply(tables,
                              function(tab) {
                                  nm <- tab[, "name"]
                                  intersect(x[, "osname"], nm)
                              }))
            ## Drop the so symbols not in any object.
            so <- attr(x, "file")
            ## (Alternatively, provide a subscript method
            ## for class "check_so_symbols".)
            osnames_in_objects <- unique(as.character(unlist(symbols)))
            x <- x[!is.na(match(x[, "osname"], osnames_in_objects)), , drop = FALSE]
            attr(x, "file") <- so
            attr(x, "objects") <-
                split(rep.int(names(symbols), sapply(symbols, length)),
                      unlist(symbols))
            class(x) <- "check_so_symbols"
            x
        }

        so_files <- if(nzchar(r_arch))
            Sys.glob(file.path(dir, "libs", r_arch,
                               sprintf("*%s", .Platform$dynlib.ext)))
        else
            Sys.glob(file.path(dir, "libs",
                               sprintf("*%s", .Platform$dynlib.ext)))
        if(!length(so_files)) return(invisible(NULL)) # typically a fake install

        bad <- Filter(length, lapply(so_files, check_so_symbols))
        objects_symbol_tables_file <- if(nzchar(r_arch))
            file.path(dir, "libs", r_arch, "symbols.rds")
        else file.path(dir, "libs", "symbols.rds")
        if(file_test("-f", objects_symbol_tables_file)) {
            tables <- readRDS(objects_symbol_tables_file)
            bad <- Filter(length, lapply(bad, compare))
        } else if(useST)
            cat("Note: information on .o files is not available\n")

        class(bad) <- "check_compiled_code"
        bad
    }
}

format.check_compiled_code <-
function(x, ...)
{
    if(!length(x)) return(character())
    ## sapply does not always simplify as one wants here if there is
    ## more than one DLL.
    paste(unlist(lapply(x, format)), collapse = "\n")
}

print.check_compiled_code <-
function(x, ...)
{
    writeLines(format(x))
    invisible(x)
}

.shlib_objects_symbol_tables <-
function(file = "symbols.rds")
{
    objects <- commandArgs(trailingOnly = TRUE)
    tables <- lapply(objects, read_symbols_from_object_file)
    names(tables) <- objects
    saveRDS(tables, file = file)
}

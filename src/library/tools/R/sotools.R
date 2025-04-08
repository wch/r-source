#  File src/library/tools/R/sotools.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 2011-2025 The R Core Team
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  https://www.R-project.org/Licenses/

if(.Platform$OS.type == "windows") {
    read_symbols_from_dll_state <- new.env(hash = FALSE) # small
    read_symbols_from_dll <- function(f, rarch)
    {
	DLL_nm <- read_symbols_from_dll_state$DLL_nm
	if (is.null(DLL_nm)) {
	    ## R CMD config will fail when 'sh' (from Rtools) is not on PATH
	    DLL_nm <- tryCatch(
	        Rcmd(c("config", "OBJDUMP"), stdout = TRUE, stderr = FALSE),
		error = function(x) NULL,
		warning = function(x) NULL)

	    if (is.null(DLL_nm) || !nzchar(DLL_nm) ||
	        !file.exists(paste0(DLL_nm, ".exe"))) {

		## fall back to the old behavior: take OBJDUMP from PATH
		DLL_nm <- "objdump.exe"
		if(!nzchar(Sys.which(DLL_nm))) {
		    warning("this requires 'objdump.exe' to be on the PATH")
		    return()
		}
	    }
	    read_symbols_from_dll_state$DLL_nm <- DLL_nm
	}
        f <- file_path_as_absolute(f)
        s0 <- suppressWarnings(system2(DLL_nm, c("-x", shQuote(f)),
                                       stdout = TRUE, stderr = TRUE))
        status <- attr(s0, "status")
        if (length(status) && status != 0) return()
        l1 <- grep("^\tDLL Name:", s0)
        l2 <- grep("^The Export Tables", s0)
        if (!length(l1) || !length(l2)) return()
        s1 <- s0[(l1[1L] + 2L):(l2 - 4L)]

        # The format of the dump of import tables changed in Rtools45
        # (binutils 2.43.1).  Previously, there was a joint column
        # "Hint/Ord".  Newly these are split ("Ordinal" and "Hint").  The
        # regex below has been relaxed to match both.
        s2 <- grep("\t[0-9a-f]+[\t ]+", s1, value = TRUE)
        sub(".* ([_A-Za-z0-9]+)$", "\\1", s2)
    }
}

read_symbols_from_object_file <- function(f)
{
    ## For GCC & LTO, we need a different command, possibly with args
    ## On macOS, the system nm works with LTO objects.
    ## Do not use NM as make sets it.
    nm <- Sys.getenv("UserNM")
    if(!nzchar(nm)) {
        ## reasonable to assume nm is on the path
        nm <- Sys.which("nm")
        if(nzchar(nm)) nm <- shQuote(nm)
    }
    if(!nzchar(nm)) {
        warning("this requires 'nm' to be on the PATH")
        return()
    }
    f <- file_path_as_absolute(f)
    if(!(file.size(f))) return()
    s <- strsplit(system(sprintf("%s -Pg %s", nm, shQuote(f)),
                         intern = TRUE),
                  " +")
    ## Cannot simply rbind() this because elements may have 2-4 entries.
    n <- length(s)
    tab <- matrix("", nrow = n, ncol = 4L)
    colnames(tab) <- c("name", "type", "value", "size")
    ## Compute desired i and j positions in tab.
    i <- rep.int(seq_len(n), lengths(s))
    j <- unlist(lapply(s, seq_along))
    tab[n * (j - 1L) + i] <- unlist(s)
    tab
}

## env variable formerly in etc/Renviron, now in ../Makefile
system_ABI <- Sys.getenv("R_SYSTEM_ABI")
if((system_ABI == "") || (substr(system_ABI, 1L, 1L) %in% c("@", "?"))) {
    system_ABI <- character()
} else {
    system_ABI <- unlist(strsplit(system_ABI, ",", fixed = TRUE))
    names(system_ABI) <- c("system", "CC", "CXX", "F77", "FC")
}

## entry points for std::terminate are commented out as almost all
## come from system headers.
so_symbol_names_table <-
    ## 'linux' == glibc, principally but checked with Alpine Linux's musl
    c("linux, C, gcc, abort, abort",
      ## https://refspecs.linuxbase.org/LSB_5.0.0/LSB-Core-generic/LSB-Core-generic/baselib---assert-fail-1.html
      "Linux, C, gcc, assert, __assert_fail",
      "linux, C, gcc, assert, __assert_fail_base",
      "linux, C, gcc, exit, exit",
      "linux, C, gcc, _exit, _exit", ## may not be seen
      "linux, C, gcc, _Exit, _Exit", ## _Exit is C99 and may not be a fn call
      "linux, C, gcc, printf, printf",
      "linux, C, gcc, printf, __printf_chk",
      "linux, C, gcc, printf, puts",
      "linux, C, gcc, puts, puts",
      "linux, C, gcc, putchar, putchar",
      "linux, C, gcc, stderr, stderr",
      "linux, C, gcc, stdout, stdout",
      "linux, C, gcc, sprintf, sprintf",
      "linux, C, gcc, sprintf, __sprintf_chk",
      "linux, C, gcc, vprintf, vprintf",
      "linux, C, gcc, vprintf, __vprintf_chk",
      "linux, C, gcc, vsprintf, vsprintf",
#      "linux, C, gcc, vprintf, vfprintf", # but also from REvprintf
      "linux, C, gcc, vsprintf, __vsprintf_chk",
      "linux, C, gcc, rand, rand",
      "linux, C, gcc, random, random",
      "linux, C, gcc, rand_r, rand_r",
      "linux, C, gcc, srand, srand",
      "linux, C, gcc, srandom, srandom",
      "linux, C, gcc, srandom_r, srandom_r",
      "linux, C, gcc, srand48, srand48",
      "linux, C++, gxx, std::cout, _ZSt4cout",
      "linux, C++, gxx, std::cerr, _ZSt4cerr",
      #"linux, C++, gxx, std::terminate, _ZSt9terminatev",
      ## libc++ variants
      "linux, C++, gxx, std::cout, _ZNSt3__14coutE", # std::__1::cout
      "linux, C++, gxx, std::cerr, _ZNSt3__14cerrE",
      "linux, Fortran, gfortran, open, _gfortran_st_open",
      "linux, Fortran, gfortran, close, _gfortran_st_close",
      "linux, Fortran, gfortran, rewind, _gfortran_st_rewind",
      "linux, Fortran, gfortran, read, _gfortran_st_read",
      "linux, Fortran, gfortran, write, _gfortran_st_write",
      "linux, Fortran, gfortran, print, _gfortran_st_write",
      "linux, Fortran, gfortran, stop, _gfortran_stop_numeric_f08",
      "linux, Fortran, gfortran, stop, _gfortran_stop_string",
      "linux, Fortran, gfortran, rand, _gfortran_rand",
      "linux, Fortran, gfortran, random_init, _gfortran_random_init",
      "linux, Fortran, gfortran, random_number, _gfortran_arandom_r4",
      "linux, Fortran, gfortran, random_number, _gfortran_arandom_r8",
      "linux, Fortran, gfortran, random_number, _gfortran_arandom_r16",
      "linux, Fortran, gfortran, random_number, _gfortran_random_r4",
      "linux, Fortran, gfortran, random_number, _gfortran_random_r8",
      "linux, Fortran, gfortran, random_number, _gfortran_random_r16",
      "linux, Fortran, gfortran, random_number, _gfortran_rand",
      "linux, Fortran, gfortran, random_seed, _gfortran_random_seed_i4",
      "linux, Fortran, gfortran, random_seed, _gfortran_random_seed_i8",
      "linux, Fortran, gfortran, exit, _gfortran_exit_i4",
      "linux, Fortran, gfortran, exit, _gfortran_exit_i8",

      ## Classic flang from Dec 2017 (and untested since)
      "linux, Fortran, ClassicFlang, open, f90io_open03",
      "linux, Fortran, ClassicFlang, open, f90io_open2003",
      "linux, Fortran, ClassicFlang, close, f90io_close",
      "linux, Fortran, ClassicFlang, rewind, f90io_rewind",
      "linux, Fortran, ClassicFlang, write, f90io_print_init",
      "linux, Fortran, ClassicFlang, print, f90io_print_init",
      "linux, Fortran, ClassicFlang, read, f90io_fmt_read",
      "linux, Fortran, ClassicFlang, write, f90io_fmt_write",
      "linux, Fortran, ClassicFlang, stop, f90_stop",
      "linux, Fortran, ClassicFlang, stop, f90_stop08",
      "linux, Fortran, ClassicFlang, rand, rand",

      ## and for for the 'flang' in the LLVM tree, currently
      ## with executable 'flang-new'.
      ## This currently has static libs for its runtimes.
      "linux, Fortran, flang-new, stop, _FortranAStopStatement",
      "linux, Fortran, flang-new, stop, _FortranAStopStatementText",
      "linux, Fortran, flang-new, open, _FortranAioBeginOpenUnit",
      "linux, Fortran, flang-new, close, _FortranAioBeginClose",
      "linux, Fortran, flang-new, rewind, _FortranAioBeginRewind",
      "linux, Fortran, flang-new, read, _FortranAioInputAscii",
      "linux, Fortran, flang-new, read, _FortranAioInputCharacter",
      "linux, Fortran, flang-new, read, _FortranAioInputComplex32",
      "linux, Fortran, flang-new, read, _FortranAioInputComplex64",
      "linux, Fortran, flang-new, read, _FortranAioOutputExternalListInput",
      "linux, Fortran, flang-new, read, _FortranAioInputInteger",
      "linux, Fortran, flang-new, read, _FortranAioInputLogical",
      "linux, Fortran, flang-new, read, _FortranAioInputNamelist",
      "linux, Fortran, flang-new, read, _FortranAioInputReal32",
      "linux, Fortran, flang-new, read, _FortranAioInputReal64",
      "linux, Fortran, flang-new, read, _FortranAioInputUnformattedBlock",
      "linux, Fortran, flang-new, print, _FortranAioOutputAscii",
      "linux, Fortran, flang-new, print, _FortranAioOutputCharacter",
      "linux, Fortran, flang-new, print, _FortranAioOutputComplex32",
      "linux, Fortran, flang-new, print, _FortranAioOutputComplex64",
      "linux, Fortran, flang-new, print, _FortranAioOutputExternalListOutput",
      "linux, Fortran, flang-new, print, _FortranAioOutputInteger128",
      "linux, Fortran, flang-new, print, _FortranAioOutputInteger16",
      "linux, Fortran, flang-new, print, _FortranAioOutputInteger32",
      "linux, Fortran, flang-new, print, _FortranAioOutputInteger64",
      "linux, Fortran, flang-new, print, _FortranAioOutputInteger8",
      "linux, Fortran, flang-new, print, _FortranAioOutputLogical",
      "linux, Fortran, flang-new, print, _FortranAioOutputNamelist",
      "linux, Fortran, flang-new, print, _FortranAioOutputReal32",
      "linux, Fortran, flang-new, print, _FortranAioOutputReal64",
      "linux, Fortran, flang-new, write, _FortranAioOutputAscii",
      "linux, Fortran, flang-new, write, _FortranAioOutputCharacter",
      "linux, Fortran, flang-new, write, _FortranAioOutputComplex32",
      "linux, Fortran, flang-new, write, _FortranAioOutputComplex64",
      "linux, Fortran, flang-new, write, _FortranAioOutputExternalListOutput",
      "linux, Fortran, flang-new, write, _FortranAioOutputInteger128",
      "linux, Fortran, flang-new, write, _FortranAioOutputInteger16",
      "linux, Fortran, flang-new, write, _FortranAioOutputInteger32",
      "linux, Fortran, flang-new, write, _FortranAioOutputInteger64",
      "linux, Fortran, flang-new, write, _FortranAioOutputInteger8",
      "linux, Fortran, flang-new, write, _FortranAioOutputLogical",
      "linux, Fortran, flang-new, write, _FortranAioOutputNamelist",
      "linux, Fortran, flang-new, write, _FortranAioOutputReal32",
      "linux, Fortran, flang-new, write, _FortranAioOutputReal64",
      "linux, Fortran, flang-new, write, _FortranAioOutputUnformatedBlock",
      ## does not support rand()
      ## https://discourse.llvm.org/t/support-for-gnu-fortran-extensions/69630
      "linux, Fortran, flang-new, random_init, _FortranARandomInit",
      "linux, Fortran, flang-new, random_number, _FortranARandomNumber",
      "linux, Fortran, flang-new, random_seed, _FortranARandomSeed",
      "linux, Fortran, flang-new, random_seed, _FortranARandomSeedGet",
      "linux, Fortran, flang-new, random_seed, _FortranARandomSeedSize",

      ## Intel 'Clasic' and 202x
      "linux, Fortran, intel, stop, for_stop",
      "linux, Fortran, intel, stop, for_stop_core",
      "linux, Fortran, intel, stop, for_stop_core8",
      "linux, Fortran, intel, stop, for_stop_core_impl",
      "linux, Fortran, intel, stop, for_stop_core_int",
      "linux, Fortran, intel, stop, for_stop_core_int8",
      "linux, Fortran, intel, stop, for_stop_core_quiet",
      "linux, Fortran, intel, stop, for_stop_core_quiet_int8",
      "linux, Fortran, intel, print, for_write_seq_lis",
      "linux, Fortran, intel, open, for_open",
      "linux, Fortran, intel, open, for_open_args",
      "linux, Fortran, intel, open, for_open_default",
      "linux, Fortran, intel, open, for_open_key",
      "linux, Fortran, intel, close, for_close",
      "linux, Fortran, intel, rewind, for_rewind",
      "linux, Fortran, intel, read, for_read_seq_lis",
      "linux, Fortran, intel, read, for_read_seq_fmt",
      "linux, Fortran, intel, write, for_write_seq_lis",
      "linux, Fortran, intel, write, for_write_seq_fmt",
      "linux, Fortran, intel, write, for_write_seq_nml",
      ## does not support rand() except in module ifport
      "linux, Fortran, intel, rand, rand_",
      "linux, Fortran, intel, random_number, for_random_number",
      "linux, Fortran, intel, random_number, for_random_number_single",
      "linux, Fortran, intel, random_seed, for_random_seed_bit_size",
      "linux, Fortran, intel, random_seed, for_random_seed_get",

      ## Apple clang identifies itself as gcc, so configure has used that
      "macos, C, gcc, abort, _abort", # not currently seen
      "macos, C, gcc, assert, ___assert_rtn", # not currently seen
      "macos, C, gcc, exit, _exit",
      "macos, C, gcc, _exit, __exit",
      "macos, C, gcc, _Exit, __Exit",
      "macos, C, gcc, _Exit, __exit",
      "macos, C, gcc, printf, _printf",
      "macos, C, gcc, printf, _puts",
      "macos, C, gcc, puts, _puts",
      "macos, C, gcc, putchar, _putchar",
      "macos, C, gcc, stderr, ___stderrp",
      "macos, C, gcc, stdout, ___stdoutp",
      "macos, C, gcc, sprintf, _sprintf", # old
      "macos, C, gcc, sprintf, ___sprintf_chk",
      "macos, C, gcc, vprintf, _vprintf",
      "macos, C, gcc, vsprintf, _vsprintf", # old
      "macos, C, gcc, vsprintf, ___vsprintf_chk",
      "macos, C, gcc, rand, _rand",
      "macos, C, gcc, random, _random",
      "macos, C, gcc, rand_r, _rand_r",
      "macos, C, gcc, srand, _srand",
      "macos, C, gcc, srandom, _srandom",
      "macos, C, gcc, srand48, _srand48",
      #"macos, C++, gxx, std::cout, __ZSt4cout", # not with clang
      #"macos, C++, gxx, std::cerr, __ZSt4cerr",
      "macos, C++, gxx, std::cout, __ZNSt3__14coutE", # std::__1::cout
      "macos, C++, gxx, std::cerr, __ZNSt3__14cerrE",
      #"macos, C++, gxx, std::terminate, __ZSt9terminatev",
      "macos, Fortran, gfortran, open, __gfortran_st_open",
      "macos, Fortran, gfortran, close, __gfortran_st_close",
      "macos, Fortran, gfortran, rewind, __gfortran_st_rewind",
      "macos, Fortran, gfortran, read, __gfortran_st_read",
      "macos, Fortran, gfortran, write, __gfortran_st_write",
      "macos, Fortran, gfortran, print, __gfortran_st_write",
      "macos, Fortran, gfortran, stop, __gfortran_stop_numeric",
      "macos, Fortran, gfortran, stop, __gfortran_stop_string",
      "macos, Fortran, gfortran, rand, __gfortran_rand",
      "macos, Fortran, gfortran, random_init, __gfortran_random_init",
      "macos, Fortran, gfortran, random_number, __gfortran_arandom_r4",
      "macos, Fortran, gfortran, random_number, __gfortran_arandom_r8",
      "macos, Fortran, gfortran, random_number, __gfortran_arandom_r16",
      "macos, Fortran, gfortran, random_number, __gfortran_random_r4",
      "macos, Fortran, gfortran, random_number, __gfortran_random_r8",
      "macos, Fortran, gfortran, random_number, __gfortran_random_r16",
      "macos, Fortran, gfortran, random_number, __gfortran_rand",
      "macos, Fortran, gfortran, random_seed, __gfortran_random_seed_i4",
      "macos, Fortran, gfortran, random_seed, __gfortran_random_seed_i8",
      "macos, Fortran, gfortran, exit, __gfortran_exit_i4",
      "macos, Fortran, gfortran, exit, __gfortran_exit_i8",

      ## This is old: freebsd defaults to clang these days, and
      ## gfortran and (classic) flang are available (and 'f18' will be)
      "freebsd, C, gcc, abort, abort",
      "freebsd, C, gcc, assert, __assert",
      "freebsd, C, gcc, exit, exit",
      "freebsd, C, gcc, _exit, _exit",
      "freebsd, C, gcc, _Exit, _Exit",
      "freebsd, C, gcc, printf, printf",
      "freebsd, C, gcc, printf, puts",
      "freebsd, C, gcc, puts, puts",
      "freebsd, C, gcc, putchar, putchar",
      "freebsd, C, gcc, stderr, __stderrp",
      "freebsd, C, gcc, stdout, __stdoutp",
      "freebsd, C, gcc, sprintf, sprintf",
      "freebsd, C, gcc, vprintf, vprintf",
      "freebsd, C, gcc, vsprintf, vsprintf",
      "freebsd, C++, gxx, std::cout, _ZSt4cout",
      "freebsd, C++, gxx, std::cerr, _ZSt4cerr",
      ## libc++ variants
      "freebsd, C++, gxx, std::cout, _ZNSt3__14coutE",
      "freebsd, C++, gxx, std::cerr, _ZNSt3__14cerrE",
      "freebsd, C, gcc, rand, rand",
      "freebsd, C, gcc, random, random",
      "freebsd, C, gcc, srand, srand",
      "freebsd, C, gcc, srandom, srandom",
      "freebsd, C, gcc, srand48, srand48",
      "freebsd, Fortran, gfortran, open, _gfortran_st_open",
      "freebsd, Fortran, gfortran, close, _gfortran_st_close",
      "freebsd, Fortran, gfortran, rewind, _gfortran_st_rewind",
      "freebsd, Fortran, gfortran, read, _gfortran_st_read",
      "freebsd, Fortran, gfortran, write, _gfortran_st_write",
      "freebsd, Fortran, gfortran, print, _gfortran_st_write",
      "freebsd, Fortran, gfortran, stop, _gfortran_stop_numeric_f08",
      "freebsd, Fortran, gfortran, stop, _gfortran_stop_string",
      "freebsd, Fortran, gfortran, rand, _gfortran_rand",

      ## stdout, stderr do not show up on Solaris
      "solaris, C, solcc, abort, abort",
      "solaris, C, solcc, assert, __assert_c99",
      "solaris, C, solcc, exit, exit",
      "solaris, C, solcc, _exit, _exit",
      "solaris, C, solcc, _Exit, _Exit",
      "solaris, C, solcc, printf, printf",
      "solaris, C, solcc, putchar, putchar",
      "solaris, C, solcc, puts, puts",
      "solaris, C, solcc, sprintf, sprintf",
      "solaris, C, solcc, vprintf, vprintf",
      "solaris, C, solcc, vsprintf, vsprintf",
      "solaris, C++, solCC, std::cout, __1cDstdEcout_",
      "solaris, C++, solCC, std::cerr, __1cDstdEcerr_",
      #"solaris, C++, solCC, std::terminate, _ZSt9terminatev",
      "solaris, C, solcc, random, random",
      "solaris, C, solcc, rand, rand",
      "solaris, C, solcc, rand_r, rand_r",
      "solaris, C, solcc, srand, srand",
      "solaris, C, solcc, srandom, srandom",
      "solaris, C, solcc, srand48, srand48",
      "solaris, Fortran, solf95, open, __f90_open",
      "solaris, Fortran, solf95, close, __f90_close",
      "solaris, Fortran, solf95, rewind, __f90_rewind",
      "solaris, Fortran, solf95, read, __f90_eifr",
      "solaris, Fortran, solf95, read, __f90_esfr",
      "solaris, Fortran, solf95, print, __f90_eslw",
      "solaris, Fortran, solf95, write, __f90_eslw",
      "solaris, Fortran, solf95, print, __f90_esfw",
      "solaris, Fortran, solf95, write, __f90_esfw",
      "solaris, Fortran, solf95, write, __f90_esuw",
      "solaris, Fortran, solf95, stop, __f90_stop",
      "solaris, Fortran, solf95, stop, __f90_stop_int",
      "solaris, Fortran, solf95, stop, __f90_stop_char",
      "solaris, Fortran, solf95, runtime, abort",
      "solaris, Fortran, solf95, rand, rand_",

      "solaris, C, gcc, abort, abort",
      "solaris, C, gcc, assert, __assert_c99",
      "solaris, C, gcc, exit, exit",
      "solaris, C, gcc, _exit, _exit",
      "solaris, C, gcc, _Exit, _Exit",
      "solaris, C, gcc, printf, printf",
      "solaris, C, gcc, printf, puts",
      "solaris, C, gcc, puts, puts",
      "solaris, C, gcc, putchar, putchar",
      "solaris, C, gcc, sprintf, sprintf",
      "solaris, C, gcc, vprintf, vprintf",
      "solaris, C, gcc, vsprintf, vsprintf",
      "solaris, C, gcc, rand, rand",
      "solaris, C, gcc, random, random",
      "solaris, C, gcc, rand_r, rand_r",
      "solaris, C, gcc, srand, srand",
      "solaris, C, gcc, srandom, srandom",
      "solaris, C, gcc, srand48, srand48",
      "solaris, C++, gxx, std::cout, _ZSt4cout",
      "solaris, C++, gxx, std::cerr, _ZSt4cerr",
      "solaris, C++, gxx, std::cerr, _ZSt4cerr",
      #"solaris, C++, gxx, std::terminate, _ZSt9terminatev",
      "solaris, Fortran, gfortran, open, _gfortran_st_open",
      "solaris, Fortran, gfortran, close, _gfortran_st_close",
      "solaris, Fortran, gfortran, rewind, _gfortran_st_rewind",
      "solaris, Fortran, gfortran, read, _gfortran_st_read",
      "solaris, Fortran, gfortran, write, _gfortran_st_write",
      "solaris, Fortran, gfortran, print, _gfortran_st_write",
      "solaris, Fortran, gfortran, stop, _gfortran_stop_numeric_f08",
      "solaris, Fortran, gfortran, stop, _gfortran_stop_string",
      "solaris, Fortran, gfortran, rand, _gfortran_rand",

      ## Windows statically links libstdc++, libgfortran
      ## only in .o, positions hard-coded in check_so_symbols
      "windows, C++, g++, std::cout, _ZSt4cout",
      "windows, C++, g++, std::cerr, _ZSt4cerr",
      #"windows, C++, gxx, std::terminate, _ZSt9terminatev",
      "windows, Fortran, gfortran, open, _gfortran_st_open",
      "windows, Fortran, gfortran, close, _gfortran_st_close",
      "windows, Fortran, gfortran, rewind, _gfortran_st_rewind",
      "windows, Fortran, gfortran, write, _gfortran_st_write",
      "windows, Fortran, gfortran, print, _gfortran_st_write",
      ## in DLL
      "windows, C, gcc, abort, abort",
      "windows, C++, gxx, runtime, abort",
      "windows, Fortran, gfortran, runtime, abort",
      "windows, C, gcc, assert, _assert",
      "windows, C, gcc, exit, exit",
      "windows, C, gcc, _exit, _exit",
      "windows, C, gcc, _Exit, _Exit",
      "windows, C, gcc, printf, printf",
      "windows, C, gcc, printf, puts",
      "windows, C, gcc, puts, puts",
      "windows, C, gcc, putchar, putchar",
      "windows, C, gcc, sprintf, sprintf",
      "windows, C, gcc, vprintf, vprintf",
      "windows, C, gcc, vsprintf, vsprintf",
      ## Windows does not have (s)random
      "windows, C, gcc, rand, rand",
      "windows, C, gcc, rand_r, rand_r",
      "windows, C, gcc, srand, srand",
      "windows, C, gcc, srand48, srand48",
      "windows, Fortran, gfortran, stop, exit",
      ## next will not show up with static libgfortran
      "windows, Fortran, gfortran, rand, _gfortran_rand",
      "windows, Fortran, gfortran, random_init, _gfortran_random_init",
      "windows, Fortran, gfortran, random_number, _gfortran_arandom_r4",
      "windows, Fortran, gfortran, random_number, _gfortran_arandom_r8",
      "windows, Fortran, gfortran, random_number, _gfortran_arandom_r16",
      "windows, Fortran, gfortran, random_number, _gfortran_random_r4",
      "windows, Fortran, gfortran, random_number, _gfortran_random_r8",
      "windows, Fortran, gfortran, random_number, _gfortran_random_r16",
      "windows, Fortran, gfortran, random_seed, _gfortran_random_seed_i4",
      "windows, Fortran, gfortran, random_seed, _gfortran_random_seed_i8",
      "windows, Fortran, gfortran, exit, _gfortran_exit_i4",
      "windows, Fortran, gfortran, exit, _gfortran_exit_i8",

      ## currently copy from Linux
      ## flang-new but executable already named 'flang'
      "windows, Fortran, flang, stop, _FortranAStopStatement",
      "windows, Fortran, flang, stop, _FortranAStopStatementText",
      "windows, Fortran, flang, open, _FortranAioBeginOpenUnit",
      "windows, Fortran, flang, close, _FortranAioBeginClose",
      "windows, Fortran, flang, rewind, _FortranAioBeginRewind",
      "windows, Fortran, flang, read, _FortranAioInputAscii",
      "windows, Fortran, flang, read, _FortranAioInputCharacter",
      "windows, Fortran, flang, read, _FortranAioInputComplex32",
      "windows, Fortran, flang, read, _FortranAioInputComplex64",
      "windows, Fortran, flang, read, _FortranAioOutputExternalListInput",
      "windows, Fortran, flang, read, _FortranAioInputInteger",
      "windows, Fortran, flang, read, _FortranAioInputLogical",
      "windows, Fortran, flang, read, _FortranAioInputNamelist",
      "windows, Fortran, flang, read, _FortranAioInputReal32",
      "windows, Fortran, flang, read, _FortranAioInputReal64",
      "windows, Fortran, flang, read, _FortranAioInputUnformattedBlock",
      "windows, Fortran, flang, print, _FortranAioOutputAscii",
      "windows, Fortran, flang, print, _FortranAioOutputCharacter",
      "windows, Fortran, flang, print, _FortranAioOutputComplex32",
      "windows, Fortran, flang, print, _FortranAioOutputComplex64",
      "windows, Fortran, flang, print, _FortranAioOutputExternalListOutput",
      "windows, Fortran, flang, print, _FortranAioOutputInteger128",
      "windows, Fortran, flang, print, _FortranAioOutputInteger16",
      "windows, Fortran, flang, print, _FortranAioOutputInteger32",
      "windows, Fortran, flang, print, _FortranAioOutputInteger64",
      "windows, Fortran, flang, print, _FortranAioOutputInteger8",
      "windows, Fortran, flang, print, _FortranAioOutputLogical",
      "windows, Fortran, flang, print, _FortranAioOutputNamelist",
      "windows, Fortran, flang, print, _FortranAioOutputReal32",
      "windows, Fortran, flang, print, _FortranAioOutputReal64",
      "windows, Fortran, flang, write, _FortranAioOutputAscii",
      "windows, Fortran, flang, write, _FortranAioOutputCharacter",
      "windows, Fortran, flang, write, _FortranAioOutputComplex32",
      "windows, Fortran, flang, write, _FortranAioOutputComplex64",
      "windows, Fortran, flang, write, _FortranAioOutputExternalListOutput",
      "windows, Fortran, flang, write, _FortranAioOutputInteger128",
      "windows, Fortran, flang, write, _FortranAioOutputInteger16",
      "windows, Fortran, flang, write, _FortranAioOutputInteger32",
      "windows, Fortran, flang, write, _FortranAioOutputInteger64",
      "windows, Fortran, flang, write, _FortranAioOutputInteger8",
      "windows, Fortran, flang, write, _FortranAioOutputLogical",
      "windows, Fortran, flang, write, _FortranAioOutputNamelist",
      "windows, Fortran, flang, write, _FortranAioOutputReal32",
      "windows, Fortran, flang, write, _FortranAioOutputReal64",
      "windows, Fortran, flang, write, _FortranAioOutputUnformatedBlock",
      ## Next is a guess.
      "windows, Fortran, flang, rand, rand_",
      "windows, Fortran, flang, random_init, _FortranARandomInit",
      "windows, Fortran, flang, random_number, _FortranARandomNumber",
      "windows, Fortran, flang, random_seed, _FortranARandomSeed",
      "windows, Fortran, flang, random_seed, _FortranARandomSeedGet",
      "windows, Fortran, flang, random_seed, _FortranARandomSeedSize",

      "windows, C++, clang++, std::cout, _ZNSt3__14coutE",
      "windows, C++, clang++, std::cerr, _ZNSt3__14cerrE",
      "windows, C++, clang++, std::terminate, _ZSt9terminatev",
      "windows, C, clang, exit, exit",
      "windows, C, clang, printf, printf",
      "windows, C, clang, printf, puts",
      "windows, C, clang, puts, puts",
      "windows, C, clang, putchar, putchar",
      "windows, C, clang, sprintf, sprintf",
      "windows, C, clang, vprintf, vprintf",
      "windows, C, clang, vsprintf, vsprintf",
      "windows, C, clang, rand, rand",
      "windows, C, clang, srand, srand"
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

## Obsolete ones first,
nonAPI <- c("chol_", "chol2inv_", "cg_", "ch_", "rg_",
            "fft_factor", "fft_work", "Brent_fmin", "optif0",

## then entry points which are not attribute-hidden
## and in a non-API header or no header at all or marked as non-API in a header

            "OutDec", "PRIMOFFSET", "RC_fopen", "R_CollectFromIndex",
            "R_CompiledFileName", "R_FileExists",
            "R_FreeStringBuffer", "R_FunTab", "R_GE_setVFontRoutines",
            "R_GetVarLocMISSING",
            "R_MethodsNamespace", "R_NewHashedEnv",
            "R_OpenCompiledFile", "R_PV", "R_ParseContext",
            "R_ParseContextLast", "R_ParseContextLine",
            "R_ParseError", "R_ParseErrorMsg", "R_SrcfileSymbol",
            "R_SrcrefSymbol", "R_Visible", "R_addTaskCallback",
            "R_cairoCdynload", "R_data_class",
            "R_deferred_default_method", "R_execMethod",
            "R_findVarLocInFrame","R_fopen", "R_gc_torture",
            "R_getTaskCallbackNames", "R_get_arith_function",
            "R_gzclose", "R_gzgets", "R_gzopen", "R_ignore_SIGPIPE",
            "R_isForkedChild", "R_isMethodsDispatchOn",
            "R_moduleCdynload", "R_primitive_generic",
            "R_primitive_methods", "R_print", "R_removeTaskCallback",
            "R_running_as_main_program", "R_setInternetRoutines",
            "R_setLapackRoutines", "R_setX11Routines",
            "R_set_prim_method", "R_set_quick_method_check",
            "R_set_standardGeneric_ptr", "R_strtod4",
            "R_subassign3_dflt", "R_taskCallbackRoutine",
            "Rconn_fgetc", "Rconn_printf", "Rdownload",
            "Rf_EncodeComplex", "Rf_EncodeElement",
            "Rf_EncodeEnvironment", "Rf_EncodeInteger",
            "Rf_EncodeLogical", "Rf_EncodeReal", "Rf_GPretty",
            "Rf_NewEnvironment", "Rf_PrintDefaults",
            "Rf_ReplIteration", "Rf_Seql", "Rf_addTaskCallback",
            "Rf_begincontext", "Rf_callToplevelHandlers",
            "Rf_checkArityCall", "Rf_con_pushback",
            "Rf_copyMostAttribNoTs", "Rf_deparse1", "Rf_deparse1line",
            "Rf_dpptr", "Rf_endcontext", "Rf_envlength",
            "Rf_formatComplex", "Rf_formatInteger",
            "Rf_formatLogical", "Rf_formatReal", "Rf_init_con",
            "Rf_isProtected", "Rf_mbrtowc", "Rf_mkFalse",
            "Rf_printNamedVector", "Rf_printRealVector",
            "Rf_printVector", "Rf_removeTaskCallbackByIndex",
            "Rf_removeTaskCallbackByName", "Rf_set_iconv",
            "Rf_sortVector", "Rf_strIsASCII", "Rf_strchr",
            "Rf_strrchr", "Rf_ucstomb", "Rf_utf8towcs",
            "Rf_wcstoutf8", "Rg_PolledEvents", "Rg_set_col_ptrs",
            "Rf_wait_usec", "Ri18n_iswctype", "Ri18n_wcswidth",
            "Ri18n_wctype", "Ri18n_wcwidth", "Rsockclose",
            "Rsockconnect", "Rsocklisten", "Rsockopen", "Rsockread",
            "Rsockwrite", "Runzip", "UNIMPLEMENTED_TYPE",
            "baseRegisterIndex", "Rf_csduplicated", "Rf_currentTime",
            "dcar", "dcdr", "dchdc_", "do_Rprof", "do_Rprofmem", "do_X11",
            "do_contourLines", "do_edit", "do_getGraphicsEventEnv",
            "do_getSnapshot", "do_playSnapshot", "do_saveplot",
            "do_set_prim_method", "dqrrsd_","dqrxb_", "dtype",
            "dummy_fgetc", "dummy_ii", "dummy_vfprintf", "epslon_",
            "extR_HTTPDCreate", "extR_HTTPDStop", "fdhess",
            "getConnection", "getPRIMNAME", "known_to_be_latin1",
            "locale2charset", "match5", "matherr",
            "max_contour_segments", "Rf_mbcsToUcs2", "Rf_memtrace_report",
            "parseError", "pythag_", "rs_", "rwarnc_",
            "tql2_", "tqlrat_", "tred1_", "tred2_", "utf8locale", "yylloc",
            "R_opendir", "R_readdir", "R_closedir",
            # "signrank_free", "wilcox_free" are API only from 4.2.0
            "ENSURE_NAMEDMAX", "IS_ASCII", "IS_UTF8", "SET_PRSEEN",
            "ddfind",

## Rinterface.h, Rembedded.h, R_ext/{RStartup,eventloop}.h
            "AllDevicesKilled", "R_CStackLimit", "R_CStackStart",
            "R_ClearerrConsole", "R_CleanTempDir", "R_Consolefile",
            "R_DefCallbacks", "R_DefParams", "R_DefParamsEx",
            "R_DirtyImage", "R_GUIType", "R_GlobalContext",
            "R_HistoryFile", "R_HistorySize", "R_Home", "R_HomeDir",
            "R_Interactive", "R_Outputfile",
            "R_PolledEvents", "R_ReplDLLdo1", "R_ReplDLLinit",
            "R_RestoreGlobalEnv", "R_RestoreGlobalEnvFromFile",
            "R_RestoreHistory", "R_RunExitFinalizers", "R_SaveGlobalEnv",
            "R_SaveGlobalEnvToFile", "R_SelectEx", "R_SetParams",
            "R_SetWin32", "R_SignalHandlers", "R_SizeFromEnv", "R_NoEcho",
            "R_Suicide", "R_TempDir", "R_checkActivity",
            "R_checkActivityEx", "R_runHandlers",
            "R_setStartTime", "R_set_command_line_arguments",
            "R_setupHistory", "R_timeout_handler", "R_timeout_val",
            "R_wait_usec", "RestoreAction", "Rf_CleanEd",
            "Rf_KillAllDevices", "Rf_endEmbeddedR", "Rf_initEmbeddedR",
            "Rf_initialize_R", "Rf_jump_to_toplevel", "Rf_mainloop",
            "SaveAction", "editorcleanall", "fpu_setup",
            "freeRUser", "free_R_HOME",
            "getDLLVersion", "getRUser", "get_R_HOME",
            "getSelectedHandler", "initStdinHandler",
            "process_site_Renviron", "process_system_Renviron",
            "process_user_Renviron", "ptr_R_Busy", "ptr_R_ChooseFile",
            "ptr_R_CleanUp", "ptr_R_ClearerrConsole", "ptr_R_EditFile",
            "ptr_R_EditFiles", "ptr_R_FlushConsole", "ptr_R_ProcessEvents",
            "ptr_R_ReadConsole", "ptr_R_ResetConsole", "ptr_R_ShowFiles",
            "ptr_R_ShowMessage", "ptr_R_Suicide", "ptr_R_WriteConsole",
            "ptr_R_WriteConsoleEx", "ptr_R_addhistory", "ptr_R_loadhistory",
            "ptr_R_savehistory", "ptr_do_dataentry", "ptr_do_dataviewer",
            "ptr_do_selectlist", "readconsolecfg",
            "run_Rmainloop", "setup_Rmainloop",

## non-API, removed in R 4.5.0 and long deprecated in R_ext/RS.h (and as call_S in S.h)
            "call_R",
## non-API, declared in Defn.h
            "Rf_setSVector",
## non-API, declared in Rinternals.h
            ## not yet, in Rcpp headers "SET_TYPEOF",
            ## not yet, used in an example in R-exts "SET_OBJECT",
            "SET_S4_OBJECT", "UNSET_S4_OBJECT",
            "R_curErrorBuf",
            "SETLENGTH", "SET_TRUELENGTH", "SETLEVELS",
            "SET_ENVFLAGS", "SET_FRAME", "SET_ENCLOS", "SET_HASHTAB",
            "SET_PRENV", "SET_PRVALUE", "SET_PRCODE", "STDVEC_DATAPTR",
            "IS_GROWABLE", "SET_GROWABLE_BIT", "SET_NAMED",
            "R_PromiseExpr",
            "R_tryWrap",
            "DDVAL", "NAMED", "INTERNAL", "SYMVALUE", "PRSEEN",
            "REAL0", "COMPLEX0", "LEVELS", "FRAME", "HASHTAB",
            "ENVFLAGS", "RDEBUG", "SET_RDEBUG",
            "STRING_PTR", "VECTOR_PTR",
            "SET_FORMALS", "SET_BODY", "SET_CLOENV", "Rf_findVarInFrame3",
            "PRCODE", "PRENV", "PRVALUE", "R_nchar",
            "Rf_NonNullStringMatch",
            "SET_TYPEOF", "TRUELENGTH", "XLENGTH_EX",
            "XTRUELENGTH", "Rf_gsetVar",
            "Rf_isValidString", "Rf_isValidStringF",
            "R_shallow_duplicate_attr",
            ## Documented in WRE in section "Some API replacements for
            ## non-API entry points":
            "EXTPTR_PROT", "EXTPTR_TAG", "EXTPTR_PTR",
            "OBJECT", "IS_S4_OBJECT",
            "Rf_GetOption", "R_lsInternal",
            "REAL0", "COMPLEX0",
            "STRING_PTR", "DATAPTR", "STDVEC_DATAPTR",
            "Rf_allocSExp",
            "Rf_isFrame",
            "BODY", "FORMALS", "CLOENV", "ENCLOS",
            "IS_ASCII", "IS_UTF8",
## in the non-API header R_ext/Connections.h
            "R_new_custom_connection", "R_ReadConnection",
            "R_WriteConnection", "R_GetConnection",

## in ../../../include/R_ext/Applic.h -- these are API now:
## 	"dqrcf_", "dqrqty_", "dqrqy_", "dqrrsd_", "dqrxb_",
##	"dqrdc2_", "dqrls_",
## "d1mach_" and "i1mach_" are API now in R-exts.
            "R_Pretty") ## hidden, so unlikely to be usable
##          "optif9")   ## used by pcaPP

## grDevices uses R_Home R_InputHandlers R_TempDir R_Visible R_cairoCdynload R_fopen R_gzclose R_gzgets R_gzopen R_isForkedChild Rf_envlength Rf_strIsASCII Rf_utf8towcs Rg_set_col_ptrs Ri18n_wcwidth addInputHandler do_X11 do_contourLines do_getGraphicsEventEnv do_getSnapshot do_playSnapshot do_saveplot locale2charset mbcsToUcs2 ptr_R_ProcessEvents

## graphics uses OutDec R_print Rf_EncodeComplex Rf_EncodeInteger Rf_EncodeLogical Rf_EncodeReal Rf_GPretty Rf_PrintDefaults Rf_envlength Rf_formatComplex Rf_formatReal baseRegisterIndex known_to_be_latin1 max_contour_segments

## methods uses R_GetVarLocMISSING R_MakeExternalPtrFn R_MethodsNamespace R_data_class R_deferred_default_method R_execMethod R_findVarLocInFrame R_primitive_generic R_primitive_methods R_set_prim_method R_set_quick_method_check R_set_standardGeneric_ptr R_subassign3_dflt Rf_NewEnvironment Rf_envlength do_set_prim_method getPRIMNAME

## parallel uses R_isForkedChild

## stats uses Rf_PrintDefaults Rf_Seql Rf_copyMostAttribNoTs Rf_deparse1 Rf_deparse1line Rf_envlength Rf_mkFalse fdhess memtrace_report signrank_free wilcox_free

## tcltk uses R_Consolefile R_GUIType R_InputHandlers R_Outputfile R_PolledEvents R_checkActivity R_runHandlers R_timeout_handler R_timeout_val R_wait_usec ptr_R_ClearerrConsole ptr_R_FlushConsole ptr_R_ReadConsole ptr_R_ResetConsole ptr_R_WriteConsole

## tools uses RC_fopen R_FileExists R_NewHashedEnv R_ParseContext R_ParseContextLast R_ParseContextLine R_ParseError R_ParseErrorMsg R_SrcfileSymbol R_SrcrefSymbol Rconn_fgetc Rf_begincontext Rf_endcontext Rf_envlength Rf_mbrtowc Rf_strchr extR_HTTPDCreate extR_HTTPDStop getConnection parseError R_opendir R_readdir R_closedir

## utils uses R_ClearerrConsole R_FreeStringBuffer R_GUIType R_moduleCdynload R_print R_strtod4 Rconn_fgetc Rconn_printf Rdownload Rf_EncodeElement Rf_PrintDefaults Rf_begincontext Rf_con_pushback Rf_endcontext Rf_envlength Rf_sortVector Rsockclose Rsockconnect Rsocklisten Rsockopen Rsockread Rsockwrite Runzip UNIMPLEMENTED_TYPE csduplicated do_Rprof do_Rprofmem do_edit getConnection known_to_be_latin1 ptr_R_addhistory ptr_R_loadhistory ptr_R_savehistory ptr_do_dataentry ptr_do_dataviewer ptr_do_selectlist

## modules use PRIMOFFSET R_GE_setVFontRoutines R_setInternetRoutines R_setLapackRoutines R_setX11Routines Rf_set_iconv currentTime dummy_fgetc dummy_vfprintf ucstomb utf8locale


check_so_symbols <- if(.Platform$OS.type == "windows") {
    function(so, rarch, have_tables = FALSE)
    {
        if(!length(system_ABI)) return()
        nms <- read_symbols_from_dll(so, rarch)
        ind <- so_symbol_names_table[, "osname"] %in% nms
        if(have_tables) ind[1:4] <- TRUE
        tab <- so_symbol_names_table[ind, , drop = FALSE]
        attr(tab, "file") <- so
        tab2 <- intersect(sub("^_", "", nms), nonAPI)
        if ("removeInputHandler" %in% tab2)
            tab2 <- setdiff(tab2, c("R_InputHandlers", "addInputHandler",
                                    "removeInputHandler"))
        if(length(tab2)) attr(tab, "nonAPI") <- tab2
        tab2b <- setdiff(c("R_registerRoutines", "R_useDynamicSymbols"),
                         sub("^_", "", nms))
        if(length(tab2b)) attr(tab, "RegSym") <- tab2b
        class(tab) <- "check_so_symbols"
        tab
    }
} else {
    function(so)
    {
        if(!length(system_ABI)) return()
        tab <- read_symbols_from_object_file(so)
        tab2 <- tab[tab[, "type"] == "U", "name"]
	nms <- tab[, "name"]
        sys <- system_ABI["system"]
        if(!is.null(snh <- so_symbol_names_handlers_db[[sys]]))
            nms <- snh(nms)
        ind <- so_symbol_names_table[, "osname"] %in% nms
        tab <- so_symbol_names_table[ind, , drop = FALSE]
        attr(tab, "file") <- so
        tab2 <- sub("^_", "", tab2)

        tab2a <- intersect(tab2, nonAPI)
        if ("removeInputHandler" %in% tab2a)
            tab2a <- setdiff(tab2a, c("R_InputHandlers", "addInputHandler",
                                    "removeInputHandler"))
        if(length(tab2a)) attr(tab, "nonAPI") <- tab2a

        tab2b <- setdiff(c("R_registerRoutines", "R_useDynamicSymbols"), tab2)
        if(length(tab2b)) attr(tab, "RegSym") <- tab2b

        class(tab) <- "check_so_symbols"
        tab
    }
}

format.check_so_symbols <-
function(x, ...)
{
    if(!length(x)) return(character())
    ## <FIXME split.matrix>
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

check_compiled_code <-
if(.Platform$OS.type == "windows") {
    function(dir)
    {
        ## Check compiled code in the DLL(s) of an installed package.

        r_arch <- .Platform$r_arch
        useST <- config_val_to_logical(Sys.getenv("_R_SHLIB_BUILD_OBJECTS_SYMBOL_TABLES_", "FALSE"))
        useSR <- config_val_to_logical(Sys.getenv("_R_CHECK_NATIVE_ROUTINE_REGISTRATION_", "FALSE"))

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
            attr(x, "file") <- .file_path_relative_to_dir(so, dir, TRUE)

            attr(x, "objects") <-
                split(rep.int(names(symbols), lengths(symbols)),
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
        nAPIs <- lapply(lapply(so_files, check_so_symbols, rarch = "i386"),
                        function(x) if(length(z <- attr(x, "nonAPI")))
                        structure(z,
                                  file =
                                  .file_path_relative_to_dir(attr(x, "file"),
                                                             dir, TRUE),
                                  class = "check_nonAPI_calls"))
        bad <- c(bad, Filter(length, nAPIs))

        if (useSR) {
            nRS <- lapply(lapply(so_files, check_so_symbols, rarch = "i386"),
                          function(x) if(length(z <- attr(x, "RegSym")))
                          structure(z,
                                    file =
                                    .file_path_relative_to_dir(attr(x, "file"),
                                                               dir, TRUE),
                                    class = "check_RegSym_calls"))
            bad <- c(bad, Filter(length, nRS))
        }

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
        nAPIs <- lapply(lapply(so_files, check_so_symbols, rarch = "x64"),
                        function(x) if(length(z <- attr(x, "nonAPI")))
                        structure(z,
                                  file =
                                  .file_path_relative_to_dir(attr(x, "file"),
                                                             dir, TRUE),
                                  class = "check_nonAPI_calls"))
        bad2 <- c(bad2, Filter(length, nAPIs))

        if (useSR) {
            nRS <- lapply(lapply(so_files, check_so_symbols, rarch = "x64"),
                          function(x) if(length(z <- attr(x, "RegSym")))
                          structure(z,
                                    file =
                                    .file_path_relative_to_dir(attr(x, "file"),
                                                               dir, TRUE),
                                    class = "check_RegSym_calls"))
            bad2 <- c(bad2, Filter(length, nRS))
        }

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
        useSR <- config_val_to_logical(Sys.getenv("_R_CHECK_NATIVE_ROUTINE_REGISTRATION_", "FALSE"))

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
            attr(x, "file") <- .file_path_relative_to_dir(so, dir, TRUE)
            attr(x, "objects") <-
                split(rep.int(names(symbols), lengths(symbols)),
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
        ## Allow experimenting with finding bad symbols not in
        ## symbols.rds, likely from following the "best approach" from
        ## section "Compiling in sub-directories" of WRE and compiling
        ## code in subdirs into static libraries instead of adding to
        ## OBJECTS.
        ## See PR#18789 <https://bugs.r-project.org/show_bug.cgi?id=18789>,
        ## "R CMD check does not check symbol tables of linked static
        ## libraries".
        if(config_val_to_logical(Sys.getenv("_R_CHECK_COMPILED_CODE_USE_OBJECTS_SYMBOL_TABLES_",
                                    "TRUE"))) {
        objects_symbol_tables_file <- if(nzchar(r_arch))
            file.path(dir, "libs", r_arch, "symbols.rds")
        else file.path(dir, "libs", "symbols.rds")
        if(file_test("-f", objects_symbol_tables_file)) {
            tables <- readRDS(objects_symbol_tables_file)
            bad <- Filter(length, lapply(bad, compare))
        } else if(useST)
            cat("Note: information on .o files is not available\n")
        }
        nAPIs <- lapply(lapply(so_files, check_so_symbols),
                        function(x) if(length(z <- attr(x, "nonAPI")))
                        structure(z,
                                  file =
                                  .file_path_relative_to_dir(attr(x, "file"),
                                                             dir, TRUE),
                                  class = "check_nonAPI_calls"))
        bad <- c(bad, Filter(length, nAPIs))

        if (useSR) {
            nRS <- lapply(lapply(so_files, check_so_symbols),
                          function(x) if(length(z <- attr(x, "RegSym")))
                          structure(z,
                                    file =
                                    .file_path_relative_to_dir(attr(x, "file"),
                                                               dir, TRUE),
                                    class = "check_RegSym_calls"))
            bad <- c(bad, Filter(length, nRS))
        }
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

format.check_nonAPI_calls <-
function(x, ...)
{
    if(length(x))
        c(gettextf("File %s:", sQuote(attr(x, "file"))),
          if (length(x) > 1L) {
              strwrap(paste("Found non-API calls to R:",
                            paste(sQuote(x), collapse = ", ")),
                      indent = 2L, exdent = 4L)
          } else paste("  Found non-API call to R:", sQuote(x))
          )
    else character()
}

format.check_RegSym_calls <-
function(x, ...)
{
    if(length(x))
        c(gettextf("File %s:", sQuote(attr(x, "file"))),
          if (length(x) > 1L) {
              strwrap(paste("Found no calls to:",
                            paste(sQuote(x), collapse = ", ")),
                      indent = 2L, exdent = 4L)
          } else paste("  Found no call to:", sQuote(x))
          )
    else character()
}

.shlib_objects_symbol_tables <-
function(file = "symbols.rds")
{
    args <- commandArgs(trailingOnly = TRUE)
    pos <- which(args == "--pkglibs")[1L]
    objects <- args[seq_len(pos - 1L)]
    pkglibs <- args[-seq_len(pos)]
    ## Also determine the local static libraries linked against by
    ## following the approach suggested in section "Compiling in
    ## sub-directories" of WRE.
    if(length(pkglibs)) {
        files <- list.files("..", recursive = TRUE, pattern = "[.]a$",
                            all.files = TRUE, full.names = TRUE)
        if(any(ind <- startsWith(files, "../src/")))
            files[ind] <- substring(files[ind], 8L)
        ## Case A: local static libs given via their path.
        libpaths <- pkglibs[file.exists(pkglibs)]
        ## Case B: local static libs given as '-lfoo'.
        libnames <- pkglibs[startsWith(pkglibs, "-l")]
        libnames <- sprintf("lib%s.a", substring(libnames, 3L))
        objects <- c(objects,
                     files[normalizePath(files) %in%
                           normalizePath(libpaths)],
                     files[basename(files) %in% libnames])
        objects <- unique(objects)
    }
    tables <- lapply(objects, read_symbols_from_object_file)
    names(tables) <- objects
    saveRDS(tables, file = file, version = 2)
}


### --- Helpers for registering native routines added in R 3.4.0 ---

package_ff_call_db <-
function(dir)
{
    ## A few packages such as CDM use base::.Call
    ff_call_names <- c(".C", ".Call", ".Fortran", ".External",
                       "base::.C", "base::.Call",
                       "base::.Fortran", "base::.External",
                       ## internal ones
                       ".Call.graphics", ".External.graphics",
                       ".External2")

    predicate <- function(e) {
        (length(e) > 1L) &&
            !is.na(match(deparse(e[[1L]])[1L], ff_call_names))
    }

    calls <- .find_calls_in_package_code(dir,
                                         predicate = predicate,
                                         recursive = TRUE)
    calls <- unlist(Filter(length, calls))

    if(!length(calls)) return(NULL)

    attr(calls, "dir") <- dir
    calls
}

native_routine_registration_db_from_ff_call_db <-
function(calls, dir = NULL, character_only = TRUE)
{
    if(!length(calls)) return(NULL)

    ff_call_names <-
        c(".C", ".Call", ".Fortran", ".External",
          ".Call.graphics", ".External.graphics", ".External2")
    ff_call_args <- lapply(ff_call_names,
                           function(e) args(get(e, baseenv())))
    names(ff_call_args) <- ff_call_names
    ff_call_args_names <-
        lapply(lapply(ff_call_args,
                      function(e) names(formals(e))), setdiff,
               "...")

    if(is.null(dir))
        dir <- attr(calls, "dir")

    package <- # drop name
        as.vector(.get_package_metadata(dir)["Package"])

    symbols <- character()
    nrdb <-
        lapply(calls,
               function(e) {
                   if (startsWith(deparse(e[[1L]]), "base::"))
                       e[[1L]] <- e[[1L]][3L]
                   ## First figure out whether ff calls had '...'.
                   pos <- which(unlist(Map(identical,
                                           lapply(e, as.character),
                                           "...")))
                   ## Then match the call with '...' dropped.
                   ## Note that only .NAME could be given by name or
                   ## positionally (the other ff interface named
                   ## arguments come after '...').
                   if(length(pos)) e <- e[-pos]
                   ## drop calls with only ...
                   if(length(e) < 2L) return(NULL)
                   cname <- as.character(e[[1L]])
                   ## The help says
                   ##
                   ## '.NAME' is always matched to the first argument
                   ## supplied (which should not be named).
                   ##
                   ## But some people do (Geneland ...).
                   nm <- names(e); nm[2L] <- ""; names(e) <- nm
                   e <- match.call(ff_call_args[[cname]], e)
                   ## Only keep ff calls where .NAME is character
                   ## or (optionally) a name.
                   s <- e[[".NAME"]]
                   t <- typeof(s)
                   if(is.name(s)) {
                       s <- deparse(s)[1L]
                       if(character_only) {
                           symbols <<- c(symbols, s)
                           return(NULL)
                       }
                   } else if(is.character(s)) {
                       s <- s[1L]
                   } else { ## expressions
                       symbols <<- c(symbols, deparse(s))
                       return(NULL)
                   }
                   ## Drop the ones where PACKAGE gives a different
                   ## package. Ignore those which are not char strings.
                   if(!is.null(p <- e[["PACKAGE"]]) &&
                      is.character(p) && !identical(p, package))
                       return(NULL)
                   n <- if(length(pos)) {
                            ## Cannot determine the number of args: use
                            ## -1 which might be ok for .External().
                            -1L
                        } else {
                            sum(is.na(match(names(e),
                                            ff_call_args_names[[cname]]))) - 1L
                        }
                   ## Could perhaps also record whether 's' was a symbol
                   ## or a character string ...
                   cbind(cname, s, n, t)
               })
    nrdb <- do.call(rbind, nrdb)
    nrdb <- as.data.frame(unique(nrdb), stringsAsFactors = FALSE)

    if(NROW(nrdb) == 0L || length(nrdb) != 4L) {
        message("no native symbols were extracted")
        return(NULL)
    }
    nrdb[, 3L] <- as.numeric(nrdb[, 3L])
    nrdb <- nrdb[order(nrdb[, 1L], nrdb[, 2L], nrdb[, 3L]), ]
    nms <- nrdb[, "s"]
    dups <- unique(nms[duplicated(nms)])

    ## Now get the namespace info for the package.
    info <- parseNamespaceFile(basename(dir), dirname(dir))
    ## Could have ff calls with symbols imported from other packages:
    ## try dropping these eventually.
    imports <- info$imports
    imports <- imports[lengths(imports) == 2L]
    imports <- unlist(lapply(imports, `[[`, 2L))

    info <- info$nativeRoutines[[package]]
    ## Adjust native routine names for explicit remapping or
    ## namespace .fixes.
    if(length(symnames <- info$symbolNames)) {
        ind <- match(nrdb[, 2L], names(symnames), nomatch = 0L)
        nrdb[ind > 0L, 2L] <- symnames[ind]
    } else if(!character_only &&
              any((fixes <- info$registrationFixes) != "")) {
        ## There are packages which have not used the fixes, e.g. utf8latex
        ## fixes[1L] is a prefix, fixes[2L] is an undocumented suffix
        nrdb[, 2L] <- sub(paste0("^", fixes[1L]), "", nrdb[, 2L])
        if(nzchar(fixes[2L]))
            nrdb[, 2L] <- sub(paste0(fixes[2L]), "$", "", nrdb[, 2L])
    }
    ## See above.
    if(any(ind <- !is.na(match(nrdb[, 2L], imports))))
        nrdb <- nrdb[!ind, , drop = FALSE]

    ## Fortran entry points are mapped to l/case
    dotF <- nrdb$cname == ".Fortran"
    nrdb[dotF, "s"] <- tolower(nrdb[dotF, "s"])

    attr(nrdb, "package") <- package
    attr(nrdb, "duplicates") <- dups
    attr(nrdb, "symbols") <- unique(symbols)
    nrdb
}

format_native_routine_registration_db_for_skeleton <-
function(nrdb, align = TRUE, include_declarations = FALSE)
{
    if(!length(nrdb))
        return(character())

    fmt1 <- function(x, n) {
        c(if(align) {
              paste(format(sprintf("    {\"%s\",", x[, 1L])),
                    format(sprintf(if(n == "Fortran")
                                       "(DL_FUNC) &F77_NAME(%s),"
                                   else
                                       "(DL_FUNC) &%s,",
                                   x[, 1L])),
                    format(sprintf("%d},", x[, 2L]),
                           justify = "right"))
          } else {
              sprintf(if(n == "Fortran")
                          "    {\"%s\", (DL_FUNC) &F77_NAME(%s), %d},"
                      else
                          "    {\"%s\", (DL_FUNC) &%s, %d},",
                      x[, 1L],
                      x[, 1L],
                      x[, 2L])
          },
          "    {NULL, NULL, 0}")
    }

    package <- attr(nrdb, "package")
    dups <- attr(nrdb, "duplicates")
    symbols <- attr(nrdb, "symbols")

    nrdb <- split(nrdb[, -1L, drop = FALSE],
                  factor(nrdb[, 1L],
                         levels =
                             c(".C", ".Call", ".Fortran", ".External")))

    has <- vapply(nrdb, NROW, 0L) > 0L
    nms <- names(nrdb)
    entries <- substring(nms, 2L)
    blocks <- Map(function(x, n) {
                      c(sprintf("static const R_%sMethodDef %sEntries[] = {",
                                n, n),
                        fmt1(x, n),
                        "};",
                        "")
                  },
                  nrdb[has],
                  entries[has])

    decls <- c(
        "/* FIXME: ",
        "   Add declarations for the native routines registered below.",
        "*/")

    if(include_declarations) {
        prepare <- function(nargs, type = "void *")
            if(nargs > 0) paste(rep.int(type, nargs), collapse=", ")
            else "void"
        decls <- c(
            "/* FIXME: ",
            "   Check these declarations against the C/Fortran source code.",
            "*/",
            if(NROW(y <- nrdb$.C)) {
                 args <- sapply(y$n, function(n) if(n >= 0) prepare(n)
                                else "/* FIXME */")
                c("", "/* .C calls */",
                  paste0("extern void ", y$s, "(", args, ");"))
           },
            if(NROW(y <- nrdb$.Call)) {
                args <- sapply(y$n, function(n) if(n >= 0) prepare(n, "SEXP")
                               else "/* FIXME */")
               c("", "/* .Call calls */",
                  paste0("extern SEXP ", y$s, "(", args, ");"))
            },
            if(NROW(y <- nrdb$.Fortran)) {
                 args <- sapply(y$n, function(n) if(n >= 0) prepare(n)
                                else "/* FIXME */")
                c("", "/* .Fortran calls */",
                  paste0("extern void F77_NAME(", y$s, ")(", args, ");"))
            },
            if(NROW(y <- nrdb$.External))
                c("", "/* .External calls */",
                  paste0("extern SEXP ", y$s, "(SEXP);"))
            )
    }

    headers <- if(NROW(nrdb$.Call) || NROW(nrdb$.External))
        c("#include <R.h>", "#include <Rinternals.h>")
    else if(NROW(nrdb$.Fortran)) "#include <R_ext/RS.h>"
    else character()

    c(headers,
      "#include <stdlib.h> // for NULL",
      "#include <R_ext/Rdynload.h>",
      "",
      if(length(symbols)) {
          c("/*",
            "  The following symbols/expressions for .NAME have been omitted",
            "", strwrap(symbols, indent = 4, exdent = 4), "",
            "  Most likely possible values need to be added below.",
            "*/", "")
      },
      if(length(dups)) {
          c("/*",
            "  The following name(s) appear with different usages",
            "  e.g., with different numbers of arguments:",
            "", strwrap(dups, indent = 4, exdent = 4), "",
            "  This needs to be resolved in the tables and any declarations.",
            "*/", "")
      },
      decls,
      "",
      unlist(blocks, use.names = FALSE),
      ## We cannot use names with '.' in: WRE mentions replacing with "_"
      sprintf("void R_init_%s(DllInfo *dll)",
              gsub(".", "_", package, fixed = TRUE)),
      "{",
      sprintf("    R_registerRoutines(dll, %s);",
              paste0(ifelse(has,
                            paste0(entries, "Entries"),
                            "NULL"),
                     collapse = ", ")),
      "    R_useDynamicSymbols(dll, FALSE);",
      "}")
}

package_native_routine_registration_db <-
function(dir, character_only = TRUE)
{
    calls <- package_ff_call_db(dir)
    native_routine_registration_db_from_ff_call_db(calls, dir, character_only)
}

package_native_routine_registration_skeleton <-
function(dir, con = stdout(), align = TRUE, character_only = TRUE,
         include_declarations = TRUE)
{
    nrdb <- package_native_routine_registration_db(dir, character_only)
    writeLines(format_native_routine_registration_db_for_skeleton(nrdb,
                align, include_declarations),
               con)
}

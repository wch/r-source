#  File src/library/tools/R/QC.R
#  Part of the R package, http://www.R-project.org
#
#  Copyright (C) 1995-2014 The R Core Team
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
#  http://www.r-project.org/Licenses/

## R CMD check uses
## .find_charset
## .check_namespace
## .check_package_depends
## .check_demo_index
## .check_vignette_index
## .check_package_subdirs
## .check_citation
## .check_package_ASCII_code
## .check_package_code_syntax
## .check_packages_used
## .checkS3methods
## .checkReplaceFuns
## .checkFF
## .check_package_code_shlib
## .check_package_code_startup_functions
## .check_package_code_assign_to_globalenv
## .check_package_code_attach
## .check_package_code_data_into_globalenv
## .check_code_usage_in_package
## .check_T_and_F
## .check_dotInternal
## .check_package_parseRd
## .check_Rd_xrefs
## undoc
## codoc
## codocData
## codocClass
## checkDocFiles
## checkDocStyle
## .check_package_datasets
## .check_package_compact_datasets
## .check_package_compact_sysdata
## .check_make_vars
## .createExdotR (testing.R)
## .runPackageTestsR (testing.R)
## .get_LaTeX_errors_from_log_file
## .check_package_CRAN_incoming
## .check_Rd_contents

## R CMD build uses .check_package_subdirs

## NB: 'tools' cannot use NAMESPACE imports from utils, as it exists first

##' a "default" print method used "below" (in several *.R):
.print.via.format <- function(x, ...) {
    writeLines(format(x, ...))
    invisible(x)
}

## utility for whether Rd sources are available.
.haveRds <- function(dir)
{
    ## either source package or pre-2.10.0 installed package
    if (file_test("-d", file.path(dir, "man"))) return(TRUE)
    file.exists((file.path(dir, "help", "paths.rds")))
}

### * undoc/F/out

undoc <-
function(package, dir, lib.loc = NULL)
{
    ## Argument handling.
    ## <NOTE>
    ## Earlier versions used to give an error if there were no Rd
    ## objects.  This is not right: if there is code or data but no
    ## documentation, everything is undocumented ...
    ## </NOTE>
    if(!missing(package)) {
        if(length(package) != 1L)
            stop("argument 'package' must be of length 1")
        dir <- find.package(package, lib.loc)
        ## Using package installed in @code{dir} ...
        is_base <- package == "base"

        all_doc_topics <- Rd_aliases(package, lib.loc = dirname(dir))

        ## Load package into code_env.
        if(!is_base)
            .load_package_quietly(package, lib.loc)
        code_env <- .package_env(package)

        code_objs <- ls(envir = code_env, all.names = TRUE)
        pkgname <- package
    }
    else {
        if(missing(dir))
            stop("you must specify 'package' or 'dir'")
        ## Using sources from directory @code{dir} ...
        if(!file_test("-d", dir))
            stop(gettextf("directory '%s' does not exist", dir),
                 domain = NA)
        else
            dir <- file_path_as_absolute(dir)
        pkgname <- basename(dir)
        is_base <- pkgname == "base"

        all_doc_topics <- Rd_aliases(dir = dir)

        code_env <- new.env(hash = TRUE)
        code_dir <- file.path(dir, "R")
        if(file_test("-d", code_dir)) {
            dfile <- file.path(dir, "DESCRIPTION")
            meta <- if(file_test("-f", dfile))
                .read_description(dfile)
            else
                character()
            .source_assignments_in_code_dir(code_dir, code_env, meta)
            sys_data_file <- file.path(code_dir, "sysdata.rda")
            if(file_test("-f", sys_data_file))
                load(sys_data_file, code_env)
        }

        code_objs <- ls(envir = code_env, all.names = TRUE)

        ## Does the package have a NAMESPACE file?  Note that when
        ## working on the sources we (currently?) cannot deal with the
        ## (experimental) alternative way of specifying the namespace.
        if(file.exists(file.path(dir, "NAMESPACE"))) {
            nsInfo <- parseNamespaceFile(basename(dir), dirname(dir))
            ## Look only at exported objects (and not declared S3
            ## methods).
            OK <- intersect(code_objs, nsInfo$exports)
            for(p in nsInfo$exportPatterns)
                OK <- c(OK, grep(p, code_objs, value = TRUE))
            code_objs <- unique(OK)
        }
    }

    ## Find the data sets to work on.
    data_dir <- file.path(dir, "data")
    data_objs <- if(file_test("-d", data_dir))
	unlist(.try_quietly(list_data_in_pkg(dataDir = data_dir)),
	       use.names = FALSE)
    else
        character()

    ## There was a time when packages contained code or data (or both).
    ## But not anymore ...
    if(!missing(package)
       && (!length(code_objs))
       && (!length(data_objs))
       && getOption("verbose"))
        message("neither code nor data objects found")

    if(!is_base) {
        ## Code objects in add-on packages with names starting with a
        ## dot are considered 'internal' (not user-level) by
        ## convention.
        code_objs <- grep("^[^.].*", code_objs, value = TRUE)
        ## Note that this also allows us to get rid of S4 meta objects
        ## (with names starting with '.__C__' or '.__M__'; well, as long
        ## as there are none in base).

        ## Implicit generic functions exist to turn method dispatch on
        ## in this package, but their definition and documentation belongs
        ## to the package in their package slot, so eliminate any
        ## foreign generic functions from code_objs
        if(.isMethodsDispatchOn()) {
            code_objs <-
                Filter(function(f) {
                    ## NB: this get() is expensive as it loads every object
                    fdef <- get(f, envir = code_env)
                    if(methods::is(fdef, "genericFunction"))
                        fdef@package == pkgname
                    else
                        TRUE
                },
                       code_objs)
        }

        ## Allow group generics to be undocumented other than in base.
        ## In particular, those from methods partially duplicate base
        ## and are documented in base's groupGenerics.Rd.
        code_objs <- setdiff(code_objs,
                             c("Arith", "Compare", "Complex", "Logic",
                               "Math", "Math2", "Ops", "Summary"))
    }

    undoc_things <-
        list("code objects" =
             unique(setdiff(code_objs, all_doc_topics)),
             "data sets" =
             unique(setdiff(data_objs, all_doc_topics)))

    if(.isMethodsDispatchOn()) {
        ## Undocumented S4 classes?
        S4_classes <- methods::getClasses(code_env)
        ## <NOTE>
        ## There is no point in worrying about exportClasses directives
        ## in a NAMESPACE file when working on a package source dir, as
        ## we only source the assignments, and hence do not get any
        ## S4 classes or methods.
        ## </NOTE>
        ## The bad ones:
        S4_classes <-
            S4_classes[!sapply(S4_classes,
                               function(u) utils:::topicName("class", u))
                       %in% all_doc_topics]
        undoc_things <-
            c(undoc_things, list("S4 classes" = unique(S4_classes)))
    }

    if(.isMethodsDispatchOn()) {
        ## Undocumented S4 methods?
        ## <NOTE>
        ## There is no point in worrying about exportMethods directives
        ## in a NAMESPACE file when working on a package source dir, as
        ## we only source the assignments, and hence do not get any
        ## S4 classes or methods.
        ## </NOTE>
        .make_S4_method_siglist <- function(g) {
            mlist <- .get_S4_methods_list(g, code_env)
            sigs <- .make_siglist(mlist) #  s/#/,/g
            if(length(sigs))
                paste0(g, ",", sigs)
            else
                character()
        }
        S4_methods <- lapply(.get_S4_generics(code_env),
                             .make_S4_method_siglist)
        S4_methods <- as.character(unlist(S4_methods, use.names = FALSE))

        ## The bad ones:
        S4_methods <-
            S4_methods[!sapply(S4_methods,
                               function(u)
                               utils:::topicName("method", u))
                       %in% all_doc_topics]
        undoc_things <-
            c(undoc_things,
              list("S4 methods" =
                   unique(sub("([^,]*),(.*)",
                              "generic '\\1' and siglist '\\2'",
                              S4_methods))))
    }
    if(is_base) {
        ## We use .ArgsEnv and .GenericArgsEnv in checkS3methods() and
        ## codoc(), so we check here that the set of primitives has not
        ## been changed.
        base_funs <- ls("package:base", all.names=TRUE)
        prim <- sapply(base_funs,
                       function(x) is.primitive(get(x, "package:base")))
        prims <- base_funs[prim]
        prototypes <- sort(c(ls(envir=.ArgsEnv, all.names=TRUE),
                             ls(envir=.GenericArgsEnv, all.names=TRUE)))
        extras <- setdiff(prototypes, prims)
        if(length(extras))
            undoc_things <- c(undoc_things, list(prim_extra=extras))
        langElts <- c("$","$<-","&&","(",":","@","@<-","[","[[",
                      "[[<-","[<-","{","||","~","<-","<<-","=","break","for",
                      "function","if","next","repeat","return", "while")
        miss <- setdiff(prims, c(langElts, prototypes))
        if(length(miss))
            undoc_things <- c(undoc_things, list(primitives=miss))
    }

    class(undoc_things) <- "undoc"
    undoc_things
}

format.undoc <-
function(x, ...)
{
    .fmt <- function(i) {
        tag <- names(x)[i]
        msg <- switch(tag,
                      "code objects" =
                      gettext("Undocumented code objects:"),
                      "data sets" =
                      gettext("Undocumented data sets:"),
                      "S4 classes" =
                      gettext("Undocumented S4 classes:"),
                      "S4 methods" =
                      gettext("Undocumented S4 methods:"),
                      prim_extra =
                      gettext("Prototyped non-primitives:"),
                      gettextf("Undocumented %s:", tag))
        c(msg,
          ## We avoid markup for indicating S4 methods, hence need to
          ## special-case output for these ...
          if(tag == "S4 methods") {
              strwrap(x[[i]], indent = 2L, exdent = 4L)
          } else {
              .pretty_format(x[[i]])
          })
    }

    as.character(unlist(lapply(which(sapply(x, length) > 0L), .fmt)))
}

### * codoc

codoc <-
function(package, dir, lib.loc = NULL,
         use.values = NULL, verbose = getOption("verbose"))
{
    has_namespace <- FALSE

    ## Argument handling.
    if(!missing(package)) {
        if(length(package) != 1L)
            stop("argument 'package' must be of length 1")
        dir <- find.package(package, lib.loc)
        ## Using package installed in @code{dir} ...
        code_dir <- file.path(dir, "R")
        if(!file_test("-d", code_dir))
            stop(gettextf("directory '%s' does not contain R code",
                          dir),
                 domain = NA)
        if(!.haveRds(dir))
            stop(gettextf("directory '%s' does not contain Rd objects", dir),
                 domain = NA)
        is_base <- basename(dir) == "base"

        ## Load package into code_env.
        if(!is_base)
            .load_package_quietly(package, lib.loc)
        code_env <- .package_env(package)

        objects_in_code <- objects(envir = code_env, all.names = TRUE)

        ## Does the package have a namespace?
        if(packageHasNamespace(package, dirname(dir))) {
            has_namespace <- TRUE
            ns_env <- asNamespace(package)
            S3Table <- get(".__S3MethodsTable__.", envir = ns_env)
            functions_in_S3Table <- ls(S3Table, all.names = TRUE)
            objects_in_ns <-
                setdiff(objects(envir = ns_env, all.names = TRUE),
                        c(".__NAMESPACE__.", ".__S3MethodsTable__."))
            objects_in_code_or_namespace <-
                unique(c(objects_in_code, objects_in_ns))
            objects_in_ns <- setdiff(objects_in_ns, objects_in_code)
        }
        else
            objects_in_code_or_namespace <- objects_in_code
        package_name <- package
    }
    else {
        if(missing(dir))
            stop("you must specify 'package' or 'dir'")
        ## Using sources from directory @code{dir} ...
        if(!file_test("-d", dir))
            stop(gettextf("directory '%s' does not exist", dir),
                 domain = NA)
        else
            dir <- file_path_as_absolute(dir)
        code_dir <- file.path(dir, "R")
        if(!file_test("-d", code_dir))
            stop(gettextf("directory '%s' does not contain R code",
                          dir),
                 domain = NA)
        if(!.haveRds(dir))
            stop(gettextf("directory '%s' does not contain Rd objects", dir),
                 domain = NA)
        package_name <- basename(dir)
        is_base <- package_name == "base"

        code_env <- new.env(hash = TRUE)
        dfile <- file.path(dir, "DESCRIPTION")
        meta <- if(file_test("-f", dfile))
            .read_description(dfile)
        else
            character()
        .source_assignments_in_code_dir(code_dir, code_env, meta)
        sys_data_file <- file.path(code_dir, "sysdata.rda")
        if(file_test("-f", sys_data_file)) load(sys_data_file, code_env)

        objects_in_code <- objects(envir = code_env, all.names = TRUE)
        objects_in_code_or_namespace <- objects_in_code

        ## Does the package have a NAMESPACE file?  Note that when
        ## working on the sources we (currently?) cannot deal with the
        ## (experimental) alternative way of specifying the namespace.
        ## Also, do not attempt to find S3 methods.
        if(file.exists(file.path(dir, "NAMESPACE"))) {
            has_namespace <- TRUE
            objects_in_ns <- objects_in_code
            functions_in_S3Table <- character()
            ns_env <- code_env
            nsInfo <- parseNamespaceFile(basename(dir), dirname(dir))
            ## Look only at exported objects.
            OK <- intersect(objects_in_code, nsInfo$exports)
            for(p in nsInfo$exportPatterns)
                OK <- c(OK, grep(p, objects_in_code, value = TRUE))
            objects_in_code <- unique(OK)
        }
    }

    ## Find the data sets to work on.
    data_dir <- file.path(dir, "data")
    data_sets_in_code <- if(file_test("-d", data_dir))
        names(.try_quietly(list_data_in_pkg(dataDir = data_dir)))
    else
        character()

    ## Find the function objects to work on.
    functions_in_code <-
        Filter(function(f) {
                   ## This is expensive
                   f <- get(f, envir = code_env)
                   typeof(f) == "closure"
               },
               objects_in_code)
    ## Sourcing all R code files in the package is a problem for base,
    ## where this misses the .Primitive functions.  Hence, when checking
    ## base for objects shown in \usage but missing from the code, we
    ## get the primitive functions from the version of R we are using.
    ## Maybe one day we will have R code for the primitives as well ...
    ## As from R 2.5.0 we do for most generics.
    if(is_base) {
        objects_in_base <-
            objects(envir = baseenv(), all.names = TRUE)
        objects_in_code <-
            c(objects_in_code,
              Filter(.is_primitive_in_base, objects_in_base),
              c(".First.lib", ".Last.lib", ".Random.seed",
                ".onLoad", ".onAttach", ".onDetach", ".onUnload"))
        objects_in_code_or_namespace <- objects_in_code
        known_env <- .make_S3_primitive_generic_env(code_env, fixup=TRUE)
        extras <- ls(known_env, all.names = TRUE)
        functions_in_code <- c(functions_in_code, extras)
        code_env <- known_env
        known_env <- .make_S3_primitive_nongeneric_env(code_env)
        extras <- ls(known_env, all.names = TRUE)
        functions_in_code <- c(functions_in_code, extras)
        code_env <- known_env
    }

    ## Build a list with the formals of the functions in the code
    ## indexed by the names of the functions.
    function_args_in_code <-
        lapply(functions_in_code,
               function(f) formals(get(f, envir = code_env))) # get is expensive
    names(function_args_in_code) <- functions_in_code
    if(has_namespace) {
        functions_in_ns <-
            Filter(function(f) {
                       f <- get(f, envir = ns_env) # get is expensive
                       is.function(f) && (length(formals(f)) > 0L)
                   },
                   objects_in_ns)
        function_args_in_ns <-
            lapply(functions_in_ns,
                   function(f) formals(get(f, envir = ns_env)))
        names(function_args_in_ns) <- functions_in_ns

        function_args_in_S3Table <-
            lapply(functions_in_S3Table,
                   function(f) formals(get(f, envir = S3Table)))
        names(function_args_in_S3Table) <- functions_in_S3Table

        tmp <- c(function_args_in_code, function_args_in_S3Table,
                 function_args_in_ns)
        keep <- !duplicated(names(tmp))
        function_args_in_code <- tmp[keep]
        functions_in_code <- names(function_args_in_code)
    }
    if(.isMethodsDispatchOn()) {
        ## <NOTE>
        ## There is no point in worrying about exportMethods directives
        ## in a NAMESPACE file when working on a package source dir, as
        ## we only source the assignments, and hence do not get any
        ## S4 classes or methods.
        ## </NOTE>
        ## <NOTE>
        ## In principle, we can get codoc checking for S4 methods
        ## documented explicitly using the \S4method{GENERIC}{SIGLIST}
        ## markup by adding the corresponding "pseudo functions" using
        ## the Rd markup as their name.  However note that the formals
        ## recorded in the methods db only pertain to the signature, not
        ## to the ones of the function actually registered ... hence we
        ## use methods::unRematchDefinition() which knows how to extract
        ## the formals in the method definition from the
        ##   function(ARGLIST) {
        ##     .local <- function(FORMALS) BODY
        ##     .local(ARGLIST)
        ##   }
        ## redefinitions obtained by methods::rematchDefinition().
        ## </NOTE>
        check_S4_methods <-
            !identical(as.logical(Sys.getenv("_R_CHECK_CODOC_S4_METHODS_")),
                       FALSE)
        if(check_S4_methods) {
            get_formals_from_method_definition <- function(m)
                formals(methods::unRematchDefinition(m))
            lapply(.get_S4_generics(code_env),
                   function(f) {
                       mlist <- .get_S4_methods_list(f, code_env)
                       sigs <- .make_siglist(mlist)
                       if(!length(sigs)) return()
                       nm <- sprintf("\\S4method{%s}{%s}", f, sigs)
                       args <- lapply(mlist,
                                      get_formals_from_method_definition)
                       names(args) <- nm
                       functions_in_code <<-
                           c(functions_in_code, nm)
                       function_args_in_code <<-
                           c(function_args_in_code, args)
                   })
        }
    }

    check_codoc <- function(fName, ffd) {
        ## Compare the formals of the function in the code named 'fName'
        ## and formals 'ffd' obtained from the documentation.
        ffc <- function_args_in_code[[fName]]
        if(identical(use.values, FALSE)) {
            ffc <- names(ffc)
            ffd <- names(ffd)
            ok <- identical(ffc, ffd)
        } else {
            if(!identical(names(ffc), names(ffd)))
                ok <- FALSE
            else {
                vffc <- as.character(ffc) # values
                vffd <- as.character(ffd) # values
                if(!identical(use.values, TRUE)) {
                    ind <- nzchar(as.character(ffd))
                    vffc <- vffc[ind]
                    vffd <- vffd[ind]
                }
                ok <- identical(vffc, vffd)
            }
        }
        if(ok)
            NULL
        else
            list(list(name = fName, code = ffc, docs = ffd))
    }

    db <- if(!missing(package))
        Rd_db(package, lib.loc = dirname(dir))
    else
        Rd_db(dir = dir)

    names(db) <- db_names <- .Rd_get_names_from_Rd_db(db)

    ## pkg-defunct.Rd is not expected to list arguments
    ind <- db_names %in% paste(package_name, "defunct", sep = "-")
    db <- db[!ind]
    db_names <- db_names[!ind]

    db_usages <- lapply(db, .Rd_get_section, "usage")
    db_usages <- lapply(db_usages, .parse_usage_as_much_as_possible)
    ind <- as.logical(sapply(db_usages,
                             function(x) !is.null(attr(x, "bad_lines"))))
    bad_lines <- lapply(db_usages[ind], attr, "bad_lines")

    bad_doc_objects <- list()
    functions_in_usages <- character()
    variables_in_usages <- character()
    data_sets_in_usages <- character()
    functions_in_usages_not_in_code <- list()
    data_sets_in_usages_not_in_code <- list()

    for(docObj in db_names) {

        exprs <- db_usages[[docObj]]
        if(!length(exprs)) next

        ## Get variable names and data set usages first, mostly for
        ## curiosity.
        ind <- ! sapply(exprs, is.call)
        if(any(ind)) {
            variables_in_usages <-
                c(variables_in_usages,
                  sapply(exprs[ind], deparse))
            exprs <- exprs[!ind]
        }
        ind <- as.logical(sapply(exprs,
                                 function(e)
                                 (length(e) == 2L)
                                 && e[[1L]] == as.symbol("data")))
        if(any(ind)) {
            data_sets <- sapply(exprs[ind],
                                function(e) as.character(e[[2L]]))
            data_sets_in_usages <- c(data_sets_in_usages, data_sets)
            data_sets <- setdiff(data_sets, data_sets_in_code)
            if(length(data_sets))
                data_sets_in_usages_not_in_code[[docObj]] <- data_sets
            exprs <- exprs[!ind]
        }
        ## Split out replacement function usages.
        ind <- as.logical(sapply(exprs,
                                 .is_call_from_replacement_function_usage))
        replace_exprs <- exprs[ind]
        exprs <- exprs[!ind]
        ## Ordinary functions.
        functions <- sapply(exprs, function(e) as.character(e[[1L]]))
        ## Catch assignments: checkDocFiles() will report these, so drop
        ## them here.
        ## And also unary/binary operators
        ind <- !(functions %in% c("<-", "=", "+", "-"))
        exprs <- exprs[ind]
        functions <- functions[ind]
        functions <- .transform_S3_method_markup(as.character(functions))
        ind <- functions %in% functions_in_code
        bad_functions <-
            mapply(functions[ind],
                   exprs[ind],
                   FUN = function(x, y)
                   check_codoc(x, as.pairlist(as.alist.call(y[-1L]))),
                   SIMPLIFY = FALSE)
        ## Replacement functions.
        if(length(replace_exprs)) {
            replace_funs <-
                paste0(sapply(replace_exprs,
                             function(e) as.character(e[[2L]][[1L]])),
                      "<-")
            replace_funs <- .transform_S3_method_markup(replace_funs)
            functions <- c(functions, replace_funs)
            ind <- (replace_funs %in% functions_in_code)
            if(any(ind)) {
                bad_replace_funs <-
                    mapply(replace_funs[ind],
                           replace_exprs[ind],
                           FUN = function(x, y)
                           check_codoc(x,
                                      as.pairlist(c(as.alist.call(y[[2L]][-1L]),
                                                    as.alist.symbol(y[[3L]])))),
                           SIMPLIFY = FALSE)
                bad_functions <-
                    c(bad_functions, bad_replace_funs)
            }
        }

        bad_functions <- do.call("c", bad_functions)
        if(length(bad_functions))
            bad_doc_objects[[docObj]] <- bad_functions

        ## Determine functions with a \usage entry in the documentation
        ## but 'missing from the code'.  If a package has a namespace, we
        ## really need to look at all objects in the namespace (hence
        ## 'objects_in_code_or_namespace'), as one can access the internal
        ## symbols via ':::' and hence package developers might want to
        ## provide function usages for some of the internal functions.
        ## <FIXME>
        ## We may still have \S4method{}{} entries in functions, which
        ## cannot have a corresponding object in the code.  Hence, we
        ## remove these function entries, but should really do better,
        ## by comparing the explicit \usage entries for S4 methods to
        ## what is actually in the code.  We most likely also should do
        ## something similar for S3 methods.
        ind <- grep(.S4_method_markup_regexp, functions)
        if(any(ind))
            functions <- functions[!ind]
        ## </FIXME>
        bad_functions <- setdiff(functions, objects_in_code_or_namespace)
        if(length(bad_functions))
            functions_in_usages_not_in_code[[docObj]] <- bad_functions

        functions_in_usages <- c(functions_in_usages, functions)
    }

    ## Determine (function) objects in the code without a \usage entry.
    ## Of course, these could still be 'documented' via \alias.
    ## </NOTE>
    ## Older versions only printed this information without returning it
    ## (in case 'verbose' was true).  We now add this as an attribute to
    ## the bad_doc_objects returned.
    ## </NOTE>
    objects_in_code_not_in_usages <-
        setdiff(objects_in_code,
                c(functions_in_usages, variables_in_usages))
    functions_in_code_not_in_usages <-
        intersect(functions_in_code, objects_in_code_not_in_usages)
    ## (Note that 'functions_in_code' does not necessarily contain all
    ## (exported) functions in the package.)

    ## Determine functions which have no usage but really should have.
    ## If there is no namespace (including base), we have no idea.
    ## If there is one, everything "exported" (in the package env)
    ## should also have a \usage, apart from
    ## * Defunct functions
    ## * S4 generics.  Note that as per R-exts,
    ##     exporting methods on a generic in the namespace will also
    ##     export the generic, and exporting a generic in the namespace
    ##     will also export its methods.
    ##   so it seems there is really no way to figure out whether an
    ##   exported S4 generic should have a \usage entry or not ...
    functions_missing_from_usages <-
        if(!has_namespace) character() else {
            functions <- functions_in_code_not_in_usages
            if(.isMethodsDispatchOn()) {
                ## Drop the functions which have S4 methods.
                functions <-
                    setdiff(functions, names(.get_S4_generics(code_env)))
            }
            ## Drop the defunct functions.
            is_defunct <- function(f) {
                f <- get(f, envir = code_env) # get is expensive
                if(!is.function(f)) return(FALSE)
                (is.call(b <- body(f))
                 && identical(as.character(b[[1L]]), ".Defunct"))
            }
            functions[!sapply(functions, is_defunct)]
        }
    objects_missing_from_usages <-
        if(!has_namespace) character() else {
            c(functions_missing_from_usages,
              setdiff(objects_in_code_not_in_usages,
                      c(functions_in_code, data_sets_in_code)))
        }

    attr(bad_doc_objects, "objects_in_code_not_in_usages") <-
        objects_in_code_not_in_usages
    attr(bad_doc_objects, "functions_in_code_not_in_usages") <-
        functions_in_code_not_in_usages
    attr(bad_doc_objects, "functions_in_usages_not_in_code") <-
        functions_in_usages_not_in_code
    attr(bad_doc_objects, "function_args_in_code") <-
        function_args_in_code
    attr(bad_doc_objects, "data_sets_in_usages_not_in_code") <-
        data_sets_in_usages_not_in_code
    attr(bad_doc_objects, "objects_missing_from_usages") <-
        objects_missing_from_usages
    attr(bad_doc_objects, "functions_missing_from_usages") <-
        functions_missing_from_usages
    attr(bad_doc_objects, "has_namespace") <- has_namespace
    attr(bad_doc_objects, "bad_lines") <- bad_lines
    class(bad_doc_objects) <- "codoc"
    bad_doc_objects
}

print.codoc <-
function(x, ...)
{
    functions_in_usages_not_in_code <-
        attr(x, "functions_in_usages_not_in_code")
    if(length(functions_in_usages_not_in_code)) {
        for(fname in names(functions_in_usages_not_in_code)) {
            writeLines(gettextf("Functions or methods with usage in documentation object '%s' but not in code:",
                                fname))
            .pretty_print(unique(functions_in_usages_not_in_code[[fname]]))
            writeLines("")
        }
    }

    data_sets_in_usages_not_in_code <-
        attr(x, "data_sets_in_usages_not_in_code")
    if(length(data_sets_in_usages_not_in_code)) {
        for(fname in names(data_sets_in_usages_not_in_code)) {
            writeLines(gettextf("Data with usage in documentation object '%s' but not in code:",
                                fname))
            .pretty_print(unique(data_sets_in_usages_not_in_code[[fname]]))
            writeLines("")
        }
    }

    ## In general, functions in the code which only have an \alias but
    ## no \usage entry are not necessarily a problem---they might be
    ## mentioned in other parts of the Rd object documenting them, or be
    ## 'internal'.  However, if a package has a namespace, then all
    ## *exported* functions should have \usage entries (apart from
    ## defunct functions and S4 generics, see the above comments for
    ## functions_missing_from_usages).  Currently, this information is
    ## returned in the codoc object but not shown.  Eventually, we might
    ## add something like
    ##     functions_missing_from_usages <-
    ##         attr(x, "functions_missing_from_usages")
    ##     if(length(functions_missing_from_usages)) {
    ##         writeLines("Exported functions without usage information:")
    ##         .pretty_print(functions_in_code_not_in_usages)
    ##         writeLines("")
    ##     }
    ## similar to the above.

    if(!length(x))
        return(invisible(x))

    has_only_names <- is.character(x[[1L]][[1L]][["code"]])

    format_args <- function(s) {
        if(!length(s))
            "function()"
        else if(has_only_names)
	    paste0("function(", paste(s, collapse = ", "), ")")
        else {
            s <- paste(deparse(s), collapse = "")
            s <- gsub(" = ([,\\)])", "\\1", s)
            s <- gsub("<unescaped bksl>", "\\", s, fixed = TRUE)
            gsub("^list", "function", s)
        }
    }

    summarize_mismatches_in_names <- function(nfc, nfd) {
        if(length(nms <- setdiff(nfc, nfd)))
            writeLines(c(gettext("  Argument names in code not in docs:"),
                         strwrap(paste(nms, collapse = " "),
                                 indent = 4L, exdent = 4L)))
        if(length(nms <- setdiff(nfd, nfc)))
            writeLines(c(gettext("  Argument names in docs not in code:"),
                         strwrap(paste(nms, collapse = " "),
                                 indent = 4L, exdent = 4L)))
        len <- min(length(nfc), length(nfd))
        if(len) {
            len <- seq_len(len)
            nfc <- nfc[len]
            nfd <- nfd[len]
            ind <- which(nfc != nfd)
            len <- length(ind)
            if(len) {
                if(len > 3L) {
                    writeLines(gettext("  Mismatches in argument names (first 3):"))
                    ind <- ind[1L:3L]
                } else {
                    writeLines(gettext("  Mismatches in argument names:"))
                }
                for(i in ind) {
                    writeLines(sprintf("    Position: %d Code: %s Docs: %s",
                                       i, nfc[i], nfd[i]))
                }
            }
        }
    }

    summarize_mismatches_in_values <- function(ffc, ffd) {
        ## Be nice, and match arguments by names first.
        nms <- intersect(names(ffc), names(ffd))
        vffc <- ffc[nms]
        vffd <- ffd[nms]
        ind <- which(as.character(vffc) != as.character(vffd))
        len <- length(ind)
        if(len) {
            if(len > 3L) {
                writeLines(gettext("  Mismatches in argument default values (first 3):"))
                ind <- ind[1L:3L]
            } else {
                writeLines(gettext("  Mismatches in argument default values:"))
            }
            for(i in ind) {
                multiline <- FALSE
                cv <- deparse(vffc[[i]])
                if(length(cv) > 1L) {
                    cv <- paste(cv, collapse = "\n      ")
                    multiline <- TRUE
                }
                dv <- deparse(vffd[[i]])
                if(length(dv) > 1L) {
                    dv <- paste(dv, collapse = "\n      ")
                    multiline <- TRUE
                }
                dv <- gsub("<unescaped bksl>", "\\", dv, fixed = TRUE)
                sep <- if(multiline) "\n    " else " "
                writeLines(sprintf("    Name: '%s'%sCode: %s%sDocs: %s",
                                   nms[i], sep, cv, sep, dv))
            }
        }
    }

    summarize_mismatches <- function(ffc, ffd) {
        if(has_only_names)
            summarize_mismatches_in_names(ffc, ffd)
        else {
            summarize_mismatches_in_names(names(ffc), names(ffd))
            summarize_mismatches_in_values(ffc, ffd)
        }
    }

    for(fname in names(x)) {
        writeLines(gettextf("Codoc mismatches from documentation object '%s':",
                            fname))
        xfname <- x[[fname]]
        for(i in seq_along(xfname)) {
            ffc <- xfname[[i]][["code"]]
            ffd <- xfname[[i]][["docs"]]
            writeLines(c(xfname[[i]][["name"]],
                         strwrap(gettextf("Code: %s", format_args(ffc)),
                                 indent = 2L, exdent = 17L),
                         strwrap(gettextf("Docs: %s", format_args(ffd)),
                                 indent = 2L, exdent = 17L)))
            summarize_mismatches(ffc, ffd)
        }
        writeLines("")
    }

    invisible(x)
}

### * codocClasses

codocClasses <-
function(package, lib.loc = NULL)
{
    ## Compare the 'structure' of S4 classes in an installed package
    ## between code and documentation.
    ## Currently, only compares the slot names.

    ## <NOTE>
    ## This is patterned after the current codoc().
    ## It would be useful to return the whole information on class slot
    ## names found in the code and matching documentation (rather than
    ## just the ones with mismatches).
    ## Currently, we only return the names of all classes checked.
    ## </NOTE>

    bad_Rd_objects <- structure(NULL, class = "codocClasses")

    ## Argument handling.
    if(length(package) != 1L)
        stop("argument 'package' must be of length 1")
    dir <- find.package(package, lib.loc)
    if(!file_test("-d", file.path(dir, "R")))
        stop(gettextf("directory '%s' does not contain R code", dir),
             domain = NA)
    if(!.haveRds(dir))
        stop(gettextf("directory '%s' does not contain Rd objects", dir),
             domain = NA)
    is_base <- basename(dir) == "base"

    ## Load package into code_env.
    if(!is_base)
        .load_package_quietly(package, lib.loc)
    code_env <- .package_env(package)

    if(!.isMethodsDispatchOn())
        return(bad_Rd_objects)

    S4_classes <- methods::getClasses(code_env)
    if(!length(S4_classes)) return(bad_Rd_objects)

    sApply <- function(X, FUN, ...) ## fast and special case - only
        unlist(lapply(X = X, FUN = FUN, ...), recursive=FALSE, use.names=FALSE)
    ## Build Rd data base.
    db <- Rd_db(package, lib.loc = dirname(dir))

    ## Need some heuristics now.  When does an Rd object document just
    ## one S4 class so that we can compare (at least) the slot names?
    ## Try the following:
    ## 1) \docType{} identical to "class";
    ## 2) either exactly one \alias{} or only one ending in "-class"
    ## 3) a non-empty user-defined section 'Slots'.

    ## As going through the db to extract sections can take some time,
    ## we do the vectorized metadata computations first, and try to
    ## subscript whenever possible.

    idx <- sApply(lapply(db, .Rd_get_doc_type), identical, "class")
    if(!any(idx)) return(bad_Rd_objects)
    db <- db[idx]
    stats <- c(n.S4classes = length(S4_classes), n.db = length(db))

    aliases <- lapply(db, .Rd_get_metadata, "alias")
    named_class <- lapply(aliases, grepl, pattern="-class$")
    nClass <- sApply(named_class, sum)
    oneAlias <- sApply(aliases, length) == 1L
    idx <- oneAlias | nClass == 1L
    if(!any(idx)) return(bad_Rd_objects)
    db <- db[idx]
    stats["n.cl"] <- length(db)

    ## keep only the foo-class alias in case there was more than one:
    multi <- idx & !oneAlias
    aliases[multi] <-
        mapply(`[`, aliases[multi], named_class[multi],
               SIMPLIFY = FALSE, USE.NAMES = FALSE)
    aliases <- unlist(aliases[idx], use.names = FALSE)

    Rd_slots <- lapply(db, .Rd_get_section, "Slots", FALSE)
    idx <- sapply(Rd_slots, length) > 0L
    if(!any(idx)) return(bad_Rd_objects)
    db <- db[idx]; aliases <- aliases[idx]; Rd_slots <- Rd_slots[idx]
    stats["n.final"] <- length(db)

    db_names <- .Rd_get_names_from_Rd_db(db)

    .get_slot_names <- function(x) {
        ## Get \describe (inside user-defined section 'Slots'):
        ## Should this allow for several \describe blocks?
        x <- .Rd_get_section(x, "describe")
        ## Get the \item tags inside \describe.
        txt <- .Rd_get_item_tags(x)
        if(!length(txt)) return(character())
        txt <- gsub("\\\\l?dots", "...", txt)
        ## And now strip enclosing '\code{...}:'
        txt <- gsub("\\\\code\\{([^}]*)\\}:?", "\\1", as.character(txt))
        txt <- unlist(strsplit(txt, ", *"))
        .strip_whitespace(txt)
    }

    .inheritedSlotNames <- function(ext) {
	supcl <- methods::.selectSuperClasses(ext)
	unique(unlist(lapply(lapply(supcl, methods::getClassDef),
			     methods::slotNames),
		      use.names=FALSE))
    }

    S4topics <- sApply(S4_classes, utils:::topicName, type="class")
    S4_checked <- S4_classes[has.a <- S4topics %in% aliases]
    idx <- match(S4topics[has.a], aliases)
    for(icl in seq_along(S4_checked)) {
        cl <- S4_checked[icl]
        cld <- methods::getClass(cl, where = code_env)
        ii <- idx[icl]
        ## Add sanity checking later ...
        scld <- methods::slotNames(cld)
        codeSlots <- if(!is.null(scld)) sort(scld) else character()
        docSlots  <- sort(.get_slot_names(Rd_slots[[ii]]))
        superSlots <- .inheritedSlotNames(cld@contains)
        if(length(superSlots)) ## allow '\dots' in docSlots
            docSlots <-
                docSlots[is.na(match(docSlots, c("...", "\\dots")))]
        ## was if(!identical(slots_in_code, slots_in_docs)) {
        if(!all(d.in.c <- docSlots %in% codeSlots) ||
           !all(c.in.d <- (setdiff(codeSlots, superSlots)) %in% docSlots) ) {
            bad_Rd_objects[[db_names[ii]]] <-
                list(name = cl,
                     code = codeSlots,
                     inherited = superSlots,
                     docs = docSlots)
        }
    }

    attr(bad_Rd_objects, "S4_classes_checked") <- S4_checked
    attr(bad_Rd_objects, "stats") <- stats
    bad_Rd_objects
} ## end{ codocClasses }

format.codocClasses <-
function(x, ...)
{
    .fmt <- function(nm) {
        wrapPart <- function(nam) {
            capWord <- function(w) sub("\\b(\\w)", "\\U\\1", w, perl = TRUE)

            if(length(O <- docObj[[nam]]))
                strwrap(sprintf("%s: %s", gettextf(capWord(nam)),
                                paste(O, collapse = " ")),
                        indent = 2L, exdent = 8L)
        }

        docObj <- x[[nm]]
        c(gettextf("S4 class codoc mismatches from documentation object '%s':",
                   nm),
          gettextf("Slots for class '%s'", docObj[["name"]]),
          wrapPart("code"),
          wrapPart("inherited"),
          wrapPart("docs"),
          "")
    }

    as.character(unlist(lapply(names(x), .fmt)))
}

### * codocData

codocData <-
function(package, lib.loc = NULL)
{
    ## Compare the 'structure' of 'data' objects (variables or data
    ## sets) in an installed package between code and documentation.
    ## Currently, only compares the variable names of data frames found.

    ## <NOTE>
    ## This is patterned after the current codoc().
    ## It would be useful to return the whole information on data frame
    ## variable names found in the code and matching documentation
    ## (rather than just the ones with mismatches).
    ## Currently, we only return the names of all data frames checked.
    ## </NOTE>

    bad_Rd_objects <- structure(NULL, class = "codocData")

    ## Argument handling.
    if(length(package) != 1L)
        stop("argument 'package' must be of length 1")

    dir <- find.package(package, lib.loc)

    ## Build Rd data base.
    db <- Rd_db(package, lib.loc = dirname(dir))

    is_base <- basename(dir) == "base"
    has_namespace <- !is_base && packageHasNamespace(package, dirname(dir))

    ## Load package into code_env.
    if(!is_base)
        .load_package_quietly(package, lib.loc)
    code_env <- .package_env(package)
    if(has_namespace) ns_env <- asNamespace(package)

    ## Could check here whether the package has any variables or data
    ## sets (and return if not).


    ## Need some heuristics now.  When does an Rd object document a
    ## data.frame (could add support for other classes later) variable
    ## or data set so that we can compare (at least) the names of the
    ## variables in the data frame?  Try the following:
    ## * just one \alias{};
    ## * if documentation was generated via prompt, there is a \format
    ##   section starting with 'A data frame with' (but many existing Rd
    ##   files instead have 'This data frame contains' and containing
    ##   one or more \describe sections inside.

    ## As going through the db to extract sections can take some time,
    ## we do the vectorized metadata computations first, and try to
    ## subscript whenever possible.
    aliases <- lapply(db, .Rd_get_metadata, "alias")
    idx <- sapply(aliases, length) == 1L
    if(!any(idx)) return(bad_Rd_objects)
    db <- db[idx]
    aliases <- aliases[idx]

    names(db) <- .Rd_get_names_from_Rd_db(db)

    .get_data_frame_var_names <- function(x) {
        ## Make sure that there is exactly one format section:
        ## using .Rd_get_section() would get the first one.
        x <- x[RdTags(x) == "\\format"]
        if(length(x) != 1L) return(character())
        ## Drop comments.
        ## <FIXME>
        ## Remove calling .Rd_drop_comments() eventually.
        x <- .Rd_drop_comments(x[[1L]])
        ## </FIXME>
        ## What did the format section start with?
        if(!grepl("^[ \n\t]*(A|This) data frame",
                  .Rd_deparse(x, tag = FALSE)))
            return(character())
        ## Get \describe inside \format.
        ## Should this allow for several \describe blocks?
        x <- .Rd_get_section(x, "describe")
        ## Get the \item tags inside \describe.
        txt <- .Rd_get_item_tags(x)
        if(!length(txt)) return(character())
        txt <- gsub("(.*):$", "\\1", as.character(txt))
        txt <- gsub("\\\\code\\{(.*)\\}:?", "\\1", txt)
        ## Argh.  Of course, variable names can have a '_', which needs
        ## to be escaped if not in \code{}, and the prompt() default is
        ## not to put variable names inside \code{}.
        txt <- gsub("\\\\_", "_", txt)
        txt <- unlist(strsplit(txt, ", *"))
        .strip_whitespace(txt)
    }

    Rd_var_names <- lapply(db, .get_data_frame_var_names)

    idx <- (sapply(Rd_var_names, length) > 0L)
    if(!length(idx)) return(bad_Rd_objects)
    aliases <- unlist(aliases[idx])
    Rd_var_names <- Rd_var_names[idx]

    db_names <- names(db)[idx]

    data_env <- new.env(hash = TRUE)
    data_dir <- file.path(dir, "data")
    ## with lazy data we have data() but don't need to use it.
    has_data <- file_test("-d", data_dir) &&
        !file_test("-f", file.path(data_dir, "Rdata.rdb"))
    data_exts <- .make_file_exts("data")

    ## Now go through the aliases.
    data_frames_checked <- character()
    for(i in seq_along(aliases)) {
        ## Store the documented variable names.
        var_names_in_docs <- sort(Rd_var_names[[i]])
        ## Try finding the variable or data set given by the alias.
        al <- aliases[i]
        if(exists(al, envir = code_env, mode = "list",
                  inherits = FALSE)) {
            al <- get(al, envir = code_env, mode = "list")
        } else if(has_namespace && exists(al, envir = ns_env, mode = "list",
                  inherits = FALSE)) {
            al <- get(al, envir = ns_env, mode = "list")
        } else if(has_data) {
            ## Should be a data set.
            if(!length(dir(data_dir)
                       %in% paste(al, data_exts, sep = "."))) {
                next                    # What the hell did we pick up?
            }
            ## Try loading the data set into data_env.
            utils::data(list = al, envir = data_env)
            if(exists(al, envir = data_env, mode = "list",
                      inherits = FALSE)) {
                al <- get(al, envir = data_env, mode = "list")
            }
            ## And clean up data_env.
            rm(list = ls(envir = data_env, all.names = TRUE),
               envir = data_env)
        }
        if(!is.data.frame(al)) next
        ## Now we should be ready:
        data_frames_checked <- c(data_frames_checked, aliases[i])
        var_names_in_code <- sort(names(al))
        if(!identical(var_names_in_code, var_names_in_docs))
            bad_Rd_objects[[db_names[i]]] <-
                list(name = aliases[i],
                     code = var_names_in_code,
                     docs = var_names_in_docs)
    }

    attr(bad_Rd_objects, "data_frames_checked") <-
        as.character(data_frames_checked)
    bad_Rd_objects
}

format.codocData <-
function(x, ...)
{
    format_args <- function(s) paste(s, collapse = " ")

    .fmt <- function(nm) {
        docObj <- x[[nm]]
        ## FIXME singular or plural?
        c(gettextf("Data codoc mismatches from documentation object '%s':", nm),
          gettextf("Variables in data frame '%s'", docObj[["name"]]),
          strwrap(gettextf("Code: %s", format_args(docObj[["code"]])),
                  indent = 2L, exdent = 8L),
          strwrap(gettextf("Docs: %s", format_args(docObj[["docs"]])),
                  indent = 2L, exdent = 8L),
          "")
    }

    as.character(unlist(lapply(names(x), .fmt)))
}

### * checkDocFiles

checkDocFiles <-
function(package, dir, lib.loc = NULL)
{
    ## Argument handling.
    if(!missing(package)) {
        if(length(package) != 1L)
            stop("argument 'package' must be of length 1")
        dir <- find.package(package, lib.loc)
        ## Using package installed in @code{dir} ...
    }
    else {
        if(missing(dir))
            stop("you must specify 'package' or 'dir'")
        ## Using sources from directory @code{dir} ...
        if(!file_test("-d", dir))
            stop(gettextf("directory '%s' does not exist", dir),
                 domain = NA)
        else
            dir <- file_path_as_absolute(dir)
    }

    db <- if(!missing(package))
        Rd_db(package, lib.loc = dirname(dir))
    else
        Rd_db(dir = dir)

    db_aliases <- lapply(db, .Rd_get_metadata, "alias")
    db_keywords <- lapply(db, .Rd_get_metadata, "keyword")

    db_names <- .Rd_get_names_from_Rd_db(db)
    names(db) <- names(db_aliases) <- db_names

    db_usages <- lapply(db, .Rd_get_section, "usage")
    ## We traditionally also use the usage "texts" for some sanity
    ## checking ...
    ## <FIXME>
    ## Remove calling .Rd_drop_comments() eventually.
    db_usage_texts <-
        lapply(db_usages,
               function(e) .Rd_deparse(.Rd_drop_comments(e)))
    ## </FIXME>
    db_usages <- lapply(db_usages, .parse_usage_as_much_as_possible)
    ind <- as.logical(sapply(db_usages,
                             function(x) !is.null(attr(x, "bad_lines"))))
    bad_lines <- lapply(db_usages[ind], attr, "bad_lines")

    ## Exclude internal objects from further computations.
    ind <- sapply(db_keywords,
                  function(x) length(grep("^ *internal *$", x)) > 0L )
    if(any(ind)) {                      # exclude them
        db <- db[!ind]
        db_names <- db_names[!ind]
        db_aliases <- db_aliases[!ind]
    }

    db_argument_names <- lapply(db, .Rd_get_argument_names)

    bad_doc_objects <- list()

    for(docObj in db_names) {

        exprs <- db_usages[[docObj]]
        if(!length(exprs)) next

        aliases <- db_aliases[[docObj]]
        arg_names_in_arg_list <- db_argument_names[[docObj]]

        ## Determine function names ('functions') and corresponding
        ## arguments ('arg_names_in_usage') in the \usage.  Note how we
        ## try to deal with data set documentation.
        ind <- as.logical(sapply(exprs,
                                 function(e)
                                 ((length(e) > 1L) &&
                                  !((length(e) == 2L)
                                    && e[[1L]] == as.symbol("data")))))
        exprs <- exprs[ind]
        ## Split out replacement function usages.
        ind <- as.logical(sapply(exprs,
                                 .is_call_from_replacement_function_usage))
        replace_exprs <- exprs[ind]
        exprs <- exprs[!ind]
        ## Ordinary functions.
        functions <- as.character(sapply(exprs,
                                         function(e)
                                         as.character(e[[1L]])))
        ## Catch assignments.
        ind <- functions %in% c("<-", "=")
        assignments <- exprs[ind]
        if(any(ind)) {
            exprs <- exprs[!ind]
            functions <- functions[!ind]
        }
        ## (Note that as.character(sapply(exprs, "[[", 1L)) does not do
        ## what we want due to backquotifying.)
        arg_names_in_usage <-
            unlist(sapply(exprs,
                          function(e) .arg_names_from_call(e[-1L])))
        ## Replacement functions.
        if(length(replace_exprs)) {
            replace_funs <-
                paste0(sapply(replace_exprs,
                             function(e) as.character(e[[2L]][[1L]])),
                      "<-")
            functions <- c(functions, replace_funs)
            arg_names_in_usage <-
                c(arg_names_in_usage,
                  unlist(sapply(replace_exprs,
                                function(e)
                                c(.arg_names_from_call(e[[2L]][-1L]),
                                  .arg_names_from_call(e[[3L]])))))
        }
        ## And finally transform the S3 \method{}{} markup into the
        ## usual function names ...
        ## <NOTE>
        ## If we were really picky, we would worry about possible
        ## namespace renaming.
        functions <- .transform_S3_method_markup(functions)
        ## </NOTE>
        ## Also transform the markup for S4 replacement methods.
        functions <- .transform_S4_method_markup(functions)

        ## Now analyze what we found.
        arg_names_in_usage_missing_in_arg_list <-
            setdiff(arg_names_in_usage, arg_names_in_arg_list)
        arg_names_in_arg_list_missing_in_usage <-
            setdiff(arg_names_in_arg_list, arg_names_in_usage)
        if(length(arg_names_in_arg_list_missing_in_usage)) {
            usage_text <- db_usage_texts[[docObj]]
            bad_args <- character()
            ## In the case of 'over-documented' arguments, try to be
            ## defensive and reduce to arguments which either are not
            ## syntactically valid names or do not match the \usage text
            ## (modulo word boundaries).
            bad <- !grepl("^[[:alnum:]._]+$",
                          arg_names_in_arg_list_missing_in_usage)
            if(any(bad)) {
                bad_args <- arg_names_in_arg_list_missing_in_usage[bad]
                arg_names_in_arg_list_missing_in_usage <-
                    arg_names_in_arg_list_missing_in_usage[!bad]
            }
            bad <- sapply(arg_names_in_arg_list_missing_in_usage,
                          function(x)
			  !grepl(paste0("\\b", x, "\\b"),
                                 usage_text))
            arg_names_in_arg_list_missing_in_usage <-
                c(bad_args,
                  arg_names_in_arg_list_missing_in_usage[as.logical(bad)])
            ## Note that the fact that we can parse the raw \usage does
            ## not imply that over-documented arguments are a problem:
            ## this works for Rd files documenting e.g. shell utilities
            ## but fails for files with special syntax (Extract.Rd).
        }

        ## Also test whether the objects we found from the \usage all
        ## have aliases, provided that there is no alias which ends in
        ## '-deprecated' (see e.g. base-deprecated.Rd).
        if(!length(grep("-deprecated$", aliases))) {
            functions <-
                setdiff(functions,
                        .functions_with_no_useful_S3_method_markup())
            ## Argh.  There are good reasons for keeping \S4method{}{}
            ## as is, but of course this is not what the aliases use ...
            ## <FIXME>
            ## Should maybe use utils:::topicName(), but in any case, we
            ## should have functions for converting between the two
            ## forms, see also the code for undoc().
            aliases <- sub("([^,]+),(.+)-method$",
                           "\\\\S4method{\\1}{\\2}",
                           aliases)
            ## </FIXME>
            aliases <- gsub("\\\\%", "%", aliases)
            functions_not_in_aliases <- setdiff(functions, aliases)
        }
        else
            functions_not_in_aliases <- character()

        if((length(arg_names_in_usage_missing_in_arg_list))
           || anyDuplicated(arg_names_in_arg_list)
           || (length(arg_names_in_arg_list_missing_in_usage))
           || (length(functions_not_in_aliases))
           || (length(assignments)))
            bad_doc_objects[[docObj]] <-
                list(missing = arg_names_in_usage_missing_in_arg_list,
                     duplicated =
                     arg_names_in_arg_list[duplicated(arg_names_in_arg_list)],
                     overdoc = arg_names_in_arg_list_missing_in_usage,
                     unaliased = functions_not_in_aliases,
                     assignments = assignments)

    }

    class(bad_doc_objects) <- "checkDocFiles"
    attr(bad_doc_objects, "bad_lines") <- bad_lines
    bad_doc_objects
}

format.checkDocFiles <-
function(x, ...)
{
    .fmt <- function(nm) {
        c(character(),
          if(length(arg_names_in_usage_missing_in_arg_list <-
                    x[[nm]][["missing"]])) {
              c(gettextf("Undocumented arguments in documentation object '%s'",
                         nm),
                .pretty_format(unique(arg_names_in_usage_missing_in_arg_list)))
          },
          if(length(duplicated_args_in_arg_list <-
                    x[[nm]][["duplicated"]])) {
              c(gettextf("Duplicated \\argument entries in documentation object '%s':",
                         nm),
                .pretty_format(duplicated_args_in_arg_list))
          },
          if(length(arg_names_in_arg_list_missing_in_usage <-
                    x[[nm]][["overdoc"]])) {
              c(gettextf("Documented arguments not in \\usage in documentation object '%s':",
                         nm),
                .pretty_format(unique(arg_names_in_arg_list_missing_in_usage)))
          },
          if(length(functions_not_in_aliases <-
                    x[[nm]][["unaliased"]])) {
              c(gettextf("Objects in \\usage without \\alias in documentation object '%s':",
                         nm),
                .pretty_format(unique(functions_not_in_aliases)))
          },
          if(length(assignments <-
                    x[[nm]][["assignments"]])) {
              c(gettextf("Assignments in \\usage in documentation object '%s':",
                         nm),
                sprintf("  %s", unlist(lapply(assignments, format))))
          },
          "")
    }

    y <- as.character(unlist(lapply(names(x), .fmt)))

    if(!identical(as.logical(Sys.getenv("_R_CHECK_WARN_BAD_USAGE_LINES_")),
                  FALSE)
       && length(bad_lines <- attr(x, "bad_lines"))) {
        y <- c(y,
               unlist(lapply(names(bad_lines),
                             function(nm) {
                                 c(gettextf("Bad \\usage lines found in documentation object '%s':",
                                            nm),
                                   paste(" ", bad_lines[[nm]]))
                             })),
               "")
    }

    y
}

### * checkDocStyle

checkDocStyle <-
function(package, dir, lib.loc = NULL)
{
    has_namespace <- auto_namespace <- FALSE

    ## Argument handling.
    if(!missing(package)) {
        if(length(package) != 1L)
            stop("argument 'package' must be of length 1")
        dir <- find.package(package, lib.loc)
        ## Using package installed in 'dir' ...
        dfile <- file.path(dir, "DESCRIPTION")
        meta <- if(file_test("-f", dfile))
            .read_description(dfile)
        else
            character()
        code_dir <- file.path(dir, "R")
        if(!file_test("-d", code_dir))
            stop(gettextf("directory '%s' does not contain R code",
                          dir),
                 domain = NA)
        if(!.haveRds(dir))
            stop(gettextf("directory '%s' does not contain Rd objects", dir),
                 domain = NA)
        package_name <- package
        is_base <- package_name == "base"

        ## Load package into code_env.
        if(!is_base)
            .load_package_quietly(package, lib.loc)
        code_env <- .package_env(package)

        objects_in_code <- objects(envir = code_env, all.names = TRUE)

        ## Does the package have a namespace?
        ## These days all packages have namespaces, but some are
        ## auto-generated.
        if(packageHasNamespace(package, dirname(dir))) {
            has_namespace <- TRUE
            ns <- readLines(file.path(dir, "NAMESPACE"), warn = FALSE)
            auto_namespace <-
                grepl("# Default NAMESPACE created by R", ns[1L],
                      useBytes = TRUE)
            ## Determine names of declared S3 methods and associated S3
            ## generics.
            ns_S3_methods_db <- getNamespaceInfo(package, "S3methods")
            ns_S3_generics <- ns_S3_methods_db[, 1L]
            ns_S3_methods <- ns_S3_methods_db[, 3L]
        }
    }
    else {
        if(missing(dir))
            stop("you must specify 'package' or 'dir'")
        ## Using sources from directory @code{dir} ...
        if(!file_test("-d", dir))
            stop(gettextf("directory '%s' does not exist", dir),
                 domain = NA)
        else
            dir <- file_path_as_absolute(dir)
        code_dir <- file.path(dir, "R")
        if(!file_test("-d", code_dir))
            stop(gettextf("directory '%s' does not contain R code",
                          dir),
                 domain = NA)
        if(!.haveRds(dir))
            stop(gettextf("directory '%s' does not contain Rd objects", dir),
                 domain = NA)
        package_name <- basename(dir)
        is_base <- package_name == "base"

        code_env <- new.env(hash = TRUE)
        dfile <- file.path(dir, "DESCRIPTION")
        meta <- if(file_test("-f", dfile))
            .read_description(dfile)
        else
            character()
        .source_assignments_in_code_dir(code_dir, code_env, meta)
        sys_data_file <- file.path(code_dir, "sysdata.rda")
        if(file_test("-f", sys_data_file)) load(sys_data_file, code_env)

        objects_in_code <- objects(envir = code_env, all.names = TRUE)

        ## Do the package sources have a NAMESPACE file?
        if(file.exists(file.path(dir, "NAMESPACE"))) {
            has_namespace <- TRUE
            nsInfo <- parseNamespaceFile(basename(dir), dirname(dir))
            ## Determine exported objects.
            OK <- intersect(objects_in_code, nsInfo$exports)
            for(p in nsInfo$exportPatterns)
                OK <- c(OK, grep(p, objects_in_code, value = TRUE))
            objects_in_code <- unique(OK)
            ## Determine names of declared S3 methods and associated S3
            ## generics.
            ns_S3_methods_db <- .get_namespace_S3_methods_db(nsInfo)
            ns_S3_generics <- ns_S3_methods_db[, 1L]
            ns_S3_methods <- ns_S3_methods_db[, 3L]
        }

    }

    ## Find the function objects in the given package.
    functions_in_code <-
        Filter(function(f) is.function(get(f, envir = code_env)),  # get is expensive
               objects_in_code)

    ## Find all S3 generics "as seen from the package".
    all_S3_generics <-
        unique(c(Filter(function(f) .is_S3_generic(f, envir = code_env),
                        functions_in_code),
                 .get_S3_generics_as_seen_from_package(dir,
                                                       !missing(package),
                                                       TRUE),
                 .get_S3_group_generics()))
    ## <FIXME>
    ## Not yet:
    code_env <- .make_S3_group_generic_env(parent = code_env)
    ## </FIXME>

    ## Find all methods in the given package for the generic functions
    ## determined above.  Store as a list indexed by the names of the
    ## generic functions.
    ## Change in 3.0.0: we only look for methods named generic.class,
    ## not those registered by a 3-arg S3method().
    methods_stop_list <- .make_S3_methods_stop_list(basename(dir))
    methods_in_package <- sapply(all_S3_generics, function(g) {
        ## This isn't really right: it assumes the generics are visible.
        if(!exists(g, envir = code_env)) return(character())
        ## <FIXME>
        ## We should really determine the name g dispatches for, see
        ## a current version of methods() [2003-07-07].  (Care is needed
        ## for internal generics and group generics.)
        ## Matching via grep() is tricky with e.g. a '$' in the name of
        ## the generic function ... hence substr().
        name <- paste0(g, ".")
        methods <-
            functions_in_code[substr(functions_in_code, 1L,
                                     nchar(name, type = "c")) == name]
        ## </FIXME>
        methods <- setdiff(methods, methods_stop_list)
        if(has_namespace) {
            ## Find registered methods for generic g.
            methods2 <- ns_S3_methods[ns_S3_generics == g]
            ## but for these purposes check name.
            OK <- substr(methods2, 1L, nchar(name, type = "c")) == name
            methods <- c(methods, methods2[OK])
        }
        methods
    })
    all_methods_in_package <- unlist(methods_in_package)
    ## There are situations where S3 methods might be documented as
    ## functions (i.e., with their full name), if they do something
    ## useful also for arguments not inheriting from the class they
    ## provide a method for.
    ## But they they should be exported under another name, and
    ## registered as an S3 method.
    ## Prior to 2.14.0 we used to allow this in the case the
    ## package has a namespace and the method is exported (even though
    ## we strongly prefer using FOO(as.BAR(x)) to FOO.BAR(x) for such
    ## cases).
    ## But this caused discontinuities with adding namespaces.
    ## Historical exception
    if(package_name == "cluster")
        all_methods_in_package <-
    	    setdiff(all_methods_in_package, functions_in_code)

    db <- if(!missing(package))
        Rd_db(package, lib.loc = dirname(dir))
    else
        Rd_db(dir = dir)

    names(db) <- db_names <- .Rd_get_names_from_Rd_db(db)

    ## Ignore pkg-deprecated.Rd and pkg-defunct.Rd.
    ind <- db_names %in% paste(package_name, c("deprecated", "defunct"),
                               sep = "-")
    db <- db[!ind]
    db_names <- db_names[!ind]

    db_usages <-
        lapply(db,
               function(Rd) {
                   Rd <- .Rd_get_section(Rd, "usage")
                   .parse_usage_as_much_as_possible(Rd)
               })
    ind <- as.logical(sapply(db_usages,
                             function(x) !is.null(attr(x, "bad_lines"))))
    bad_lines <- lapply(db_usages[ind], attr, "bad_lines")

    bad_doc_objects <- list()

    for(docObj in db_names) {

        ## Determine function names in the \usage.
        exprs <- db_usages[[docObj]]
        exprs <- exprs[sapply(exprs, length) > 1L]
        ## Ordinary functions.
        functions <-
            as.character(sapply(exprs,
                                function(e) as.character(e[[1L]])))
        ## (Note that as.character(sapply(exprs, "[[", 1L)) does not do
        ## what we want due to backquotifying.)
        ## Replacement functions.
        ind <- as.logical(sapply(exprs,
                                 .is_call_from_replacement_function_usage))
        if(any(ind)) {
            replace_funs <-
                paste0(sapply(exprs[ind],
                              function(e) as.character(e[[2L]][[1L]])),
                       "<-")
            functions <- c(functions, replace_funs)
        }

        methods_with_full_name <-
            intersect(functions, all_methods_in_package)

        functions <- .transform_S3_method_markup(functions)

        methods_with_generic <-
            sapply(intersect(functions, all_S3_generics),
                   function(g)
                   intersect(functions, methods_in_package[[g]]),
                   simplify = FALSE)

        if((length(methods_with_generic)) ||
           (length(methods_with_full_name)))
            bad_doc_objects[[docObj]] <-
                list(withGeneric  = methods_with_generic,
                     withFullName = methods_with_full_name)

    }

    attr(bad_doc_objects, "bad_lines") <- bad_lines
    class(bad_doc_objects) <- "checkDocStyle"
    bad_doc_objects
}

format.checkDocStyle <-
function(x, ...)
{
    .fmt <- function(nm) {
        ## <NOTE>
        ## With \method{GENERIC}{CLASS} now being transformed to show
        ## both GENERIC and CLASS info, documenting S3 methods on the
        ## same page as their generic is not necessarily a problem any
        ## more (as one can refer to the generic or the methods in the
        ## documentation, in particular for the primary argument).
        ## Hence, even if we still provide information about this, we
        ## no longer print it by default.  One can still access it via
        ##   lapply(checkDocStyle("foo"), "[[", "withGeneric")
        ## (but of course it does not print that nicely anymore),
        ## </NOTE>
        methods_with_full_name <- x[[nm]][["withFullName"]]
        if(length(methods_with_full_name)) {
            c(gettextf("S3 methods shown with full name in documentation object '%s':",
                       nm),
              .pretty_format(methods_with_full_name),
              "")
        } else {
            character()
        }
    }

    as.character(unlist(lapply(names(x), .fmt)))
}


### * checkFF

checkFF <-
function(package, dir, file, lib.loc = NULL,
         registration = FALSE,
         verbose = getOption("verbose"))
{
    allow_suppress <- !nzchar(Sys.getenv("_R_CHECK_FF_AS_CRAN_"))
    suppressCheck <- function(e)
        allow_suppress &&
            length(e) == 2L && is.call(e) && is.symbol(e[[1L]]) &&
                as.character(e[[1L]]) == "dontCheck"

    has_namespace <- FALSE
    is_installed_msg <- is_installed <- FALSE
    ## Argument handling.
    if(!missing(package)) {
        if(length(package) != 1L)
            stop("argument 'package' must be of length 1")
        dir <- find.package(package, lib.loc)
        dfile <- file.path(dir, "DESCRIPTION")
        db <- .read_description(dfile)
        pkg <- pkgDLL <- basename(dir)
        ## Using package installed in @code{dir} ...
        code_dir <- file.path(dir, "R")
        if(!file_test("-d", code_dir))
            stop(gettextf("directory '%s' does not contain R code",
                          dir),
                 domain = NA)
        have_registration <- FALSE
        if(basename(dir) != "base") {
            .load_package_quietly(package, lib.loc)
            code_env <- asNamespace(package)
            if(exists("DLLs", envir = code_env$.__NAMESPACE__.)) {
                DLLs <- get("DLLs", envir = code_env$.__NAMESPACE__.)
                ## fake installs have this, of class DLLInfoList
                if(length(DLLs)) has_namespace <- TRUE
                if(length(DLLs) && inherits(DLLs[[1L]], "DLLInfo")) {
                    pkgDLL <- unclass(DLLs[[1L]])$name # different for data.table
                    if(registration) {
                        reg <- getDLLRegisteredRoutines(DLLs[[1L]])
                        have_registration <- sum(sapply(reg, length)) > 0L
                    }
                }
            }
        } else {
            has_namespace <- have_registration <- TRUE
            code_env <-.package_env(package)
        }
        is_installed <- TRUE
    }
    else if(!missing(dir)) {
        have_registration <- FALSE
        ## Using sources from directory @code{dir} ...
        if(!file_test("-d", dir))
            stop(gettextf("directory '%s' does not exist", dir),
                 domain = NA)
        else
            dir <- file_path_as_absolute(dir)
        pkg <- pkgDLL <- basename(dir)
        dfile <- file.path(dir, "DESCRIPTION")
        enc <- NA; db <- NULL
        if(file.exists(dfile)) {
            db <- .read_description(dfile)
            enc <- db["Encoding"]
        }
        if(pkg == "base") has_namespace <- TRUE
        if(file.exists(file.path(dir, "NAMESPACE"))) {
            nm <- parseNamespaceFile(basename(dir), dirname(dir))
            has_namespace <- length(nm$dynlibs) > 0L
        }
        code_dir <- file.path(dir, "R")
        if(!file_test("-d", code_dir))
            stop(gettextf("directory '%s' does not contain R code",
                          dir),
                 domain = NA)
        file <- tempfile()
        on.exit(unlink(file))
        if(!file.create(file)) stop("unable to create ", file, domain = NA)
        if(!all(.file_append_ensuring_LFs(file,
                                          list_files_with_type(code_dir,
                                                               "code"))))
            stop("unable to write code files", domain = NA)
    }
    else if(!missing(file)) {
        pkg <- enc <- NA
    } else
        stop("you must specify 'package', 'dir' or 'file'")

    if(missing(package) && !file_test("-f", file))
        stop(gettextf("file '%s' does not exist", file),
             domain = NA)

    ## Should there really be a 'verbose' argument?
    ## It may be useful to extract all foreign function calls but then
    ## we would want the calls back ...
    ## What we currently do is the following: if 'verbose' is true, we
    ## show all foreign function calls in abbreviated form with the line
    ## ending in either 'OK' or 'MISSING', and we return the list of
    ## 'bad' FF calls (i.e., where the 'PACKAGE' argument is missing)
    ## *invisibly* (so that output is not duplicated).
    ## Otherwise, if not verbose, we return the list of bad FF calls.

    bad_exprs <- empty_exprs <- wrong_pkg <- other_problem <- list()
    other_desc <- character()
    bad_pkg <- character()
    FF_funs <- FF_fun_names <- c(".C", ".Fortran", ".Call", ".External",
                                 ".Call.graphics", ".External.graphics")
    ## As pointed out by DTL, packages could use non-base FF calls for
    ## which missing 'PACKAGE' arguments are not necessarily a problem.
    if(!missing(package)) {
        is_FF_fun_from_base <-
            sapply(FF_funs,
                   function(f) {
                       e <- .find_owner_env(f, code_env)
                       (identical(e, baseenv())
                        || identical(e, .BaseNamespaceEnv))
                   })
        FF_funs <- FF_funs[is_FF_fun_from_base]
    }
    ## Also, need to handle base::.Call() etc ...
    FF_funs <- c(FF_funs, sprintf("base::%s", FF_fun_names))

    allowed <- character()

    check_registration <- function(e, fr) {
    	sym <- e[[2L]]
    	name <- deparse(sym, nlines = 1L)
        if (name == "...")
            return ("SYMBOL OK") # we cannot check this, e.g. RProtoBuf

        if (is.character(sym)) {
            if (!have_registration) return ("SYMBOL OK")
            FF_fun <- as.character(e[[1L]])
            sym <- reg[[FF_fun]][[sym]]
            if(is.null(sym)) return ("SYMBOL OK")
        }

        if (!is_installed) {
            if (!is_installed_msg) {
        	other_problem <<- c(other_problem, e)
        	other_desc <<- c(other_desc, "foreign function registration not tested, as package was not installed")
        	is_installed_msg <<- TRUE
            }
            return("OTHER") # registration checks need the package to be installed
        }
    	if (is.symbol(sym)) { # it might be something like pkg::sym (that's a call)
	    if (!exists(name, code_env, inherits = FALSE)) {
		if (allow_suppress &&
                    name %in% suppressForeignCheck(, package))
		    return ("SYMBOL OK") # skip false positives
                if (have_registration || !allow_suppress) {
                    if (name %in% fr) {
                        if (have_registration) { # at least for now
                            other_problem <<- c(other_problem, e)
                            other_desc <<-
                                c(other_desc,
                                  sprintf("symbol %s in the local frame%s",
                                          sQuote(name),
                                          if(!have_registration) ", but no registered symbols" else ""))
                        }
                    } else {
                        other_problem <<- c(other_problem, e)
                        other_desc <<- c(other_desc,
                                         sprintf("symbol %s not in namespace%s",
                                             sQuote(name),
                                                 if(!have_registration) ", which does not have registered symbols" else ""))
                    }
                }
    	    	return("OTHER")
    	    }
    	} else if (suppressCheck(sym))
    	    return("SKIPPED")

    	sym <- tryCatch(eval(sym, code_env), error = function(e) e)
    	if (inherits(sym, "error")) {
            if (have_registration || !allow_suppress)  {
                other_problem <<- c(other_problem, e)
                other_desc <<-
                    c(other_desc, sprintf("Evaluating %s during check gives error\n%s",
                                          sQuote(name), sQuote(sym$message)))
            }
    	    return("OTHER")
    	}

        FF_fun <- as.character(e[[1L]])
        ## lmom's sym evaluate to character, so try to look up.
        ## FIXME: maybe check this is not PACKAGE = "another package"
        if (is.character(sym)) {
            if (!have_registration) return ("SYMBOL OK")
            sym <- reg[[FF_fun]][[sym]]
            if(is.null(sym)) return ("SYMBOL OK")
        }

        ## These are allowed and used by SU's packages so skip for now
    	if (inherits(sym, "RegisteredNativeSymbol")
            || inherits(sym, "NativeSymbol"))
            return ("SYMBOL OK")

        if (!inherits(sym, "NativeSymbolInfo")) {
    	    other_problem <<- c(other_problem, e)
            ## other_desc <<- c(other_desc, sprintf("\"%s\" is not of class \"%s\"", name, "NativeSymbolInfo"))
    	    other_desc <<- c(other_desc, sprintf("%s is of class \"%s\"",
                                                 sQuote(name), class(sym)))
    	    return("OTHER")
    	}
        ## This might be symbol from another (base?) package.
        ## Allow for Rcpp modules
        parg <- unclass(sym$dll)$name
        if(length(parg) == 1L && ! parg %in% c("Rcpp", pkgDLL)) {
            wrong_pkg <<- c(wrong_pkg, e)
            bad_pkg <<- c(bad_pkg, parg)
        }
    	numparms <- sym$numParameters
        if (length(numparms) && numparms >= 0) {
            ## We have to be careful if ... is in the call.
            if (any(as.character(e) == "...")) {
                other_problem <<- c(other_problem, e)
                other_desc <<-
                    c(other_desc,
                      sprintf("call includes ..., expected %d %s",
                              numparms,
                              if(numparms > 1L) "parameters" else "parameter"))
            } else {
                callparms <- length(e) - 2L
                if ("PACKAGE" %in% names(e)) callparms <- callparms - 1L
                if (FF_fun %in% c(".C", ".Fortran"))
                    callparms <- callparms - length(intersect(names(e), c("NAOK", "DUP", "ENCODING")))
                if (!is.null(numparms) && numparms >= 0L && numparms != callparms) {
                    other_problem <<- c(other_problem, e)
                    other_desc <<-
                        c(other_desc,
                          sprintf("call to %s with %d %s, expected %d",
                                  sQuote(name), callparms,
                                  if(callparms > 1L) "parameters" else "parameter",
                                  numparms))
                    return("OTHER")
                }
            }
        }
    	if (inherits(sym, "CallRoutine") && !(FF_fun %in% c(".Call", ".Call.graphics"))) {
    	    other_problem <<- c(other_problem, e)
    	    other_desc <<- c(other_desc, sprintf("%s registered as %s, but called with %s", sQuote(name), ".Call", FF_fun))
    	    return("OTHER")
    	}
    	if (inherits(sym, "ExternalRoutine") && !(FF_fun %in% c(".External", ".External.graphics"))) {
	    other_problem <<- c(other_problem, e)
	    other_desc <<- c(other_desc, sprintf("%s registered as %s, but called with %s", sQuote(name), ".External", FF_fun))
	    return("OTHER")
	}

        "SYMBOL OK"
    }

    find_bad_exprs <- function(e) {
        if(is.call(e) || is.expression(e)) {
            ## <NOTE>
            ## This picks up all calls, e.g. a$b, and they may convert
            ## to a vector.  The function is the first element in all
            ## the calls we are interested in.
            ## BDR 2002-11-28
            ## </NOTE>
            if(deparse(e[[1L]])[1L] %in% FF_funs) {
                if(registration) {
                    check_registration(e, fr)
                }
                this <- ""
                this <- parg <- e[["PACKAGE"]]
                if (!is.na(pkg) && is.character(parg) &&
                    nzchar(parg) && parg != pkgDLL) {
                    wrong_pkg <<- c(wrong_pkg, e)
                    bad_pkg <<- c(bad_pkg, this)
                }
                parg <- if(!is.null(parg) && (parg != "")) "OK"
                else if(identical(parg, "")) {
                    empty_exprs <<- c(empty_exprs, e)
                    "EMPTY"
                } else if(!is.character(sym <- e[[2L]])) {
                    if (!registration) {
                        sym <- tryCatch(eval(sym, code_env),
                                        error = function(e) e)
                        if (inherits(sym, "NativeSymbolInfo")) {
                            ## This might be symbol from another package.
                            ## Allow for Rcpp modules
                            parg <- unclass(sym$dll)$name
                            if(length(parg) == 1L && !parg %in% c("Rcpp", pkgDLL)) {
                                wrong_pkg <<- c(wrong_pkg, e)
                                bad_pkg <<- c(bad_pkg, parg)
                            }
                        }
                    }
                    "Called with symbol"
                } else if(!has_namespace) {
                    bad_exprs <<- c(bad_exprs, e)
                    "MISSING"
                } else "MISSING but in a function in a namespace"
                if(verbose)
                    if(is.null(this))
                        cat(deparse(e[[1L]]), "(", deparse(e[[2L]]),
                            ", ... ): ", parg, "\n", sep = "")
                    else
                        cat(deparse(e[[1L]]), "(", deparse(e[[2L]]),
                            ", ..., PACKAGE = \"", this, "\"): ",
                            parg, "\n", sep = "")
            } else if (deparse(e[[1L]])[1L] %in% "<-") {
                fr <<- c(fr, as.character(e[[2L]]))
            }
            for(i in seq_along(e)) Recall(e[[i]])
        }
    }

    if(!missing(package)) {
        checkFFmy <- function(f)
            if(typeof(f) == "closure") {
                env <- environment(f)
                if(isNamespace(env)) {
                    nm <- getNamespaceName(env)
                    if (nm == package) body(f) else NULL
                } else body(f)
            } else NULL
        exprs <- lapply(ls(envir = code_env, all.names = TRUE),
                        function(f) {
                            f <- get(f, envir = code_env) # get is expensive
                            checkFFmy(f)
                        })
        if(.isMethodsDispatchOn()) {
            ## Also check the code in S4 methods.
            ## This may find things twice if a setMethod() with a bad FF
            ## call is from inside a function (e.g., InitMethods()).
            for(f in .get_S4_generics(code_env)) {
                mlist <- .get_S4_methods_list(f, code_env)
                exprs <- c(exprs, lapply(mlist, body))
            }
            refs <- .get_ref_classes(code_env)
            if(length(refs)) {
                exprs2 <- lapply(unlist(refs, FALSE), checkFFmy)
                exprs <- c(exprs, exprs2)
            }
        }
    } else {
        if(!is.na(enc) &&
           !(Sys.getlocale("LC_CTYPE") %in% c("C", "POSIX"))) {
            ## FIXME: what if conversion fails on e.g. UTF-8 comments
	    con <- file(file, encoding=enc)
            on.exit(close(con))
	} else con <- file
        exprs <-
            tryCatch(parse(file = con, n = -1L),
                     error = function(e)
                     stop(gettextf("parse error in file '%s':\n%s",
                                   file,
                                   .massage_file_parse_error_message(conditionMessage(e))),
                          domain = NA, call. = FALSE))
    }
    for(i in seq_along(exprs)) {
        fr <- character()
        find_bad_exprs(exprs[[i]])
    }
    attr(bad_exprs, "wrong_pkg") <- wrong_pkg
    attr(bad_exprs, "bad_pkg") <- bad_pkg
    attr(bad_exprs, "empty") <- empty_exprs
    attr(bad_exprs, "other_problem") <- other_problem
    attr(bad_exprs, "other_desc") <- other_desc
    if (length(bad_pkg)) {              # check against dependencies.
        bases <- .get_standard_package_names()$base
        bad <- bad_pkg[!bad_pkg %in% bases]
        if (length(bad)) {
            depends <- .get_requires_from_package_db(db, "Depends")
            imports <- .get_requires_from_package_db(db, "Imports")
            suggests <- .get_requires_from_package_db(db, "Suggests")
            enhances <- .get_requires_from_package_db(db, "Enhances")
            bad <- bad[!bad %in% c(depends, imports, suggests, enhances)]
            attr(bad_exprs, "undeclared") <- bad
        }
    }
    class(bad_exprs) <- "checkFF"
    if(verbose)
        invisible(bad_exprs)
    else
        bad_exprs
}

format.checkFF <-
function(x, ...)
{
    xx <- attr(x, "empty")
    y <- attr(x, "wrong_pkg")
    z <- attr(x, "bad_pkg")
    zz <- attr(x, "undeclared")
    other_problem <- attr(x, "other_problem")

    res <- character()
    if (length(x)) {
        .fmt <- function(x)
            paste0("  ", deparse(x[[1L]]), "(", deparse(x[[2L]]), ", ...)")
        msg <- ngettext(length(x),
                        "Foreign function call without 'PACKAGE' argument:",
                        "Foreign function calls without 'PACKAGE' argument:",
                        domain = NA)
        res <- c(msg, unlist(lapply(x, .fmt)))
    }
    if (length(xx)) {
        .fmt <- function(x)
            paste0("  ", deparse(x[[1L]]), "(", deparse(x[[2L]]), ", ...)")
        msg <- ngettext(length(x),
                        "Foreign function call with empty 'PACKAGE' argument:",
                        "Foreign function calls with empty 'PACKAGE' argument:",
                        domain = NA)
       res <- c(res, msg, unlist(lapply(xx, .fmt)))
    }

    if (length(y)) {
        bases <- .get_standard_package_names()$base
        .fmt2 <- function(x, z) {
            if("PACKAGE" %in% names(x))
                paste0("  ", deparse(x[[1L]]), "(", deparse(x[[2L]]),
                       ", ..., PACKAGE = \"", z, "\")")
            else
                paste0("  ", deparse(x[[1L]]), "(", deparse(x[[2L]]), ", ...)")
        }
        base <- z %in% bases
        if(any(base)) {
            xx <- unlist(lapply(seq_along(y)[base],
                                function(i) .fmt2(y[[i]], z[i])))
            xx <- unique(xx)
            msg <- ngettext(length(xx),
                            "Foreign function call to a base package:",
                            "Foreign function calls to a base package:",
                            domain = NA)
            res <- c(res, msg, sort(xx))
        }
        if(any(!base)) {
            xx <-  unlist(lapply(seq_along(y)[!base],
                                 function(i) .fmt2(y[[i]], z[i])))
            xx <- unique(xx)
            msg <- ngettext(length(xx),
                            "Foreign function call to a different package:",
                            "Foreign function calls to a different package:",
                            domain = NA)
            res <- c(res, msg, sort(xx))
        }
    }
    if (length(zz)) {
        zz <- unique(zz)
        msg <- ngettext(length(zz),
                        "Undeclared package in foreign function calls:",
                        "Undeclared packages in foreign function calls:",
                        domain = NA)
        res <- c(res, msg, paste("  ", paste(sQuote(sort(zz)), collapse = ", ")))
    }
    if (length(other_problem)) {
    	msg <- ngettext(length(other_problem),
    		        "Registration problem:",
    		        "Registration problems:",
    		        domain = NA)
        res <- c(res, msg)
        other_desc <- attr(x, "other_desc")
        for (i in seq_along(other_problem)) {
            res <- c(res, paste0("  ", other_desc[i], ":"),
                          paste0("   ", deparse(other_problem[[i]])))
        }
    }
    res
}

### * checkS3methods

checkS3methods <-
function(package, dir, lib.loc = NULL)
{
    has_namespace <- FALSE
    ## If an installed package has a namespace, we need to record the S3
    ## methods which are registered but not exported (so that we can
    ## get() them from the right place).
    S3_reg <- character()

    ## Argument handling.
    if(!missing(package)) {
        if(length(package) != 1L)
            stop("argument 'package' must be of length 1")
        dir <- find.package(package, lib.loc)
        ## Using package installed in @code{dir} ...
        code_dir <- file.path(dir, "R")
        if(!file_test("-d", code_dir))
            stop(gettextf("directory '%s' does not contain R code",
                          dir),
                 domain = NA)
        is_base <- basename(dir) == "base"

        ## Load package into code_env.
        if(!is_base)
            .load_package_quietly(package, lib.loc)
        code_env <- .package_env(package)

        objects_in_code <- objects(envir = code_env, all.names = TRUE)

        ## Does the package have a namespace?
        if(packageHasNamespace(package, dirname(dir))) {
            has_namespace <- TRUE
            ## Determine names of declared S3 methods and associated S3
            ## generics.
            ns_S3_methods_db <- getNamespaceInfo(package, "S3methods")
            ns_S3_generics <- ns_S3_methods_db[, 1L]
            ns_S3_methods <- ns_S3_methods_db[, 3L]
            ## Determine unexported but declared S3 methods.
            S3_reg <- setdiff(ns_S3_methods, objects_in_code)
        }
    }
    else {
        if(missing(dir))
            stop("you must specify 'package' or 'dir'")
        ## Using sources from directory @code{dir} ...
        if(!file_test("-d", dir))
            stop(gettextf("directory '%s' does not exist", dir),
                 domain = NA)
        else
            dir <- file_path_as_absolute(dir)
        code_dir <- file.path(dir, "R")
        if(!file_test("-d", code_dir))
            stop(gettextf("directory '%s' does not contain R code",
                          dir),
                 domain = NA)
        is_base <- basename(dir) == "base"

        code_env <- new.env(hash = TRUE)
        dfile <- file.path(dir, "DESCRIPTION")
        meta <- if(file_test("-f", dfile))
            .read_description(dfile)
        else
            character()
        .source_assignments_in_code_dir(code_dir, code_env, meta)
        sys_data_file <- file.path(code_dir, "sysdata.rda")
        if(file_test("-f", sys_data_file)) load(sys_data_file, code_env)

        objects_in_code <- objects(envir = code_env, all.names = TRUE)

        ## Does the package have a NAMESPACE file?
        if(file.exists(file.path(dir, "NAMESPACE"))) {
            has_namespace <- TRUE
            nsInfo <- parseNamespaceFile(basename(dir), dirname(dir))
            ## Determine exported objects.
            OK <- intersect(objects_in_code, nsInfo$exports)
            for(p in nsInfo$exportPatterns)
                OK <- c(OK, grep(p, objects_in_code, value = TRUE))
            objects_in_code <- unique(OK)
            ## Determine names of declared S3 methods and associated S3
            ## generics.
            ns_S3_methods_db <- .get_namespace_S3_methods_db(nsInfo)
            ns_S3_generics <- ns_S3_methods_db[, 1L]
            ns_S3_methods <- ns_S3_methods_db[, 3L]
        }

    }

    ## Find the function objects in the given package.
    functions_in_code <-
        Filter(function(f) is.function(get(f, envir = code_env)), # get is expensive
               objects_in_code)

    ## This is the virtual groyp generics, not the members
    S3_group_generics <- .get_S3_group_generics()
    ## This includes the primitive group generics as from R 2.6.0
    S3_primitive_generics <- .get_S3_primitive_generics()

    checkArgs <- function(g, m) {
        ## Do the arguments of method m (in code_env) 'extend' those of
        ## the generic g as seen from code_env?  The method must have all
        ## arguments the generic has, with positional arguments of g in
        ## the same positions for m.
        ## Exception: '...' in the method swallows anything.
        genfun <- get(g, envir = code_env)
        gArgs <- names(formals(genfun))
        if(g == "plot") gArgs <- gArgs[-2L]
        ogArgs <- gArgs
        gm <- if(m %in% S3_reg) {
            ## See registerS3method() in ../../base/R/namespace.R.
            defenv <-
                if (g %in% S3_group_generics || g %in% S3_primitive_generics)
                    .BaseNamespaceEnv
                else {
                    if(.isMethodsDispatchOn()
                       && methods:::is(genfun, "genericFunction"))
                        genfun <- methods:::finalDefaultMethod(genfun@default)
                    if (typeof(genfun) == "closure") environment(genfun)
                    else .BaseNamespaceEnv
                }
            if(!exists(".__S3MethodsTable__.", envir = defenv,
                       inherits = FALSE)) {
                ## Happens e.g. if for some reason, we get "plot" as
                ## standardGeneric for "plot" defined from package
                ## "graphics" with its own environment which does not
                ## contain an S3 methods table ...
                return(NULL)
            }
            S3Table <- get(".__S3MethodsTable__.", envir = defenv,
                           inherits = FALSE)
            if(!exists(m, envir = S3Table)) {
                warning(gettextf("declared S3 method '%s' not found",
                                 m),
                        domain = NA,
                        call. = FALSE)
                return(NULL)
            } else get(m, envir = S3Table)
        } else get(m, envir = code_env)
        mArgs <- omArgs <- names(formals(gm))
        ## If m is a formula method, its first argument *may* be called
        ## formula.  (Note that any argument name mismatch throws an
        ## error in current S-PLUS versions.)
        if(length(grep("\\.formula$", m))) {
            if(gArgs[1L] != "...") gArgs <- gArgs[-1L]
            mArgs <- mArgs[-1L]
        }
        dotsPos <- which(gArgs == "...")
        ipos <- if(length(dotsPos))
            seq.int(from = 1L, length.out = dotsPos[1L] - 1L)
        else
            seq_along(gArgs)

        ## careful, this could match multiply in incorrect funs.
        dotsPos <- which(mArgs == "...")
        if(length(dotsPos))
            ipos <- ipos[seq.int(from = 1L, length.out = dotsPos[1L] - 1L)]
        posMatchOK <- identical(gArgs[ipos], mArgs[ipos])
        argMatchOK <- all(gArgs %in% mArgs) || length(dotsPos) > 0L
        margMatchOK <- all(mArgs %in% c("...", gArgs)) || "..." %in% ogArgs
        if(posMatchOK && argMatchOK && margMatchOK)
            NULL
        else if (g %in% c("+", "-", "*", "/", "^", "%%", "%/%", "&", "|",
                          "!", "==", "!=", "<", "<=", ">=", ">")
                 && (length(ogArgs) == length(omArgs)) )
            NULL
        else {
            l <- list(ogArgs, omArgs)
            names(l) <- c(g, m)
            list(l)
        }
    }

    all_S3_generics <-
        unique(c(Filter(function(f) .is_S3_generic(f, envir = code_env),
                        functions_in_code),
                 .get_S3_generics_as_seen_from_package(dir,
                                                       !missing(package),
                                                       FALSE),
                 S3_group_generics, S3_primitive_generics))
    ## <FIXME>
    ## Not yet:
    code_env <- .make_S3_group_generic_env(parent = code_env)
    ## </FIXME>
    code_env <- .make_S3_primitive_generic_env(parent = code_env)

    ## Now determine the 'bad' methods in the function objects of the
    ## package.
    bad_methods <- list()
    methods_stop_list <- .make_S3_methods_stop_list(basename(dir))
    for(g in all_S3_generics) {
        if(!exists(g, envir = code_env)) next
        ## Find all methods in functions_in_code for S3 generic g.
        ## <FIXME>
        ## We should really determine the name g dispatches for, see
        ## a current version of methods() [2003-07-07].  (Care is
        ## needed for internal generics and group generics.)
        ## Matching via grep() is tricky with e.g. a '$' in the name
        ## of the generic function ... hence substr().
        name <- paste0(g, ".")
        methods <-
            functions_in_code[substr(functions_in_code, 1L,
                                     nchar(name, type="c")) == name]
        ## </FIXME>
        methods <- setdiff(methods, methods_stop_list)
        if(has_namespace) {
            ## Find registered methods for generic g.
            methods <- c(methods, ns_S3_methods[ns_S3_generics == g])
        }

        for(m in methods)
            ## Both all() and all.equal() are generic.
            bad_methods <- if(g == "all") {
                m1 <- m[-grep("^all\\.equal", m)]
                c(bad_methods, if(length(m1)) checkArgs(g, m1))
            } else c(bad_methods, checkArgs(g, m))
    }

    class(bad_methods) <- "checkS3methods"
    bad_methods
}

format.checkS3methods <-
function(x, ...)
{
    format_args <- function(s)
        paste0("function(", paste(s, collapse = ", "), ")")

    .fmt <- function(entry) {
        c(paste0(names(entry)[1L], ":"),
          strwrap(format_args(entry[[1L]]), indent = 2L, exdent = 11L),
          paste0(names(entry)[2L], ":"),
          strwrap(format_args(entry[[2L]]), indent = 2L, exdent = 11L),
          "")
    }

    as.character(unlist(lapply(x, .fmt)))
}

### * checkReplaceFuns

checkReplaceFuns <-
function(package, dir, lib.loc = NULL)
{
    has_namespace <- FALSE

    ## Argument handling.
    if(!missing(package)) {
        if(length(package) != 1L)
            stop("argument 'package' must be of length 1")
        dir <- find.package(package, lib.loc)
        ## Using package installed in @code{dir} ...
        code_dir <- file.path(dir, "R")
        if(!file_test("-d", code_dir))
            stop(gettextf("directory '%s' does not contain R code",
                          dir),
                 domain = NA)
        is_base <- basename(dir) == "base"

        ## Load package into code_env.
        if(!is_base)
            .load_package_quietly(package, lib.loc)
        ## In case the package has a namespace, we really want to check
        ## all replacement functions in the package.  (If not, we need
        ## to change the code for the non-installed case to only look at
        ## exported (replacement) functions.)
        if(packageHasNamespace(package, dirname(dir))) {
            has_namespace <- TRUE
            code_env <- asNamespace(package)
            ns_S3_methods_db <- getNamespaceInfo(package, "S3methods")
        }
        else
            code_env <- .package_env(package)
    }

    else {
        if(missing(dir))
            stop("you must specify 'package' or 'dir'")
        ## Using sources from directory @code{dir} ...
        if(!file_test("-d", dir))
            stop(gettextf("directory '%s' does not exist", dir),
                 domain = NA)
        else
            dir <- file_path_as_absolute(dir)
        code_dir <- file.path(dir, "R")
        if(!file_test("-d", code_dir))
            stop(gettextf("directory '%s' does not contain R code",
                          dir),
                 domain = NA)
        is_base <- basename(dir) == "base"

        code_env <- new.env(hash = TRUE)
        dfile <- file.path(dir, "DESCRIPTION")
        meta <- if(file_test("-f", dfile))
            .read_description(dfile)
        else
            character()
        .source_assignments_in_code_dir(code_dir, code_env, meta)
        sys_data_file <- file.path(code_dir, "sysdata.rda")
        if(file_test("-f", sys_data_file)) load(sys_data_file, code_env)

        ## Does the package have a NAMESPACE file?  Note that when
        ## working on the sources we (currently?) cannot deal with the
        ## (experimental) alternative way of specifying the namespace.
        if(file.exists(file.path(dir, "NAMESPACE"))) {
            has_namespace <- TRUE
            nsInfo <- parseNamespaceFile(basename(dir), dirname(dir))
            ns_S3_methods_db <- .get_namespace_S3_methods_db(nsInfo)
        }
    }

    objects_in_code <- objects(envir = code_env, all.names = TRUE)
    replace_funs <- character()

    if(has_namespace) {
        ns_S3_generics <- ns_S3_methods_db[, 1L]
        ns_S3_methods <- ns_S3_methods_db[, 3L]
        ## S3 replacement methods from namespace registration?
        idx <- grep("<-$", ns_S3_generics)
        if(length(idx)) replace_funs <- ns_S3_methods[idx]
        ## Now remove the functions registered as S3 methods.
        objects_in_code <- setdiff(objects_in_code, ns_S3_methods)
    }

    replace_funs <-
        c(replace_funs, grep("<-", objects_in_code, value = TRUE))

    .check_last_formal_arg <- function(f) {
        arg_names <- names(formals(f))
        if(!length(arg_names))
            TRUE                        # most likely a .Primitive()
        else
            identical(arg_names[length(arg_names)], "value")
    }

    ## Find the replacement functions (which have formal arguments) with
    ## last arg not named 'value'.
    bad_replace_funs <- if(length(replace_funs)) {
        Filter(function(f) {
                   ## Always get the functions from code_env ...
                   ## Should maybe get S3 methods from the registry ...
                   f <- get(f, envir = code_env)  # get is expensive
                   if(!is.function(f)) return(FALSE)
                   ! .check_last_formal_arg(f)
               },
               replace_funs)
    } else character()

    if(.isMethodsDispatchOn()) {
        S4_generics <- .get_S4_generics(code_env)
        ## Assume that the ones with names ending in '<-' are always
        ## replacement functions.
        S4_generics <- S4_generics[grepl("<-$", names(S4_generics))]
        bad_S4_replace_methods <-
            sapply(S4_generics,
                   function(f) {
                       mlist <- .get_S4_methods_list(f, code_env)
                       ind <- !as.logical(sapply(mlist,
                                                 .check_last_formal_arg))
                       if(!any(ind))
                           character()
                       else {
                           sigs <- .make_siglist(mlist[ind])
                           sprintf("\\S4method{%s}{%s}", f, sigs)
                       }
                   })
        bad_replace_funs <-
            c(bad_replace_funs,
              unlist(bad_S4_replace_methods, use.names = FALSE))
    }

    class(bad_replace_funs) <- "checkReplaceFuns"
    bad_replace_funs
}

format.checkReplaceFuns <-
function(x, ...)
{
    if(length(x))
        .pretty_format(unclass(x))
    else
        character()
}

### * checkTnF

checkTnF <-
function(package, dir, file, lib.loc = NULL)
{
    code_files <- docs_files <- character()

    ## Argument handling.
    if(!missing(package)) {
        if(length(package) != 1L)
            stop("argument 'package' must be of length 1")
        ## Using package installed in @code{dir} ...
        dir <- find.package(package, lib.loc)
        if(file.exists(file.path(dir, "R", "all.rda"))) {
            warning("cannot check R code installed as image")
        }
        code_file <- file.path(dir, "R", package)
        if(file.exists(code_file))      # could be data-only
            code_files <- code_file
        example_dir <- file.path(dir, "R-ex")
        if(file_test("-d", example_dir)) {
            code_files <- c(code_files,
                            list_files_with_exts(example_dir, "R"))
        }
    }
    else if(!missing(dir)) {
        ## Using sources from directory @code{dir} ...
        if(!file_test("-d", dir))
            stop(gettextf("directory '%s' does not exist", dir),
                 domain = NA)
        else
            dir <- file_path_as_absolute(dir)
        code_dir <- file.path(dir, "R")
        if(file_test("-d", code_dir))   # could be data-only
            code_files <- list_files_with_type(code_dir, "code")
        docs_dir <- file.path(dir, "man")
        if(file_test("-d", docs_dir))
            docs_files <- list_files_with_type(docs_dir, "docs")
    }
    else if(!missing(file)) {
        if(!file_test("-f", file))
            stop(gettextf("file '%s' does not exist", file),
                 domain = NA)
        else
            code_files <- file
    }
    else
        stop("you must specify 'package', 'dir' or 'file'")

    find_TnF_in_code <- function(file, txt) {
        ## If 'txt' is given, it contains the extracted examples from
        ## the R documentation file 'file'.  Otherwise, 'file' gives a
        ## file with (just) R code.
        matches <- list()
        TnF <- c("T", "F")
        find_bad_exprs <- function(e, p) {
            if(is.name(e)
               && (as.character(e) %in% TnF)
               && !is.null(p)) {
                ## Need the 'list()' to deal with T/F in function
                ## arglists which are pairlists ...
                matches <<- c(matches, list(p))
            }
            else if(is.recursive(e)) {
                for(i in seq_along(e)) Recall(e[[i]], e)
            }
        }
        exprs <- if(missing(txt))
            tryCatch(parse(file = file, n = -1L),
                     error = function(e)
                     stop(gettextf("parse error in file '%s':\n",
                                   file,
                                   .massage_file_parse_error_message(conditionMessage(e))),
                          domain = NA, call. = FALSE))
        else
            tryCatch(parse(text = txt),
                     error = function(e)
                     stop(gettextf("parse error in examples from file '%s':\n",
                                   file, conditionMessage(e)),
                          domain = NA, call. = FALSE))
        for(i in seq_along(exprs))
            find_bad_exprs(exprs[[i]], NULL)
        matches
    }

    bad_exprs <- list()
    for(file in code_files) {
        exprs <- find_TnF_in_code(file)
        if(length(exprs)) {
            exprs <- list(exprs)
            names(exprs) <- file
            bad_exprs <- c(bad_exprs, exprs)
        }
    }
    for(file in docs_files) {
        Rd <- prepare_Rd(file, defines = .Platform$OS.type)
        txt <- .Rd_get_example_code(Rd)
        exprs <- find_TnF_in_code(file, txt)
        if(length(exprs)) {
            exprs <- list(exprs)
            names(exprs) <- file
            bad_exprs <- c(bad_exprs, exprs)
        }
    }
    class(bad_exprs) <- "checkTnF"
    bad_exprs
}

format.checkTnF <-
function(x, ...)
{
    .fmt <- function(fname) {
        xfname <- x[[fname]]
        c(gettextf("File '%s':", fname),
          unlist(lapply(seq_along(xfname),
                        function(i) {
                            strwrap(gettextf("found T/F in %s",
                                             paste(deparse(xfname[[i]]),
                                                   collapse = "")),
                                    exdent = 4L)
                        })),
          "")
    }

    as.character(unlist(lapply(names(x), .fmt)))
}

### * .check__depends

## changed in 2.3.0 to refer to a source dir.


.check_package_depends <-
function(dir, force_suggests = TRUE, check_incoming = FALSE)
{
    .check_dependency_cycles <-
        function(db, available = available.packages(),
                 dependencies = c("Depends", "Imports", "LinkingTo"))
        {
            ## given a package, find its recursive dependencies.
            ## We want the dependencies of the current package,
            ## not of a version on the repository.
            pkg <- db[["Package"]]
            this <- db[dependencies]; names(this) <- dependencies;
            known <- setdiff(utils:::.clean_up_dependencies(this), "R")
            info <- available[, dependencies, drop = FALSE]
            rn <- rownames(info)
            deps <- function(p) {
                if(!(p %in% rn)) return(character())
                this <- utils:::.clean_up_dependencies(info[p, ])
                setdiff(this, "R")
            }
            extra <- known
            repeat {
                extra <- unlist(lapply(extra, deps))
                extra <- setdiff(extra, known)
                if(!length(extra)) break
                known <- c(known, extra)
            }
            known
        }

    if(length(dir) != 1L)
        stop("argument 'package' must be of length 1")

    ## We definitely need a valid DESCRIPTION file.
    db <- .read_description(file.path(dir, "DESCRIPTION"))

    dir_name <- basename(dir)
    package_name <- db["Package"]
    if(!identical(package_name, dir_name) &&
       (!is.character(package_name) || !nzchar(package_name))) {
	message(sprintf(
            "package name '%s' seems invalid; using directory name '%s' instead",
            package_name, dir_name))
	package_name <- dir_name
    }

    bad_depends <- list()
    ## and we cannot have cycles
    ## this check needs a package db from repository(s), so
    if(!any(grepl("@CRAN@", getOption("repos")))) {
        ad <- .check_dependency_cycles(db)
        pkgname <- db[["Package"]]
        if(pkgname %in% ad)
            bad_depends$all_depends <- setdiff(ad, pkgname)
    } else if (check_incoming)
        bad_depends$skipped <-
            "  No repository set, so cyclic dependency check skipped"

    ldepends <-  .get_requires_with_version_from_package_db(db, "Depends")
    limports <-  .get_requires_with_version_from_package_db(db, "Imports")
    llinks <-  .get_requires_with_version_from_package_db(db, "LinkingTo")
    lsuggests <- .get_requires_with_version_from_package_db(db, "Suggests")
    ## NB: no one checks version for 'Enhances'.
    lenhances <- .get_requires_with_version_from_package_db(db, "Enhances")

    depends <- sapply(ldepends, `[[`, 1L)
    imports <- sapply(limports, `[[`, 1L)
    links <- sapply(llinks, `[[`, 1L)
    suggests <- sapply(lsuggests, `[[`, 1L)

    standard_package_names <- .get_standard_package_names()

    ## Are all packages listed in Depends/Suggests/Imports/LinkingTo installed?
    lreqs <- c(ldepends, limports, llinks,
               if(force_suggests) lsuggests)
    lreqs2 <- c(if(!force_suggests) lsuggests, lenhances)
    if(length(c(lreqs, lreqs2))) {
        ## Do this directly for speed.
        installed <- character()
        installed_in <- character()
        for(lib in .libPaths()) {
            pkgs <- list.files(lib)
            pkgs <- pkgs[file.access(file.path(lib, pkgs, "DESCRIPTION"), 4) == 0]
            installed <- c(installed, pkgs)
            installed_in <- c(installed_in, rep.int(lib, length(pkgs)))
        }
        if (length(lreqs)) {
            reqs <- unique(sapply(lreqs, `[[`, 1L))
            reqs <- setdiff(reqs, installed)
            m <- reqs %in% standard_package_names$stubs
            if(length(reqs[!m])) {
                bad <- reqs[!m]
                ## EDanalysis has a package in all of Depends, Imports, Suggests.
                bad1 <-  bad[bad %in% c(depends, imports, links)]
                if(length(bad1))
                    bad_depends$required_but_not_installed <- bad1
                bad2 <-  setdiff(bad, bad1)
                if(length(bad2))
                    bad_depends$suggested_but_not_installed <- bad2
            }
            if(length(reqs[m]))
                bad_depends$required_but_stub <- reqs[m]
            ## now check versions
            have_ver <- unlist(lapply(lreqs, function(x) length(x) == 3L))
            lreqs3 <- lreqs[have_ver]
            if(length(lreqs3)) {
                bad <- character()
                for (r in lreqs3) {
                    pkg <- r[[1L]]
                    op <- r[[2L]]
                    where <- which(installed == pkg)
                    if(!length(where)) next
                    ## want the first one
                    desc <- readRDS(file.path(installed_in[where[1L]], pkg,
                                              "Meta", "package.rds"))
                    current <- desc$DESCRIPTION["Version"]
                    target <- as.package_version(r[[3L]])
                    if(eval(parse(text = paste("!(current", op, "target)"))))
                        bad <- c(bad, pkg)
                }
                if(length(bad))
                    bad_depends$required_but_obsolete <- bad
            }
        }
        if (length(lenhances)) {
            m <- setdiff(sapply(lenhances, `[[`, 1L), installed)
            if(length(m))
                bad_depends$enhances_but_not_installed <- m
        }
        if (!force_suggests && length(lsuggests)) {
            m <- setdiff(sapply(lsuggests, `[[`, 1L), installed)
            if(length(m))
                bad_depends$suggests_but_not_installed <- m
        }
    }
    ## FIXME: is this still needed now we do dependency analysis?
    ## Are all vignette dependencies at least suggested or equal to
    ## the package name?

    ## This is a check for old-location vignettes.
    ## If the package itself is the VignetteBuilder,
    ## we may not have installed it yet.
    defer <- package_name %in%  db["VignetteBuilder"]
    vigns <- pkgVignettes(dir = dir, subdirs = file.path("inst", "doc"),
                          check = !defer)

    if(length(vigns$msg))
        bad_depends$bad_engine <- vigns$msg
    if (!is.null(vigns) && length(vigns$docs) > 0L) {
        reqs <- unique(unlist(.build_vignette_index(vigns)$Depends))
        ## For the time being, ignore base packages missing from the
        ## DESCRIPTION dependencies even if explicitly given as vignette
        ## dependencies.
        reqs <- setdiff(reqs,
                        c(depends, imports, suggests, package_name,
                          standard_package_names$base))
        if(length(reqs))
            bad_depends$missing_vignette_depends <- reqs
    }

    ## Are all namespace dependencies listed as package dependencies?
    if(file_test("-f", file.path(dir, "NAMESPACE"))) {
        reqs <- .get_namespace_package_depends(dir)
        ## <FIXME>
        ## Not clear whether we want to require *all* namespace package
        ## dependencies listed in DESCRIPTION, or e.g. just the ones on
        ## non-base packages.  Do the latter for time being ...
        ## Actually we need to know at least about S4-using packages,
        ## since we need to reinstall if those change.
        allowed_imports <-
            setdiff(standard_package_names$base, c("methods", "stats4"))
        reqs <- setdiff(reqs, c(imports, depends, allowed_imports))
        if(length(reqs))
            bad_depends$missing_namespace_depends <- reqs
    }

    ## Check for excessive 'Depends'

    deps <- setdiff(depends, c("R", "base", "datasets", "grDevices", "graphics",
                               "methods", "utils", "stats"))
    if(length(deps) > 5L) bad_depends$many_depends <- deps

    class(bad_depends) <- "check_package_depends"
    bad_depends
}

format.check_package_depends <-
function(x, ...)
{
    c(character(),
      if(length(x$skipped)) c(x$skipped, ""),
      if(length(x$all_depends)) {
          c("There is circular dependency in the installation order:",
            .pretty_format2("  One or more packages in", x$all_depends),
            "  depend on this package (for the versions on the repositories).",
            "")
      },
      if(length(bad <- x$required_but_not_installed) > 1L) {
          c(.pretty_format2("Packages required but not available:", bad), "")
      } else if(length(bad)) {
          c(sprintf("Package required but not available: %s", sQuote(bad)), "")
      },
      if(length(bad <- x$suggested_but_not_installed) > 1L) {
          c(.pretty_format2("Packages suggested but not available:", bad), "")
      } else if(length(bad)) {
          c(sprintf("Package suggested but not available: %s", sQuote(bad)), "")
      },
      if(length(bad <- x$required_but_obsolete) > 1L) {
          c(.pretty_format2("Packages required and available but unsuitable versions:",
                            bad),
            "")
      } else if(length(bad)) {
          c(sprintf("Package required and available but unsuitable version: %s", sQuote(bad)),
            "")
      },
      if(length(bad <- x$required_but_stub) > 1L) {
          c("Former standard packages required but now defunct:",
            .pretty_format(bad),
            "")
      } else if(length(bad)) {
          c(sprintf("Former standard package required but now defunct: %s",
                    sQuote(bad)), "")
      },
      if(length(bad <- x$suggests_but_not_installed) > 1L) {
          c(.pretty_format2("Packages suggested but not available for checking:",
                            bad),
            "")
      } else if(length(bad)) {
          c(sprintf("Package suggested but not available for checking: %s",
                     sQuote(bad)),
            "")
      },
      if(length(bad <- x$enhances_but_not_installed) > 1L) {
          c(.pretty_format2("Packages which this enhances but not available for checking:",
                            bad),
            "")
      } else if(length(bad)) {
          c(sprintf("Package which this enhances but not available for checking: %s", sQuote(bad)),
            "")
      },
      if(length(bad <- x$missing_vignette_depends)) {
          c(if(length(bad) > 1L) {
                c("Vignette dependencies not required:", .pretty_format(bad))
            } else {
                sprintf("Vignette dependencies not required: %s", sQuote(bad))
            },
            strwrap(gettextf("Vignette dependencies (%s entries) must be contained in the DESCRIPTION Depends/Suggests/Imports entries.",
                             "\\VignetteDepends{}")),
            "")
      },
      if(length(bad <- x$missing_namespace_depends) > 1L) {
          c(.pretty_format2("Namespace dependencies not required:", bad), "")
      } else if(length(bad)) {
          c(sprintf("Namespace dependency not required: %s", sQuote(bad)), "")
      },
      if(length(y <- x$many_depends)) {
          c(.pretty_format2("Depends: includes the non-default packages:", y),
            strwrap(paste("Adding so many packages to the search path",
                          "is excessive",
                          "and importing selectively is preferable."
                          , collapse = ", ")),
            "")
      },
      if(length(y <- x$bad_engine)) {
          c(y, "")
      }
      )
}

### * .check_package_description

.check_package_description <-
function(dfile, strict = FALSE)
{
    dfile <- file_path_as_absolute(dfile)
    db <- .read_description(dfile)

    standard_package_names <- .get_standard_package_names()

    valid_package_name_regexp <-
        .standard_regexps()$valid_package_name
    valid_package_version_regexp <-
        .standard_regexps()$valid_package_version

    is_base_package <-
        !is.na(priority <- db["Priority"]) && priority == "base"

    out <- list()                       # For the time being ...

    ## Check encoding-related things first.

    ## All field tags must be ASCII.
    if(any(ind <- !.is_ASCII(names(db))))
        out$fields_with_non_ASCII_tags <- names(db)[ind]
    ## For all fields used by the R package management system, values
    ## must be ASCII as well (so that the RPM works in a C locale).
    ASCII_fields <- c(.get_standard_repository_db_fields(),
                      "Encoding", "License")
    ASCII_fields <- intersect(ASCII_fields, names(db))
    if(any(ind <- !.is_ASCII(db[ASCII_fields])))
        out$fields_with_non_ASCII_values <- ASCII_fields[ind]

    ## Determine encoding and re-encode if necessary and possible.
    if("Encoding" %in% names(db)) {
        encoding <- db["Encoding"]
        if((! Sys.getlocale("LC_CTYPE") %in% c("C", "POSIX")))
            db <- iconv(db, encoding, sub = "byte")
    }
    else if(!all(.is_ISO_8859(db))) {
        ## No valid Encoding metadata.
        ## Determine whether we can assume Latin1.
        out$missing_encoding <- TRUE
    }

    if(any(is.na(nchar(db, "c", TRUE)))) {
        ## Ouch, invalid in the current locale.
        ## (Can only happen in a MBCS locale.)
        ## Try re-encoding from Latin1.
        db <- iconv(db, "latin1")
    }

    ## Check Authors@R and expansion if needed.
    if(!is.na(aar <- db["Authors@R"]) &&
       (is.na(db["Author"]) || is.na(db["Maintainer"]))) {
        res <- .check_package_description_authors_at_R_field(aar)
        if(is.na(db["Author"]) &&
           !is.null(s <- attr(res, "Author")))
            db["Author"] <- s
        if(is.na(db["Maintainer"]) &&
           !is.null(s <- attr(res, "Maintainer")))
            db["Maintainer"] <- s
        mostattributes(res) <- NULL     # Keep names.
        out <- c(out, res)
    }

    val <- package_name <- db["Package"]
    if(!is.na(val)) {
        tmp <- character()
        ## We allow 'R', which is not a valid package name.
        if(!grepl(sprintf("^(R|%s)$", valid_package_name_regexp), val))
            tmp <- c(tmp, gettext("Malformed package name"))
        if(!is_base_package) {
            if(val %in% standard_package_names$base)
                tmp <- c(tmp,
                         c("Invalid package name.",
                           "This is the name of a base package."))
            else if(val %in% standard_package_names$stubs)
                tmp <- c(tmp,
                         c("Invalid package name.",
                           "This name was used for a base package and is remapped by library()."))
        }
        if(length(tmp))
            out$bad_package <- tmp
    }
    if(!is.na(val <- db["Version"])
       && !is_base_package
       && !grepl(sprintf("^%s$", valid_package_version_regexp), val))
        out$bad_version <- val
    if(!is.na(val <- db["Maintainer"])
       && !grepl(.valid_maintainer_field_regexp, val))
        out$bad_maintainer <- val

    ## Optional entries in DESCRIPTION:
    ##   Depends/Suggests/Imports/Enhances, Namespace, Priority.
    ## These must be correct if present.

    val <- db[match(c("Depends", "Suggests", "Imports", "Enhances"),
                    names(db), nomatch = 0L)]
    if(length(val)) {
        depends <- .strip_whitespace(unlist(strsplit(val, ",")))
        bad_dep_entry <- bad_dep_op <- bad_dep_version <- character()
        dep_regexp <-
            paste0("^[[:space:]]*",
                   paste0("(R|", valid_package_name_regexp, ")"),
                   "([[:space:]]*\\(([^) ]+)[[:space:]]+([^) ]+)\\))?",
                   "[[:space:]]*$")
        for(dep in depends) {
            if(!grepl(dep_regexp, dep)) {
                ## Entry does not match the regexp.
                bad_dep_entry <- c(bad_dep_entry, dep)
                next
            }
            if(nzchar(sub(dep_regexp, "\\2", dep))) {
                ## If not just a valid package name ...
                if(!sub(dep_regexp, "\\3", dep) %in%
                   c("<=", ">=", "<", ">", "==", "!="))
                    bad_dep_op <- c(bad_dep_op, dep)
                else if(grepl("^[[:space:]]*R", dep)) {
                    if(!grepl(sprintf("^(r[0-9]+|%s)$",
                                      valid_package_version_regexp),
                              sub(dep_regexp, "\\4", dep)))
                    bad_dep_version <- c(bad_dep_version, dep)
                } else if(!grepl(sprintf("^%s$",
                                         valid_package_version_regexp),
                                 sub(dep_regexp, "\\4", dep)))
                    bad_dep_version <- c(bad_dep_version, dep)
            }
        }
        if(length(c(bad_dep_entry, bad_dep_op, bad_dep_version)))
            out$bad_depends_or_suggests_or_imports <-
                list(bad_dep_entry = bad_dep_entry,
                     bad_dep_op = bad_dep_op,
                     bad_dep_version = bad_dep_version)
    }
    if(strict && !is.na(val <- db["VignetteBuilder"])) {
        depends <- .strip_whitespace(unlist(strsplit(val, ",")))
        if(length(depends) < 1L || !all(grepl("^[[:alnum:].]*$", depends)))
            out$bad_vignettebuilder <- TRUE
    }
    if(!is.na(val <- db["Priority"])
       && !is.na(package_name)
       && (tolower(val) %in% c("base", "recommended", "defunct-base"))
       && !(package_name %in% unlist(standard_package_names)))
        out$bad_priority <- val

    class(out) <- "check_package_description"

    out
}

print.check_package_description <-
function(x, ...)
{
    if(length(x$missing_encoding))
        writeLines(c(gettext("Unknown encoding"), ""))

    if(length(x$fields_with_non_ASCII_tags)) {
        writeLines(gettext("Fields with non-ASCII tags:"))
        .pretty_print(x$fields_with_non_ASCII_tags)
        writeLines(c(gettext("All field tags must be ASCII."), ""))
    }

    if(length(x$fields_with_non_ASCII_values)) {
        writeLines(gettext("Fields with non-ASCII values:"))
        .pretty_print(x$fields_with_non_ASCII_values)
        writeLines(c(gettext("These fields must have ASCII values."), ""))
    }

    s <- .format_check_package_description_authors_at_R_field_results(x)
    if(length(s))
        writeLines(c(s, ""))

##     if(length(x$missing_required_fields)) {
##         writeLines(gettext("Required fields missing or empty:"))
##         .pretty_print(x$missing_required_fields)
##         writeLines("")
##     }

    if(length(x$bad_package))
        writeLines(c(strwrap(x$bad_package), ""))

    if(length(x$bad_version))
        writeLines(c(gettext("Malformed package version."), ""))

    if(length(x$bad_maintainer))
        writeLines(c(gettext("Malformed maintainer field."), ""))

    if(any(as.integer(sapply(x$bad_depends_or_suggests_or_imports, length)) > 0L )) {
        bad <- x$bad_depends_or_suggests_or_imports
        writeLines(gettext("Malformed Depends or Suggests or Imports or Enhances field."))
        if(length(bad$bad_dep_entry)) {
            tmp <- c(gettext("Offending entries:"),
                     paste(" ", bad$bad_dep_entry),
                     strwrap(gettextf("Entries must be names of packages optionally followed by '<=' or '>=', white space, and a valid version number in parentheses.")))
            writeLines(tmp)
        }
        if(length(bad$bad_dep_op)) {
            tmp <- c(gettext("Entries with infeasible comparison operator:"),
                     paste(" ", bad$bad_dep_entry),
                     strwrap(gettextf("Only operators '<=' and '>=' are possible.")))

            writeLines(tmp)
        }
        if(length(bad$bad_dep_version)) {
            tmp <- c(gettext("Entries with infeasible version number:"),
                     paste(" ", bad$bad_dep_version),
                     strwrap(gettextf("Version numbers must be sequences of at least two non-negative integers, separated by single '.' or '-'.")))
            writeLines(tmp)
        }
        writeLines("")
    }
    if(identical(x$bad_vignettebuilder, TRUE)) {
        writeLines(c(gettext("Invalid VignetteBuilder field."),
                     strwrap(gettextf("This field must contain one or more packages (and no version requirement).")),
                     ""))
    }

    if(length(x$bad_priority))
        writeLines(c(gettext("Invalid Priority field."),
                     strwrap(gettextf("Packages with priorities 'base' or 'recommended' or 'defunct-base' must already be known to R.")),
                     ""))

    if(any(as.integer(sapply(x, length)) > 0L))
        writeLines(c(strwrap(gettextf("See the information on DESCRIPTION files in section 'Creating R packages' of the 'Writing R Extensions' manual.")),
                     ""))

    invisible(x)
}

### * .check_package_description2

.check_package_description2 <-
function(dfile)
{
    dfile <- file_path_as_absolute(dfile)
    db <- .read_description(dfile)
    depends <- .get_requires_from_package_db(db, "Depends")
    imports <- .get_requires_from_package_db(db, "Imports")
    suggests <- .get_requires_from_package_db(db, "Suggests")
    enhances <- .get_requires_from_package_db(db, "Enhances")
    allpkgs <- c(depends, imports, suggests, enhances)
    out <- unique(allpkgs[duplicated(allpkgs)])
    links <- missing_incs <- character()
    llinks <-  .get_requires_with_version_from_package_db(db, "LinkingTo")
    have_src <- TRUE # dummy
    if(length(llinks)) {
        ## This is pointless unless there is compilable code
        dir.exists <- function(x) !is.na(isdir <- file.info(x)$isdir) & isdir
        have_src <- dir.exists(file.path(dirname(dfile), "src"))

        ## See if this is installable under 3.0.1:
        ## if so check for versioned specs
        deps <- .split_description(db, verbose = TRUE)$Rdepends2
        status <- 0L
        current <- as.numeric_version("3.0.1")
        for(depends in deps) {
            if(!depends$op %in% c("<=", ">=", "<", ">", "==", "!=")) next
            status <- if(inherits(depends$version, "numeric_version"))
                !do.call(depends$op, list(current, depends$version))
            else {
                ver <- R.version
                if (ver$status %in% c("", "Patched")) FALSE
                else !do.call(depends$op,
                              list(ver[["svn rev"]],
                                   as.numeric(sub("^r", "", depends$version))))
            }
        }
        if(!status) {
            llinks <- llinks[sapply(llinks, length) > 1L]
            if(length(llinks)) links <- sapply(llinks, `[[`, 1L)
        }
        ## and check if we can actually link to these.
        llinks <-  .get_requires_from_package_db(db, "LinkingTo")
        incs <- lapply(llinks, function(x) system.file("include", package = x))
        missing_incs <- as.vector(llinks[!nzchar(incs)])
    }
    out <- list(duplicates = unique(allpkgs[duplicated(allpkgs)]),
                bad_links = links, missing_incs = missing_incs,
                have_src = have_src)
    class(out) <- "check_package_description2"
    out
}

format.check_package_description2 <- function(x, ...)
{
    c(if(length(xx <- x$duplicates)) {
        c(if(length(xx) > 1L)
          "Packages listed in more than one of Depends, Imports, Suggests, Enhances:"
        else
          "Package listed in more than one of Depends, Imports, Suggests, Enhances:",
          paste(c(" ", sQuote(xx)), collapse = " "),
          "A package should be listed in only one of these fields.")
    },
      if(!x$have_src) "'LinkingTo' field is unused: package has no 'src' directory",
      if(length(xx <- x$bad_links)) {
          if(length(xx) > 1L)
              c("Versioned 'LinkingTo' values for",
                paste(c(" ", sQuote(xx)), collapse = " "),
                "are only usable in R >= 3.0.2")
          else
              sprintf("Versioned 'LinkingTo' value for %s is only usable in R >= 3.0.2",
                      sQuote(xx))
      },
      if(x$have_src && length(xx <- x$missing_incs)) {
          if(length(xx) > 1L)
              c("'LinkingTo' for",
                paste(c(" ", sQuote(xx)), collapse = " "),
                "are unused as they have no 'include' directory")
          else
              sprintf("'LinkingTo' for %s is unused as it has no 'include' directory", sQuote(xx))
      })
}

.check_package_description_authors_at_R_field <-
function(aar, strict = FALSE)
{
    out <- list()
    if(is.na(aar)) return(out)
    aar <- tryCatch(utils:::.read_authors_at_R_field(aar),
                    error = identity)
    if(inherits(aar, "error")) {
        out$bad_authors_at_R_field <- conditionMessage(aar)
    } else {
        ## Check whether we can expand to something non-empty.
        s <- tryCatch(utils:::.format_authors_at_R_field_for_author(aar),
                      error = identity)
        if(inherits(s, "error")) {
            out$bad_authors_at_R_field_for_author <-
                conditionMessage(s)
        } else {
            if(s == "")
                out$bad_authors_at_R_field_has_no_author <- TRUE
            else {
                attr(out, "Author") <- s
                if(strict) {
                    ## Specifically check for persons with missing or
                    ## non-standard roles.
                    s <- format(aar[sapply(aar,
                                           utils:::.format_person_for_plain_author_spec)
                                    == ""])
                    if(length(s))
                        out$bad_authors_at_R_field_has_author_without_role <- s
                }
            }
        }
        s <- tryCatch(utils:::.format_authors_at_R_field_for_maintainer(aar),
                      error = identity)
        if(inherits(s, "error")) {
            out$bad_authors_at_R_field_for_maintainer <-
                conditionMessage(s)
        } else {
            if(s == "")
                out$bad_authors_at_R_field_has_no_maintainer <- TRUE
            else
                attr(out, "Maintainer") <- s
        }
    }
    out
}

.format_check_package_description_authors_at_R_field_results <-
function(x)
{
    c(character(),
      if(length(bad <- x[["bad_authors_at_R_field"]])) {
          c(gettext("Malformed Authors@R field:"),
            paste(" ", bad))
      },
      if(length(bad <- x[["bad_authors_at_R_field_for_author"]])) {
          c(gettext("Cannot extract Author field from Authors@R field:"),
            paste(" ", bad))
      },
      if(length(x[["bad_authors_at_R_field_has_no_author"]])) {
          gettext("Authors@R field gives no person with author role.")
      },
      if(length(bad <-
                x[["bad_authors_at_R_field_has_author_without_role"]])) {
          c(gettext("Authors@R field gives persons with no valid roles:"),
            paste(" ", bad))
      },
      if(length(bad <- x[["bad_authors_at_R_field_for_maintainer"]])) {
          c(gettext("Cannot extract Maintainer field from Authors@R field:"),
            paste(" ", bad))
      },
      if(length(x[["bad_authors_at_R_field_has_no_maintainer"]])) {
          gettext("Authors@R field gives no person with maintainer role and email address.")
      }
      )
}

### * .check_package_description_encoding

.check_package_description_encoding <-
function(dfile)
{
    dfile <- file_path_as_absolute(dfile)
    db <- .read_description(dfile)
    out <- list()

    ## Check encoding-related things.

    ## All field tags must be ASCII.
    if(any(ind <- !.is_ASCII(names(db))))
        out$fields_with_non_ASCII_tags <- names(db)[ind]

    if(! "Encoding" %in% names(db)) {
        ind <- !.is_ASCII(db)
        if(any(ind)) {
            out$missing_encoding <- TRUE
            out$fields_with_non_ASCII_values <- names(db)[ind]
        }
    } else {
        enc <- db[["Encoding"]]
        if (! enc %in% c("latin1", "latin2", "UTF-8"))
            out$non_portable_encoding <- enc
    }

    class(out) <- "check_package_description_encoding"
    out
}

format.check_package_description_encoding <-
function(x, ...)
{
    c(character(),
      if(length(x$non_portable_encoding)) {
          c(gettextf("Encoding '%s' is not portable",
                     x$non_portable_encoding),
            "")
      },
      if(length(x$missing_encoding)) {
          gettext("Unknown encoding with non-ASCII data")
      },
      if(length(x$fields_with_non_ASCII_tags)) {
          c(gettext("Fields with non-ASCII tags:"),
            .pretty_format(x$fields_with_non_ASCII_tags),
            gettext("All field tags must be ASCII."),
            "")
      },
      if(length(x$fields_with_non_ASCII_values)) {
          c(gettext("Fields with non-ASCII values:"),
            .pretty_format(x$fields_with_non_ASCII_values))
      },
      if(any(as.integer(sapply(x, length)) > 0L)) {
          c(strwrap(gettextf("See the information on DESCRIPTION files in section 'Creating R packages' of the 'Writing R Extensions' manual.")),
            "")
      })
}

### * .check_package_license

.check_package_license <-
function(dfile, dir)
{
    dfile <- file_path_as_absolute(dfile)
    db <- .read_description(dfile)

    if(missing(dir))
        dir <- dirname(dfile)

    ## Analyze the license information here.
    ## Cannot easily do this in .check_package_description(), as R CMD
    ## check's R::Utils::check_package_description() takes any output
    ## from this as indication of an error.

    out <- list()
    if(!is.na(val <- db["License"])) {
        ## If there is no License field, .check_package_description()
        ## will give an error.
        status <- analyze_license(val)
        ok <- status$is_canonical
        ## This analyzes the license specification but does not verify
        ## whether pointers exist, so let us do this here.
        if(length(pointers <- status$pointers)) {
            bad_pointers <-
                pointers[!file_test("-f", file.path(dir, pointers))]
            if(length(bad_pointers)) {
                status$bad_pointers <- bad_pointers
                ok <- FALSE
            }
        }
        depr <- "BSD"
        if(any(status$components %in% depr)) {
            status$deprecated <- intersect(status$components, depr)
            ok <- FALSE
        }
        ## Components with extensions but not extensible:
        if(length(extensions <- status$extensions) &&
           any(ind <- !extensions$extensible)) {
            status$bad_extensions <- extensions$components[ind]
            ok <- FALSE
        }
        ## Components which need extensions:
        if(any(ind <- status$components %in%
               c("MIT", "BSD_2_clause", "BSD_3_clause"))) {
            status$miss_extension <- status$components[ind]
            ok <- FALSE
        }
        ## Could always return the analysis results and not print them
        ## if ok, but it seems more standard to only return trouble.
        if(!ok)
            out <- c(list(license = val), status)
    }

    class(out) <- "check_package_license"
    out
}

format.check_package_license <-
function(x, ...)
{
    if(!length(x))
        return(character())

    check <- Sys.getenv("_R_CHECK_LICENSE_")
    check <- if(check %in% c("maybe", ""))
        (!(x$is_standardizable)
         || length(x$bad_pointers)
         || length(x$bad_extensions))
    else
        isTRUE(as.logical(check))
    if(!check)
        return(character())

    c(character(),
      if(!(x$is_canonical)) {
          c(gettext("Non-standard license specification:"),
            strwrap(x$license, indent = 2L, exdent = 2L),
            gettextf("Standardizable: %s", x$is_standardizable),
            if(x$is_standardizable) {
                c(gettext("Standardized license specification:"),
                  strwrap(x$standardization, indent = 2L, exdent = 2L))
            })
      },
      if(length(y <- x$deprecated)) {
          c(gettextf("Deprecated license: %s",
                     paste(y, collapse = " ")))
      },
      if(length(y <- x$bad_pointers)) {
          c(gettextf("Invalid license file pointers: %s",
                     paste(y, collapse = " ")))
      },
      if(length(y <- x$bad_extensions)) {
          c(gettext("License components with restrictions not permitted:"),
            paste(" ", y))
      },
      if(length(y <- x$miss_extension)) {
          c(gettext("Licenses which are templates and need '+ file LICENSE':"),
            paste(" ", y))
      }
      )
}

### * .check_make_vars

.check_make_vars <-
function(dir, makevars = c("Makevars.in", "Makevars"))
{
    bad_flags <- list()
    class(bad_flags) <- "check_make_vars"

    paths <- file.path(dir, makevars)
    paths <- paths[file_test("-f", paths)]
    if(!length(paths)) return(bad_flags)
    bad_flags$paths <- file.path("src", basename(paths))
    ## Makevars could be used with --no-configure
    ## and maybe configure does not even use src/Makevars.in
    mfile <- paths[1L]
    make <- Sys.getenv("MAKE")
    if(make == "") make <- "make"
    command <- sprintf("%s -f %s -f %s -f %s",
                       make,
                       shQuote(file.path(R.home("share"), "make",
                                         "check_vars_ini.mk")),
                       shQuote(mfile),
                       shQuote(file.path(R.home("share"), "make",
                                         "check_vars_out.mk")))
    lines <- suppressWarnings(tryCatch(system(command, intern = TRUE,
                                              ignore.stderr = TRUE),
                                       error = identity))
    if(!length(lines) || inherits(lines, "error"))
        return(bad_flags)

    prefixes <- c("CPP", "C", "CXX", "F", "FC", "OBJC", "OBJCXX")

    uflags_re <- sprintf("^(%s)FLAGS: *(.*)$",
                         paste(prefixes, collapse = "|"))
    pos <- grep(uflags_re, lines)
    ind <- (sub(uflags_re, "\\2", lines[pos]) != "-o /dev/null")
    if(any(ind))
        bad_flags$uflags <- lines[pos[ind]]

    ## Try to be careful ...
    pflags_re <- sprintf("^PKG_(%s)FLAGS: ",
                         paste(prefixes, collapse = "|"))
    lines <- lines[grepl(pflags_re, lines)]
    names <- sub(":.*", "", lines)
    lines <- sub(pflags_re, "", lines)
    flags <- strsplit(lines, "[[:space:]]+")
    ## Bad flags:
    ##   -O*
    ##      (BDR: for example Sun Fortran compilers used to accept -O
    ##      but not -O2, and VC++ accepts -Ox (literal x) but not -O.)
    ##   -Wall -pedantic -ansi -traditional -std* -f* -m* [GCC]
    ##   -x [Solaris]
    ##   -q [AIX]
    ## It is hard to think of anything apart from -I* and -D* that is
    ## safe for general use ...
    bad_flags_regexp <-
        sprintf("^-(%s)$",
                paste(c("O.*",
                        "W",
                        "W[^l].*", # -Wl, might just be portable
                        "ansi", "pedantic", "traditional",
                        "f.*", "m.*", "std.*",
                        "x",
                        "q"),
                      collapse = "|"))
    for(i in seq_along(lines)) {
        bad <- grep(bad_flags_regexp, flags[[i]], value = TRUE)
        if(length(bad))
            bad_flags$pflags <-
                c(bad_flags$pflags,
                  structure(list(bad), names = names[i]))
    }

    bad_flags
}

format.check_make_vars <-
function(x, ...)
{
    .fmt <- function(x) {
        s <- Map(c,
                 gettextf("Non-portable flags in variable '%s':",
                          names(x)),
                 sprintf("  %s", lapply(x, paste, collapse = " ")))
        as.character(unlist(s))
    }

    c(character(),
      if(length(bad <- x$pflags)) .fmt(bad),
      if(length(bad <- x$uflags)) {
          c(gettextf("Variables overriding user/site settings:"),
            sprintf("  %s", bad))
      },
      if(length(x$paths) > 1L) {
          c(sprintf("Package has both %s and %s.",
                  sQuote("src/Makevars.in"), sQuote("src/Makevars")),
            strwrap(sprintf("Installation with --no-configure' is unlikely to work.  If you intended %s to be used on Windows, rename it to %s otherwise remove it.  If %s created %s, you need a %s script.",
                            sQuote("src/Makevars"),
                            sQuote("src/Makevars.win"),
                            sQuote("configure"),
                            sQuote("src/Makevars"),
                            sQuote("cleanup"))))
      })
}

### * .check_code_usage_in_package

.check_code_usage_in_package <-
function(package, lib.loc = NULL)
{
    is_base <- package == "base"
    if(!is_base) {
        .load_package_quietly(package, lib.loc)

        .eval_with_capture({
            ## avoid warnings about code in other packages the package
            ## uses
            desc <- readRDS(file.path(find.package(package, NULL),
                                       "Meta", "package.rds"))
            pkgs1 <- sapply(desc$Suggests, "[[", "name")
            pkgs2 <- sapply(desc$Enhances, "[[", "name")
            for(pkg in unique(c(pkgs1, pkgs2)))
                ## tcltk warns if no DISPLAY variable
		##, errors if not compiled in
                suppressWarnings(suppressMessages(try(require(pkg,
                                                              character.only = TRUE,
                                                              quietly = TRUE),
                                                      silent = TRUE)))
        }, type = "output")

        runif(1) # create .Random.seed
        compat <- new.env(hash=TRUE)
        if(.Platform$OS.type != "unix") {
            assign("nsl", function(hostname) {}, envir = compat)
            assign("X11Font", function(font) {}, envir = compat)
            assign("X11Fonts", function(...) {}, envir = compat)
            assign("X11.options", function(..., reset = TRUE) {},
                   envir = compat)
            assign("quartz",
                   function(title, width, height, pointsize, family,
                            fontsmooth, antialias, type, file = NULL,
                            bg, canvas, dpi) {},
                   envir = compat)
            assign("quartzFont", function(family) {}, envir = compat)
            assign("quartzFonts", function(...) {}, envir = compat)
            assign("quartz.options", function(..., reset = TRUE) {},
                   envir = compat)
        }
        if(.Platform$OS.type != "windows") {
            assign("bringToTop", function (which = dev.cur(), stay = FALSE) {},
                   envir = compat)
            assign("choose.dir",
                   function (default = "", caption = "Select folder") {},
                   envir = compat)
            assign("choose.files",
                   function (default = "", caption = "Select files",
                             multi = TRUE, filters = Filters,
                             index = nrow(Filters)) {Filters=NULL}, envir = compat)
            assign("DLL.version", function(path) {}, envir = compat)
            assign("getClipboardFormats", function(numeric = FALSE) {},
                   envir = compat)
            assign("getIdentification", function() {}, envir = compat)
            assign("getWindowsHandle", function(which = "Console") {},
                   envir = compat)
            assign("getWindowTitle", function() {}, envir = compat)
            assign("readClipboard", function(format = 1, raw = FALSE) {},
                   envir = compat)
            assign("setWindowTitle",
                   function(suffix, title = paste(getIdentification(), suffix)) {},
                   envir = compat)
            assign("shell",
                   function(cmd, shell, flag = "/c", intern = FALSE,
                            wait = TRUE, translate = FALSE, mustWork = FALSE,
                            ...) {},
                   envir = compat)
            assign("shell.exec", function(file) {}, envir = compat)
            assign("shortPathName", function(path) {}, envir = compat)
            assign("win.version", function() {}, envir = compat)
            assign("zip.unpack", function(zipname, dest) {}, envir = compat)
            assign("savePlot",
                   function (filename = "Rplot",
                             type = c("wmf", "emf", "png", "jpeg", "jpg",
                                      "bmp", "ps", "eps", "pdf"),
                             device = dev.cur(), restoreConsole = TRUE) {},
                   envir = compat)
            assign("win.graph",
                   function(width = 7, height = 7, pointsize = 12,
                            restoreConsole = FALSE) {}, envir = compat)
            assign("win.metafile",
                   function (filename = "", width = 7, height = 7,
                             pointsize = 12, family = "",
                             restoreConsole = TRUE) {},
                   envir = compat)
            assign("win.print",
                   function(width = 7, height = 7, pointsize = 12,
                            printer = "", family = "", antialias = "default",
                            restoreConsole = TRUE) {},
                   envir = compat)
            assign("windows",
                   function(width, height, pointsize,
                            record, rescale, xpinch, ypinch,
                            bg, canvas, gamma, xpos, ypos,
                            buffered, title, restoreConsole, clickToConfirm,
                            fillOddEven, family = "", antialias) {},
                            envir = compat)
            assign("windowsFont", function(font) {}, envir = compat)
            assign("windowsFonts", function(...) {}, envir = compat)
            assign("windows.options", function(..., reset = TRUE) {},
                   envir = compat)

            assign("winDialog", function(type = "ok", message) {},
                   envir = compat)
            assign("winDialogString", function(message, default) {},
                   envir = compat)
            assign("winMenuAdd", function(menuname) {}, envir = compat)
            assign("winMenuAddItem", function(menuname, itemname, action) {},
                   envir = compat)
            assign("winMenuDel", function(menuname) {}, envir = compat)
            assign("winMenuDelItem", function(menuname, itemname) {},
                   envir = compat)
            assign("winMenuNames", function() {}, envir = compat)
            assign("winMenuItems", function(menuname) {}, envir = compat)
            assign("winProgressBar",
                   function(title = "R progress bar", label = "",
                            min = 0, max = 1, initial = 0, width = 300) {},
                   envir = compat)
            assign("setWinProgressBar",
                   function(pb, value, title=NULL, label=NULL) {},
                   envir = compat)
            assign(".install.winbinary",
                   function(pkgs, lib, repos = getOption("repos"),
                            contriburl = contrib.url(repos),
                            method, available = NULL, destdir = NULL,
                            dependencies = FALSE, libs_only = FALSE,
                            ...) {}, envir = compat)
            assign("Sys.junction", function(from, to) {}, envir = compat)
        }
        attach(compat, name="compat", pos = length(search()),
               warn.conflicts = FALSE)
    }

    ## A simple function for catching the output from the codetools
    ## analysis using the checkUsage report mechanism.
    out <- character()
    foo <- function(x) out <<- c(out, x)
    ## (Simpler than using a variant of capture.output().)
    ## Of course, it would be nice to return a suitably structured
    ## result, but we can always do this by suitably splitting the
    ## messages on the double colons ...

    ## Not only check function definitions, but also S4 methods
    ## [a version of this should be part of codetools eventually] :
    checkMethodUsageEnv <- function(env, ...) {
	for(g in .get_S4_generics(env))
	    for(m in .get_S4_methods_list(g, env)) {
		fun <- methods::getDataPart(m)
		signature <- paste(m@generic,
				   paste(m@target, collapse = "-"),
				   sep = ",")
		codetools::checkUsage(fun, signature, ...)
	    }
    }
    checkMethodUsagePackage <- function (pack, ...) {
	pname <- paste("package", pack, sep = ":")
	if (!pname %in% search())
	    stop("package must be loaded", domain = NA)
	checkMethodUsageEnv(if (pack %in% loadedNamespaces())
			    getNamespace(pack) else as.environment(pname), ...)
    }

    ## Allow specifying a codetools "profile" for checking via the
    ## environment variable _R_CHECK_CODETOOLS_PROFILE_, used as e.g.
    ##   _R_CHECK_CODETOOLS_PROFILE_="suppressLocalUnused=FALSE"
    ## (where the values get converted to logicals "the usual way").
    args <- list(skipWith = TRUE,
                 suppressPartialMatchArgs = FALSE,
                 suppressLocalUnused = TRUE)
    opts <- unlist(strsplit(Sys.getenv("_R_CHECK_CODETOOLS_PROFILE_"),
                            "[[:space:]]*,[[:space:]]*"))
    if(length(opts)) {
        args[sub("[[:space:]]*=.*", "", opts)] <-
            lapply(sub(".*=[[:space:]]*", "", opts),
                   config_val_to_logical)
    }
    ## look for globalVariables declaration in package
    .glbs <- utils::globalVariables(,package)
    if(length(.glbs))
        ## codetools doesn't allow adding to its default
        args$suppressUndefined <-
            c(codetools:::dfltSuppressUndefined, .glbs)

    args <- c(list(package, report = foo), args)
    suppressMessages(do.call(codetools::checkUsagePackage, args))
    suppressMessages(do.call(checkMethodUsagePackage, args))

    out <- unique(out)
    class(out) <- "check_code_usage_in_package"
    out
}

format.check_code_usage_in_package <-
function(x, ...)
{
    if(length(x)) {
        ## There seems no easy we can gather usage diagnostics by type,
        ## so try to rearrange to some extent when formatting.
        ind <- grepl(": partial argument match of", x, fixed = TRUE)
        if(any(ind)) x <- c(x[ind], x[!ind])
    }
    strwrap(x, indent = 0L, exdent = 2L)
}

### * .check_Rd_xrefs

.check_Rd_xrefs <-
function(package, dir, lib.loc = NULL)
{
    ## Build a db with all possible link targets (aliases) in the base
    ## and recommended packages.
    base <- unlist(.get_standard_package_names()[c("base", "recommended")],
                   use.names = FALSE)
    aliases <- lapply(base, Rd_aliases, lib.loc = NULL)
    ## (Don't use lib.loc = .Library, as recommended packages may have
    ## been installed to a different place.)

    ## Now find the aliases in packages it depends on
    if(!missing(package)) {
        pfile <- system.file("Meta", "package.rds", package = package,
                             lib.loc = lib.loc)
        pkgInfo <- readRDS(pfile)
    } else {
        outDir <- file.path(tempdir(), "fake_pkg")
        dir.create(file.path(outDir, "Meta"), FALSE, TRUE)
        .install_package_description(dir, outDir)
        pfile <- file.path(outDir, "Meta", "package.rds")
        pkgInfo <- readRDS(pfile)
        unlink(outDir, recursive = TRUE)
    }
    ## only 'Depends' are guaranteed to be on the search path, but
    ## 'Imports' have to be installed and hence help there will be found
    deps <- c(names(pkgInfo$Depends), names(pkgInfo$Imports))
    pkgs <- setdiff(unique(deps), base)
    try_Rd_aliases <- function(...) tryCatch(Rd_aliases(...), error = identity)
    aliases <- c(aliases, lapply(pkgs, try_Rd_aliases, lib.loc = lib.loc))
    aliases[sapply(aliases, class) == "error"] <- NULL

    ## Add the aliases from the package itself, and build a db with all
    ## (if any) \link xrefs in the package Rd objects.
    if(!missing(package)) {
        aliases1 <- Rd_aliases(package, lib.loc = lib.loc)
        if(!length(aliases1))
            return(structure(NULL, class = "check_Rd_xrefs"))
        aliases <- c(aliases, list(aliases1))
        db <- .build_Rd_xref_db(package, lib.loc = lib.loc)
    } else {
        aliases1 <- Rd_aliases(dir = dir)
        if(!length(aliases1))
            return(structure(NULL, class = "check_Rd_xrefs"))
        aliases <- c(aliases, list(aliases1))
        db <- .build_Rd_xref_db(dir = dir)
    }

    ## Flatten the xref db into one big matrix.
    db <- cbind(do.call("rbind", db), rep(names(db), sapply(db, NROW)))
    if(nrow(db) == 0L) return(structure(NULL, class = "check_Rd_xrefs"))

    ## fixup \link[=dest] form
    anchor <- db[, 2L]
    have_equals <- grepl("^=", anchor)
    if(any(have_equals))
        db[have_equals, 1:2] <- cbind(sub("^=", "", anchor[have_equals]), "")

    db <- cbind(db, bad = FALSE, report = db[, 1L])
    have_anchor <- nzchar(anchor <- db[, 2L])
    db[have_anchor, "report"] <-
        paste0("[", db[have_anchor, 2L], "]{", db[have_anchor, 1L], "}")

    ## Check the targets from the non-anchored xrefs.
    db[!have_anchor, "bad"] <- !( db[!have_anchor, 1L] %in% unlist(aliases))

    ## and then check the anchored ones if we can.
    have_colon <- grepl(":", anchor, fixed = TRUE)
    unknown <- character()
    thispkg <- anchor
    thisfile <- db[, 1L]
    thispkg [have_colon] <- sub("([^:]*):(.*)", "\\1", anchor[have_colon])
    thisfile[have_colon] <- sub("([^:]*):(.*)", "\\2", anchor[have_colon])

    use_aliases_from_CRAN <-
        config_val_to_logical(Sys.getenv("_R_CHECK_XREFS_USE_ALIASES_FROM_CRAN_",
                                         FALSE))
    if(use_aliases_from_CRAN) {
        CRAN <- .get_standard_repository_URLs()[1L]
        CRAN_aliases_db <- NULL
    }

    for (pkg in unique(thispkg[have_anchor])) {
        ## we can't do this on the current uninstalled package!
        if (missing(package) && pkg == basename(dir)) next
        this <- have_anchor & (thispkg %in% pkg)
        top <- system.file(package = pkg, lib.loc = lib.loc)
        if(nzchar(top)) {
            RdDB <- file.path(top, "help", "paths.rds")
            if(!file.exists(RdDB)) {
                message(gettextf("package %s exists but was not installed under R >= 2.10.0 so xrefs cannot be checked", sQuote(pkg)),
                        domain = NA)
                next
            }
            nm <- sub("\\.[Rr]d", "", basename(readRDS(RdDB)))
            good <- thisfile[this] %in% nm
            suspect <- if(any(!good)) {
                aliases1 <- if (pkg %in% names(aliases)) aliases[[pkg]]
                else Rd_aliases(pkg, lib.loc = lib.loc)
                !good & (thisfile[this] %in% aliases1)
            } else FALSE
            db[this, "bad"] <- !good & !suspect
        } else if(use_aliases_from_CRAN) {
            if(is.null(CRAN_aliases_db)) {
                ## Not yet read in.
                ## message("Reading in aliases db ...")
                con <- gzcon(url(sprintf("%s/src/contrib/Meta/aliases.rds",
                                         CRAN),
                                 "rb"))
                CRAN_aliases_db <- readRDS(con)
                close(con)
            }
            aliases <- CRAN_aliases_db[[pkg]]
            if(is.null(aliases)) {
                unknown <- c(unknown, pkg)
                next
            }
            ## message(sprintf("Using aliases db for package %s", pkg))
            nm <- sub("\\.[Rr]d", "", basename(names(aliases)))
            good <- thisfile[this] %in% nm
            suspect <- if(any(!good)) {
                aliases1 <- unique(as.character(unlist(aliases,
                                                       use.names =
                                                       FALSE)))
                !good & (thisfile[this] %in% aliases1)
            } else FALSE
        }
        else
            unknown <- c(unknown, pkg)
    }

    unknown <- unique(unknown)
    obsolete <- unknown %in% c("ctest", "eda", "lqs", "mle", "modreg", "mva", "nls", "stepfun", "ts")
    if (any(obsolete)) {
        message(sprintf(ngettext(sum(obsolete),
                                 "Obsolete package %s in Rd xrefs",
                                 "Obsolete packages %s in Rd xrefs"),
                        paste(sQuote(unknown[obsolete]), collapse = ", ")),
                domain = NA)
    }
    unknown <- unknown[!obsolete]
    if (length(unknown)) {
        repos <- .get_standard_repository_URLs()
        known <-
            try(suppressWarnings(utils::available.packages(utils::contrib.url(repos, "source"),
               filters = c("R_version", "duplicates"))[, "Package"]))
        miss <- if(inherits(known, "try-error")) TRUE
        else unknown %in% c(known, c("GLMMGibbs", "survnnet", "yags"))
        ## from CRANextras
        if(any(miss))
            message(sprintf(ngettext(sum(miss),
                                     "Package unavailable to check Rd xrefs: %s",
                                     "Packages unavailable to check Rd xrefs: %s"),
                             paste(sQuote(unknown[miss]), collapse = ", ")),
                    domain = NA)
        if(any(!miss))
            message(sprintf(ngettext(sum(!miss),
                                     "Unknown package %s in Rd xrefs",
                                     "Unknown packages %s in Rd xrefs"),
                             paste(sQuote(unknown[!miss]), collapse = ", ")),
                    domain = NA)
    }
    ## The bad ones:
    bad <- db[, "bad"] == "TRUE"
    res1 <- split(db[bad, "report"], db[bad, 3L])
    structure(list(bad = res1), class = "check_Rd_xrefs")
}

format.check_Rd_xrefs <-
function(x, ...)
{
    xx <- x$bad
    if(length(xx)) {
        .fmt <- function(i) {
            c(gettextf("Missing link or links in documentation object '%s':",
                       names(xx)[i]),
              ## NB, link might be empty, and was in mvbutils
              .pretty_format(unique(xx[[i]])),
              "")
        }
        c(unlist(lapply(seq_along(xx), .fmt)),
          strwrap(gettextf("See the information in section 'Cross-references' of the 'Writing R Extensions' manual.")),
          "")
    } else {
        character()
    }
}

### * .check_package_datasets

.check_package_datasets <-
function(pkgDir)
{
    Sys.setlocale("LC_CTYPE", "C")
    options(warn=-1)
    check_one <- function(x, ds)
    {
        if(!length(x)) return()
        ## avoid as.list methods
        if(is.list(x)) lapply(unclass(x), check_one, ds = ds)
        if(is.character(x)) {
            xx <- unclass(x)
            enc <- Encoding(xx)
            latin1 <<- latin1 + sum(enc == "latin1")
            utf8 <<- utf8 + sum(enc == "UTF-8")
            bytes <<- bytes + sum(enc == "bytes")
            unk <- xx[enc == "unknown"]
            ind <- .Call(check_nonASCII2, unk)
            if(length(ind)) {
                non_ASCII <<- c(non_ASCII, unk[ind])
                where <<- c(where, rep.int(ds, length(ind)))
            }
        }
        a <- attributes(x)
        if(!is.null(a)) {
            lapply(a, check_one, ds = ds)
            check_one(names(a), ds)
        }
        invisible()
    }

    sink(tempfile()) ## suppress startup messages to stdout
    on.exit(sink())
    files <- list_files_with_type(file.path(pkgDir, "data"), "data")
    files <- unique(basename(file_path_sans_ext(files)))
    ans <- vector("list", length(files))
    dataEnv <- new.env(hash=TRUE)
    names(ans) <- files
    old <- setwd(pkgDir)
    for(f in files)
        .try_quietly(utils::data(list = f, package = character(),
                                 envir = dataEnv))
    setwd(old)

    non_ASCII <- where <- character()
    latin1 <- utf8 <- bytes <- 0L
    ## avoid messages about loading packages that started with r48409
    suppressPackageStartupMessages({
        for(ds in ls(envir = dataEnv, all.names = TRUE))
            check_one(get(ds, envir = dataEnv), ds)
    })
    unknown <- unique(cbind(non_ASCII, where))
    structure(list(latin1 = latin1, utf8 = utf8, bytes = bytes,
                   unknown = unknown),
              class = "check_package_datasets")
}

format.check_package_datasets <-
function(x, ...)
{
    ## not sQuote as we have mucked about with locales.
    iconv0 <- function(x, ...) paste0("'", iconv(x, ...), "'")

    c(character(),
      if(n <- x$latin1) {
          sprintf(
                  ngettext(n,
                   "Note: found %d marked Latin-1 string",
                   "Note: found %d marked Latin-1 strings"), n)
      },
      if(n <- x$utf8) {
          sprintf(
                  ngettext(n,
                           "Note: found %d marked UTF-8 string",
                           "Note: found %d marked UTF-8 strings"), n)
      },
      if(n <- x$bytes) {
          sprintf(
                  ngettext(n,
                           "Note: found %d string marked as \"bytes\"",
                           "Note: found %d strings marked as \"bytes\""), n)
      },
      if(nr <- nrow(x$unknown)) {
          msg <- ngettext(nr,
                          "Warning: found non-ASCII string",
                          "Warning: found non-ASCII strings",
                          domain = NA)
          c(msg,
            paste0(iconv0(x$unknown[, 1L], "", "ASCII", sub = "byte"),
                   " in object '", x$unknown[, 2L], "'"))
      })
}

### * .check_package_datasets

.check_package_compact_datasets <-
function(pkgDir, thorough = FALSE)
{
    msg <- NULL
    rdas <- checkRdaFiles(file.path(pkgDir, "data"))
    row.names(rdas) <- basename(row.names(rdas))
    problems <- with(rdas, (ASCII | compress == "none") & (size > 1e5))
    if (any(rdas$compress %in% c("bzip2", "xz"))) {
        OK <- FALSE
        Rdeps <- .split_description(.read_description(file.path(pkgDir, "DESCRIPTION")))$Rdepends2
        for(dep in Rdeps) {
            if(dep$op != '>=') next
            if(dep$version >= package_version("2.10")) {OK <- TRUE; break;}
        }
        if(!OK) msg <- "Warning: package needs dependence on R (>= 2.10)"
    }
    if (sum(rdas$size) < 1e5 || # we don't report unless we get a 1e5 reduction
        any(rdas$compress %in% c("bzip2", "xz"))) # assume already optimized
        thorough <- FALSE
    sizes <- improve <- NULL
    if (thorough) {
        files <- Sys.glob(c(file.path(pkgDir, "data", "*.rda"),
                            file.path(pkgDir, "data", "*.RData")))
        ## Exclude .RData, which this may or may not match
        files <- grep("/[.]RData$", files, value = TRUE, invert = TRUE)
        if (length(files)) {
            cpdir <- tempfile('cp')
            dir.create(cpdir)
            file.copy(files, cpdir)
            resaveRdaFiles(cpdir)
            rdas2 <- checkRdaFiles(cpdir)
            row.names(rdas2) <- basename(row.names(rdas2))
            diff2 <- (rdas2$ASCII != rdas$ASCII) | (rdas2$compress != rdas$compress)
            diff2 <- diff2 & (rdas$size > 1e4) & (rdas2$size < 0.9*rdas$size)
            sizes <- c(sum(rdas$size), sum(rdas2$size))
            improve <- data.frame(old_size = rdas$size,
                                  new_size = rdas2$size,
                                  compress = rdas2$compress,
                                  row.names = row.names(rdas))[diff2, ]
        }
    }
    structure(list(rdas = rdas[problems, 1:3], msg = msg,
                   sizes = sizes, improve = improve),
              class = "check_package_compact_datasets")
}

print.check_package_compact_datasets <-
function(x, ...)
{
    reformat <- function(x) {
        xx <- paste0(x, "b")
        ind1 <- (x >= 1024)
        xx[ind1] <- sprintf("%.0fKb", x[ind1]/1024)
        ind2 <- x >= 1024^2
        xx[ind2] <- sprintf("%.1fMb", x[ind2]/(1024^2))
        ind3 <- x >= 1024^3
        xx[ind3] <- sprintf("%.1fGb", x[ind3]/1024^3)
        xx
    }
    if(nr <- nrow(x$rdas)) {
        msg <- ngettext(nr,
                        "Warning: large data file saved inefficiently:",
                        "Warning: large data files saved inefficiently:",
                        domain = NA)
        writeLines(msg)
        rdas <- x$rdas
        rdas$size <- reformat(rdas$size)
        print(rdas)
    }
    if(!is.null(x$msg)) writeLines(x$msg)
    if(!is.null(s <- x$sizes) && s[1L] - s[2L] > 1e5  # save at least 100Kb
       && s[2L]/s[1L] < 0.9) { # and at least 10%
        writeLines(c("",
                     "Note: significantly better compression could be obtained",
                     "      by using R CMD build --resave-data"))
        if(nrow(x$improve)) {
            improve <- x$improve
            improve$old_size <- reformat(improve$old_size)
            improve$new_size <- reformat(improve$new_size)
            print(improve)
        }
    }
    invisible(x)
}

.check_package_compact_sysdata <-
function(pkgDir, thorough = FALSE)
{
    msg <- NULL
    files <- file.path(pkgDir, "R", "sysdata.rda")
    rdas <- checkRdaFiles(files)
    row.names(rdas) <- basename(row.names(rdas))
    problems <- with(rdas, (ASCII | compress == "none") & (size > 1e5))
    if (any(rdas$compress %in% c("bzip2", "xz"))) {
        OK <- FALSE
        Rdeps <- .split_description(.read_description(file.path(pkgDir, "DESCRIPTION")))$Rdepends2
        for(dep in Rdeps) {
            if(dep$op != '>=') next
            if(dep$version >= package_version("2.10")) {OK <- TRUE; break;}
        }
        if(!OK) msg <- "Warning: package needs dependence on R (>= 2.10)"
    }
    if (sum(rdas$size) < 1e5 || # we don't report unless we get a 1e5 reduction
        any(rdas$compress %in% c("bzip2", "xz"))) # assume already optimized
        thorough <- FALSE
    if (thorough) {
        cpdir <- tempfile('cp')
        dir.create(cpdir)
        file.copy(files, cpdir)
        resaveRdaFiles(cpdir)
        rdas2 <- checkRdaFiles(cpdir)
        row.names(rdas2) <- basename(row.names(rdas2))
        diff2 <- (rdas2$ASCII != rdas$ASCII) | (rdas2$compress != rdas$compress)
        diff2 <- diff2 & (rdas$size > 1e4) & (rdas2$size < 0.9*rdas$size)
        sizes <- c(sum(rdas$size), sum(rdas2$size))
        improve <- data.frame(old_size = rdas$size,
                              new_size = rdas2$size,
                              compress = rdas2$compress,
                              row.names = row.names(rdas))[diff2, ]
    } else sizes <- improve <- NULL
    structure(list(rdas = rdas[problems, 1:3], msg = msg,
                   sizes = sizes, improve = improve),
              class = "check_package_compact_datasets")
}


### * .check_package_subdirs

## used by R CMD build
.check_package_subdirs <-
function(dir, doDelete = FALSE)
{
    OS_subdirs <- c("unix", "windows")

    mydir <- function(dir)
    {
        d <- list.files(dir, all.files = TRUE, full.names = FALSE)
        if(!length(d)) return(d)
        if(basename(dir) %in% c("R", "man"))
            for(os in OS_subdirs) {
                os_dir <- file.path(dir, os)
                if(file_test("-d", os_dir))
                    d <- c(d,
                           file.path(os,
                                     list.files(os_dir,
                                                all.files = TRUE,
                                                full.names = FALSE)))
            }
        d[file_test("-f", file.path(dir, d))]
    }

    if(!file_test("-d", dir))
        stop(gettextf("directory '%s' does not exist", dir), domain = NA)
    else
        dir <- file_path_as_absolute(dir)

    wrong_things <- list(R = character(), man = character(),
                         demo = character(), `inst/doc` = character())

    code_dir <- file.path(dir, "R")
    if(file_test("-d", code_dir)) {
        all_files <- mydir(code_dir)
        ## Under Windows, need a Makefile.win for methods.
        R_files <- c("sysdata.rda", "Makefile.win",
                     list_files_with_type(code_dir, "code",
                                          full.names = FALSE,
                                          OS_subdirs = OS_subdirs))
        wrong <- setdiff(all_files, R_files)
        ## now configure might generate files in this directory
        generated <- grep("\\.in$", wrong)
        if(length(generated)) wrong <- wrong[-generated]
        if(length(wrong)) {
            wrong_things$R <- wrong
            if(doDelete) unlink(file.path(dir, "R", wrong))
        }
    }

    man_dir <- file.path(dir, "man")
    if(file_test("-d", man_dir)) {
        all_files <- mydir(man_dir)
        man_files <- list_files_with_type(man_dir, "docs",
                                          full.names = FALSE,
                                          OS_subdirs = OS_subdirs)
        wrong <- setdiff(all_files, man_files)
        if(length(wrong)) {
            wrong_things$man <- wrong
            if(doDelete) unlink(file.path(dir, "man", wrong))
        }
    }

    demo_dir <- file.path(dir, "demo")
    if(file_test("-d", demo_dir)) {
        all_files <- mydir(demo_dir)
        demo_files <- list_files_with_type(demo_dir, "demo",
                                           full.names = FALSE)
        wrong <- setdiff(all_files, c("00Index", demo_files))
        if(length(wrong)) {
            wrong_things$demo <- wrong
            if(doDelete) unlink(file.path(dir, "demo", wrong))
        }
    }

    ## check installed vignette material
    subdir <- file.path("inst", "doc")
    vigns <- pkgVignettes(dir = dir, subdirs = subdir)
    if (!is.null(vigns) && length(vigns$docs)) {
        vignettes <- basename(vigns$docs)

        ## Add vignette output files, if they exist
        tryCatch({
            vigns <- pkgVignettes(dir = dir, subdirs = subdir, output = TRUE)
            vignettes <- c(vignettes, basename(vigns$outputs))
        }, error = function(ex) {})

        ## 'the file names should start with an ASCII letter and be comprised
        ## entirely of ASCII letters or digits or hyphen or underscore'
        ## Do this in a locale-independent way.
        OK <- grep("^[ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz][ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789._-]+$", vignettes)
        wrong <- vignettes
        if(length(OK)) wrong <- wrong[-OK]
        if(length(wrong)) wrong_things$`inst/doc` <- wrong
    }

    class(wrong_things) <- "subdir_tests"
    wrong_things
}

format.subdir_tests <-
function(x, ...)
{
    .fmt <- function(i) {
        tag <- names(x)[i]
        c(sprintf("Subdirectory '%s' contains invalid file names:",
                  tag),
          .pretty_format(x[[i]]))
    }

    as.character(unlist(lapply(which(sapply(x, length) > 0L), .fmt)))
}

### * .check_package_ASCII_code

.check_package_ASCII_code <-
function(dir, respect_quotes = FALSE)
{
    OS_subdirs <- c("unix", "windows")
    if(!file_test("-d", dir))
        stop(gettextf("directory '%s' does not exist", dir), domain = NA)
    else
        dir <- file_path_as_absolute(dir)

    code_dir <- file.path(dir, "R")
    wrong_things <- character()
    if(file_test("-d", code_dir)) {
        R_files <- list_files_with_type(code_dir, "code",
                                        full.names = FALSE,
                                        OS_subdirs = OS_subdirs)
        for(f in R_files) {
            text <- readLines(file.path(code_dir, f), warn = FALSE)
            if(.Call(check_nonASCII, text, !respect_quotes))
                wrong_things <- c(wrong_things, f)
        }
    }
    if(length(wrong_things)) cat(wrong_things, sep = "\n")
    invisible(wrong_things)
}

### * .check_package_code_syntax

.check_package_code_syntax <-
function(dir)
{
    if(!file_test("-d", dir))
        stop(gettextf("directory '%s' does not exist", dir), domain = NA)
    else
        dir <- file_path_as_absolute(dir)
    dir_name <- basename(dir)

    dfile <- file.path(dirname(dir), "DESCRIPTION")
    enc <- if(file.exists(dfile))
        .read_description(dfile)["Encoding"] else NA

    ## This was always run in the C locale < 2.5.0
    ## However, what chars are alphabetic depends on the locale,
    ## so as from R 2.5.0 we try to set a locale.
    ## Any package with no declared encoding should have only ASCII R code.
    if(!is.na(enc)) {  ## try to use the declared encoding
        if(.Platform$OS.type == "windows") {
            ## "C" is in fact "en", and there are no UTF-8 locales
            switch(enc,
                   "latin2" = Sys.setlocale("LC_CTYPE", 'polish'),
                   Sys.setlocale("LC_CTYPE", "C")
                   )
        } else {
            loc <- Sys.getenv("R_ENCODING_LOCALES", NA)
            if(!is.na(loc)) {
                loc <- strsplit(strsplit(loc, ":")[[1L]], "=")
                nm <- lapply(loc, "[[", 1L)
                loc <- lapply(loc, "[[", 2L)
                names(loc) <- nm
                if(!is.null(l <- loc[[enc]]))
                    Sys.setlocale("LC_CTYPE", l)
                else
                    Sys.setlocale("LC_CTYPE", "C")

            } else if(l10n_info()[["UTF-8"]]) {
                ## the hope is that the conversion to UTF-8 works and
                ## so we can validly test the code in the current locale.
            } else {
                ## these are the POSIX forms, but of course not all Unixen
                ## abide by POSIX.  These locales need not exist, but
                ## do in glibc.
                switch(enc,
                       "latin1" = Sys.setlocale("LC_CTYPE", "en_US"),
                       "utf-8"  =,  # not valid, but used
                       "UTF-8"  = Sys.setlocale("LC_CTYPE", "en_US.utf8"),
                       "latin2" = Sys.setlocale("LC_CTYPE", "pl_PL"),
                       "latin9" = Sys.setlocale("LC_CTYPE",
                       "fr_FR.iso885915@euro"),
                       Sys.setlocale("LC_CTYPE", "C")
                      )
            }
        }
    }

    collect_parse_woes <- function(f) {
        .error <- .warnings <- character()
        file <- file.path(dir, f)
        if(!is.na(enc) &&
           !(Sys.getlocale("LC_CTYPE") %in% c("C", "POSIX"))) {
            lines <- iconv(readLines(file, warn = FALSE), from = enc, to = "",
                           sub = "byte")
            withCallingHandlers(tryCatch(parse(text = lines),
                                         error = function(e)
                                         .error <<- conditionMessage(e)),
                                warning = function(e) {
                                    .warnings <<- c(.warnings,
                                                    conditionMessage(e))
                                    invokeRestart("muffleWarning")
                                })
        } else {
            withCallingHandlers(tryCatch(parse(file),
                                         error = function(e)
                                         .error <<- conditionMessage(e)),
                                warning = function(e) {
                                    .warnings <<- c(.warnings,
                                                    conditionMessage(e))
                                    invokeRestart("muffleWarning")
                                })
        }
        ## (We show offending file paths starting with the base of the
        ## given directory as this provides "nicer" output ...)
        if(length(.error) || length(.warnings))
            list(File = file.path(dir_name, f),
                 Error = .error, Warnings = .warnings)
        else
            NULL
    }

    out <-
        lapply(list_files_with_type(dir, "code", full.names = FALSE,
                                    OS_subdirs = c("unix", "windows")),
               collect_parse_woes)
    Sys.setlocale("LC_CTYPE", "C")
    structure(out[sapply(out, length) > 0L],
              class = "check_package_code_syntax")
}

print.check_package_code_syntax <-
function(x, ...)
{
    first <- TRUE
    for(i in seq_along(x)) {
        if(!first) writeLines("") else first <- FALSE
        xi <- x[[i]]
        if(length(xi$Error)) {
            msg <- gsub("\n", "\n  ", sub("[^:]*: *", "", xi$Error),
			perl = TRUE, useBytes = TRUE)
            writeLines(c(sprintf("Error in file '%s':", xi$File),
                         paste(" ", msg)))
        }
        if(len <- length(xi$Warnings))
            writeLines(c(sprintf(ngettext(len,
                                          "Warning in file %s:",
                                          "Warnings in file %s:"),
                                 sQuote(xi$File)),
                         paste(" ", gsub("\n\n", "\n  ", xi$Warnings,
					 perl = TRUE, useBytes = TRUE))))
    }
    invisible(x)
}

### * .check_package_code_shlib

.check_package_code_shlib <-
function(dir)
{
    predicate <- function(e) {
        ((length(e) > 1L)
         && (as.character(e[[1L]]) %in%
             c("library.dynam", "library.dynam.unload"))
         && is.character(e[[2L]])
         && grepl("\\.(so|sl|dll)$", e[[2L]]))
    }

    x <- Filter(length,
                .find_calls_in_package_code(dir, predicate,
                                            recursive = TRUE))

    ## Because we really only need this for calling from R CMD check, we
    ## produce output here in case we found something.
    if(length(x))
        writeLines(c(unlist(Map(.format_calls_in_file, x, names(x))),
                     ""))
    ## (Could easily provide format() and print() methods ...)

    invisible(x)
}

### * .check_package_code_startup_functions

.check_package_code_startup_functions <-
function(dir)
{
    bad_call_names <-
        unlist(.bad_call_names_in_startup_functions)

    .check_startup_function <- function(fcode, fname) {
        out <- list()
        nms <- names(fcode[[2L]])
        ## Check names of formals.
        ## Allow anything containing ... (for now); otherwise, insist on
        ## length two with names starting with lib and pkg, respectively.
        if(is.na(match("...", nms)) &&
           ((length(nms) != 2L) ||
            any(substring(nms, 1L, 3L) != c("lib", "pkg"))))
            out$bad_arg_names <- nms
        ## Look at all calls (not only at top level).
        calls <- .find_calls(fcode[[3L]], recursive = TRUE)
        if(!length(calls)) return(out)
        cnames <- .call_names(calls)
        ## And pick the ones which should not be there ...
        bcn <- bad_call_names
        if(fname == ".onAttach") bcn <- c(bcn, "library.dynam")
        if(fname == ".onLoad") bcn <- c(bcn, "packageStartupMessage")
        ind <- (cnames %in% bcn)
        if(any(ind)) {
            calls <- calls[ind]
            cnames <- cnames[ind]
            ## Exclude library(help = ......) calls.
            pos <- which(cnames == "library")
            if(length(pos)) {
                pos <- pos[sapply(calls[pos],
                                  function(e)
                                  any(names(e)[-1L] == "help"))]
                ## Could also match.call(base::library, e) first ...
                if(length(pos)) {
                    calls <- calls[-pos]
                    cnames <- cnames[-pos]
                }
            }
            if(length(calls)) {
                out$bad_calls <-
                    list(calls = calls, names = cnames)
            }
        }
        out
    }

    calls <- .find_calls_in_package_code(dir,
                                         .worker =
                                         .get_startup_function_calls_in_file)
    FL <- unlist(lapply(calls, "[[", ".First.lib"))
    calls <- Filter(length,
                    lapply(calls,
                           function(e)
                           Filter(length,
                                  Map(.check_startup_function,
                                      e, names(e)))))
    if(length(FL)) attr(calls, ".First.lib") <- TRUE
    class(calls) <- "check_package_code_startup_functions"
    calls
}

format.check_package_code_startup_functions <-
function(x, ...)
{
    res <- if(!is.null(attr(x, ".First.lib"))) "NB: .First.lib is obsolete and will not be used in R >= 3.0.0" else character()
    if(length(x)) {

        ## Flatten out doubly recursive list of functions within list of
        ## files structure for computing summary messages.
        y <- unlist(x, recursive = FALSE)

        has_bad_wrong_args <-
            "bad_arg_names" %in% unlist(lapply(y, names))
        calls <-
            unique(unlist(lapply(y,
                                 function(e) e[["bad_calls"]][["names"]])))
        has_bad_calls_for_load <-
            any(calls %in% .bad_call_names_in_startup_functions$load)
        has_bad_calls_for_output <-
            any(calls %in% .bad_call_names_in_startup_functions$output)
        has_unsafe_calls <-
            any(calls %in% .bad_call_names_in_startup_functions$unsafe)

        .fmt_entries_for_file <- function(e, f) {
            c(gettextf("File %s:", sQuote(f)),
              unlist(Map(.fmt_entries_for_function, e, names(e))),
              "")
        }

        .fmt_entries_for_function <- function(e, f) {
            c(if(length(bad <- e[["bad_arg_names"]])) {
                gettextf("  %s has wrong argument list %s",
                         f, sQuote(paste(bad, collapse = ", ")))
            },
              if(length(bad <- e[["bad_calls"]])) {
                  c(gettextf("  %s calls:", f),
                    paste0("    ",
                           unlist(lapply(bad[["calls"]], function(e)
                                         paste(deparse(e), collapse = "")))))
              })
        }

        res <-
            c(res,
              unlist(Map(.fmt_entries_for_file, x, names(x)),
                     use.names = FALSE),
              if(has_bad_wrong_args)
              strwrap(gettextf("Package startup functions should have two arguments with names starting with %s and %s, respectively.",
                               sQuote("lib"), sQuote("pkg")),
                      exdent = 2L),
              if(has_bad_calls_for_load)
              strwrap(gettextf("Package startup functions should not change the search path."),
                      exdent = 2L),
              if(has_bad_calls_for_output)
              strwrap(gettextf("Package startup functions should use %s to generate messages.",
                               sQuote("packageStartupMessage")),
                      exdent = 2L),
              if(has_unsafe_calls)
              strwrap(gettextf("Package startup functions should not call %s.",
                               sQuote("installed.packages")),
                      exdent = 2L),
              gettextf("See section %s in '%s'.",
                       sQuote("Good practice"),
                       "?.onAttach")
              )
    }
    res
}

.bad_call_names_in_startup_functions <-
    list(load = c("library", "require"),
         output = c("cat", "message", "print", "writeLines"),
         unsafe = c("installed.packages", "utils::installed.packages"))

.get_startup_function_calls_in_file <-
function(file, encoding = NA)
{
    exprs <- .parse_code_file(file, encoding)

    ## Use a custom gatherer rather than .find_calls() with a suitable
    ## predicate so that we record the name of the startup function in
    ## which the calls were found.
    calls <- list()
    for(e in exprs) {
        if((length(e) > 2L) &&
	   (is.name(x <- e[[1L]])) &&
           (as.character(x) %in%
            c("<-", "=")) &&
           (as.character(y <- e[[2L]]) %in%
            c(".First.lib", ".onAttach", ".onLoad")) &&
	   (is.call(z <- e[[3L]])) &&
           (as.character(z[[1L]]) == "function")) {
            new <- list(z)
            names(new) <- as.character(y)
            calls <- c(calls, new)
        }
    }
    calls
}

.call_names <-
function(x)
    as.character(sapply(x, function(e) deparse(e[[1L]])))


### * .check_package_code_unload_functions

.check_package_code_unload_functions <-
function(dir)
{
    bad_call_names <- "library.dynam.unload"

    .check_unload_function <- function(fcode, fname) {
        out <- list()
        nms <- names(fcode[[2L]])
        ## Check names of formals.
        ## Allow anything containing ... (for now); otherwise, insist on
        ## length one with names starting with lib.
        if(is.na(match("...", nms)) &&
           (length(nms) != 1L || substring(nms, 1L, 3L) != "lib"))
            out$bad_arg_names <- nms
        ## Look at all calls (not only at top level).
        calls <- .find_calls(fcode[[3L]], recursive = TRUE)
        if(!length(calls)) return(out)
        cnames <- .call_names(calls)
        ## And pick the ones which should not be there ...
        ind <- cnames %in% bad_call_names
        if(any(ind))
            out$bad_calls <- list(calls = calls[ind], names = cnames[ind])
        out
    }

    calls <- .find_calls_in_package_code(dir,
                                         .worker =
                                         .get_unload_function_calls_in_file)
    LL <- unlist(lapply(calls, "[[", ".Last.lib"))
    calls <- Filter(length,
                    lapply(calls,
                           function(e)
                           Filter(length,
                                  Map(.check_unload_function,
                                      e, names(e)))))
    if(length(LL)) {
        code_objs <- ".Last.lib"
        nsInfo <- parseNamespaceFile(basename(dir), dirname(dir))
        OK <- intersect(code_objs, nsInfo$exports)
        for(p in nsInfo$exportPatterns)
            OK <- c(OK, grep(p, code_objs, value = TRUE))
        if(!length(OK)) attr(calls, ".Last.lib") <- TRUE
    }
    class(calls) <- "check_package_code_unload_functions"
    calls
}

format.check_package_code_unload_functions <-
function(x, ...)
{
    res <- if(!is.null(attr(x, ".Last.lib"))) "NB: .Last.lib will not be used unless it is exported" else character()
    if(length(x)) {

        ## Flatten out doubly recursive list of functions within list of
        ## files structure for computing summary messages.
        y <- unlist(x, recursive = FALSE)

        has_bad_wrong_args <-
            "bad_arg_names" %in% unlist(lapply(y, names))
        calls <-
            unique(unlist(lapply(y,
                                 function(e) e[["bad_calls"]][["names"]])))
        .fmt_entries_for_file <- function(e, f) {
            c(gettextf("File %s:", sQuote(f)),
              unlist(Map(.fmt_entries_for_function, e, names(e))),
              "")
        }

        .fmt_entries_for_function <- function(e, f) {
            c(if(length(bad <- e[["bad_arg_names"]])) {
                gettextf("  %s has wrong argument list %s",
                         f, sQuote(paste(bad, collapse = ", ")))
            },
              if(length(bad <- e[["bad_calls"]])) {
                  c(gettextf("  %s calls:", f),
                    paste0("    ",
                           unlist(lapply(bad[["calls"]], function(e)
                                         paste(deparse(e), collapse = "")))))
              })
        }

        res <-
            c(res,
              unlist(Map(.fmt_entries_for_file, x, names(x)),
                     use.names = FALSE),
              if(has_bad_wrong_args)
              strwrap(gettextf("Package detach functions should have one arguments with names starting with %s.", sQuote("lib")),
                      exdent = 2L),
              if(length(call))
              strwrap(gettextf("Package detach functions should not call %s.",
                               sQuote("library.dynam.unload")),
                      exdent = 2L),
              gettextf("See section %s in '%s'.",
                       sQuote("Good practice"), "?.Last.lib")
              )
    }
    res
}

.get_unload_function_calls_in_file <-
function(file, encoding = NA)
{
    exprs <- .parse_code_file(file, encoding)

    ## Use a custom gatherer rather than .find_calls() with a suitable
    ## predicate so that we record the name of the unload function in
    ## which the calls were found.
    calls <- list()
    for(e in exprs) {
        if((length(e) > 2L) &&
	   (is.name(x <- e[[1L]])) &&
           (as.character(x) %in%
            c("<-", "=")) &&
           (as.character(y <- e[[2L]]) %in%
            c(".Last.lib", ".onDetach")) &&
	   (is.call(z <- e[[3L]])) &&
           (as.character(z[[1L]]) == "function")) {
            new <- list(z)
            names(new) <- as.character(y)
            calls <- c(calls, new)
        }
    }
    calls
}

### * .check_package_code_tampers

.check_package_code_tampers <-
function(dir)
{
    dfile <- file.path(dir, "DESCRIPTION")
    pkgname <- if(file.exists(dfile))
        .read_description(dfile)["Package"] else ""

    predicate <- function(e) {
        if(length(e) <= 1L) return(FALSE)
        if(as.character(e[[1L]])[1L] %in% "unlockBinding") {
            e3 <- as.character(e[[3L]])
            if (e3[[1L]] == "asNamespace") e3 <- as.character(e[[3L]][[2L]])
            return(e3 != pkgname)
        }
        if((as.character(e[[1L]])[1L] %in% ".Internal") &&
           as.character(e[[2L]][[1L]]) == "unlockBinding") return(TRUE)
        if(as.character(e[[1L]])[1L] %in% "assignInNamespace") {
            e3 <- as.character(e[[4L]])
            if (e3 == "asNamespace") e3 <- as.character(e[[4L]][[2L]])
            return(e3 != pkgname)
        }
        FALSE
    }

    x <- Filter(length,
                .find_calls_in_package_code(dir, predicate,
                                            recursive = TRUE))

    ## Because we really only need this for calling from R CMD check, we
    ## produce output here in case we found something.
    if(length(x))
        writeLines(unlist(Map(.format_calls_in_file, x, names(x))))
    ## (Could easily provide format() and print() methods ...)

    invisible(x)
}

### * .check_package_code_assign_to_globalenv

.check_package_code_assign_to_globalenv <-
function(dir)
{
    predicate <- function(e) {
        if(!is.call(e) || as.character(e[[1L]]) != "assign")
            return(FALSE)
        e <- e[as.character(e) != "..."]
        ## Capture assignments to global env unless to .Random.seed.
        ## (This may fail for conditionalized code not meant for R
        ## [e.g., argument 'where'].)
        mc <- tryCatch(match.call(base::assign, e), error = identity)
        if(inherits(mc, "error") || mc$x == ".Random.seed")
            return(FALSE)
        if(!is.null(env <- mc$envir) &&
           identical(tryCatch(eval(env),
                              error = identity),
                     globalenv()))
            return(TRUE)
        if(!is.null(pos <- mc$pos) &&
           identical(tryCatch(eval(call("as.environment", pos)),
                              error = identity),
                     globalenv()))
            return(TRUE)
        FALSE
    }

    calls <- Filter(length,
                    .find_calls_in_package_code(dir, predicate,
                                                recursive = TRUE))
    class(calls) <- "check_package_code_assign_to_globalenv"
    calls
}

format.check_package_code_assign_to_globalenv <-
function(x, ...)
{
    if(!length(x)) return(character())

    c("Found the following assignments to the global environment:",
      unlist(Map(.format_calls_in_file, x, names(x))))
}

### * .check_package_code_attach

.check_package_code_attach <-
function(dir)
{
    predicate <- function(e)
        as.character(e[[1L]]) == "attach"

    calls <- Filter(length,
                    .find_calls_in_package_code(dir, predicate,
                                                recursive = TRUE))
    class(calls) <- "check_package_code_attach"
    calls
}

format.check_package_code_attach <-
function(x, ...)
{
    if(!length(x)) return(character())

    c("Found the following calls to attach():",
      unlist(Map(.format_calls_in_file, x, names(x))))
}

### * .check_package_code_data_into_globalenv

.check_package_code_data_into_globalenv <-
function(dir)
{
    predicate <- function(e) {
        if(!is.call(e) || as.character(e[[1L]]) != "data")
            return(FALSE)
        ## As data() has usage
        ##   data(..., list = character(), package = NULL, lib.loc = NULL,
        ##        verbose = getOption("verbose"), envir = .GlobalEnv))
        ## argument 'envir' must be matched exactly, and calls which
        ## only have the last four arguments do not load any data.
        env <- e$envir
        tab <- c("package", "lib.loc", "verbose", "envir")
        if(!is.null(nms <- names(e)))
            e <- e[is.na(match(nms, tab))]
        ((length(e) > 1L) &&
         (is.null(env) ||
          (is.name(env) && as.character(env) == ".GlobalEnv") ||
          (is.call(env) && as.character(env) == "globalenv")))
    }

    calls <- Filter(length,
                    .find_calls_in_package_code(dir, predicate,
                                                recursive = TRUE))
    class(calls) <- "check_package_code_data_into_globalenv"
    calls
}

format.check_package_code_data_into_globalenv <-
function(x, ...)
{
    if(!length(x)) return(character())

    c("Found the following calls to data() loading into the global environment:",
      unlist(Map(.format_calls_in_file, x, names(x))))
}

### * .check_packages_used

.check_packages_used <-
function(package, dir, lib.loc = NULL)
{
    ## Argument handling.
    ns <- NULL
    if(!missing(package)) {
        if(length(package) != 1L)
            stop("argument 'package' must be of length 1")
        dir <- find.package(package, lib.loc)
        ## Using package installed in @code{dir} ...
        code_dir <- file.path(dir, "R")
        if(!file_test("-d", code_dir))
            stop(gettextf("directory '%s' does not contain R code",
                          dir),
                 domain = NA)
        if(basename(dir) != "base")
            .load_package_quietly(package, lib.loc)
        code_env <- if(packageHasNamespace(package, dirname(dir)))
            asNamespace(package)
        else
            .package_env(package)
        dfile <- file.path(dir, "DESCRIPTION")
        db <- .read_description(dfile)
        nsfile <- file.path(dir, "Meta", "nsInfo.rds")
        if (file.exists(nsfile)) ns <- readRDS(nsfile)
    }
    else if(!missing(dir)) {
        ## Using sources from directory @code{dir} ...
        if(!file_test("-d", dir))
            stop(gettextf("directory '%s' does not exist", dir),
                 domain = NA)
        else
            dir <- file_path_as_absolute(dir)
        dfile <- file.path(dir, "DESCRIPTION")
        db <- .read_description(dfile)
        nsfile <- file.path(dir, "NAMESPACE")
        if(file.exists(nsfile))
           ns <- parseNamespaceFile(basename(dir), dirname(dir))
        code_dir <- file.path(dir, "R")
        if(file_test("-d", code_dir)) {
            file <- tempfile()
            on.exit(unlink(file))
            if(!file.create(file)) stop("unable to create ", file)
            if(!all(.file_append_ensuring_LFs(file,
                                              list_files_with_type(code_dir,
                                                                   "code"))))
                stop("unable to write code files")
        } else return(invisible())
    }
    pkg_name <- db["Package"]
    depends <- .get_requires_from_package_db(db, "Depends")
    imports <- imports0 <- .get_requires_from_package_db(db, "Imports")
    suggests <- .get_requires_from_package_db(db, "Suggests")
    enhances <- .get_requires_from_package_db(db, "Enhances")

    ## it is OK to refer to yourself and non-S4 standard packages
    standard_package_names <-
        setdiff(.get_standard_package_names()$base,
                c("methods", "stats4"))
    ## It helps to know if non-default standard packages are require()d
    default_package_names<-
        setdiff(standard_package_names,
                c("grid", "splines", "tcltk", "tools"))
    depends_suggests <- c(depends, suggests, enhances, pkg_name, default_package_names)
    imports <- c(imports, depends, suggests, enhances, pkg_name,
                 standard_package_names)
    ## the first argument could be named, or could be a variable name.
    ## we just have a stop list here.
    common_names <- c("pkg", "pkgName", "package", "pos")

    bad_exprs <- character()
    bad_imports <- all_imports <- imp2 <- imp2f <- imp3 <- imp3f <- character()
    bad_deps <- character()
    uses_methods <- FALSE
    find_bad_exprs <- function(e) {
        if(is.call(e) || is.expression(e)) {
            Call <- deparse(e[[1L]])[1L]
            if((Call %in% c("library", "require")) &&
               (length(e) >= 2L)) {
                ## We need to rempve '...': OTOH the argument could be NULL
                keep <- sapply(e, function(x) deparse(x)[1L] != "...")
                mc <- match.call(get(Call, baseenv()), e[keep])
                if(!is.null(pkg <- mc$package)) {
                    ## <NOTE>
                    ## Using code analysis, we really don't know which
                    ## package was called if character.only = TRUE and
                    ## the package argument is not a string constant.
                    ## (BTW, what if character.only is given a value
                    ## which is an expression evaluating to TRUE?)
                    dunno <- FALSE
                    if(identical(mc$character.only, TRUE)
                       && !identical(class(pkg), "character"))
                        dunno <- TRUE
                    ## </NOTE>
                    ## <FIXME> could be inside substitute or a variable
                    ## and is in e.g. R.oo
                    if(!dunno) {
                        pkg <- sub('^"(.*)"$', '\\1', deparse(pkg))
                        if(! pkg %in% c(depends_suggests, common_names))
                            bad_exprs <<- c(bad_exprs, pkg)
                        if(pkg %in% depends)
                            bad_deps <<- c(bad_deps, pkg)
                    }
                }
            } else if(Call %in% "::") {
                pkg <- deparse(e[[2L]])
                all_imports <<- c(all_imports, pkg)
                if(! pkg %in% imports)
                    bad_imports <<- c(bad_imports, pkg)
                else {
                    imp2 <<- c(imp2, pkg)
                    imp2f <<- c(imp2f, deparse(e[[3L]]))
                }
            } else if(Call %in% ":::") {
                pkg <- deparse(e[[2L]])
                all_imports <<- c(all_imports, pkg)
                imp3 <<- c(imp3, pkg)
                imp3f <<- c(imp3f, deparse(e[[3L]]))
                if(! pkg %in% imports)
                    bad_imports <<- c(bad_imports, pkg)
            } else if(Call %in% c("setClass", "setMethod")) {
                uses_methods <<- TRUE
            }
            for(i in seq_along(e)) Recall(e[[i]])
        }
    }

    if(!missing(package)) {
        ## <FIXME>
        ## Suggested way of checking for S4 metadata.
        ## Change to use as envir_has_S4_metadata() once this makes it
        ## into base or methods.
        if(length(objects(code_env, all.names = TRUE,
                          pattern = "^[.]__[CT]_")))
            uses_methods <- TRUE
        ## </FIXME>
        exprs <- lapply(ls(envir = code_env, all.names = TRUE),
                        function(f) {
                            f <- get(f, envir = code_env) # get is expensive
			    if(typeof(f) == "closure") body(f) # else NULL
                        })
        if(.isMethodsDispatchOn()) {
            ## Also check the code in S4 methods.
            ## This may find things twice.
            for(f in .get_S4_generics(code_env)) {
                mlist <- .get_S4_methods_list(f, code_env)
                exprs <- c(exprs, lapply(mlist, body))
            }
        }
    }
    else {
        enc <- db["Encoding"]
        if(!is.na(enc) &&
           !(Sys.getlocale("LC_CTYPE") %in% c("C", "POSIX"))) {
            ## FIXME: what if conversion fails on e.g. UTF-8 comments
	    con <- file(file, encoding=enc)
            on.exit(close(con))
        } else con <- file
        exprs <-
            tryCatch(parse(file = con, n = -1L),
                     error = function(e)
                     stop(gettextf("parse error in file '%s':\n%s",
                                   file,
                                   .massage_file_parse_error_message(conditionMessage(e))),
                               domain = NA, call. = FALSE))
    }

    for(i in seq_along(exprs)) find_bad_exprs(exprs[[i]])

    depends_not_import <- character()
    if(length(ns)) {
        imp <- c(ns$imports, ns$importClasses, ns$importMethods)
        if (length(imp)) {
            imp <- sapply(imp, function(x) x[[1L]])
            all_imports <- unique(c(imp, all_imports))
        }
    } else imp <- character()
    bad_imp <- setdiff(imports0, all_imports)
    depends_not_import <- setdiff(depends, c(imp, standard_package_names))

    methods_message <-
        if(uses_methods && !"methods" %in% c(depends, imports))
            gettext("package 'methods' is used but not declared")
        else ""

    extras <- list(
        base = c("Sys.junction", "shell", "shell.exec"),
        grDevices = c("X11.options", "X11Font", "X11Fonts", "quartz",
        "quartz.options", "quartz.save", "quartzFont", "quartzFonts",
        "bringToTop", "msgWindow", "win.graph", "win.metafile", "win.print",
        "windows", "windows.options", "windowsFont", "windowsFonts"),
        parallel = c("mccollect", "mcparallel", "mc.reset.stream", "mcaffinity"),
        utils = c("nsl", "DLL.version", "Filters",
        "choose.dir", "choose.files", "getClipboardFormats",
        "getIdentification", "getWindowsHandle", "getWindowsHandles",
        "getWindowTitle", "loadRconsole", "readClipboard",
        "readRegistry", "setStatusBar", "setWindowTitle",
        "shortPathName", "win.version", "winDialog",
        "winDialogString", "winMenuAdd", "winMenuAddItem",
        "winMenuDel", "winMenuDelItem", "winMenuNames",
        "winMenuItems", "writeClipboard", "zip.unpack",
        "winProgressBar", "getWinProgressBar", "setWinProgressBar",
        "setInternet2", "arrangeWindows"),
        RODBC = c("odbcConnectAccess", "odbcConnectAccess2007",
        "odbcConnectDbase", "odbcConnectExcel", "odbcConnectExcel2007")
        )
    imp2un <- character()
    if(length(imp2)) { ## Try to check these are exported
        names(imp2f) <- imp2
        imp2 <- unique(imp2)
        imps <- split(imp2f, names(imp2f))
        for (p in names(imps)) {
            ## some people have these quoted:
            this <- imps[[p]]
            this <- sub('^"(.*)"$', "\\1", this)
            this <- sub("^'(.*)'$", "\\1", this)
            if (p %in% "base") {
                this <- setdiff(this, ls(baseenv(), all.names = TRUE))
                if(length(this))
                    imp2un <- c(imp2un, paste(p, this, sep = "::"))
                next
            }
            ns <- .getNamespace(p)
            value <- if(is.null(ns)) {
                ## this could be noisy
                tryCatch(suppressWarnings(suppressMessages(loadNamespace(p))),
                         error = function(e) e)
            } else NULL
            if (!inherits(value, "error")) {
                exps <- c(ls(envir = getNamespaceInfo(p, "exports"),
                             all.names = TRUE), extras[[p]])
                this2 <- setdiff(this, exps)
                if(length(this2))
                    imp2un <- c(imp2un, paste(p, this2, sep = "::"))
            }
        }
    }

    names(imp3f) <- imp3
    imp3 <- unique(imp3)
    imp3self <- pkg_name %in% imp3
    imp3selfcalls <- as.vector(imp3f[names(imp3f) == pkg_name])
    imp3 <- setdiff(imp3, pkg_name)
    if(length(imp3)) {
        imp3f <- imp3f[names(imp3f) %in% imp3]
        imps <- split(imp3f, names(imp3f))
        imp32 <- imp3 <- imp3f <- imp3ff <- unknown <- character()
        for (p in names(imps)) {
            this <- imps[[p]]
            this <- sub('^"(.*)"$', "\\1", this)
            this <- sub("^'(.*)'$", "\\1", this)
            if (p %in% "base") {
                imp32 <- c(imp32, paste(p, this, sep = ":::"))
                next
            }
            ns <- .getNamespace(p)
            value <- if(is.null(ns)) {
                ## this could be noisy
                tryCatch(suppressWarnings(suppressMessages(loadNamespace(p))),
                         error = function(e) e)
            } else NULL
            if (inherits(value, "error")) {
                unknown <- c(unknown, p)
            } else {
                 exps <- c(ls(envir = getNamespaceInfo(p, "exports"),
                              all.names = TRUE), extras[[p]])
                 this2 <- this %in% exps
                 if (any(this2))
                     imp32 <- c(imp32, paste(p, this[this2], sep = ":::"))
                 if (any(!this2)) {
                     imp3 <- c(imp3, p)
                     this <- this[!this2]
                     pp <- ls(envir = asNamespace(p), all.names = TRUE)
                     this2 <- this %in% pp
                     if(any(this2))
                         imp3f <- c(imp3f, paste(p, this[this2], sep = ":::"))
                     if(any(!this2))
                         imp3ff <- c(imp3ff, paste(p, this[!this2], sep = ":::"))
                 }
            }
        }
        if(length(imp3f)) {
            ## remove other packages which have the same maintainer,
            ## but report references to itself.  Unless they should be :: .
            maintainers <-
                sapply(strsplit(imp3f, ":::", fixed = TRUE),
                       function(p) {
                           dfile <- system.file("DESCRIPTION", package = p[[1L]])
                           if(dfile == "") return("")
                           unname(.read_description(dfile)["Maintainer"])
                       })
            imp3f <- imp3f[(maintainers != db["Maintainer"])]
        }
    } else imp32 <- imp3f <- imp3ff <- unknown <- character()
    res <- list(others = unique(bad_exprs),
                imports = unique(bad_imports),
                in_depends = unique(bad_deps),
                unused_imports = bad_imp,
                depends_not_import = depends_not_import,
                imp2un = sort(unique(imp2un)),
                imp32 = sort(unique(imp32)),
                imp3 = imp3, imp3f = sort(unique(imp3f)),
                imp3ff = sort(unique(imp3ff)),
                imp3self = imp3self,
                imp3selfcalls = sort(unique(imp3selfcalls)),
                imp3unknown = unknown,
                methods_message = methods_message)
    class(res) <- "check_packages_used"
    res
}

format.check_packages_used <-
function(x, ...)
{
    incoming <-
        identical(Sys.getenv("_R_CHECK_PACKAGES_USED_CRAN_INCOMING_NOTES_",
                             "FALSE"),
                  "TRUE")
    c(character(),
      if(length(xx <- x$imports)) {
          if(length(xx) > 1L) {
              c(gettext("'::' or ':::' imports not declared from:"),
                .pretty_format(sort(xx)))
          } else {
              gettextf("'::' or ':::' import not declared from: %s", sQuote(xx))
          }
      },
      if(length(xx <- x$others)) {
          if(length(xx) > 1L) {
              c(gettext("'library' or 'require' calls not declared from:"),
                .pretty_format(sort(xx)))
          } else {
              gettextf("'library' or 'require' call not declared from: %s",
                       sQuote(xx))
          }
      },
      if(length(xx <- x$in_depends)) {
          msg <- "  Please remove these calls from your code."
          if(length(xx) > 1L) {
              c(gettext("'library' or 'require' calls to packages already attached by Depends:"),
                .pretty_format(sort(xx)), msg)
          } else {
              c(gettextf("'library' or 'require' call to %s which was already attached by Depends.",
                         sQuote(xx)), msg)
          }
      },
      if(length(xx <- x$unused_imports)) {
          msg <- "  All declared Imports should be used."
          if(length(xx) > 1L) {
              c(gettext("Namespaces in Imports field not imported from:"),
                .pretty_format(sort(xx)), msg)
          } else {
              c(gettextf("Namespace in Imports field not imported from: %s",
                       sQuote(xx)), msg)
          }
      },
      if(length(xx <- x$depends_not_import)) {
          msg <- c("  These packages need to be imported from for the case when",
                   "  this namespace is loaded but not attached.")
          if(length(xx) > 1L) {
              c(gettext("Packages in Depends field not imported from:"),
                .pretty_format(sort(xx)), msg)
          } else {
              c(gettextf("Package in Depends field not imported from: %s",
                         sQuote(xx)), msg)
          }
      },
      if(length(xx <- x$imp2un)) {
          if(length(xx) > 1L) {
              c(gettext("Missing or unexported objects:"),
                .pretty_format(sort(xx)))
          } else {
              gettextf("Missing or unexported object: %s", sQuote(xx))
          }
      },
      if(length(xx <- x$imp32)) { ## ' ' seems to get converted to dir quotes
          msg <- "See the note in ?`:::` about the use of this operator."
          msg <- strwrap(paste(msg, collapse = " "), indent = 2L, exdent = 2L)
          if(length(xx) > 1L) {
              c(gettext("':::' calls which should be '::':"),
                .pretty_format(sort(xx)), msg)
          } else {
              c(gettextf("':::' call which should be '::': %s",
                         sQuote(xx)), msg)
          }
      },
      if(length(xx <- x$imp3ff)) {
           if(length(xx) > 1L) {
              c(gettext("Missing objects imported by ':::' calls:"),
                .pretty_format(sort(xx)))
          } else {
              gettextf("Missing object imported by a ':::' call: %s",
                       sQuote(xx))
          }
     },
      if(length(xxx <- x$imp3f)) { ## ' ' seems to get converted to dir quotes
          msg <- "See the note in ?`:::` about the use of this operator."
          msg <- strwrap(paste(msg, collapse = " "), indent = 2L, exdent = 2L)
          if(incoming) {
              z <- sub(":::.*", "", xxx)
              base <- unlist(.get_standard_package_names()[c("base", "recommended")])
              if (any(z %in% base))
                  msg <- c(msg,
                           "  Including base/recommended package(s):",
                           .pretty_format(intersect(base, z)))
          }
          if(length(xxx) > 1L) {
              c(gettext("Unexported objects imported by ':::' calls:"),
                .pretty_format(sort(xxx)), msg)
          } else  if(length(xxx)) {
              c(gettextf("Unexported object imported by a ':::' call: %s",
                         sQuote(xxx)), msg)
          }
      },
      if(identical(x$imp3self, TRUE)) {
          msg <-
              c("There are ::: calls to the package's namespace in its code.",
                "A package almost never needs to use ::: for its own objects:")
          c(strwrap(paste(msg, collapse = " "), indent = 0L, exdent = 2L),
            .pretty_format(sort(x$imp3selfcalls)))
      },
      if(length(xx <- x$imp3unknown)) {
          msg <- "See the note in ?`:::` about the use of this operator."
          msg <- strwrap(paste(msg, collapse = " "), indent = 2L, exdent = 2L)
          if(length(xx) > 1L) {
              c(gettext("Unavailable namespaces imported from by ':::' calls:"),
                .pretty_format(sort(xx)), msg)
          } else {
              c(gettextf("Unavailable namespace imported from by a ':::' call: %s",
                         sQuote(xx)), msg)
          }
      },
      if(length(xx <- x$data)) {
          if(length(xx) > 1L) {
              c(gettext("'data(package=)' calls not declared from:"),
                .pretty_format(sort(xx)))
          } else {
              gettextf("'data(package=)' call not declared from: %s",
                       sQuote(xx))
          }
      },
      if(nzchar(x$methods_message)) {
          x$methods_message
      })
}

### * .check_packages_used_in_examples

.check_packages_used_helper <-
function(db, files)
{
    pkg_name <- db["Package"]
    depends <- .get_requires_from_package_db(db, "Depends")
    imports <- .get_requires_from_package_db(db, "Imports")
    suggests <- .get_requires_from_package_db(db, "Suggests")
    enhances <- .get_requires_from_package_db(db, "Enhances")

    ## it is OK to refer to yourself and standard packages
    standard_package_names <- .get_standard_package_names()$base
    depends_suggests <- c(depends, imports, suggests, enhances, pkg_name,
                          standard_package_names)
    ## the first argument could be named, or could be a variable name.
    ## we just have a stop list here.
    common_names <- c("pkg", "pkgName", "package", "pos")

    bad_exprs <- character()
    bad_imports <- character()
    bad_data <- character()
    find_bad_exprs <- function(e) {
        if(is.call(e) || is.expression(e)) {
            Call <- deparse(e[[1L]])[1L]
            if(length(e) >= 2L) pkg <- deparse(e[[2L]])
            if(Call %in% c("library", "require")) {
                if(length(e) >= 2L) {
                    ## We need to rempve '...': OTOH the argument could be NULL
                    keep <- sapply(e, function(x) deparse(x)[1L] != "...")
                    mc <- match.call(get(Call, baseenv()), e[keep])
                    if(!is.null(pkg <- mc$package)) {
                        pkg <- sub('^"(.*)"$', '\\1', pkg)
                        ## <NOTE>
                        ## Using code analysis, we really don't know which
                        ## package was called if character.only = TRUE and
                        ## the package argument is not a string constant.
                        ## (Btw, what if character.only is given a value
                        ## which is an expression evaluating to TRUE?)
                        dunno <- FALSE
                        pos <- which(!is.na(pmatch(names(e),
                                                   "character.only")))
                        if(length(pos)
                           && identical(e[[pos]], TRUE)
                           && !identical(class(e[[2L]]), "character"))
                            dunno <- TRUE
                        ## </NOTE>
                        if(! dunno
                           && ! pkg %in% c(depends_suggests, common_names))
                            bad_exprs <<- c(bad_exprs, pkg)
                    }
                }
            } else if(Call %in%  "::") {
                if(! pkg %in% depends_suggests)
                    bad_imports <<- c(bad_imports, pkg)
            } else if(Call %in%  ":::") {
                if(! pkg %in% depends_suggests)
                    bad_imports <<- c(bad_imports, pkg)
            } else if(Call %in%  "data" && length(e) >= 3L) {
                mc <- match.call(utils::data, e)
                if(!is.null(pkg <- mc$package) && !pkg %in% depends_suggests)
                    bad_data <<- c(bad_data, pkg)
            } else if(deparse(e[[1L]])[1L] %in% c("utils::data", "utils:::data")) {
                mc <- match.call(utils::data, e)
                if(!is.null(pkg <- mc$package) && !pkg %in% depends_suggests)
                    bad_data <<- c(bad_data, pkg)
            }

            for(i in seq_along(e)) Recall(e[[i]])
        }
    }

    if (is.character(files)) {
        for (f in files) {
            tryCatch({
                exprs <- parse(file = f, n = -1L)
                for(i in seq_along(exprs)) find_bad_exprs(exprs[[i]])
            },
                     error = function(e)
                     warning(gettextf("parse error in file '%s':\n%s", f,
                                      .massage_file_parse_error_message(conditionMessage(e))),
                             domain = NA, call. = FALSE))
        }
    } else {
        ## called for examples with translation
        tryCatch({
            exprs <- parse(file = files, n = -1L)
            for(i in seq_along(exprs)) find_bad_exprs(exprs[[i]])
        },
                 error = function(e)
                 warning(gettextf("parse error in file '%s':\n%s",
                                  summary(files)$description,
                                  .massage_file_parse_error_message(conditionMessage(e))),
                         domain = NA, call. = FALSE))
    }

    res <- list(others = unique(bad_exprs),
                imports = unique(bad_imports),
                data = unique(bad_data),
                methods_message = "")
    class(res) <- "check_packages_used"
    res
}

.check_packages_used_in_examples <-
function(package, dir, lib.loc = NULL)
{
    ## Argument handling.
    if(!missing(package)) {
        if(length(package) != 1L)
            stop("argument 'package' must be of length 1")
        dir <- find.package(package, lib.loc)
        dfile <- file.path(dir, "DESCRIPTION")
        db <- .read_description(dfile)
    }
    else if(!missing(dir)) {
        ## Using sources from directory @code{dir} ...
        ## FIXME: not yet supported by .createExdotR.
        if(!file_test("-d", dir))
            stop(gettextf("directory '%s' does not exist", dir), domain = NA)
        else
            dir <- file_path_as_absolute(dir)
        dfile <- file.path(dir, "DESCRIPTION")
        db <- .read_description(dfile)
    }
    pkg_name <- db["Package"]

    file <- .createExdotR(pkg_name, dir, silent = TRUE)
    if (is.null(file)) return(invisible(NULL)) # e.g, no examples
    on.exit(unlink(file))
    enc <- db["Encoding"]
    if(!is.na(enc) &&
       !(Sys.getlocale("LC_CTYPE") %in% c("C", "POSIX"))) {
        ## FIXME: what if conversion fails on e.g. UTF-8 comments
        con <- file(file, encoding=enc)
        on.exit(close(con))
    } else con <- file

    .check_packages_used_helper(db, con)
}


### * .check_packages_used_in_tests

.check_packages_used_in_tests <-
function(dir, lib.loc = NULL)
{
    ## Argument handling.
    ## Using sources from directory @code{dir} ...
    if(!file_test("-d", dir))
        stop(gettextf("directory '%s' does not exist", dir), domain = NA)
    else
        dir <- file_path_as_absolute(dir)
    dfile <- file.path(dir, "DESCRIPTION")
    db <- .read_description(dfile)

    testsrcdir <- file.path(dir, "tests")
    od <- setwd(testsrcdir)
    on.exit(setwd(od))
    Rinfiles <- dir(".", pattern="\\.Rin$") # only trackOjs has *.Rin
    Rfiles <- dir(".", pattern="\\.R$")
    .check_packages_used_helper(db, c(Rinfiles, Rfiles))
}

### * .check_packages_used_in_vignettes

.check_packages_used_in_vignettes <-
function(package, lib.loc = NULL)
{
    ## Argument handling.
    if(missing(package) || length(package) != 1L)
        stop("argument 'package' must be of length 1")
    dir <- find.package(package, lib.loc)
    ## FIXME: use Meta directory.
    db <- .read_description(file.path(dir, "DESCRIPTION"))
    vinfo <- pkgVignettes(dir = dir, subdirs = "doc", source = TRUE)
    Rfiles <- unique(as.character(unlist(vinfo$sources)))
    .check_packages_used_helper(db, Rfiles)
}

### * .check_T_and_F

## T and F checking, next generation.
##
## What are we really trying to do?
##
## In R, T and F are "just" variables which upon startup are bound to
## TRUE and FALSE, respectively, in the base package/namespace.  Hence,
## if code uses "global" variables T and F and dynamic lookup is in
## place (for packages, if they do not have a namespace), there may be
## trouble in case T or F were redefined.  So we'd like to warn about
## these cases.
##
## A few things to note:
## * Package code top-level bindings *to* T and F are not a problem for
##   packages installed for lazy-loading (as the top-level T and F get
##   evaluated "appropriately" upon installation.
## * Code in examples using "global" T and F is always a problem, as
##   this is evaluated in the global envionment by examples().
## * There is no problem with package code using T and F as local
##   variables.
## * Functions in a namespace will always find the T or F in the
##   namespace, imports or base, never in the global environment.
##
## Our current idea is the following.  Function findGlobals() in
## codetools already provides a way to (approximately) determine the
## globals.  So we can try to get these and report them.
##
## Note that findGlobals() only works on closures, so we definitely miss
## top-level assignments to T or F.  This could be taken care of rather
## easily, though.
##
## Note also that we'd like to help people find where the offending
## globals were found.  Seems that codetools currently does not offer a
## way of recording e.g. the parent expression, so we do our own thing
## based on the legacy checkTnF code.

.check_T_and_F <-
function(package, dir, lib.loc = NULL)
{
    ## Seems that checking examples has several problems, and can result
    ## in "strange" diagnostic output.  Let's more or less disable this
    ## for the time being.
    check_examples <-
        isTRUE(as.logical(Sys.getenv("_R_CHECK_RD_EXAMPLES_T_AND_F_")))


    bad_closures <- character()
    bad_examples <- character()

    find_bad_closures <- function(env) {
        objects_in_env <- objects(env, all.names = TRUE)
        x <- lapply(objects_in_env,
                    function(o) {
                        v <- get(o, envir = env)
                        if (typeof(v) == "closure")
                            codetools::findGlobals(v)
                    })
        objects_in_env[sapply(x,
                              function(s) any(s %in% c("T", "F")))]
    }

    find_bad_examples <- function(txts) {
        env <- new.env(hash = TRUE) # might be many
        x <- lapply(txts,
                    function(txt) {
                        tryCatch({
                            eval(parse(text =
                                       paste("FOO <- function() {",
                                             paste(txt, collapse = "\n"),
                                             "}",
                                             collapse = "\n")),
                                 env)
                            find_bad_closures(env)
                        },
                                 error = function(e) character())
                    })
        names(txts)[sapply(x, length) > 0L]
    }

    if(!missing(package)) {
        if(length(package) != 1L)
            stop("argument 'package' must be of length 1")
        dir <- find.package(package, lib.loc)
        if((package != "base")
           && !packageHasNamespace(package, dirname(dir))) {
            .load_package_quietly(package, lib.loc)
            code_env <- .package_env(package)
            bad_closures <- find_bad_closures(code_env)
        }
        if(check_examples)
            example_texts <-
                .get_example_texts_from_example_dir(file.path(dir, "R-ex"))
    }
    else {
        ## The dir case.
        if(missing(dir))
            stop("you must specify 'package' or 'dir'")
        dir <- file_path_as_absolute(dir)
        code_dir <- file.path(dir, "R")
        if(!packageHasNamespace(basename(dir), dirname(dir))
           && file_test("-d", code_dir)) {
            code_env <- new.env(hash = TRUE)
            dfile <- file.path(dir, "DESCRIPTION")
            meta <- if(file_test("-f", dfile))
                .read_description(dfile)
            else
                character()
            .source_assignments_in_code_dir(code_dir, code_env, meta)
            bad_closures <- find_bad_closures(code_env)
        }
        if(check_examples)
            example_texts <- .get_example_texts_from_source_dir(dir)
    }

    if(check_examples)
        bad_examples <- find_bad_examples(example_texts)

    out <- list(bad_closures = bad_closures,
                bad_examples = bad_examples)
    class(out) <- "check_T_and_F"
    out
}

.get_example_texts_from_example_dir <-
function(dir)
{
    if(!file_test("-d", dir)) return(NULL)
    files <- list_files_with_exts(dir, "R")
    texts <- lapply(files,
                    function(f) paste(readLines(f, warn = FALSE),
                                      collapse = "\n"))
    names(texts) <- files
    texts
}

.get_example_texts_from_source_dir <-
function(dir)
{
    if(!file_test("-d", file.path(dir, "man"))) return(NULL)
    sapply(Rd_db(dir = dir), .Rd_get_example_code)
}

format.check_T_and_F <-
function(x, ...)
{
    c(character(),
      if(length(x$bad_closures)) {
          msg <- ngettext(length(x$bad_closures),
                          "Found possibly global 'T' or 'F' in the following function:",
                          "Found possibly global 'T' or 'F' in the following functions:"
                          )
          c(strwrap(msg),
            .pretty_format(x$bad_closures))
      },
      if(length(x$bad_examples)) {
          msg <- ngettext(length(x$bad_examples),
                          "Found possibly global 'T' or 'F' in the examples of the following Rd file:",
                          "Found possibly global 'T' or 'F' in the examples of the following Rd files:"
                          )
          c(strwrap(msg),
            paste(" ", x$bad_examples))
      })
}

### * .check_dotIntenal

.check_dotInternal <-
function(package, dir, lib.loc = NULL, details = TRUE)
{
    bad_closures <- character()

    find_bad_closures <- function(env) {
        objects_in_env <- objects(env, all.names = TRUE)
        x <- lapply(objects_in_env,
                    function(o) {
                        v <- get(o, envir = env)
                        if (typeof(v) == "closure")
                            codetools::findGlobals(v)
                    })
        objects_in_env[sapply(x, function(s) any(s %in% ".Internal"))]
    }

    find_bad_S4methods <- function(env) {
        gens <- .get_S4_generics(code_env)
        x <- lapply(gens, function(f) {
            tab <- get(methods:::.TableMetaName(f, attr(f, "package")),
                       envir = code_env)
            ## The S4 'system' does **copy** base code into packages ....
            any(unlist(eapply(tab, function(v) !inherits(v, "derivedDefaultMethod") && any(codetools::findGlobals(v) %in% ".Internal"))))
        })
        gens[unlist(x)]
    }

    find_bad_refClasses <- function(refs) {
        cl <- names(refs)
        x <- lapply(refs, function(z) {
            any(unlist(sapply(z, function(v) any(codetools::findGlobals(v) %in% ".Internal"))))
        })
        cl[unlist(x)]
    }


    bad_S4methods <- list()
    bad_refs <- character()
    if(!missing(package)) {
        if(length(package) != 1L)
            stop("argument 'package' must be of length 1")
        dir <- find.package(package, lib.loc)
        if(! package %in% .get_standard_package_names()$base) {
            .load_package_quietly(package, lib.loc)
            code_env <- if(packageHasNamespace(package, dirname(dir)))
                           asNamespace(package)
            else .package_env(package)
            bad_closures <- find_bad_closures(code_env)
            if(.isMethodsDispatchOn()) {
                bad_S4methods <- find_bad_S4methods(code_env)
                refs <- .get_ref_classes(code_env)
                if(length(refs)) bad_refs <- find_bad_refClasses(refs)
            }
        }
    }
    else {
        ## The dir case.
        if(missing(dir))
            stop("you must specify 'package' or 'dir'")
        dir <- file_path_as_absolute(dir)
        code_dir <- file.path(dir, "R")
        if(file_test("-d", code_dir)) {
            code_env <- new.env(hash = TRUE)
            dfile <- file.path(dir, "DESCRIPTION")
            meta <- if(file_test("-f", dfile))
                .read_description(dfile)
            else
                character()
            .source_assignments_in_code_dir(code_dir, code_env, meta)
            bad_closures <- find_bad_closures(code_env)
        }
    }

    internals <- character()
    if (length(bad_closures) && details) {
        lapply(bad_closures, function(o) {
            v <- get(o, envir = code_env)
            calls <- .find_calls(v, recursive = TRUE)
            if(!length(calls)) return()
            calls <- calls[.call_names(calls) == ".Internal"]
            calls2 <- lapply(calls, "[", 2L)
            calls3 <-
                sapply(calls2, function(x) sub("\\(.*", "", deparse(x)[1L]))
            internals <<- c(internals, calls3)
        })
    }
    out <- list(bad_closures = bad_closures, internals = internals,
                bad_S4methods = bad_S4methods, bad_refs = bad_refs)
    class(out) <- "check_dotInternal"
    out
}

format.check_dotInternal <-
function(x, ...)
{
    out <- if(length(x$bad_closures)) {
        msg <- ngettext(length(x$bad_closures),
                        "Found a .Internal call in the following function:",
                        "Found .Internal calls in the following functions:"
                        )
        out <- c(strwrap(msg), .pretty_format(x$bad_closures))
        if (length(unique(x$internals)))
            out <- c(out, "with calls to .Internal functions",
                     .pretty_format(sort(unique(x$internals))))
        out
    } else character()
    if(length(x$bad_S4methods)) {
        msg <- ngettext(length(x$bad_S4methods),
                        "Found a.Internal call in methods for the following S4 generic:",
                        "Found .Internal calls in methods for the following S4 generics:"
                        )
        out <- c(out, strwrap(msg), .pretty_format(x$bad_S4methods))
    }
    if(length(x$bad_refs)) {
        msg <- ngettext(length(x$bad_refs),
                        "Found a .Internal call in methods for the following reference class:",
                        "Found .Internal calls in methods for the following reference classes:"
                        )
        out <- c(out, strwrap(msg), .pretty_format(x$bad_refs))
    }
    out
}

### * .check_namespace

.check_namespace <-
function(dir)
{
    dir <- file_path_as_absolute(dir)
    invisible(tryCatch(parseNamespaceFile(basename(dir), dirname(dir)),
                       error = function(e) {
                           writeLines("Invalid NAMESPACE file, parsing gives:")
                           stop(e)
                       }))
}

### * .check_citation

.check_citation <-
function(cfile, dir = NULL)
{
    cfile <- file_path_as_absolute(cfile)

    if(!is.null(dir)) {
        meta <- utils::packageDescription(basename(dir), dirname(dir))
        db <- tryCatch(suppressMessages(utils::readCitationFile(cfile,
                                                                meta)),
                       error = identity)
        if(inherits(db, "error")) {
            msg <- conditionMessage(db)
            call <- conditionCall(db)
            if(is.null(call))
                msg <- c("Error: ", msg)
            else
                msg <- c("Error in ", deparse(call), ": ", msg)
            writeLines(paste(msg, collapse = ""))
        }
        return(invisible())
    }

    meta <- if(basename(dir <- dirname(cfile)) == "inst")
        as.list(.get_package_metadata(dirname(dir)))
    else
        NULL

    db <- tryCatch(suppressMessages(get_CITATION_entry_fields(cfile,
                                                              meta$Encoding)),
                   error = identity)

    if(inherits(db, "error")) {
        writeLines(conditionMessage(db))
        return(invisible())
    }

    if(!NROW(db)) return(invisible())

    bad <- Map(find_missing_required_BibTeX_fields, db$Entry, db$Fields,
               USE.NAMES = FALSE)
    ind <- sapply(bad, identical, NA_character_)
    if(length(pos <- which(ind))) {
        entries <- db$Entry[pos]
        entries <-
            ifelse(nchar(entries) < 20L,
                   entries,
                   paste(substring(entries, 1L, 20L), "[TRUNCATED]"))
        writeLines(sprintf("entry %d: invalid type %s",
                           pos, sQuote(entries)))
    }
    pos <- which(!ind & (sapply(bad, length) > 0L))
    if(length(pos)) {
        writeLines(strwrap(sprintf("entry %d (%s): missing required field(s) %s",
                                   pos,
                                   db$Entry[pos],
                                   sapply(bad[pos],
                                          function(s)
                                          paste(sQuote(s),
                                                collapse = ", "))),
                           indent = 0L, exdent = 2L))
    }
}

### * .check_package_parseRd

## FIXME: could use dumped files, except for use of encoding = "ASCII"
.check_package_parseRd <-
function(dir, silent = FALSE, def_enc = FALSE, minlevel = -1)
{
    if(file.exists(dfile <- file.path(dir, "DESCRIPTION"))) {
        enc <- read.dcf(dfile)[1L, ]["Encoding"]
        if(is.na(enc)) enc <- "ASCII"
        else def_enc <- TRUE
    } else enc <- "ASCII"
    owd <- setwd(file.path(dir, "man"))
    on.exit(setwd(owd))
    pg <- c(Sys.glob("*.Rd"), Sys.glob("*.rd"),
            Sys.glob(file.path("*", "*.Rd")),
            Sys.glob(file.path("*", "*.rd")))
    ## (Note that using character classes as in '*.[Rr]d' is not
    ## guaranteed to be portable.)
    bad <- character()
    for (f in pg) {
        ## Kludge for now
        if(basename(f) %in%  c("iconv.Rd", "showNonASCII.Rd")) def_enc <- TRUE
	tmp <- tryCatch(suppressMessages(checkRd(f, encoding = enc,
						 def_enc = def_enc)),
			error = function(e)e)
	if(inherits(tmp, "error")) {
	    bad <- c(bad, f)
            if(!silent) message(geterrmessage())
        } else print(tmp, minlevel = minlevel)
    }
    if(length(bad)) bad <- sQuote(sub(".*/", "", bad))
    if(length(bad) > 1L)
        cat("problems found in ", paste(bad, collapse=", "), "\n", sep = "")
    else if(length(bad))
        cat("problem found in ", bad, "\n", sep = "")
    invisible()
}


### * .check_depdef

.check_depdef <-
function(package, dir, lib.loc = NULL, WINDOWS = FALSE)
{
    bad_depr <- c("plclust")

    bad_def <- c("La.eigen", "tetragamma", "pentagamma",
                 "package.description", "gammaCody",
                 "manglePackageName", ".readRDS", ".saveRDS",
                 "mem.limits", "trySilent", "traceOn", "traceOff",
                 "print.coefmat", "anovalist.lm", "lm.fit.null",
                 "lm.wfit.null", "glm.fit.null", "tkcmd",
                 "tkfile.tail", "tkfile.dir", "tkopen", "tkclose",
                 "tkputs", "tkread", "Rd_parse", "CRAN.packages",
                 "zip.file.extract",
                 "real", "as.real", "is.real",
                 ".find.package", ".path.package")

    ## X11 may not work on even a Unix-alike: it needs X support
    ## (optional) at install time and and an X server at run time.
    bad_dev <- c("quartz", "x11", "X11")
    if(!WINDOWS)
        bad_dev <- c(bad_dev,  "windows", "win.graph", "win.metafile", "win.print")

    bad <- c(bad_depr, bad_def, bad_dev)
    bad_closures <- character()
    found <- character()

    find_bad_closures <- function(env) {
        objects_in_env <- objects(env, all.names = TRUE)
        x <- lapply(objects_in_env,
                    function(o) {
                        v <- get(o, envir = env)
                        if (typeof(v) == "closure")
                            codetools::findGlobals(v)
                    })
        objects_in_env[sapply(x, function(s) {
            res <- any(s %in% bad)
            if(res) found <<- c(found, s)
            res
        })]
    }

    find_bad_S4methods <- function(env) {
        gens <- .get_S4_generics(code_env)
        x <- lapply(gens, function(f) {
            tab <- get(methods:::.TableMetaName(f, attr(f, "package")),
                       envir = code_env)
            ## The S4 'system' does **copy** base code into packages ....
            any(unlist(eapply(tab, function(v) {
                if(!inherits(v, "derivedDefaultMethod")) FALSE
                else {
                    s <- codetools::findGlobals(v)
                    found <<- c(found, s)
                    any(s %in% bad)
                }
            })))
        })
        gens[unlist(x)]
    }

    find_bad_refClasses <- function(refs) {
        cl <- names(refs)
        x <- lapply(refs, function(z) {
            any(unlist(sapply(z, function(v) {
                s <- codetools::findGlobals(v)
                found <<- c(found, s)
                any(s %in% bad)
            })))
        })
        cl[unlist(x)]
    }


    bad_S4methods <- list()
    bad_refs <- character()
    if(!missing(package)) {
        if(length(package) != 1L)
            stop("argument 'package' must be of length 1")
        dir <- find.package(package, lib.loc)
        if(! package %in% .get_standard_package_names()$base) {
            .load_package_quietly(package, lib.loc)
            code_env <- if(packageHasNamespace(package, dirname(dir)))
                           asNamespace(package)
            else .package_env(package)
            bad_closures <- find_bad_closures(code_env)
            if(.isMethodsDispatchOn()) {
                bad_S4methods <- find_bad_S4methods(code_env)
                refs <- .get_ref_classes(code_env)
                if(length(refs)) bad_refs <- find_bad_refClasses(refs)
            }
        }
    }
    else {
        ## The dir case.
        if(missing(dir))
            stop("you must specify 'package' or 'dir'")
        dir <- file_path_as_absolute(dir)
        code_dir <- file.path(dir, "R")
        if(file_test("-d", code_dir)) {
            code_env <- new.env(hash = TRUE)
            dfile <- file.path(dir, "DESCRIPTION")
            meta <- if(file_test("-f", dfile))
                .read_description(dfile)
            else
                character()
            .source_assignments_in_code_dir(code_dir, code_env, meta)
            bad_closures <- find_bad_closures(code_env)
        }
    }

    found <- sort(unique(found))
    deprecated <- found[found %in% bad_depr]
    defunct <- found[found %in% bad_def]
    devices <- found[found %in% bad_dev]

    out <- list(bad_closures = bad_closures, deprecated = deprecated,
                defunct = defunct, devices = devices)
    class(out) <- "check_depdef"
    out
}

format.check_depdef <-
function(x, ...)
{
    out <- if(length(x$bad_closures)) {
        msg <- ngettext(length(x$bad_closures),
                        "Found an obsolete/platform-specific call in the following function:",
                        "Found an obsolete/platform-specific call in the following functions:"
                        )
        c(strwrap(msg), .pretty_format(x$bad_closures))
    } else character()
    if(length(x$bad_S4methods)) {
        msg <- ngettext(length(x$bad_S4methods),
                        "Found an obsolete/platform-specific call in methods for the following S4 generic:",
                        "Found an obsolete/platform-specific call in methods for the following S4 generics:"
                        )
        out <- c(out, strwrap(msg), .pretty_format(x$bad_S4methods))
    }
    if(length(x$bad_refs)) {
        msg <- ngettext(length(x$bad_refs),
                        "Found an obsolete/platform-specific call in methods for the following reference class:",
                        "Found an obsolete/platform-specific call in methods for the following reference classes:"
                        )
        out <- c(out, strwrap(msg), .pretty_format(x$bad_refs))
    }
    if(length(x$deprecated)) {
        msg <- ngettext(length(x$deprecated),
                        "Found the deprecated function:",
                        "Found the deprecated functions:"
                        )
        out <- c(out, strwrap(msg), .pretty_format(x$deprecated))
    }
    if(length(x$defunct)) {
        msg <- ngettext(length(x$defunct),
                        "Found the defunct/removed function:",
                        "Found the defunct/removed functions:"
                        )
        out <- c(out, strwrap(msg), .pretty_format(x$defunct))
    }
    if(length(x$devices)) {
        msg <- ngettext(length(x$devices),
                        "Found the platform-specific device:",
                        "Found the platform-specific devices:"
                        )
        out <- c(out, strwrap(msg), .pretty_format(x$devices),
                 strwrap(paste("dev.new() is the preferred way to open a new device,",
                               "in the unlikely event one is needed.",
                               collapse = " ")))
    }
    out
}

### * .check_package_CRAN_incoming

.check_package_CRAN_incoming <-
function(dir)
{
    out <- list()
    class(out) <- "check_package_CRAN_incoming"

    meta <- .get_package_metadata(dir, FALSE)
    info <- analyze_license(meta["License"])
    ## Use later to indicate changes from FOSS to non-FOSS licence.
    foss <- info$is_verified
    ## Record to notify about components extending a base license which
    ## permits extensions.
    if(length(extensions <- info$extensions) &&
       any(ind <- extensions$extensible)) {
        out$extensions <- extensions$components[ind]
        out$pointers <-
            Filter(length,
                   lapply(info$pointers,
                          function(p) {
                              fp <- file.path(dir, p)
                              if(file_test("-f", fp)) {
                                  ## Should this use the package
                                  ## encoding?
                                  c(p, readLines(fp, warn = FALSE))
                              } else NULL
                          }))
    }

    out$Maintainer <- meta["Maintainer"]
    ## pick out 'display name'
    display <- gsub("<.*", "", as.vector(out$Maintainer))
    display <- sub("[[:space:]]+$", "",
                   sub("^[[:space:]]+", "", display, useBytes = TRUE),
                   useBytes = TRUE)
    ## RFC 5322 allows '.' in the display name, but 2822 did not.
    ## ',' separates email addresses.
    out$Maintainer_needs_quotes <-
        grepl("[,]", display, useBytes = TRUE) && !grepl('^".*"$', display, useBytes = TRUE)
    out$empty_Maintainer_name <- !nzchar(display)

    ver <- meta["Version"]
    if(is.na(ver))
        stop("Package has no 'Version' field", call. = FALSE)
    if(grepl("(^|[.-])0[0-9]+", ver))
        out$version_with_leading_zeroes <- ver

    language <- meta["Language"]
    if((is.na(language) || language == "en") &&
       config_val_to_logical(Sys.getenv("_R_CHECK_CRAN_INCOMING_USE_ASPELL_",
                                        FALSE))) {
        ignore <- c("[ \t]'[^']*'[ \t[:punct:]]",
                    "[ \t][[:alnum:]_.]*\\(\\)[ \t[:punct:]]")
        a <- utils:::aspell_package_description(dir,
                                                ignore = ignore,
                                                control =
                                                 c("--master=en_US",
                                                   "--add-extra-dicts=en_GB"),
                                                program = "aspell",
                                                dictionaries = "en_stats")
        if(NROW(a))
            out$spelling <- a
    }

    urls <- .get_standard_repository_URLs()

    parse_description_field <- function(desc, field, default = TRUE)
    {
        tmp <- desc[field]
        if (is.na(tmp)) default
        else switch(tmp,
                    "yes"=, "Yes" =, "true" =, "True" =, "TRUE" = TRUE,
                    "no" =, "No" =, "false" =, "False" =, "FALSE" = FALSE,
                    default)
    }

    ## If a package has a FOSS license, check whether any of its strong
    ## recursive dependencies restricts use.
    if(foss) {
        available <-
            utils::available.packages(utils::contrib.url(urls, "source"),
                                      filters =
                                      c("R_version", "duplicates"))
        ## We need the current dependencies of the package (so batch
        ## upload checks will not necessarily do "the right thing").
        package <- meta["Package"]
        depends <- c("Depends", "Imports", "LinkingTo")
        ## Need to be careful when merging the dependencies of the
        ## package (in case it is not yet available).
        if(!is.na(pos <- match(package, rownames(available)))) {
            available[package, depends] <- meta[depends]
        } else {
            entry <- rbind(meta[colnames(available)])
            rownames(entry) <- package
            available <- rbind(available, entry)
        }
        ldb <- analyze_licenses(available[, "License"], available)
        depends <- unlist(package_dependencies(package, available,
                                               recursive = TRUE))
        ru <- ldb$restricts_use
        pnames_restricts_use_TRUE <- rownames(available)[!is.na(ru) & ru]
        pnames_restricts_use_NA <- rownames(available)[is.na(ru)]
        bad <- intersect(depends, pnames_restricts_use_TRUE)
        if(length(bad))
            out$depends_with_restricts_use_TRUE <- bad
        bad <- intersect(depends, pnames_restricts_use_NA)
        if(length(bad))
            out$depends_with_restricts_use_NA <- bad
        bv <- parse_description_field(meta, "BuildVignettes", TRUE)
        if (!bv) out$foss_with_BuildVignettes <- TRUE
    }

    ## Check for possibly mis-spelled field names.
    nms <- names(meta)
    stdNms <- .get_standard_DESCRIPTION_fields()
    nms <- nms[is.na(match(nms, stdNms)) &
               !grepl("^(X-CRAN|Repository/R-Forge)", nms)]
    if(length(nms) && ## Allow maintainer notes  <stdName>Note :
       length(nms <- nms[is.na(match(nms, paste0(stdNms,"Note")))]))
        out$fields <- nms

    ## We do not want to use utils::available.packages() for now, as
    ## this unconditionally filters according to R version and OS type.
    ## <FIXME>
    ## This is no longer true ...
    ## </FIXME>
    .repository_db <- function(u) {
        con <- gzcon(url(sprintf("%s/src/contrib/PACKAGES.gz", u), "rb"))
        on.exit(close(con))
        ## hopefully all these fields are ASCII, or we need to re-encode.
        cbind(read.dcf(con,
                       c(.get_standard_repository_db_fields(), "Path")),
              Repository = u)

    }
    db <- tryCatch(lapply(urls, .repository_db), error = identity)
    if(inherits(db, "error")) {
        message("NB: need Internet access to use CRAN incoming checks")
        ## Actually, all repositories could be local file:// mirrors.
        return(out)
    }
    db <- do.call(rbind, db)

    ## Note that .get_standard_repository_URLs() puts the CRAN master first.
    CRAN <- urls[1L]

    ## Check for CRAN repository db overrides and possible conflicts.
    con <- url(sprintf("%s/src/contrib/PACKAGES.in", CRAN))
    odb <- read.dcf(con)
    close(con)
    ## For now (2012-11-28), PACKAGES.in is all ASCII, so there is no
    ## need to re-encode.  Eventually, it might be in UTF-8 ...
    entry <- odb[odb[, "Package"] == meta["Package"], ]
    entry <- entry[!is.na(entry) & (names(entry) != "Package")]
    if(length(entry)) {
        ## Check for conflicts between package license implications and
        ## repository overrides.  Note that the license info predicates
        ## are logicals (TRUE, NA or FALSE) and the repository overrides
        ## are character ("yes", missing or "no").
        if(!is.na(iif <- info$is_FOSS) &&
           !is.na(lif <- entry["License_is_FOSS"]) &&
           ((lif == "yes") != iif))
            out$conflict_in_license_is_FOSS <- lif
        if(!is.na(iru <- info$restricts_use) &&
           !is.na(lru <- entry["License_restricts_use"]) &&
           ((lru == "yes") != iru))
            out$conflict_in_license_restricts_use <- lru

        fmt <- function(s)
            unlist(lapply(s,
                          function(e) {
                              paste(strwrap(e, indent = 2L, exdent = 4L),
                                    collapse = "\n")
                          }))
        nms <- names(entry)
        ## Report all overrides for visual inspection.
        entry <- fmt(sprintf("  %s: %s", nms, entry))
        names(entry) <- nms
        out$overrides <- entry
        fields <- intersect(names(meta), nms)
        if(length(fields)) {
            ## Find fields where package metadata and repository
            ## overrides are in conflict.
            ind <- ! unlist(Map(identical,
                                fmt(sprintf("  %s: %s", fields, meta[fields])),
                                entry[fields]))
            if(any(ind))
                out$conflicts <- fields[ind]
        }
    }

    ## For now, information about the CRAN package archive is provided
    ## in CRAN's src/contrib/Meta/archive.rds.
    con <- gzcon(url(sprintf("%s/src/contrib/Meta/archive.rds", CRAN),
                     "rb"))
    CRAN_archive_db <- readRDS(con)
    close(con)
    packages_in_CRAN_archive <- names(CRAN_archive_db)

    ## Package names must be unique within standard repositories when
    ## ignoring case.
    package <- meta["Package"]
    packages <- db[, "Package"]
    if(! package %in% packages) out$new_submission <- TRUE
    clashes <- character()
    pos <- which((tolower(packages) == tolower(package)) &
                 (packages != package))
    if(length(pos))
        clashes <-
            sprintf("%s [%s]", packages[pos], db[pos, "Repository"])
    ## If possible, also catch clashes with archived CRAN packages
    ## (which might get un-archived eventually).
    if(length(packages_in_CRAN_archive)) {
        pos <- which((tolower(packages_in_CRAN_archive) ==
                      tolower(package)) &
                     (packages_in_CRAN_archive != package))
        if(length(pos)) {
            clashes <-
                c(clashes,
                  sprintf("%s [CRAN archive]",
                          packages_in_CRAN_archive[pos]))
        }
    }
    if(length(clashes))
        out$bad_package <- list(package, clashes)

    ## Is this duplicated from another repository?
    repositories <- db[(packages == package) &
                       (db[, "Repository"] != CRAN),
                       "Repository"]
    if(length(repositories))
        out$repositories <- repositories

    uses <- character()
    BUGS <- character()
    for (field in c("Depends", "Imports", "Suggests")) {
        p <- strsplit(meta[field], " *, *")[[1L]]
        p2 <- grep("^(multicore|snow|igraph0)( |\\(|$)", p, value = TRUE)
        uses <- c(uses, p2)
        p2 <- grep("^(BRugs|R2OpenBUGS|R2WinBUGS)( |\\(|$)", p, value = TRUE)
        BUGS <- c(BUGS, p2)
    }
    if (length(uses)) out$uses <- sort(unique(uses))
    if (length(BUGS)) out$BUGS <- sort(unique(BUGS))

    ## Check for non-Sweave vignettes (as indicated by the presense of a
    ## 'VignetteBuilder' field in DESCRIPTION) without
    ## 'build/vignette.rds'.

    vds <- character()
    if(!is.na(meta["VignetteBuilder"])) {
        if(!file.exists(vds <- file.path(dir, "build", "vignette.rds")))
            out$missing_vignette_index <- TRUE
        else
            vds <- readRDS(vds)[, "File"]
    }

    ## Check for vignette source (only) in old-style 'inst/doc' rather
    ## than 'vignettes'.
    vign_dir <- file.path(dir, "vignettes")
    if(length(vds)) {
        sources <- setdiff(list.files(file.path(dir, "inst", "doc")),
                           list.files(vign_dir))
        sources <- intersect(vds, sources)
    } else {
        pattern <- vignetteEngine("Sweave")$pattern
        sources <- setdiff(list.files(file.path(dir, "inst", "doc"),
                                      pattern = pattern),
                           list.files(vign_dir, pattern = pattern))
    }

    if(length(sources)) {
        out$have_vignettes_dir <- file_test("-d", vign_dir)
        out$vignette_sources_only_in_inst_doc <- sources
    }

    ## Is this an update for a package already on CRAN?
    ## Things from this point down should be.
    db <- db[(packages == package) &
             (db[, "Repository"] == CRAN) &
             is.na(db[, "Path"]), , drop = FALSE]
    ## This drops packages in version-specific subdirectories.
    ## It also does not know about archived versions.
    if(!NROW(db)) {
        if(package %in% packages_in_CRAN_archive) {
            out$CRAN_archive <- TRUE
            v_m <- package_version(meta["Version"])
            v_a <- sub("^.*_(.*)\\.tar.gz$", "\\1",
                       basename(rownames(CRAN_archive_db[[package]])))
            v_a <- max(package_version(v_a, strict = FALSE),
                       na.rm = TRUE)
            if(v_m <= v_a)
                out$bad_version <- list(v_m, v_a)
        }
        if(!foss)
            out$bad_license <- meta["License"]
        return(out)
    }
    ## For now, there should be no duplicates ...

    ## Package versions should be newer than what we already have on CRAN.

    v_m <- package_version(meta["Version"])
    v_d <- max(package_version(db[, "Version"]))
    if((v_m <= v_d) &&
       !config_val_to_logical(Sys.getenv("_R_CHECK_CRAN_INCOMING_SKIP_VERSIONS_",
                                         FALSE)))
        out$bad_version <- list(v_m, v_d)
    if((v_m$major == v_d$major) & (v_m$minor >= v_d$minor + 10))
        out$version_with_jump_in_minor <- list(v_m, v_d)

    ## Check submission recency and frequency.
    con <- gzcon(url(sprintf("%s/src/contrib/Meta/current.rds", CRAN),
                     "rb"))
    CRAN_current_db <- readRDS(con)
    close(con)
    mtimes <- c(CRAN_current_db[match(package,
                                      sub("_.*", "",
                                          rownames(CRAN_current_db)),
                                      nomatch = 0L),
                                "mtime"],
                CRAN_archive_db[[package]]$mtime)
    if(length(mtimes)) {
        deltas <- Sys.Date() - as.Date(sort(mtimes, decreasing = TRUE))
        ## Number of days since last update.
        recency <- as.numeric(deltas[1L])
        if(recency < 7)
            out$recency <- recency
        ## Number of updates in past 6 months.
        frequency <- sum(deltas <= 180)
        if(frequency > 6)
            out$frequency <- frequency
    }

    ## Watch out for maintainer changes.
    ## Note that we cannot get the maintainer info from the PACKAGES
    ## files.
    con <- gzcon(url(sprintf("%s/web/packages/packages.rds", CRAN), "rb"))
    db <- tryCatch(readRDS(con), error = identity)
    close(con)
    if(inherits(db, "error")) return(out)

    m_m <- as.vector(meta["Maintainer"]) # drop name
    m_d <- db[db[, "Package"] == package, "Maintainer"]
    # There may be white space differences here
    m_m_1 <- gsub("[[:space:]]+", " ", m_m)
    m_d_1 <- gsub("[[:space:]]+", " ", m_d)
    if(!all(m_m_1== m_d_1)) {
        ## strwrap is used below, so we need to worry about encodings.
        ## m_d is in UTF-8 already
        if(Encoding(m_m) == "latin1") m_m <- iconv(m_m, "latin1")
        out$new_maintainer <- list(m_m, m_d)
    }

    l_d <- db[db[, "Package"] == package, "License"]
    if(!foss && analyze_license(l_d)$is_verified)
        out$new_license <- list(meta["License"], l_d)

    out
}

format.check_package_CRAN_incoming <-
function(x, ...)
{
    c(character(),
      if(length(x$Maintainer))
          sprintf("Maintainer: %s", sQuote(paste(x$Maintainer, collapse = " ")))
      else "No maintainer field in DESCRIPTION file",
      if(x$empty_Maintainer_name)
          'The maintainer field lacks a name',
      if(x$Maintainer_needs_quotes)
          'The display-name part of the maintainer field should be enclosed in ""',
      if(length(x$new_submission))
          "New submission",
      if(length(y <- x$bad_package))
          sprintf("Conflicting package names (submitted: %s, existing: %s)",
                  y[[1L]], y[[2L]]),
      if(length(y <- x$repositories))
          sprintf("Package duplicated from %s", y),
      if(length(y <- x$CRAN_archive))
          "Package was archived on CRAN",
      if(length(y <- x$bad_version))
          sprintf("Insufficient package version (submitted: %s, existing: %s)",
                  y[[1L]], y[[2L]]),
      if(length(y <- x$version_with_leading_zeroes))
          sprintf("Version contains leading zeroes (%s)", y),
      if(length(y <- x$version_with_jump_in_minor))
          sprintf("Version jumps in minor (submitted: %s, existing: %s)",
                  y[[1L]], y[[2L]]),
      if(length(y <- x$recency))
          sprintf("Days since last update: %d", y),
      if(length(y <- x$frequency))
          sprintf("Number of updates in past 6 months: %d", y),
      if(length(y <- x$new_maintainer))
          c("New maintainer:",
            strwrap(y[[1L]], indent = 2L, exdent = 4L),
            "Old maintainer(s):",
            strwrap(y[[2L]], indent = 2L, exdent = 4L)),
      if(length(y <- x$bad_license))
          sprintf("Non-FOSS package license (%s)", y),
      if(length(y <- x$new_license))
          c("Change to non-FOSS package license.",
            "New license:",
            strwrap(y[[1L]], indent = 2L, exdent = 4L),
            "Old license:",
            strwrap(y[[2L]], indent = 2L, exdent = 4L)),
      if(length(y <- x$extensions)) {
          c("Components with restrictions and base license permitting such:",
            paste(" ", y),
            unlist(lapply(x$pointers,
                          function(e) {
                              c(sprintf("File '%s':", e[1L]),
                                paste(" ", e[-1L]))
                          })))
      },
      if(NROW(y <- x$spelling)) {
          s <- split(sprintf("%d:%d", y$Line, y$Column), y$Original)
          c("Possibly mis-spelled words in DESCRIPTION:",
            sprintf("  %s (%s)",
                    names(s),
                    lapply(s, paste, collapse = ", ")))
      },
      if(identical(x$foss_with_BuildVignettes, TRUE)) {
          "FOSS licence with BuildVignettes: false"
      },
      if(length(y <- x$fields)) {
          c("Possibly mis-spelled fields in DESCRIPTION:",
            sprintf("  %s", paste(sQuote(y), collapse = " ")))
      },
      if(length(y <- x$overrides)) {
          c("CRAN repository db overrides:", y)
      },
      if(length(y <- x$conflicts)) {
          sprintf("CRAN repository db conflicts: %s",
                  sQuote(y))
      },
      if(length(y <- x$conflict_in_license_is_FOSS)) {
          sprintf("Package license conflicts with %s override",
                  sQuote(paste("License_is_FOSS:", y)))
      },
      if(length(y <- x$conflict_in_license_restricts_use)) {
          sprintf("Package license conflicts with %s override",
                  sQuote(paste("License_restricts_use:", y)))
      },
      if(length(y <- x$depends_with_restricts_use_TRUE)) {
          c("Package has a FOSS license but eventually depends on the following",
	    if(length(y) > 1L)
	    "packages which restrict use:" else
	    "package which restricts use:",
            strwrap(paste(y, collapse = ", "), indent = 2L, exdent = 4L))
      },
      if(length(y <- x$depends_with_restricts_use_NA)) {
          c("Package has a FOSS license but eventually depends on the following",
	    if(length(y) > 1L)
            "packages which may restrict use:" else
	    "package which may restrict use:",
            strwrap(paste(y, collapse = ", "), indent = 2L, exdent = 4L))
      },
      if (length(y <- x$uses)) {
          paste(if(length(y) > 1L)
		"Uses the superseded packages:" else
		"Uses the superseded package:",
                paste(sQuote(y), collapse = ", "))
      },
      if (length(y <- x$BUGS)) {
          paste(if(length(y) > 1L)
		"Uses the non-portable packages:" else
		"Uses the non-portable package:",
                paste(sQuote(y), collapse = ", "))
      },
      if(length(y <- x$vignette_sources_only_in_inst_doc)) {
          if(identical(x$have_vignettes_dir, FALSE))
              c("Vignette sources in 'inst/doc' with no 'vignettes' directory:",
                strwrap(paste(sQuote(y), collapse = ", "),
                        indent = 2L, exdent = 2L),
                "A 'vignettes' directory is required as from R 3.1.0")
          else
              c("Vignette sources in 'inst/doc' missing from the 'vignettes' directory:",
                strwrap(paste(sQuote(y), collapse = ", "),
                        indent = 2L, exdent = 2L))
      },
      if(length(y <- x$missing_vignette_index)) {
          "Package has a VignetteBuilder field but no prebuilt vignette index."
      }
      )
}

### * .check_Rd_metadata

.check_Rd_metadata <-
function(package, dir, lib.loc = NULL)
{
    ## Perform package-level Rd metadata checks:
    ## names and aliases must be unique within a package.

    ## Note that we cannot use Rd_aliases(), as this does
    ##   if(length(aliases))
    ##       sort(unique(unlist(aliases, use.names = FALSE)))

    out <- structure(list(), class = "check_Rd_metadata")

    if(!missing(package)) {
        if(length(package) != 1L)
            stop("argument 'package' must be of length 1")
        dir <- find.package(package, lib.loc)
        rds <- file.path(dir, "Meta", "Rd.rds")
        if(file_test("-f", rds)) {
            meta <- readRDS(rds)
            files <- meta$File
            names <- meta$Name
            aliases <- meta$Aliases
        } else {
            return(out)
        }
    } else {
        if(file_test("-d", file.path(dir, "man"))) {
            db <- Rd_db(dir = dir)
            files <- basename(names(db))
            names <- sapply(db, .Rd_get_metadata, "name")
            aliases <- lapply(db, .Rd_get_metadata, "alias")
        } else {
            return(out)
        }
    }

    ## <FIXME>
    ## Remove eventually, as .Rd_get_metadata() and hence Rd_info() now
    ## eliminate duplicated entries ...
    aliases <- lapply(aliases, unique)
    ## </FIXME>

    files_grouped_by_names <- split(files, names)
    files_with_duplicated_names <-
        files_grouped_by_names[sapply(files_grouped_by_names,
                                      length) > 1L]
    if(length(files_with_duplicated_names))
        out$files_with_duplicated_names <-
            files_with_duplicated_names

    files_grouped_by_aliases <-
        split(rep.int(files, sapply(aliases, length)),
              unlist(aliases, use.names = FALSE))
    files_with_duplicated_aliases <-
        files_grouped_by_aliases[sapply(files_grouped_by_aliases,
                                      length) > 1L]
    if(length(files_with_duplicated_aliases))
        out$files_with_duplicated_aliases <-
            files_with_duplicated_aliases

    out
}

format.check_Rd_metadata <-
function(x, ...)
{
    c(character(),
      if(length(bad <- x$files_with_duplicated_name)) {
          unlist(lapply(names(bad),
                 function(nm) {
                     c(gettextf("Rd files with duplicated name '%s':",
                                nm),
                       .pretty_format(bad[[nm]]))
                 }))
      },
      if(length(bad <- x$files_with_duplicated_aliases)) {
          unlist(lapply(names(bad),
                 function(nm) {
                     c(gettextf("Rd files with duplicated alias '%s':",
                                nm),
                       .pretty_format(bad[[nm]]))
                 }))
      })
}

## * .check_Rd_contents

.check_Rd_contents <-
function(package, dir, lib.loc = NULL)
{
    out <- list()
    class(out) <- "check_Rd_contents"

    ## Argument handling.
    if(!missing(package)) {
        if(length(package) != 1L)
            stop("argument 'package' must be of length 1")
        dir <- find.package(package, lib.loc)
        ## Using package installed in @code{dir} ...
    }
    else {
        if(missing(dir))
            stop("you must specify 'package' or 'dir'")
        ## Using sources from directory @code{dir} ...
        if(!file_test("-d", dir))
            stop(gettextf("directory '%s' does not exist", dir),
                 domain = NA)
        else
            dir <- file_path_as_absolute(dir)
    }

    db <- if(!missing(package))
        Rd_db(package, lib.loc = dirname(dir))
    else
        Rd_db(dir = dir)

    names(db) <- .Rd_get_names_from_Rd_db(db)

    ## Exclude internal objects from further computations.
    ind <- sapply(lapply(db, .Rd_get_metadata, "keyword"),
                  function(x) length(grep("^ *internal *$", x)) > 0L )
    if(any(ind))                        # exclude them
        db <- db[!ind]

    check_offending_autogenerated_content <-
        !identical(as.logical(Sys.getenv("_R_CHECK_RD_CONTENTS_AUTO_")),
                   FALSE)
    offending_autogenerated_content <- NULL

    for(nm in names(db)) {
        rd <- db[[nm]]

        ## Arguments with no description.
        arg_table <- .Rd_get_argument_table(rd)
        arguments_with_no_description <-
            arg_table[grepl("^[[:blank:]]*$", arg_table[, 2L]),
                      1L]

        ## Autogenerated Rd content which needs editing.
        if(check_offending_autogenerated_content)
            offending_autogenerated_content <-
                .Rd_get_offending_autogenerated_content(rd)

        if(length(arguments_with_no_description)
           || length(offending_autogenerated_content)) {
            out[[nm]] <-
                list(arguments_with_no_description =
                     arguments_with_no_description,
                     offending_autogenerated_content =
                     offending_autogenerated_content)
        }
    }

    out
}

format.check_Rd_contents <-
function(x, ...)
{
    .fmt <- function(nm) {
        y <- x[[nm]]
        c(if(length(arguments_with_no_description <-
                    y[["arguments_with_no_description"]])) {
              c(gettextf("Argument items with no description in Rd object '%s':",
                         nm),
                .pretty_format(arguments_with_no_description))
          },
          if(length(offending_autogenerated_content <-
                    y[["offending_autogenerated_content"]])) {
              c(gettextf("Auto-generated content requiring editing in Rd object '%s':",
                         nm),
                sprintf("  %s", offending_autogenerated_content[, 1L]))
          },
          "")
    }

    as.character(unlist(lapply(names(x), .fmt)))
}

### * .check_Rd_line_widths

.check_Rd_line_widths <-
function(dir, limit = c(usage = 95, examples = 105), installed = FALSE)
{
    db <- if(installed)
        Rd_db(basename(dir), lib.loc = dirname(dir))
    else
        Rd_db(dir = dir)
    out <- find_wide_Rd_lines_in_Rd_db(db, limit)
    class(out) <- "check_Rd_line_widths"
    attr(out, "limit") <- limit
    out
}

format.check_Rd_line_widths <-
function(x, ...)
{
    if(!length(x)) return(character())

    .truncate <- function(s) {
        ifelse(nchar(s) > 140L,
               paste(substring(s, 1, 140L),
                     "... [TRUNCATED]"),
               s)
    }

    limit <- attr(x, "limit")
    ## Rd2txt() by default adds a section indent of 5 also incorporated
    ## in the limits used for checking.  But users actually look at the
    ## line widths in their source Rd file, so remove the indent when
    ## formatting for reporting check results.
    ## (This should reduce confusion as long as we only check the line
    ## widths in verbatim type sections.)
    limit <- limit - 5L

    sections <- names(limit)

    .fmt <- function(nm) {
        y <- x[[nm]]
        c(sprintf("Rd file '%s':", nm),
          unlist(lapply(sections,
                        function(s) {
                            lines <- y[[s]]
                            if(!length(lines)) character() else {
                                c(sprintf("  \\%s lines wider than %d characters:",
                                          s, limit[s]),
                                  .truncate(lines))
                            }
                        }),
                 use.names = FALSE),
          "")
    }

    as.character(unlist(lapply(names(x), .fmt)))
}

find_wide_Rd_lines_in_Rd_db <-
function(x, limit = NULL)
{
    y <- lapply(x, find_wide_Rd_lines_in_Rd_object, limit)
    Filter(length, y)
}

find_wide_Rd_lines_in_Rd_object <-
function(x, limit = NULL)
{
    if(is.null(limit))
        limit <- list(usage = c(79, 95), examples = c(87, 105))
    sections <- names(limit)
    if(is.null(sections))
        stop("no Rd sections specified")
    y <- Map(function(s, l) {
        out <- NULL
        zz <- textConnection("out", "w", local = TRUE)
        on.exit(close(zz))
        pos <- which(RdTags(x) == s)
        Rd2txt(x[pos[1L]], out = zz, fragment = TRUE)
        nc <- nchar(out)
        if(length(l) > 1L) {
            ind_warn <- (nc > max(l))
            ind_note <- (nc > min(l)) & !ind_warn
            Filter(length,
                   list(warn = out[ind_warn], note = out[ind_note]))
        } else {
            out[nc > l]
        }
    },
             paste0("\\", sections),
             limit)
    names(y) <- sections
    Filter(length, y)
}


### * .find_charset

.find_charset <-
function()
{
    l10n <- l10n_info()
    enc <- if(l10n[["UTF-8"]]) "UTF-8" else utils::localeToCharset()
    cat("charset: ", enc, "\n", sep = "")
    invisible()
}


### * Utilities

### ** as.alist.call

as.alist.call <-
function(x)
{
    y <- as.list(x)
    ind <- if(is.null(names(y)))
        seq_along(y)
    else
        which(names(y) == "")
    if(length(ind)) {
        names(y)[ind] <- sapply(y[ind], paste, collapse = " ")
        y[ind] <- rep.int(list(alist(irrelevant = )[[1L]]), length(ind))
    }
    y
}

### ** as.alist.symbol

as.alist.symbol <-
function(x)
{
    as.alist.call(call(as.character(x)))
}

### ** .arg_names_from_call

.arg_names_from_call <-
function(x)
{
    y <- as.character(x)
    if(!is.null(nx <- names(x))) {
        ind <- which(nx != "")
        y[ind] <- nx[ind]
    }
    y
}

### ** .dquote_method_markup

## See the notes below.
## An alternative and possibly more efficient implementation could be
## based using gregexpr(re, txt), massaging the matches and merging with
## the non-matched parts.

.dquote_method_markup <-
function(txt, re)
{
    out <- ""
    while((ipos <- regexpr(re, txt)) > -1L) {
        epos <- ipos + attr(ipos, "match.length") - 1L
        str <- substring(txt, ipos, epos)
        str <- sub("\"", "\\\"", str, fixed = TRUE)
        str <- sub("\\", "\\\\", str, fixed = TRUE)
        out <- sprintf("%s%s\"%s\"", out,
                       substring(txt, 1L, ipos - 1L), str)
        txt <- substring(txt, epos + 1L)
    }
    paste0(out, txt)
}

### ** .format_calls_in_file

.format_calls_in_file <-
function(calls, f)
{
    c(gettextf("File %s:", sQuote(f)),
      paste0("  ",
             unlist(lapply(calls,
                           function(e)
                           paste(deparse(e), collapse = "\n")))))
}

### ** .functions_to_be_ignored_from_usage

.functions_to_be_ignored_from_usage <-
function(package_name)
{
    c("<-", "=",
      if(package_name == "base")
      c("(", "{", "function", "if", "for", "while", "repeat",
        "Math", "Ops", "Summary", "Complex"),
      if(package_name == "utils") "?",
      if(package_name == "methods") "@")
}

### ** .functions_with_no_useful_S3_method_markup

## <FIXME>
## Remove eventually ...
.functions_with_no_useful_S3_method_markup <-
function()
{
    ## Once upon a time ... there was no useful markup for S3 methods
    ## for subscripting/subassigning and binary operators.

    c(if(identical(as.logical(Sys.getenv("_R_CHECK_RD_USAGE_METHOD_SUBSET_")),
                   FALSE))
      c("[", "[[", "$", "[<-", "[[<-", "$<-"),
      if(identical(as.logical(Sys.getenv("_R_CHECK_RD_USAGE_METHOD_BINOPS_")),
                   FALSE))
      c("+", "-", "*", "/", "^", "<", ">", "<=", ">=", "!=", "==", "%%",
        "%/%", "&", "|"),
      "!")
}
## </FIXME>

### ** get_S4_generics_with_methods

## FIXME: make option of methods::getGenerics()
## JMC agreed & proposed argument  'excludeEmpty = FALSE'
get_S4_generics_with_methods <-
function(env, verbose = getOption("verbose"))
{
    env <- as.environment(env)
    ##  Filter(function(g) methods::isGeneric(g, where = env),
    ##	       methods::getGenerics(env))
    r <- methods::getGenerics(env)
    if(length(r) && {
	hasM <- lapply(r, function(g)
		       tryCatch(methods::hasMethods(g, where = env),
				error = identity))
	if(any(hasErr <- sapply(hasM, inherits, what = "error"))) {
            dq <- function(ch) paste0('"', ch ,'"')
            rErr <- r[hasErr]
            pkgs <- r@package[hasErr]
            ## FIXME: This warning should not happen here when called
            ## from R CMD check, but rather be part of a new "check"
            ## there !
	    warning(gettextf("Generics 'g' in 'env' %s where '%s' errors: %s\nMay need something like\n\n%s\nin NAMESPACE.",
                             format(env),
                             "hasMethods(g, env)",
                             paste(sQuote(rErr), collapse = ", "),
                             paste0("  importFrom(",
                                    paste(dq(pkgs), dq(rErr), sep =", "),
                                    ")\n")
                             ),
                    domain = NA)
	    hasM <- hasM[!hasErr]
	}
	!all(ok <- unlist(hasM))
    }) {
	if(verbose)
            message(sprintf(ngettext(sum(!ok),
                                     "Generic without any methods in %s: %s",
                                     "Generics without any methods in %s: %s"),
                            format(env),
                            paste(sQuote(r[!ok]), collapse = ", ")),
                    domain = NA)
	r[ok]
    }
    else as.vector(r)# for back-compatibility and current ..../tests/reg-S4.R
}

### ** .get_S4_generics

## For several QC tasks, we need to compute on "all S4 methods in/from a
## package".  These days, this can straightforwardly be accomplished by
## looking at all methods tables in the package environment or namespace.
## Somewhat historically, we organize our computations by first using
## using methods::getGenerics() to find all S4 generics the package has
## methods for, and then iterating over these.  To make this work
## conveniently, we wrap around methods::getGenerics() to rewrite its
## "ObjectsWithPackage" result into a (currently unclassed) list of
## generic-name-with-package-name-attribute objects, and wrap around
## methods::findMethods() to perform lookup based on this information
## (rather than the genericFunction object itself), and also rewrite the
## MethodsList result into a simple list.

.get_S4_generics <-
function(env)
{
    env <- as.environment(env)
    g <- methods::getGenerics(env)
    Map(function(f, p) {
            attr(f, "package") <- p
            f
        },
        g@.Data,
        g@package)
}

### ** .get_S4_methods_list

.get_S4_methods_list <-
function(f, env)
{
    ## Get S4 methods in environment env for f a structure with the name
    ## of the S4 generic and its package in the corresponding attribute.

    ## For the QC computations, we really only want the S4 methods
    ## defined in a package, so we try to exclude derived default
    ## methods as well as methods inherited from other environments.

    env <- as.environment(env)

    ## <FIXME>
    ## Use methods::findMethods() once this gets a package argument.
    ## This will return a listOfMethods object: turn this into a simple
    ## list of methods named by hash-collapsed signatures.
    tab <- get(methods:::.TableMetaName(f, attr(f, "package")), envir = env)
    nms <- objects(tab, all.names = TRUE)
    mlist <- lapply(nms, get, envir = tab)
    names(mlist) <- nms
    ## </FIXME>

    ## First, derived default methods (signature w/ "ANY").
    if(any(ind <- as.logical(sapply(mlist, methods::is,
				    "derivedDefaultMethod"))))
	mlist <- mlist[!ind]

    if(length(mlist)) {
        ## Determining the methods defined in a package from the package
        ## env or the associated namespace seems rather tricky.  What we
        ## seem to observe is the following.
        ## * If there is a namespace N, methods defined in the package
        ##   have N as their environment, for both the package env and
        ##   the associated namespace.
        ## * If there is no namespace, methods defined in the package
        ##   have an environment E which is empty and has globalenv() as
        ##   its parent.  (If the package defines generics, these seem
        ##   to have E as their parent env.)
        ## However, in the latter case, there seems no way to infer E
        ## from the package env.  In the old days predating methods
        ## tables, we compared methods in the package env with those in
        ## its parent env, and excluded the ones already found there.
        ## This no longer works, so we exclude "at least" all methods
        ## with a namespace environment (as these cannot come from a
        ## package with no namespace).

        namespace <- if(isNamespace(env)) env else .get_namespace_from_package_env(env)
        mlist <- if(!is.null(namespace))
            Filter(function(m) identical(environment(m), namespace), mlist)
        else
            Filter(function(m) environmentName(environment(m)) == "", mlist)
    }

    mlist
}

.get_ref_classes <-
function(env)
{
    env <- as.environment(env)
    cl <- methods::getClasses(env)
    cl <- cl[unlist(lapply(cl, function(Class) methods::is(methods::getClass(Class, where = env), "refClassRepresentation")))]
    if(length(cl)) {
        res <- lapply(cl, function(Class) {
            def <- methods::getClass(Class, where = env)
            ff <- def@fieldPrototypes
            accs <- unlist(lapply(ff, function(what) methods::is(what, "activeBindingFunction") && !methods::is(what, "defaultBindingFunction")))
            c(as.list(def@refMethods), as.list(ff)[accs])
        })
        names(res) <- cl
        res
    } else list()
}

.get_namespace_from_package_env <-
function(env)
{
    package <-
        sub(".*:([^_]*).*", "\\1", attr(env, "name", exact = TRUE))
    if(length(package) && nzchar(package)) .getNamespace(as.name(package))
}


### ** .is_call_from_replacement_function_usage

.is_call_from_replacement_function_usage <-
function(x)
{
    ((length(x) == 3L)
     && (identical(x[[1L]], as.symbol("<-")))
     && (length(x[[2L]]) > 1L)
     && is.symbol(x[[3L]]))
}

### ** .make_siglist

.make_siglist <-
function(x)
{
    ## Argument 'x' should be a named list of methods as obtained by
    ## methods::findMethods() or .get_S4_methods_list().
    gsub("#", ",", names(x), fixed = TRUE)
}

### ** .make_signatures

.make_signatures <-
function(cls)
{
    ## Note that (thanks JMC), when comparing signatures, the signature
    ## has to be stripped of trailing "ANY" elements (which are always
    ## implicit) or padded to a fixed length.
    sub("(#ANY)*$", "", unlist(lapply(cls, paste, collapse = "#")))
}

### ** .massage_file_parse_error_message

.massage_file_parse_error_message <-
function(x)
    sub("^[^:]+:[[:space:]]*", "", x)

### ** .package_env

.package_env <-
function(package_name)
{
    as.environment(paste("package", package_name, sep = ":"))
}

### ** .parse_text_as_much_as_possible

.parse_text_as_much_as_possible <-
function(txt)
{
    exprs <- tryCatch(parse(text = txt), error = identity)
    if(!inherits(exprs, "error")) return(exprs)
    exprs <- expression()
    lines <- unlist(strsplit(txt, "\n"))
    bad_lines <- character()
    while((n <- length(lines))) {
        i <- 1L; txt <- lines[1L]
        while(inherits(yy <- tryCatch(parse(text = txt),
                                      error = identity),
                       "error")
              && (i < n)) {
            i <- i + 1L; txt <- paste(txt, lines[i], collapse = "\n")
        }
        if(inherits(yy, "error")) {
            bad_lines <- c(bad_lines, lines[1L])
            lines <- lines[-1L]
        }
        else {
            exprs <- c(exprs, yy)
            lines <- lines[-seq_len(i)]
        }
    }
    attr(exprs, "bad_lines") <- bad_lines
    exprs
}

### ** .parse_usage_as_much_as_possible

.parse_usage_as_much_as_possible <-
function(x)
{
    if(!length(x)) return(expression())
    ## Drop specials and comments.
    ## <FIXME>
    ## Remove calling .Rd_drop_comments() eventually.
    x <- .Rd_drop_comments(x)
    ## </FIXME>
    txt <- .Rd_deparse(.Rd_drop_nodes_with_tags(x, "\\special"),
                       tag = FALSE)
    txt <- gsub("\\\\l?dots", "...", txt)
    txt <- .dquote_method_markup(txt, .S3_method_markup_regexp)
    txt <- .dquote_method_markup(txt, .S4_method_markup_regexp)
    ## Transform <<see below>> style markup so that we can catch and
    ## throw it, rather than "basically ignore" it by putting it in the
    ## bad_lines attribute.
    txt <- gsub("(<<?see below>>?)", "`\\1`", txt)
    ## \usage is only 'verbatim-like'
    ## <FIXME>
    ## 'LanguageClasses.Rd' in package methods has '"\{"' in its usage.
    ## But why should it use the backslash escape?
    txt <- gsub("\\{", "{", txt, fixed = TRUE)
    txt <- gsub("\\}", "}", txt, fixed = TRUE)
    ## </FIXME>
    ## now any valid escape by \ is
    ##   \a \b \f \n \r \t \u \U \v \x \' \" \\ or \octal
    txt <- gsub("(^|[^\\])\\\\($|[^abfnrtuUvx0-9'\"\\])",
                "\\1<unescaped bksl>\\2", txt)
    ## and since this may overlap, try again
    txt <- gsub("(^|[^\\])\\\\($|[^abfnrtuUvx0-9'\"\\])",
                "\\1<unescaped bksl>\\2", txt)
    .parse_text_as_much_as_possible(txt)
}

### ** .pretty_format

.pretty_format <-
function(x)
{
    strwrap(paste(sQuote(x), collapse = " "),
            indent = 2L, exdent = 2L)
}
.pretty_format2 <-
function(msg, x)
{
    xx <- strwrap(paste(sQuote(x), collapse = " "), exdent = 2L)
    if (length(xx) > 1L || (nchar(msg) + nchar(xx) + 1L > 75L))
        c(msg, .pretty_format(x))
    else paste(msg, xx, sep = " ")
}

### ** .pretty_print

.pretty_print <-
function(x)
{
    writeLines(strwrap(paste(x, collapse = " "),
                       indent = 2L, exdent = 2L))
}

### ** .strip_backticks

.strip_backticks <-
function(x)
    gsub("`", "", x)

### ** .transform_S3_method_markup

.transform_S3_method_markup <-
function(x)
{
    ## Note how we deal with S3 replacement methods found.
    ## These come out named "\method{GENERIC}{CLASS}<-" which we
    ## need to turn into 'GENERIC<-.CLASS'.
    re <- sprintf("%s(<-)?", .S3_method_markup_regexp)
    ## Note that this is really only called on "function" names obtained
    ## by parsing the \usage texts, so that the method regexps possibly
    ## augmented by '<-' fully match if they match.
    ## We should be able to safely strip all backticks; alternatively,
    ## we could do something like
    ##   cl <- .strip_backticks(sub(re, "\\4", x))
    ##   sub(re, sprintf("\\3\\5.%s", cl), x)
    .strip_backticks(sub(re, "\\3\\5.\\4", x))
}

### ** .transform_S4_method_markup

.transform_S4_method_markup <-
function(x)
{
    re <- sprintf("%s(<-)?", .S4_method_markup_regexp)
    ## We should be able to safely strip all backticks; alternatively,
    ## we could do something like
    ##   sl <- .strip_backticks(sub(re, "\\3", x))
    ##   sub(re, sprintf("\\\\S4method{\\2\\7}{%s}", sl), x)
    .strip_backticks(sub(re, "\\\\S4method{\\2\\7}{\\3}", x))
}

### ** .S3_method_markup_regexp

## For matching \(S3)?method{GENERIC}{CLASS}.
## GENERIC can be
## * a syntactically valid name
## * one of $ [ [[
## * one of the binary operators
##   + - * / ^ < <= > >= != == | & %something%
## (as supported by Rdconv).
## See also .functions_with_no_useful_S3_method_markup.
## CLASS can be a syntactic name (we could be more precise about the
## fact that these must start with a letter or '.'), or anything quoted
## by backticks (not containing backticks itself for now).  Arguably,
## non-syntactic class names should best be avoided, but R has always
## had them at least for
## R> class(bquote({.}))
## [1] "{"
## R> class(bquote((.)))
## [1] "("

## <NOTE>
## Handling S3/S4 method markup is somewhat tricky.
## When using R to parse the usage entries, we turn the
##   \METHOD{GENERIC}{CLASS_OR_SIGLIST}(args)
## markup into (something which parses to) a function call by suitably
## quoting the \METHOD{GENERIC}{CLASS_OR_SIGLIST} part.  In case of a
## replacement method
##   \METHOD{GENERIC}{CLASS_OR_SIGLIST}(args) <- value
## parsing results in a
##   \METHOD{GENERIC}{CLASS_OR_SIGLIST}<-
## pseudo name, which need to be transformed to
##   \METHOD{GENERIC<-}{CLASS_OR_SIGLIST}
## We currently use double quoting for the parse step.  As we also allow
## for non-syntactic class names quoted by backticks, this means that
## double quotes and backslashes need to be escaped.  Alternatively, we
## could strip backticks right away and quote by backticks, but then the
## replacement method transformation would need different regexps.
## </NOTE>

.S3_method_markup_regexp <-
    sprintf("(\\\\(S3)?method\\{(%s)\\}\\{(%s)\\})",
            paste(c("[._[:alnum:]]*",
                    ## Subscripting
                    "\\$", "\\[\\[?",
                    ## Binary operators and unary '!'.
                    "\\+", "\\-", "\\*", "\\/", "\\^",
                    "<=?", ">=?", "!=?", "==", "\\&", "\\|",
                    "\\%[[:alnum:][:punct:]]*\\%"),
                  collapse = "|"),
            "[._[:alnum:]]+|`[^`]+`")

### ** .S4_method_markup_regexp

## For matching \S4method{GENERIC}{SIGLIST}.
## SIGLIST can be a comma separated list of CLASS specs as above.

.S4_method_markup_regexp <-
    sprintf("(\\\\S4method\\{(%s)\\}\\{(%s)\\})",
            paste(c("[._[:alnum:]]*",
                    ## Subscripting
                    "\\$", "\\[\\[?",
                    ## Binary operators and unary '!'.
                    "\\+", "\\-", "\\*", "\\/", "\\^",
                    "<=?", ">=?", "!=?", "==", "\\&", "\\|",
                    "\\%[[:alnum:][:punct:]]*\\%"),
                  collapse = "|"),
            "(([._[:alnum:]]+|`[^`]+`),)*([._[:alnum:]]+|`[^`]+`)")

### ** .valid_maintainer_field_regexp

.make_RFC_2822_email_address_regexp <-
function()
{
    ## Local part consists of ASCII letters and digits, the characters
    ##   ! # $ % * / ? | ^ { } ` ~ & ' + = _ -
    ## and . provided it is not leading or trailing or repeated, or must
    ## be a quoted string.
    ## Domain part consists of dot-separated elements consisting of
    ## ASCII letters, digits and hyphen.
    ## We could also check that the local and domain parts are no longer
    ## than 64 and 255 characters, respectively.
    ## See http://en.wikipedia.org/wiki/Email_address.
    ASCII_letters_and_digits <-
        "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"
    l <- sprintf("[%s%s]", ASCII_letters_and_digits, "!#$%*/?|^{}`~&'+=_-")
    d <- sprintf("[%s%s]", ASCII_letters_and_digits, "-")
    ## Be careful to arrange the hyphens to come last in the range spec.
    sprintf("(\\\".+\\\"|(%s+\\.)*%s+)@(%s+\\.)*%s+", l, l, d, d)
}

.valid_maintainer_field_regexp <-
    sprintf("^[[:space:]]*(.*<%s>|ORPHANED)[[:space:]]*$",
            .make_RFC_2822_email_address_regexp())

### ** .Rd_get_offending_autogenerated_content

.Rd_get_offending_autogenerated_content <-
function(x)
{
    out <- NULL

    ## /data/rsync/PKGS/geoR/man/globalvar.Rd
    s <- .Rd_get_section(x, "title")
    if(length(s)) {
        s <- .Rd_deparse(s, tag = FALSE)
        if(.strip_whitespace(s) == "~~function to do ... ~~")
            out <- rbind(out, c("\\title", s))
    }
    s <- .Rd_get_section(x, "description")
    if(length(s)) {
        s <- .Rd_deparse(s, tag = FALSE)
        if(.strip_whitespace(s) ==
           "~~ A concise (1-5 lines) description of what the function does. ~~")
            out <- rbind(out, c("\\description", s))
    }
    s <- .Rd_get_section(x, "details")
    if(length(s)) {
        s <- .Rd_deparse(s, tag = FALSE)
        if(.strip_whitespace(s) ==
           "~~ If necessary, more details than the description above ~~")
            out <- rbind(out, c("\\details", s))
    }

    ## /data/rsync/PKGS/mimR/man/plot.Rd:\author{ ~~who you are~~ }
    s <- .Rd_get_section(x, "author")
    if(length(s)) {
        s <- .Rd_deparse(s, tag = FALSE)
        if(.strip_whitespace(s) == "~~who you are~~")
            out <- rbind(out, c("\\author", s))
    }
    ## /data/rsync/PKGS/mimR/man/mim-class.Rd:\note{ ~~further notes~~ }
    s <- .Rd_get_section(x, "note")
    if(length(s)) {
        s <- .Rd_deparse(s, tag = FALSE)
        if(.strip_whitespace(s) == "~~further notes~~")
            out <- rbind(out, c("\\note", s))
    }

    tab <- .Rd_get_argument_table(x)
    if(length(tab)) {
        ## /data/rsync/PKGS/Rmpfr/man/mpfrArray.Rd:
        ##   \item{precBits}{ ~~Describe \code{precBits} here~~ }
        descriptions <- .strip_whitespace(tab[, 2L])
        ind <- (descriptions ==
                sprintf("~~Describe \\code{%s} here~~", tab[, 1L]))
        if(any(ind))
            out <- rbind(out,
                         cbind(sprintf("\\arguments, description of item '%s'",
                                       tab[ind, 1L]),
                               tab[ind, 2L]))
    }

    ## <NOTE>
    ## Obviously, auto-generation does too much here, so maybe do not
    ## include these in production check code ...
    tab <- .Rd_get_methods_description_table(x)
    if(length(tab)) {
        descriptions <- .strip_whitespace(tab[, 2L])
        ## /data/rsync/PKGS/coin/man/initialize-methods.Rd
        ind <- descriptions == "~~describe this method here"
        if(any(ind))
            out <- rbind(out,
                         cbind(sprintf("section 'Methods', description of item '%s'",
                                       tab[ind, 1L]),
                               tab[ind, 2L]))
    }
    ## </NOTE>

    out
}


### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "### [*]+" ***
### End: ***

#  File src/library/tools/R/QC.R
#  Part of the R package, http://www.R-project.org
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
## .check_make_vars
## .createExdotR (testing.R)
## .runPackageTestsR (testing.R)
## .get_LaTeX_errors_from_log_file
## .check_package_CRAN_incoming
## .check_Rd_contents

## R CMD build uses .check_package_subdirs

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
        dir <- .find.package(package, lib.loc)
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

        code_env <- new.env()
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

        ## Need to do something about S4 generic functions 'created' by
        ## setGeneric() or setMethod() on 'ordinary' functions.
        ## The test below exempts objects that are generic functions
        ## which are 'derived', either by importing from another
        ## package or from a default method.
        ## In the long run we need dynamic documentation.
        if(.isMethodsDispatchOn()) {
            code_objs <-
                Filter(function(f) {
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
        code_objs <-
	    code_objs %w/o% c("Arith", "Compare", "Complex", "Logic",
			      "Math", "Math2", "Ops", "Summary")
    }

    undoc_things <-
        list("code objects" =
             unique(code_objs %w/o% all_doc_topics),
             "data sets" =
             unique(data_objs %w/o% all_doc_topics))

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
                paste(g, ",", sigs, sep = "")
            else
                character()
        }
        S4_methods <- lapply(get_S4_generics_with_methods(code_env),
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
        ## we use .ArgsEnv and .GenericArgsEnv in checkS3methods and codoc,
        ## so we check here that the set of primiitives has not been changed.
        base_funs <- ls("package:base", all.names=TRUE)
        prim <- sapply(base_funs,
                       function(x) is.primitive(get(x, "package:base")))
        prims <- base_funs[prim]
        prototypes <- sort(c(ls(envir=.ArgsEnv, all.names=TRUE),
                             ls(envir=.GenericArgsEnv, all.names=TRUE)))
        extras <- prototypes %w/o% prims
        if(length(extras))
            undoc_things <- c(undoc_things, list(prim_extra=extras))
        langElts <- c("$","$<-","&&","(",":","@","[","[[",
                      "[[<-","[<-","{","||","~","<-","<<-","=","break","for",
                      "function","if","next","repeat","return", "while")
        miss <- prims %w/o% c(langElts, prototypes)
        if(length(miss))
            undoc_things <- c(undoc_things, list(primitives=miss))
    }

    class(undoc_things) <- "undoc"
    undoc_things
}

print.undoc <-
function(x, ...)
{
    for(i in which(sapply(x, length) > 0L)) {
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
        writeLines(msg)
        ## We avoid markup for indicating S4 methods, hence need to
        ## special-case output for these ...
        if(tag == "S4 methods")
            writeLines(strwrap(x[[i]], indent = 2L, exdent = 4L))
        else
            .pretty_print(x[[i]])
    }
    invisible(x)
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
        dir <- .find.package(package, lib.loc)
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
                (objects(envir = ns_env, all.names = TRUE) %w/o%
                 c(".__NAMESPACE__.", ".__S3MethodsTable__."))
            objects_in_code_or_namespace <-
                unique(c(objects_in_code, objects_in_ns))
            objects_in_ns <- objects_in_ns %w/o% objects_in_code
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

        code_env <- new.env()
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
            functions_in_S3Table <- character(0L)
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
                ".onLoad", ".onAttach", ".onUnload"))
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
               function(f) formals(get(f, envir = code_env)))
    names(function_args_in_code) <- functions_in_code
    if(has_namespace) {
        functions_in_ns <-
            Filter(function(f) {
                       f <- get(f, envir = ns_env)
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
            lapply(get_S4_generics_with_methods(code_env),
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
    ind <- db_names %in% paste(package_name, "defunct", sep="-")
    db <- db[!ind]
    db_names <- db_names[!ind]

    db_usages <- lapply(db, .Rd_get_section, "usage")
    db_synopses <- lapply(db, .Rd_get_section, "synopsis")
    ind <- sapply(db_synopses, length) > 0L
    db_usages[ind] <- db_synopses[ind]
    with_synopsis <- as.character(db_names[ind])
    db_usages <- lapply(db_usages, .parse_usage_as_much_as_possible)
    ind <- as.logical(sapply(db_usages,
                             function(x) !is.null(attr(x, "bad_lines"))))
    bad_lines <- lapply(db_usages[ind], attr, "bad_lines")

    functions_to_be_ignored <-
        c(.functions_to_be_ignored_from_usage(basename(dir)),
          .functions_with_no_useful_S3_method_markup())

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
            data_sets <- data_sets %w/o% data_sets_in_code
            if(length(data_sets))
                data_sets_in_usages_not_in_code[[docObj]] <- data_sets
            exprs <- exprs[!ind]
        }
        functions <- sapply(exprs, function(e) as.character(e[[1L]]))
        functions <- .transform_S3_method_markup(as.character(functions))
        ind <- (! functions %in% functions_to_be_ignored
                & functions %in% functions_in_code)
        bad_functions <-
            mapply(functions[ind],
                   exprs[ind],
                   FUN = function(x, y)
                   check_codoc(x, as.pairlist(as.alist.call(y[-1L]))),
                   SIMPLIFY = FALSE)
        ## Replacement functions.
        ind <- as.logical(sapply(exprs,
                                 .is_call_from_replacement_function_usage))
        if(any(ind)) {
            exprs <- exprs[ind]
            replace_funs <-
                paste(sapply(exprs,
                             function(e) as.character(e[[2L]][[1L]])),
                      "<-",
                      sep = "")
            replace_funs <- .transform_S3_method_markup(replace_funs)
            functions <- c(functions, replace_funs)
            ind <- (replace_funs %in% functions_in_code)
            if(any(ind)) {
                bad_replace_funs <-
                    mapply(replace_funs[ind],
                           exprs[ind],
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
        bad_functions <-
            functions %w/o% c(objects_in_code_or_namespace,
                              functions_to_be_ignored)
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
        objects_in_code %w/o% c(functions_in_usages, variables_in_usages)
    functions_in_code_not_in_usages <-
        intersect(functions_in_code, objects_in_code_not_in_usages)
    ## (Note that 'functions_in_code' does not necessarily contain all
    ## (exported) functions in the package.)

    ## Determine functions which have no usage but really should have.
    ## If there is no name space (including base), we have no idea.
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
                    functions %w/o% get_S4_generics_with_methods(code_env)
            }
            ## Drop the defunct functions.
            is_defunct <- function(f) {
                f <- get(f, envir = code_env)
                if(!is.function(f)) return(FALSE)
                (is.call(b <- body(f))
                 && identical(as.character(b[[1L]]), ".Defunct"))
            }
            functions[!sapply(functions, is_defunct)]
        }
    objects_missing_from_usages <-
        if(!has_namespace) character() else {
            c(functions_missing_from_usages,
              (objects_in_code_not_in_usages
               %w/o% c(functions_in_code, data_sets_in_code)))
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
    attr(bad_doc_objects, "with_synopsis") <- with_synopsis
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
            writeLines(gettextf("Functions/methods with usage in documentation object '%s' but not in code:",
                                fname))
            .pretty_print(unique(functions_in_usages_not_in_code[[fname]]))
            writeLines("")
        }
    }

    data_sets_in_usages_not_in_code <-
        attr(x, "data_sets_in_usages_not_in_code")
    if(length(data_sets_in_usages_not_in_code)) {
        for(fname in names(data_sets_in_usages_not_in_code)) {
            writeLines(gettextf("Data sets with usage in documentation object '%s' but not in code:",
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
            paste("function(", paste(s, collapse = ", "), ")", sep = "")
        else {
            s <- paste(deparse(s), collapse = "")
            s <- gsub(" = ([,\\)])", "\\1", s)
            s <- gsub("<unescaped bksl>", "\\", s, fixed = TRUE)
            gsub("^list", "function", s)
        }
    }

    summarize_mismatches_in_names <- function(nfc, nfd) {
        if(length(nms <- nfc %w/o% nfd))
            writeLines(c(gettext("  Argument names in code not in docs:"),
                         strwrap(paste(nms, collapse = " "),
                                 indent = 4L, exdent = 4L)))
        if(length(nms <- nfd %w/o% nfc))
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
                cv <- deparse(vffc[[i]])
                dv <- deparse(vffd[[i]])
                dv <- gsub("<unescaped bksl>", "\\", dv, fixed = TRUE)
                writeLines(sprintf("    Name: '%s' Code: %s Docs: %s",
                                   nms[i], cv, dv))
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
    dir <- .find.package(package, lib.loc)
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
        unlist(lapply(X, FUN, ...), recursive=FALSE, use.names=FALSE)
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
           !all(c.in.d <- (codeSlots %w/o% superSlots) %in% docSlots) ) {
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

print.codocClasses <-
function(x, ...)
{
    if(!length(x))
        return(invisible(x))
    capWord <- function(w) sub("\\b(\\w)", "\\U\\1", w, perl=TRUE)
    wrapPart <- function(nam) {
	if(length(O <- docObj[[nam]]))
	    strwrap(sprintf("%s: %s", gettextf(capWord(nam)),
			    paste(O, collapse = " ")),
		    indent = 2L, exdent = 8L)
    }
    for (docObj in names(x)) {
        writeLines(gettextf("S4 class codoc mismatches from documentation object '%s':",
                            docObj))
        docObj <- x[[docObj]]
        writeLines(c(gettextf("Slots for class '%s'", docObj[["name"]]),
		     wrapPart("code"),
		     wrapPart("inherited"),
		     wrapPart("docs")))
        writeLines("")
    }
    invisible(x)
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

    dir <- .find.package(package, lib.loc)

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

    data_env <- new.env()
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

print.codocData <-
function(x, ...)
{
    format_args <- function(s) paste(s, collapse = " ")
    for(docObj in names(x)) {
        writeLines(gettextf("Data codoc mismatches from documentation object '%s':",
                            docObj))
        docObj <- x[[docObj]]
        writeLines(c(gettextf("Variables in data frame '%s'",
                              docObj[["name"]]),
                     strwrap(gettextf("Code: %s",
                                      format_args(docObj[["code"]])),
                             indent = 2L, exdent = 8L),
                     strwrap(gettextf("Docs: %s",
                                      format_args(docObj[["docs"]])),
                             indent = 2L, exdent = 8L)))
        writeLines("")
    }
    invisible(x)
}

### * checkDocFiles

checkDocFiles <-
function(package, dir, lib.loc = NULL)
{
    ## Argument handling.
    if(!missing(package)) {
        if(length(package) != 1L)
            stop("argument 'package' must be of length 1")
        dir <- .find.package(package, lib.loc)
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

    functions_to_be_ignored <-
        .functions_to_be_ignored_from_usage(basename(dir))

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
        ## Ordinary functions.
        functions <- as.character(sapply(exprs,
                                         function(e)
                                         as.character(e[[1L]])))
        ## (Note that as.character(sapply(exprs, "[[", 1L)) does not do
        ## what we want due to backquotifying.)
        ind <- ! functions %in% functions_to_be_ignored
        functions <- functions[ind]
        arg_names_in_usage <-
            unlist(sapply(exprs[ind],
                          function(e) .arg_names_from_call(e[-1L])))
        ## Replacement functions.
        ind <- as.logical(sapply(exprs,
                                 .is_call_from_replacement_function_usage))
        if(any(ind)) {
            replace_funs <-
                paste(sapply(exprs[ind],
                             function(e) as.character(e[[2L]][[1L]])),
                      "<-",
                      sep = "")
            functions <- c(functions, replace_funs)
            arg_names_in_usage <-
                c(arg_names_in_usage,
                  unlist(sapply(exprs[ind],
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
            arg_names_in_usage %w/o% arg_names_in_arg_list
        arg_names_in_arg_list_missing_in_usage <-
            arg_names_in_arg_list %w/o% arg_names_in_usage
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
                          !grepl(paste("\\b", x, "\\b", sep = ""),
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
                functions %w/o% .functions_with_no_useful_S3_method_markup()
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
            functions_not_in_aliases <- functions %w/o% aliases
        }
        else
            functions_not_in_aliases <- character()

        if((length(arg_names_in_usage_missing_in_arg_list))
           || anyDuplicated(arg_names_in_arg_list)
           || (length(arg_names_in_arg_list_missing_in_usage))
           || (length(functions_not_in_aliases)))
            bad_doc_objects[[docObj]] <-
                list(missing = arg_names_in_usage_missing_in_arg_list,
                     duplicated =
                     arg_names_in_arg_list[duplicated(arg_names_in_arg_list)],
                     overdoc = arg_names_in_arg_list_missing_in_usage,
                     unaliased = functions_not_in_aliases)

    }

    class(bad_doc_objects) <- "checkDocFiles"
    attr(bad_doc_objects, "bad_lines") <- bad_lines
    bad_doc_objects
}

print.checkDocFiles <-
function(x, ...)
{
    for(doc_obj in names(x)) {
        arg_names_in_usage_missing_in_arg_list <- x[[doc_obj]][["missing"]]
        if(length(arg_names_in_usage_missing_in_arg_list)) {
            writeLines(gettextf("Undocumented arguments in documentation object '%s'",
                                doc_obj))
            .pretty_print(unique(arg_names_in_usage_missing_in_arg_list))
        }
        duplicated_args_in_arg_list <- x[[doc_obj]][["duplicated"]]
        if(length(duplicated_args_in_arg_list)) {
            writeLines(gettextf("Duplicated \\argument entries in documentation object '%s':",
                                doc_obj))
            .pretty_print(duplicated_args_in_arg_list)
        }
        arg_names_in_arg_list_missing_in_usage <- x[[doc_obj]][["overdoc"]]
        if(length(arg_names_in_arg_list_missing_in_usage)) {
            writeLines(gettextf("Documented arguments not in \\usage in documentation object '%s':",
                                doc_obj))
            .pretty_print(unique(arg_names_in_arg_list_missing_in_usage))
        }
        functions_not_in_aliases <- x[[doc_obj]][["unaliased"]]
        if(length(functions_not_in_aliases)) {
            writeLines(gettextf("Objects in \\usage without \\alias in documentation object '%s':",
                                doc_obj))
            .pretty_print(unique(functions_not_in_aliases))
        }

        writeLines("")
    }

    if(!identical(as.logical(Sys.getenv("_R_CHECK_WARN_BAD_USAGE_LINES_")),
                  FALSE)
       && length(bad_lines <- attr(x, "bad_lines"))) {
        for(doc_obj in names(bad_lines)) {
            writeLines(gettextf("Bad \\usage lines found in documentation object '%s':",
                                doc_obj))
            writeLines(paste(" ", bad_lines[[doc_obj]]))
        }
        writeLines("")
    }

    invisible(x)
}

### * checkDocStyle

checkDocStyle <-
function(package, dir, lib.loc = NULL)
{
    has_namespace <- FALSE

    ## Argument handling.
    if(!missing(package)) {
        if(length(package) != 1L)
            stop("argument 'package' must be of length 1")
        dir <- .find.package(package, lib.loc)
        ## Using package installed in 'dir' ...
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
        if(packageHasNamespace(package, dirname(dir))) {
            has_namespace <- TRUE
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

        code_env <- new.env()
        dfile <- file.path(dir, "DESCRIPTION")
        meta <- if(file_test("-f", dfile))
            .read_description(dfile)
        else
            character()
        .source_assignments_in_code_dir(code_dir, code_env, meta)
        sys_data_file <- file.path(code_dir, "sysdata.rda")
        if(file_test("-f", sys_data_file)) load(sys_data_file, code_env)

        objects_in_code <- objects(envir = code_env, all.names = TRUE)

        ## Does the package have a NAMESPACE file?  Note that when
        ## working on the sources we (currently?) cannot deal with the
        ## (experimental) alternative way of specifying the namespace.
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
        Filter(function(f) is.function(get(f, envir = code_env)),
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
    methods_stop_list <- .make_S3_methods_stop_list(basename(dir))
    methods_in_package <- sapply(all_S3_generics, function(g) {
        if(!exists(g, envir = code_env)) return(character())
        ## <FIXME>
        ## We should really determine the name g dispatches for, see
        ## a current version of methods() [2003-07-07].  (Care is needed
        ## for internal generics and group generics.)
        ## Matching via grep() is tricky with e.g. a '$' in the name of
        ## the generic function ... hence substr().
        name <- paste(g, ".", sep = "")
        methods <-
            functions_in_code[substr(functions_in_code, 1L,
                                     nchar(name, type="c")) == name]
        ## </FIXME>
        methods <- methods %w/o% methods_stop_list
        if(has_namespace) {
            ## Find registered methods for generic g.
            methods <- c(methods, ns_S3_methods[ns_S3_generics == g])
        }
        methods
    })
    all_methods_in_package <- unlist(methods_in_package)
    ## There are situations where S3 methods might be documented as
    ## functions (i.e., with their full name), if they do something
    ## useful also for arguments not inheriting from the class they
    ## provide a method for.  Let's allow for this in the case the
    ## package has a namespace and the method is exported (even though
    ## we strongly prefer using FOO(as.BAR(x)) to FOO.BAR(x) for such
    ## cases).
    if(has_namespace)
        all_methods_in_package <-
            all_methods_in_package %w/o% functions_in_code

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
                paste(sapply(exprs[ind],
                             function(e) as.character(e[[2L]][[1L]])),
                      "<-",
                      sep = "")
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

print.checkDocStyle <-
function(x, ...) {
    for(docObj in names(x)) {
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
        methods_with_full_name <- x[[docObj]][["withFullName"]]
        if(length(methods_with_full_name)) {
            writeLines(gettextf("S3 methods shown with full name in documentation object '%s':",
                                docObj))
            .pretty_print(methods_with_full_name)
            writeLines("")
        }
    }
    invisible(x)
}

### * checkFF

checkFF <-
function(package, dir, file, lib.loc = NULL,
         verbose = getOption("verbose"))
{
    has_namespace <- FALSE
    ## Argument handling.
    if(!missing(package)) {
        if(length(package) != 1L)
            stop("argument 'package' must be of length 1")
        dir <- .find.package(package, lib.loc)
        ## Using package installed in @code{dir} ...
        code_dir <- file.path(dir, "R")
        if(!file_test("-d", code_dir))
            stop(gettextf("directory '%s' does not contain R code",
                          dir),
                 domain = NA)
        if(basename(dir) != "base")
            .load_package_quietly(package, lib.loc)
        code_env <- if(packageHasNamespace(package, dirname(dir))) {
            ce <- asNamespace(package)
            if(exists("DLLs", envir = ce$.__NAMESPACE__.)) {
                DLLs <- get("DLLs", envir = ce$.__NAMESPACE__.)
                has_namespace <- length(DLLs) > 0L
            }
            ce
        } else
            .package_env(package)
    }
    else if(!missing(dir)) {
        ## Using sources from directory @code{dir} ...
        if(!file_test("-d", dir))
            stop(gettextf("directory '%s' does not exist", dir),
                 domain = NA)
        else
            dir <- file_path_as_absolute(dir)
        dfile <- file.path(dir, "DESCRIPTION")
        enc <- if(file.exists(dfile))
            .read_description(dfile)["Encoding"] else NA
        if(file.exists(file.path(dir, "NAMESPACE"))) {
            nm <- parseNamespaceFile(basename(dir), dirname(dir))
            has_namespace <- length(nm$dynlibs)
        }
        code_dir <- file.path(dir, "R")
        if(!file_test("-d", code_dir))
            stop(gettextf("directory '%s' does not contain R code",
                          dir),
                 domain = NA)
        file <- tempfile()
        on.exit(unlink(file))
        if(!file.create(file)) stop("unable to create ", file)
        if(!all(.file_append_ensuring_LFs(file,
                                          list_files_with_type(code_dir,
                                                               "code"))))
            stop("unable to write code files")
    }
    else if(!missing(file))
        enc <- NA
    else
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

    bad_exprs <- list()
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

    find_bad_exprs <- function(e) {
        if(is.call(e) || is.expression(e)) {
            ## <NOTE>
            ## This picks up all calls, e.g. a$b, and they may convert
            ## to a vector.  The function is the first element in all
            ## the calls we are interested in.
            ## BDR 2002-11-28
            ## </NOTE>
            if(deparse(e[[1L]])[1L] %in% FF_funs) {
                if(!is.character(e[[2L]])) parg <- "Called with symbol"
                else {
                    parg <- e[["PACKAGE"]]
                    parg <- if(!is.null(parg) && (parg != "")) "OK"
                    else if(!has_namespace) {
                        bad_exprs <<- c(bad_exprs, e)
                        "MISSING"
                    } else "MISSING but in a function in a namespace"
                }
                if(verbose)
                    cat(deparse(e[[1L]]), "(", deparse(e[[2L]]),
                        ", ...): ", parg, "\n", sep = "")
            }
            for(i in seq_along(e)) Recall(e[[i]])
        }
    }

    if(!missing(package)) {
        exprs <- lapply(ls(envir = code_env, all.names = TRUE),
                        function(f) {
                            f <- get(f, envir = code_env)
                            if(typeof(f) == "closure")
                                body(f)
                            else
                                NULL
                        })
        if(.isMethodsDispatchOn()) {
            ## Also check the code in S4 methods.
            ## This may find things twice if a setMethod() with a bad FF
            ## call is from inside a function (e.g., InitMethods()).
            for(f in get_S4_generics_with_methods(code_env)) {
                mlist <- .get_S4_methods_list(f, code_env)
                exprs <- c(exprs, lapply(mlist, body))
            }
        }
    }
    else {
        if(!is.na(enc) &&
           !(Sys.getlocale("LC_CTYPE") %in% c("C", "POSIX"))) {
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
    class(bad_exprs) <- "checkFF"
    if(verbose)
        invisible(bad_exprs)
    else
        bad_exprs
}

print.checkFF <-
function(x, ...)
{
    if(length(x)) {
        writeLines(gettextf("Foreign function calls without 'PACKAGE' argument:"))
        for(i in seq_along(x)) {
            writeLines(paste(deparse(x[[i]][[1L]]),
                             "(",
                             deparse(x[[i]][[2L]]),
                             ", ...)",
                             sep = ""))
        }
    }
    invisible(x)
}

### * checkS3methods

checkS3methods <-
function(package, dir, lib.loc = NULL)
{
    has_namespace <- FALSE
    ## If an installed package has a namespace, we need to record the S3
    ## methods which are registered but not exported (so that we can
    ## get() them from the right place).
    S3_reg <- character(0L)

    ## Argument handling.
    if(!missing(package)) {
        if(length(package) != 1L)
            stop("argument 'package' must be of length 1")
        dir <- .find.package(package, lib.loc)
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
            S3_reg <- ns_S3_methods %w/o% objects_in_code
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

        code_env <- new.env()
        dfile <- file.path(dir, "DESCRIPTION")
        meta <- if(file_test("-f", dfile))
            .read_description(dfile)
        else
            character()
        .source_assignments_in_code_dir(code_dir, code_env, meta)
        sys_data_file <- file.path(code_dir, "sysdata.rda")
        if(file_test("-f", sys_data_file)) load(sys_data_file, code_env)

        objects_in_code <- objects(envir = code_env, all.names = TRUE)

        ## Does the package have a NAMESPACE file?  Note that when
        ## working on the sources we (currently?) cannot deal with the
        ## (experimental) alternative way of specifying the namespace.
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
        Filter(function(f) is.function(get(f, envir = code_env)),
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
        name <- paste(g, ".", sep = "")
        methods <-
            functions_in_code[substr(functions_in_code, 1L,
                                     nchar(name, type="c")) == name]
        ## </FIXME>
        methods <- methods %w/o% methods_stop_list
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

print.checkS3methods <-
function(x, ...)
{
    format_args <- function(s)
        paste("function(", paste(s, collapse = ", "), ")", sep = "")
    for(entry in x) {
        writeLines(c(paste(names(entry)[1L], ":", sep = ""),
                     strwrap(format_args(entry[[1L]]),
                             indent = 2L, exdent = 11L),
                     paste(names(entry)[2L], ":", sep = ""),
                     strwrap(format_args(entry[[2L]]),
                             indent = 2L, exdent = 11L),
                     ""))
    }
    invisible(x)
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
        dir <- .find.package(package, lib.loc)
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

        code_env <- new.env()
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
        objects_in_code <- objects_in_code %w/o% ns_S3_methods
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
                   f <- get(f, envir = code_env)
                   if(!is.function(f)) return(FALSE)
                   ! .check_last_formal_arg(f)
               },
               replace_funs)
    } else character(0L)

    if(.isMethodsDispatchOn()) {
        S4_generics <- get_S4_generics_with_methods(code_env)
        ## Assume that the ones with names ending in '<-' are always
        ## replacement functions.
        S4_generics <- grep("<-$", S4_generics, value = TRUE)
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

print.checkReplaceFuns <-
function(x, ...)
{
    if(length(x)) .pretty_print(unclass(x))
    invisible(x)
}

### * checkTnF

checkTnF <-
function(package, dir, file, lib.loc = NULL)
{
    code_files <- docs_files <- character(0L)

    ## Argument handling.
    if(!missing(package)) {
        if(length(package) != 1L)
            stop("argument 'package' must be of length 1")
        ## Using package installed in @code{dir} ...
        dir <- .find.package(package, lib.loc)
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

print.checkTnF <-
function(x, ...)
{
    for(fname in names(x)) {
        writeLines(gettextf("File '%s':", fname))
        xfname <- x[[fname]]
        for(i in seq_along(xfname)) {
            writeLines(strwrap(gettextf("found T/F in %s",
                                        paste(deparse(xfname[[i]]),
                                              collapse = "")),
                               exdent = 4L))
        }
        writeLines("")
    }
    invisible(x)
}

### * .check__depends

## changed in 2.3.0 to refer to a source dir.

.check_package_depends <-
function(dir, force_suggests = TRUE)
{
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
    ldepends <- .get_requires_with_version_from_package_db(db, "Depends")
    limports <- .get_requires_with_version_from_package_db(db, "Imports")
    lsuggests <- .get_requires_with_version_from_package_db(db, "Suggests")
    ## NB: no one checks version for 'Enhances'.
    lenhances <- .get_requires_with_version_from_package_db(db, "Enhances")

    depends <- sapply(ldepends, `[[`, 1L)
    imports <- sapply(limports, `[[`, 1L)
    suggests <- sapply(lsuggests, `[[`, 1L)

    standard_package_names <- .get_standard_package_names()

    bad_depends <- list()

    ## Are all packages listed in Depends/Suggests/Imports installed?
    lreqs <- c(ldepends,
               limports,
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
            reqs <- reqs %w/o% installed
            m <- reqs %in% standard_package_names$stubs
            if(length(reqs[!m]))
                bad_depends$required_but_not_installed <- reqs[!m]
            if(length(reqs[m]))
                bad_depends$required_but_stub <- reqs[m]
            ## now check versions
            have_ver <- unlist(lapply(lreqs, function(x) length(x) == 3L))
            lreqs3 <- lreqs[have_ver]
            if(length(lreqs3)) {
                bad <- character(0)
                for (r in lreqs3) {
                    pkg <- r[[1L]]
                    op <- r[[2L]]
                    where <- which(installed == pkg)
                    if(!length(where)) next
                    ## want the first one
                    desc <- .readRDS(file.path(installed_in[where[1L]], pkg,
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
            m <- sapply(lenhances, `[[`, 1L)  %w/o% installed
            if(length(m))
                bad_depends$enhances_but_not_installed <- m
        }
        if (!force_suggests && length(lsuggests)) {
            m <- sapply(lsuggests, `[[`, 1L)  %w/o% installed
            if(length(m))
                bad_depends$suggests_but_not_installed <- m
        }
   }
    ## Are all vignette dependencies at least suggested or equal to
    ## the package name?
    vignette_dir <- file.path(dir, "inst", "doc")
    if(file_test("-d", vignette_dir)
       && length(list_files_with_type(vignette_dir, "vignette"))) {
        reqs <- unique(unlist(.build_vignette_index(vignette_dir)$Depends))
        ## For the time being, ignore base packages missing from the
        ## DESCRIPTION dependencies even if explicitly given as vignette
        ## dependencies.
        reqs <- reqs %w/o% c(depends, imports, suggests, package_name,
                             standard_package_names$base)
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
            standard_package_names$base %w/o% c("methods", "stats4")
        reqs <- reqs %w/o% c(imports, depends, allowed_imports)
        if(length(reqs))
            bad_depends$missing_namespace_depends <- reqs
    }

    class(bad_depends) <- "check_package_depends"
    bad_depends
}

print.check_package_depends <-
function(x, ...)
{
    if(length(bad <- x$required_but_not_installed) > 1L) {
        writeLines(gettext("Packages required but not available:"))
        .pretty_print(bad)
        writeLines("")
    } else if (length(bad))
        writeLines(c(gettextf("Package required but not available: %s", bad),
                     ""))
    if(length(bad <- x$required_but_obsolete) > 1L) {
        writeLines(gettext("Packages required and available but unsuitable versions:"))
        .pretty_print(bad)
        writeLines("")
    } else if (length(bad))
        writeLines(c(gettextf("Package required and available but unsuitable version: %s", bad),
                     ""))
    if(length(bad <- x$required_but_stub) > 1L) {
        writeLines(gettext("Former standard packages required but now defunct:"))
        .pretty_print(bad)
        writeLines("")
    } else if (length(bad))
        writeLines(c(gettextf("Package required but not available: %s", bad),
                     ""))

    if(length(bad <- x$suggests_but_not_installed) > 1L) {
        writeLines(gettext("Packages suggested but not available for checking:"))
        .pretty_print(bad)
        writeLines("")
    } else if (length(bad))
        writeLines(c(gettextf("Package suggested but not available for checking: %s", bad),
                     ""))

    if(length(bad <- x$enhances_but_not_installed) > 1L) {
        writeLines(gettext("Packages which this enhances but not available for checking:"))
        .pretty_print(bad)
        writeLines("")
    } else if (length(bad))
        writeLines(c(gettextf("Package which this enhances but not available for checking: %s", bad),
                     ""))

    if(length(bad <- x$missing_vignette_depends)) {
        if(length(bad) > 1L) {
            writeLines(gettext("Vignette dependencies not required:"))
            .pretty_print(bad)
        } else
           writeLines(gettextf("Vignette dependencies not required: %s", bad))
        msg <- gettext("Vignette dependencies (\\VignetteDepends{} entries) must be contained in the DESCRIPTION Depends/Suggests/Imports entries.")
        writeLines(strwrap(msg))
        writeLines("")
    }
    if(length(bad <- x$missing_namespace_depends) > 1L) {
        writeLines(gettext("Namespace dependencies not required:"))
        .pretty_print(bad)
        writeLines("")
    } else if (length(bad))
        writeLines(c(gettextf("Namespace dependency not required: %s", bad),
                     ""))

    invisible(x)
}


### * .check_package_description

.check_package_description <-
function(dfile)
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

    ## Mandatory entries in DESCRIPTION:
    ##   Package, Version, License, Description, Title, Author,
    ##   Maintainer.
    required_fields <- c("Package", "Version", "License", "Description",
                         "Title", "Author", "Maintainer")
    if(length(i <- which(is.na(match(required_fields, names(db))) |
                         is.na(db[required_fields]))))
        out$missing_required_fields <- required_fields[i]

    val <- package_name <- db["Package"]
    if(!is.na(val)) {
        tmp <- character()
        if(!grepl(sprintf("^%s$", valid_package_name_regexp), val)
           && !grepl("^Translation-[[:alnum:].]+$", val))
            tmp <- c(tmp, gettext("Malformed package name"))
        if(!is_base_package) {
            if(val %in% standard_package_names$base)
                tmp <- c(tmp,
                         c(gettext("Invalid package name."),
                           gettext("This is the name of a base package.")))
            else if(val %in% standard_package_names$stubs)
                tmp <- c(tmp,
                         c(gettext("Invalid package name."),
                           gettext("This name was used for a base package and is remapped by library().")))
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
            paste("^[[:space:]]*",
                  paste("(", valid_package_name_regexp, ")", sep = ""),
                  "([[:space:]]*\\(([^) ]+)[[:space:]]+([^) ]+)\\))?",
                  "[[:space:]]*$",
                  sep = "")
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
                else if(!grepl(sprintf("^%s$",
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
    if(!is.na(val <- db["Namespace"])
       && !is.na(package_name)
       && (val != package_name))
        out$bad_namespace <- val
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
    if(length(x$missing_required_fields)) {
        writeLines(gettext("Required fields missing:"))
        .pretty_print(x$missing_required_fields)
        writeLines("")
    }
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
    if(length(x$bad_namespace))
        writeLines(c(gettext("Package name and namespace differ."), ""))
    if(length(x$bad_priority))
        writeLines(c(gettext("Invalid Priority field."),
                     strwrap(gettextf("Packages with priorities 'base' or 'recommended' or 'defunct-base' must already be known to R.")),
                     ""))

    if(any(as.integer(sapply(x, length)) > 0L))
        writeLines(c(strwrap(gettextf("See the information on DESCRIPTION files in section 'Creating R packages' of the 'Writing R Extensions' manual.")),
                     ""))

    invisible(x)
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

print.check_package_description_encoding <-
function(x, ...)
{
    if(length(x$non_portable_encoding))
       writeLines(c(gettextf("Encoding '%s' is not portable",
                             x$non_portable_encoding),
                    ""))
    if(length(x$missing_encoding))
        writeLines(gettext("Unknown encoding with non-ASCII data"))
    if(length(x$fields_with_non_ASCII_tags)) {
        writeLines(gettext("Fields with non-ASCII tags:"))
        .pretty_print(x$fields_with_non_ASCII_tags)
        writeLines(c(gettext("All field tags must be ASCII."), ""))
    }
    if(length(x$fields_with_non_ASCII_values)) {
        writeLines(gettext("Fields with non-ASCII values:"))
        .pretty_print(x$fields_with_non_ASCII_values)
    }
    if(any(as.integer(sapply(x, length)) > 0L))
        writeLines(c(strwrap(gettextf("See the information on DESCRIPTION files in section 'Creating R packages' of the 'Writing R Extensions' manual.")),
                     ""))

    invisible(x)
}

###

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
        ## Could always return the analysis results and not print them
        ## if ok, but it seems more standard to only return trouble.
        if(!ok)
            out <- c(list(license = val), status)
    }

    class(out) <- "check_package_license"
    out
}

print.check_package_license <-
function(x, ...)
{
    if(length(x)) {
        check <- Sys.getenv("_R_CHECK_LICENSE_")
        check <- if(check %in% c("maybe", ""))
            !(x$is_standardizable) || length(x$bad_pointers)
        else
            isTRUE(as.logical(check))
        if(check) {
            if(!(x$is_canonical))
                writeLines(c(gettext("Non-standard license specification:"),
                             strwrap(x$license, indent = 2L, exdent = 2L),
                             gettextf("Standardizable: %s",
                                      x$is_standardizable),
                             if(x$is_standardizable)
                             c(gettext("Standardized license specification:"),
                               strwrap(x$standardization,
                                       indent = 2L, exdent = 2L))))
            if(length(x$bad_pointers))
                writeLines(gettextf("Invalid license file pointers: %s",
                                    paste(x$bad_pointers, collapse = " ")))
        }
    }
    invisible(x)
}

### * .check_make_vars

.check_make_vars <-
function(dir)
{

    bad_flags <- list()
    class(bad_flags) <- "check_make_vars"

    paths <- file.path(dir, c("Makevars.in", "Makevars"))
    paths <- paths[file_test("-f", paths)]
    if(!length(paths)) return(bad_flags)
    mfile <- paths[1L]
    make <- Sys.getenv("MAKE")
    if(make == "") make <- "make"

    lines <-
        suppressWarnings(tryCatch(system(sprintf("%s -f %s -f %s",
                                                 make,
                                                 shQuote(mfile),
                                                 shQuote(file.path(R.home("share"), "make", "check.mk"))),
                                         intern = TRUE, ignore.stderr = TRUE),
                                  error = identity))
    if(!length(lines) || inherits(lines, "error"))
        return(bad_flags)

    ## Try to be careful ...
    pkg_flags_re <- "^PKG_(CPP|C|CXX|F|FC|OBJC|OBJCCXX)FLAGS: "
    lines <- lines[grepl(pkg_flags_re, lines)]
    names <- sub(":.*", "", lines)
    lines <- sub(pkg_flags_re, "", lines)
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
                        "Wall", "ansi", "pedantic", "traditional",
                        "f.*", "m.*", "std.*",
                        "x",
                        "q"),
                      collapse = "|"))
    for(i in seq_along(lines)) {
        bad <- grep(bad_flags_regexp, flags[[i]], value = TRUE)
        if(length(bad))
            bad_flags <- c(bad_flags,
                           structure(list(bad), names = names[i]))
    }

    class(bad_flags) <- "check_make_vars"
    bad_flags
}

print.check_make_vars <-
function(x, ...)
{
    if(length(x)) {
        for(i in seq_along(x)) {
            writeLines(c(gettextf("Non-portable flags in variable '%s':",
                                  names(x)[i]),
                         sprintf("  %s", paste(x[[i]], collapse = " "))))
        }
    }
    invisible(x)
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
            desc <- .readRDS(file.path(.find.package(package, NULL),
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
            assign("cairo_pdf",
                   function(filename =
                            if (onefile) "Rplots.pdf" else "Rplot%03d.pdf",
                            width = 7, height = 7, pointsize = 12,
                            onefile = FALSE, bg = "white", antialias) {},
                   envir = compat)
            assign("quartz",
                   function(display = "", width = 5, height = 5,
                            pointsize = 12, family = "Helvetica",
                            antialias = TRUE, autorefresh = TRUE) {},
                   envir = compat)
            assign("quartzFont", function(font) {}, envir = compat)
            assign("quartzFonts", function(...) {}, envir = compat)
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
            assign("getClipboardFormats", function() {}, envir = compat)
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

            assign("bmp",
                   function (filename = "Rplot%03d.bmp", width = 480,
                             height = 480, units = "px", pointsize = 12,
                             bg = "white", res = NA, restoreConsole = TRUE) {},
                   envir = compat)
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
                             pointsize = 12, restoreConsole = TRUE) {},
                   envir = compat)
            assign("win.print",
                   function(width = 7, height = 7, pointsize = 12,
                            printer = "", restoreConsole = TRUE) {},
                   envir = compat)
            assign("windows",
                   function(width = 7, height = 7, pointsize = 12,
                            record = getOption("graphics.record"),
                            rescale = c("R", "fit", "fixed"), xpinch, ypinch,
                            bg = "transparent", canvas = "white",
                            gamma = getOption("gamma"),
                            xpos = NA, ypos = NA,
                            buffered = getOption("windowsBuffered"),
                            restoreConsole = FALSE) {}, envir = compat)
            assign("windowsFont", function(font) {}, envir = compat)
            assign("windowsFonts", function(...) {}, envir = compat)

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
	for (g in methods::getGenerics(where = env))
	    for (m in methods::findMethods(g, where = env)) {
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
	    stop("package must be loaded")
	checkMethodUsageEnv(if (pack %in% loadedNamespaces())
			    getNamespace(pack) else as.environment(pname), ...)
    }

    ## <NOTE>
    ## Eventually, we should be able to specify a codetools "profile"
    ## for checking.
    ## </NOTE>

    suppressMessages(codetools::checkUsagePackage(package,
                                                  report = foo,
                                                  suppressLocalUnused = TRUE,
                                                  skipWith = TRUE))
    suppressMessages(checkMethodUsagePackage     (package,
                                                  report = foo,
                                                  suppressLocalUnused = TRUE,
                                                  skipWith = TRUE))
    out <- unique(out)
    class(out) <- "check_code_usage_in_package"
    out
}

print.check_code_usage_in_package <-
function(x, ...)
{
    if(length(x))
        writeLines(strwrap(x, indent = 0L, exdent = 2L))
    invisible(x)
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
        pkgInfo <- .readRDS(pfile)
    } else {
        outDir <- file.path(tempdir(), "fake_pkg")
        dir.create(file.path(outDir, "Meta"), FALSE, TRUE)
        .install_package_description(dir, outDir)
        pfile <- file.path(outDir, "Meta", "package.rds")
        pkgInfo <- .readRDS(pfile)
        unlink(outDir, recursive = TRUE)
    }
    ## only 'Depends' are guaranteed to be on the search path, but
    ## 'Imports' have to be installed and hence help there will be found
    deps <- c(names(pkgInfo$Depends), names(pkgInfo$Imports))
    pkgs <- unique(deps) %w/o% base
    try_Rd_aliases <- function(...) tryCatch(Rd_aliases(...), error = identity)
    aliases <- c(aliases, lapply(pkgs, try_Rd_aliases, lib.loc = lib.loc))
    aliases[sapply(aliases, class) == "error"] <- NULL

    ## See testRversion in library()
    new_only <- FALSE
    if(length(Rdeps <- pkgInfo$Rdepends2)) {
        ## has this if installed in > 2.7.0
        current <- as.numeric_version("2.9.2")
        for(dep in Rdeps)
            if(length(dep) > 1L) {
                target <- as.numeric_version(dep$version)
                res <- eval(parse(text=paste("current", dep$op, "target")))
                if(!res) new_only <- TRUE
            }
    }

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
        paste("[", db[have_anchor, 2L], "]{", db[have_anchor, 1L], "}", sep = "")

    ## Check the targets from the non-anchored xrefs.
    db[!have_anchor, "bad"] <- !( db[!have_anchor, 1L] %in% unlist(aliases))

    ## and then check the anchored ones if we can.
    have_colon <- grepl(":", anchor, fixed = TRUE)
    unknown <- character()
    thispkg <- anchor
    thisfile <- db[, 1L]
    thispkg[have_colon] <- sub("([^:]*):(.*)", "\\1", anchor[have_colon])
    thisfile[have_colon] <- sub("([^:]*):(.*)", "\\2", anchor[have_colon])
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
            nm <- sub("\\.[Rr]d", "", basename(.readRDS(RdDB)))
            good <- thisfile[this] %in% nm
            suspect <- if(any(!good)) {
                aliases1 <- if (pkg %in% names(aliases)) aliases[[pkg]]
                else Rd_aliases(pkg, lib.loc = lib.loc)
                !good & (thisfile[this] %in% aliases1)
            } else FALSE
            db[this, "bad"] <- !good & !suspect
        } else
            unknown <- c(unknown, pkg)
    }

    unknown <- unique(unknown)
    obsolete <- unknown %in% c("ctest", "eda", "lqs", "mle", "modreg", "mva", "nls", "stepfun", "ts")
    if (any(obsolete)) {
        message(gettextf("Obsolete package(s) %s in Rd xrefs",
                         paste(sQuote(unknown[obsolete]), collapse = ", ")),
                domain = NA)
    }
    unknown <- unknown[!obsolete]
    if (length(unknown)) {
        repos <- .get_standard_repository_URLs()
        known <-
            try(utils::available.packages(utils::contrib.url(repos,
                                                             "source"),
                                          filters =
                                          c("R_version",
                                            "duplicates"))[, "Package"])
        miss <- if(inherits(known, "try-error")) TRUE
        else unknown %in% c(known, c("BRugs", "GLMMGibbs", "survnnet", "yags"))
        ## from CRANextras
        if(any(miss))
            message(gettextf("Package(s) unavailable to check Rd xrefs: %s",
                             paste(sQuote(unknown[miss]), collapse = ", ")),
                    domain = NA)
        if(any(!miss))
            message(gettextf("Unknown package(s) %s in Rd xrefs",
                             paste(sQuote(unknown[!miss]), collapse = ", ")),
                    domain = NA)
    }
    ## The bad ones:
    bad <- db[, "bad"] == "TRUE"
    res1 <- split(db[bad, "report"], db[bad, 3L])
    structure(list(bad = res1), class = "check_Rd_xrefs")
}

print.check_Rd_xrefs <-
function(x, ...)
{
    xx <- x$bad
    if(length(xx)) {
        for(i in seq_along(xx)) {
            writeLines(gettextf("Missing link(s) in documentation object '%s':",
                                names(xx)[i]))
            ## NB, link might be empty, and was in mvbutils
            .pretty_print(sQuote(unique(xx[[i]])))
            writeLines("")
        }
        msg <- strwrap(gettextf("See the information in section 'Cross-references' of the 'Writing R Extensions' manual."))
        writeLines(c(msg, ""))
    }
    invisible(x)
}


### * .check_package_datasets

.check_package_datasets <-
function(pkgDir)
{
    Sys.setlocale("LC_CTYPE", "C")
    options(warn=-1)
    check_charsxp <- function(txt)
    {
        if(any(charToRaw(txt) > as.raw(127)))
            switch(Encoding(txt),
                   "latin1" = {latin1 <<- c(latin1, txt)},
                   "UTF-8" = {utf8 <<- c(utf8, txt)},
                   {
                       non_ASCII <<- c(non_ASCII, txt)
                       where <<- c(where, ds)
                   })
        invisible()
    }
    check_one <- function(x, ds)
    {
        if(!length(x)) return()
        ## avoid as.list methods
        if(is.list(x)) lapply(unclass(x), check_one)
        if(is.character(x)) lapply(unclass(x), check_charsxp)
        a <- attributes(x)
        if(!is.null(a)) {
            lapply(a, check_one)
            check_one(names(a))
        }
        invisible()
    }

    sink(tempfile()) ## suppress startup messages to stdout
    files <- list_files_with_type(file.path(pkgDir, "data"), "data")
    files <- unique(basename(file_path_sans_ext(files)))
    ans <- vector("list", length(files))
    dataEnv <- new.env(hash=TRUE)
    names(ans) <- files
    old <- setwd(pkgDir)
    for(f in files)
        .try_quietly(utils::data(list = f, package = character(0L), envir = dataEnv))
    setwd(old)

    non_ASCII <- latin1 <- utf8 <- where <- character(0L)
    ## avoid messages about loading packages that started with r48409
    suppressPackageStartupMessages({
        for(ds in ls(envir = dataEnv, all.names = TRUE))
            check_one(get(ds, envir = dataEnv), ds)
    })
    sink()
    unknown <- unique(cbind(non_ASCII, where))
    structure(list(latin1 = unique(latin1), utf8 = unique(utf8),
                   unknown = unknown),
              class = "check_package_datasets")
}

print.check_package_datasets <-
function(x, ...)
{
    ## not sQuote as we have mucked about with locales.
    iconv0 <- function(x, ...) paste("'", iconv(x, ...), "'", sep="")

    if(n <- length(x$latin1))
        cat(sprintf("Note: found %d marked Latin-1 string(s)\n", n))
    if(n <- length(x$utf8))
        cat(sprintf("Note: found %d marked UTF-8 string(s)\n", n))
    if(nrow(x$unknown)) {
        cat("Warning: found non-ASCII string(s)\n")
        writeLines(paste(iconv0(x$unknown[,1L], "", "ASCII", sub="byte"),
                         " in object '", x$unknown[,2L], "'", sep = ""))
    }
    invisible(x)
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

    wrong_things <- list(R = character(0L), man = character(0L),
                         demo = character(0L), `inst/doc` = character(0L))

    code_dir <- file.path(dir, "R")
    if(file_test("-d", code_dir)) {
        all_files <- mydir(code_dir)
        ## Under Windows, need a Makefile.win for methods.
        R_files <- c("sysdata.rda", "Makefile.win",
                     list_files_with_type(code_dir, "code",
                                          full.names = FALSE,
                                          OS_subdirs = OS_subdirs))
        wrong <- all_files %w/o% R_files
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
        wrong <- all_files %w/o% man_files
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
        wrong <- all_files %w/o% c("00Index", demo_files)
        if(length(wrong)) {
            wrong_things$demo <- wrong
            if(doDelete) unlink(file.path(dir, "demo", wrong))
        }
    }

    vign_dir <- file.path(dir, "inst", "doc")
    if(file_test("-d", vign_dir)) {
        vignettes <- list_files_with_type(vign_dir, "vignette",
                                          full.names = FALSE)
        vignettes <- c(vignettes,
                       list_files_with_exts(vign_dir, "pdf",
                                            full.names = FALSE))
        ## we specify ASCII filenames starting with a letter in R-exts
        ## do this in a locale-independent way.
        OK <- grep("^[ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz][ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789._-]+$", vignettes)
        wrong <- vignettes
        if(length(OK)) wrong <- wrong[-OK]
        if(length(wrong)) wrong_things$`inst/doc` <- wrong
    }

    class(wrong_things) <- "subdir_tests"
    wrong_things
}

print.subdir_tests <-
function(x, ...)
{
    for(i in which(sapply(x, length) > 0L)) {
        tag <- names(x)[i]
        writeLines(sprintf("Subdirectory '%s' contains invalid file names:",
                           names(x)[i]))
        .pretty_print(x[[i]])
    }
    invisible(x)
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
    wrong_things <- character(0L)
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
    if(length(wrong_things)) cat(wrong_things, sep="\n")
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
            con <- file(file, encoding = enc)
            on.exit(close(con))
        } else con <- file
        withCallingHandlers(tryCatch(parse(con),
                                     error = function(e)
                                     .error <<- conditionMessage(e)),
                            warning = function(e) {
                                .warnings <<- c(.warnings,
                                                conditionMessage(e))
                                invokeRestart("muffleWarning")
                            })
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
                                          "Warning in file '%s':",
                                          "Warnings in file '%s':"),
                                 xi$File),
                         paste(" ", gsub("\n\n", "\n  ", xi$Warnings,
					 perl = TRUE, useBytes = TRUE))))
    }
    invisible(x)
}

### * .check_package_code_shlib

.check_package_code_shlib <-
function(dir)
{
    ## <NOTE>
    ## This is very similar to what happens with checkTnF() etc.
    ## We should really have a more general-purpose tree walker.
    ## </NOTE>

    dfile <- file.path(dir, "..", "DESCRIPTION")
    enc <- if(file.exists(dfile))
        .read_description(dfile)["Encoding"] else NA

    ## Workhorse function.
    filter <- function(file) {
        matches <- list()
        walker <- function(e) {
            if((length(e) > 1L)
               && is.call(e)
               && as.character(e[[1L]]) %in% c("library.dynam",
                                              "library.dynam.unload")
               && is.character(e[[2L]])
               && grepl("\\.(so|sl|dll)$", e[[2L]])
               )
                matches <<- c(matches, list(e))
            if(is.recursive(e))
                for(i in seq_along(e)) Recall(e[[i]])
        }
        ## assume that if locale if 'C' we can read encodings unchanged.
        exprs <- if(!is.na(enc) &&
                    !(Sys.getlocale("LC_CTYPE") %in% c("C", "POSIX"))) {
	    con <- file(file, encoding=enc)
            on.exit(close(con))
	    parse(con)
	} else parse(file)
        for(i in seq_along(exprs)) walker(exprs[[i]])
        matches
    }

    code_files <-
        list_files_with_type(dir, "code",
                             OS_subdirs = c("unix", "windows"))
    x <- lapply(code_files, filter)
    names(x) <- code_files
    x <- x[sapply(x, length) > 0L]

    ## Because we really only need this for calling from R CMD check, we
    ## produce output here in case we found something.
    for(fname in names(x)) {
        writeLines(gettextf("File '%s':", fname))
        xfname <- x[[fname]]
        for(i in seq_along(xfname)) {
            writeLines(strwrap(gettextf("found %s",
                                        paste(deparse(xfname[[i]]),
                                              collapse = "")),
                               indent = 2L, exdent = 4L))
        }
    }

    invisible(x)
}


### * .check_packages_used

.check_packages_used <-
function(package, dir, lib.loc = NULL)
{
    ## Argument handling.
    if(!missing(package)) {
        if(length(package) != 1L)
            stop("argument 'package' must be of length 1")
        dir <- .find.package(package, lib.loc)
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
    imports <- .get_requires_from_package_db(db, "Imports")
    suggests <- .get_requires_from_package_db(db, "Suggests")
    enhances <- .get_requires_from_package_db(db, "Enhances")

    ## it is OK to refer to yourself and non-S4 standard packages
    standard_package_names <-
        .get_standard_package_names()$base %w/o% c("methods", "stats4")
    ## It helps to know if non-default standard packages are require()d
    default_package_names<-
         standard_package_names %w/o% c("grid", "splines", "tcltk", "tools")
    depends_suggests <- c(depends, suggests, pkg_name, default_package_names)
    imports <- c(imports, depends, suggests, enhances, pkg_name,
                 standard_package_names)
    ## the first argument could be named, or could be a variable name.
    ## we just have a stop list here.
    common_names <- c("pkg", "pkgName", "package", "pos")

    bad_exprs <- character()
    bad_imports <- character()
    uses_methods <- FALSE
    find_bad_exprs <- function(e) {
        if(is.call(e) || is.expression(e)) {
            Call <- deparse(e[[1L]])[1L]
            if(length(e) >= 2L) pkg <- deparse(e[[2L]])
            if(Call %in% c("library", "require")) {
                ## Zelig has library()
                if(length(e) >= 2L) {
                    ## FIXME: and base has library(.lib.loc = .Library)
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
                    ## <FIXME> could be inside substitute or a variable
                    ## and is in e.g. R.oo
                    if(! dunno
                       && ! pkg %in% c(depends_suggests, common_names))
                        bad_exprs <<- c(bad_exprs, pkg)
                }
            } else if(Call %in%  "::") {
                if(! pkg %in% imports)
                    bad_imports <<- c(bad_imports, pkg)
            } else if(Call %in%  ":::") {
                ## <FIXME> fathom out if this package has a namespace
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
                            f <- get(f, envir = code_env)
			    if(typeof(f) == "closure") body(f) # else NULL
                        })
        if(.isMethodsDispatchOn()) {
            ## Also check the code in S4 methods.
            ## This may find things twice.
            for(f in get_S4_generics_with_methods(code_env)) {
                mlist <- .get_S4_methods_list(f, code_env)
                exprs <- c(exprs, lapply(mlist, body))
            }
        }
    }
    else {
        enc <- db["Encoding"]
        if(!is.na(enc) &&
           !(Sys.getlocale("LC_CTYPE") %in% c("C", "POSIX"))) {
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

    methods_message <-
        if(uses_methods && !"methods" %in% c(depends, imports))
            gettext("package 'methods' is used but not declared")
        else ""
    res <- list(others = unique(bad_exprs),
                imports = unique(bad_imports),
                methods_message = methods_message)
    class(res) <- "check_packages_used"
    res
}

print.check_packages_used <-
function(x, ...)
{
    if(length(xx <- x$imports)) {
        if (length(xx) > 1L) {
            writeLines(gettext("'::' or ':::' imports not declared from:"))
            .pretty_print(sort(xx))
        } else
            writeLines(gettextf("'::' or ':::' import not declared from: %s", xx))
    }
    if(length(xx <- x$others)) {
        if (length(xx) > 1L) {
            writeLines(gettext("'library' or 'require' calls not declared from:"))
            .pretty_print(sort(x$others))
        } else
            writeLines(gettextf("'library' or 'require' call not declared from: %s", xx))
    }
    if(nzchar(x$methods_message))
        writeLines(x$methods_message)
    invisible(x)
}


### * .check_packages_used_in_examples

.check_packages_used_in_examples <-
function(package, dir, lib.loc = NULL)
{
    ## Argument handling.
    if(!missing(package)) {
        if(length(package) != 1L)
            stop("argument 'package' must be of length 1")
        dir <- .find.package(package, lib.loc)
        dfile <- file.path(dir, "DESCRIPTION")
        db <- .read_description(dfile)
    }
    else if(!missing(dir)) {
        ## Using sources from directory @code{dir} ...
        ## FIXME: not yet supported by .createExdotR.
        if(!file_test("-d", dir))
            stop(gettextf("directory '%s' does not exist", dir),
                 domain = NA)
        else
            dir <- file_path_as_absolute(dir)
        dfile <- file.path(dir, "DESCRIPTION")
        db <- .read_description(dfile)
    }
    pkg_name <- db["Package"]
    depends <- .get_requires_from_package_db(db, "Depends")
    imports <- .get_requires_from_package_db(db, "Imports")
    suggests <- .get_requires_from_package_db(db, "Suggests")
    enhances <- .get_requires_from_package_db(db, "Enhances")

    ## it is OK to refer to yourself and standard packages
    standard_package_names <- .get_standard_package_names()$base
    depends_suggests <- c(depends, imports, suggests, enhances, pkg_name,
                          standard_package_names)
    imports <- c(imports, depends, suggests, enhances, pkg_name,
                 standard_package_names)
    ## the first argument could be named, or could be a variable name.
    ## we just have a stop list here.
    common_names <- c("pkg", "pkgName", "package", "pos")

    bad_exprs <- character()
    bad_imports <- character()
    find_bad_exprs <- function(e) {
        if(is.call(e) || is.expression(e)) {
            Call <- deparse(e[[1L]])[1L]
            if(length(e) >= 2L) pkg <- deparse(e[[2L]])
            if(Call %in% c("library", "require")) {
                if(length(e) >= 2L) {
                    ## FIXME: base has library(.lib.loc = .Library)
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
                    ## <FIXME> could be inside substitute or a variable
                    ## and is in e.g. R.oo
                    if(! dunno
                       && ! pkg %in% c(depends_suggests, common_names))
                        bad_exprs <<- c(bad_exprs, pkg)
                }
            } else if(Call %in%  "::") {
                if(! pkg %in% imports)
                    bad_imports <<- c(bad_imports, pkg)
            } else if(Call %in%  ":::") {
                ## <FIXME> fathom out if this package has a namespace
                if(! pkg %in% imports)
                    bad_imports <<- c(bad_imports, pkg)
            }
            for(i in seq_along(e)) Recall(e[[i]])
        }
    }

    file <- .createExdotR(pkg_name, dir, silent = TRUE)
    if (is.null(file)) return(invisible(NULL)) # e.g, no examples
    on.exit(unlink(file))
    enc <- db["Encoding"]
    if(!is.na(enc) &&
       !(Sys.getlocale("LC_CTYPE") %in% c("C", "POSIX"))) {
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

    for(i in seq_along(exprs)) find_bad_exprs(exprs[[i]])

    res <- list(others = unique(bad_exprs),
                imports = unique(bad_imports),
                methods_message = "")
    class(res) <- "check_packages_used"
    res
}


### * .check_packages_used_in_tests

.check_packages_used_in_tests <-
function(dir, lib.loc = NULL)
{
    ## Argument handling.
    ## Using sources from directory @code{dir} ...
    if(!file_test("-d", dir))
        stop(gettextf("directory '%s' does not exist", dir),
             domain = NA)
    else
        dir <- file_path_as_absolute(dir)
    dfile <- file.path(dir, "DESCRIPTION")
    db <- .read_description(dfile)
    testsrcdir <- file.path(dir, "tests")

    pkg_name <- db["Package"]
    depends <- .get_requires_from_package_db(db, "Depends")
    imports <- .get_requires_from_package_db(db, "Imports")
    suggests <- .get_requires_from_package_db(db, "Suggests")
    enhances <- .get_requires_from_package_db(db, "Enhances")

    ## it is OK to refer to yourself and standard packages
    standard_package_names <- .get_standard_package_names()$base
    depends_suggests <- c(depends, imports, suggests, enhances, pkg_name,
                          standard_package_names)
    imports <- c(imports, depends, suggests, enhances, pkg_name,
                 standard_package_names)
    ## the first argument could be named, or could be a variable name.
    ## we just have a stop list here.
    common_names <- c("pkg", "pkgName", "package", "pos")

    bad_exprs <- character()
    bad_imports <- character()
    find_bad_exprs <- function(e) {
        if(is.call(e) || is.expression(e)) {
            Call <- deparse(e[[1L]])[1L]
            if(length(e) >= 2L) pkg <- deparse(e[[2L]])
            if(Call %in% c("library", "require")) {
                if(length(e) >= 2L) {
                    ## FIXME: base has library(.lib.loc = .Library)
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
                    ## <FIXME> could be inside substitute or a variable
                    ## and is in e.g. R.oo
                    if(! dunno
                       && ! pkg %in% c(depends_suggests, common_names))
                        bad_exprs <<- c(bad_exprs, pkg)
                }
            } else if(Call %in%  "::") {
                if(! pkg %in% imports)
                    bad_imports <<- c(bad_imports, pkg)
            } else if(Call %in%  ":::") {
                ## <FIXME> fathom out if this package has a namespace
                if(! pkg %in% imports)
                    bad_imports <<- c(bad_imports, pkg)
            }
            for(i in seq_along(e)) Recall(e[[i]])
        }
    }

    od <- setwd(testsrcdir)
    on.exit(setwd(od))
    Rinfiles <- dir(".", pattern="\\.Rin$") # only trackOjs has *.Rin
    Rfiles <- dir(".", pattern="\\.R$")

    for(f in c(Rinfiles, Rfiles)) {
        exprs <- tryCatch(parse(file = f, n = -1L),
                          error = function(e)
                          stop(gettextf("parse error in file '%s':\n%s",
                                        file,
                                        .massage_file_parse_error_message(conditionMessage(e))),
                               domain = NA, call. = FALSE))
        for(i in seq_along(exprs)) find_bad_exprs(exprs[[i]])
    }

    res <- list(others = unique(bad_exprs),
                imports = unique(bad_imports),
                methods_message = "")
    class(res) <- "check_packages_used"
    res
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
        env <- new.env()
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
        dir <- .find.package(package, lib.loc)
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
            code_env <- new.env()
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

print.check_T_and_F <-
function(x, ...)
{
    if(length(x$bad_closures)) {
        msg <- ngettext(length(x$bad_closures),
                        "Found possibly global 'T' or 'F' in the following function:",
                        "Found possibly global 'T' or 'F' in the following functions:"
                        )
        writeLines(strwrap(msg))
        .pretty_print(x$bad_closures)
    }
    if(length(x$bad_examples)) {
        msg <- ngettext(length(x$bad_examples),
                        "Found possibly global 'T' or 'F' in the following Rd example file:",
                        "Found possibly global 'T' or 'F' in the following Rd example files:"
                        )
        writeLines(strwrap(msg))
        writeLines(paste(" ", x$bad_examples))
    }
    invisible(x)
}

### * .check_dotIntenal

.check_dotInternal <-
function(package, dir, lib.loc = NULL)
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
        objects_in_env[sapply(x,
                              function(s) any(s %in% ".Internal"))]
    }

    if(!missing(package)) {
        if(length(package) != 1L)
            stop("argument 'package' must be of length 1")
        dir <- .find.package(package, lib.loc)
        if(! package %in% .get_standard_package_names()$base) {
            .load_package_quietly(package, lib.loc)
            code_env <- if(packageHasNamespace(package, dirname(dir)))
                           asNamespace(package)
            else .package_env(package)
            bad_closures <- find_bad_closures(code_env)
        }
    }
    else {
        ## The dir case.
        if(missing(dir))
            stop("you must specify 'package' or 'dir'")
        dir <- file_path_as_absolute(dir)
        code_dir <- file.path(dir, "R")
        if(file_test("-d", code_dir)) {
            code_env <- new.env()
            dfile <- file.path(dir, "DESCRIPTION")
            meta <- if(file_test("-f", dfile))
                .read_description(dfile)
            else
                character()
            .source_assignments_in_code_dir(code_dir, code_env, meta)
            bad_closures <- find_bad_closures(code_env)
        }
    }

    out <- list(bad_closures = bad_closures)
    class(out) <- "check_dotInternal"
    out
}

print.check_dotInternal <-
function(x, ...)
{
    if(length(x$bad_closures)) {
        msg <- ngettext(length(x$bad_closures),
                        "Found .Internal call in the following function:",
                        "Found .Internal calls in the following functions:"
                        )
        writeLines(strwrap(msg))
        .pretty_print(x$bad_closures)
    }
    invisible(x)
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
function(cfile)
{
    cfile <- file_path_as_absolute(cfile)
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
        tmp <- try(checkRd(f, encoding = enc, def_enc = def_enc), silent=TRUE)
        if(inherits(tmp, "try-error")) {
	    bad <- c(bad, f)
            if(!silent) message(geterrmessage())
        } else print(tmp, minlevel = minlevel)
    }
    if(length(bad)) bad <- sQuote(sub(".*/","", bad))
    if(length(bad) > 1L)
        cat("problems found in ", paste(bad, collapse=", "), "\n", sep="")
    else if(length(bad))
        cat("problem found in ", bad, "\n", sep="")
    invisible()
}

### * .check_package_CRAN_incoming

.check_package_CRAN_incoming <-
function(dir)
{
    out <- list()
    class(out) <- "check_package_CRAN_incoming"

    meta <- .get_package_metadata(dir, FALSE)
    foss <- analyze_license(meta["License"])$is_verified

    urls <- .get_standard_repository_URLs()
    ## We do not want to use utils::available.packages() for now, as
    ## this unconditionally filters according to R version and OS type.
    .repository_db <- function(u) {
        con <- gzcon(url(sprintf("%s/src/contrib/PACKAGES.gz", u), "rb"))
        on.exit(close(con))
        ## hopefully all these fields are ASCII, or we need to re-encode.
        cbind(read.dcf(con,
                       c(.get_standard_repository_db_fields(), "Path")),
              Repository = u)

    }
    db <- tryCatch(lapply(urls, .repository_db), error = identity)
    if(inherits(db, "error")) return(out)
    db <- do.call(rbind, db)

    ## Package names must be unique within standard repositories when
    ## ignoring case.
    package <- meta["Package"]
    packages <- db[, "Package"]
    existing <-
        packages[(tolower(packages) == tolower(package)) &
                 (packages != package)]
    if(length(existing))
        out$bad_package <- list(package, existing)
    ## Could there be more than one?

    ## Is this an update for package already on CRAN?
    db <- db[(packages == package) &
             (db[, "Repository"] == urls[1L]) &
             is.na(db[, "Path"]), , drop = FALSE]
    ## This assumes the CRAN URL comes first, and drops packages in
    ## version-specific subdirectories.  It also does not know about
    ## archived versions.
    if(!NROW(db)) {
        if(!foss)
            out$bad_license <- meta["License"]
        return(out)
    }
    ## For now, there should be no duplicates ...

    ## Package versions should be newer than what we already have on
    ## CRAN.

    v_m <- package_version(meta["Version"])
    v_d <- package_version(db[, "Version"])
    if(v_m <= max(v_d))
        out$bad_version <- list(v_m, v_d)

    ## Watch out for maintainer changes.
    ## Note that we cannot get the maintainer info from the PACKAGES
    ## files.
    con <- url(sprintf("%s/web/packages/packages.rds", urls[1L]), "rb")
    db <- tryCatch(.readRDS(con), error = identity)
    close(con)
    if(inherits(db, "error")) return(out)

    m_m <- meta["Maintainer"]
    m_d <- db[db[, "Package"] == package, "Maintainer"]
    if(!all(m_m == m_d))
        out$new_maintainer <- list(m_m, m_d)

    l_d <- db[db[, "Package"] == package, "License"]
    if(!foss && analyze_license(l_d)$is_verified)
        out$new_license <- list(meta["License"], l_d)

    out

}

print.check_package_CRAN_incoming <-
function(x, ...)
{
    if(length(y <- x$bad_package))
        writeLines(sprintf("Conflicting package names (submitted: %s, existing: %s)",
                           y[[1L]], y[[2L]]))
    if(length(y <- x$bad_version))
        writeLines(sprintf("Insufficient package version (submitted: %s, existing: %s)",
                           y[[1L]], y[[2L]]))
    if(length(y <- x$new_maintainer)) {
        writeLines(c("New maintainer:",
                     strwrap(y[[1L]], indent = 2L, exdent = 4L),
                     "Old maintainer(s):",
                     strwrap(y[[2L]], indent = 2L, exdent = 4L)))
    }
    if(length(y <- x$bad_license)) {
        writeLines(sprintf("Non-FOSS package license (%s)", y))
    }
    if(length(y <- x$new_license)) {
        writeLines(c("Change to non-FOSS package license.",
                     "New license:",
                     strwrap(y[[1L]], indent = 2L, exdent = 4L),
                     "Old license:",
                     strwrap(y[[2L]], indent = 2L, exdent = 4L)))
    }
    invisible(x)
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
        dir <- .find.package(package, lib.loc)
        rds <- file.path(dir, "Meta", "Rd.rds")
        if(file_test("-f", rds)) {
            meta <- .readRDS(rds)
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

print.check_Rd_metadata <-
function(x, ...)
{
    if(length(x$files_with_duplicated_name)) {
        bad <- x$files_with_duplicated_name
        for(nm in names(bad)) {
            writeLines(gettextf("Rd files with duplicated name '%s':", nm))
            .pretty_print(bad[[nm]])
        }
    }

    if(length(x$files_with_duplicated_aliases)) {
        bad <- x$files_with_duplicated_aliases
        for(nm in names(bad)) {
            writeLines(gettextf("Rd files with duplicated alias '%s':", nm))
            .pretty_print(bad[[nm]])
        }
    }

    invisible(x)
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
        dir <- .find.package(package, lib.loc)
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

print.check_Rd_contents <-
function(x, ...)
{
    for(nm in names(x)) {
        y <- x[[nm]]
        arguments_with_no_description <-
            y[["arguments_with_no_description"]]
        if(length(arguments_with_no_description)) {
            writeLines(gettextf("Argument items with no description in Rd object '%s':",
                                nm))
            .pretty_print(arguments_with_no_description)
        }
        offending_autogenerated_content <-
            y[["offending_autogenerated_content"]]
        if(length(offending_autogenerated_content)) {
            writeLines(gettextf("Auto-generated content requiring editing in Rd object '%s':",
                                nm))
            writeLines(sprintf("  %s",
                               offending_autogenerated_content[, 1L]))
        }
        writeLines("")
    }

    invisible(x)
}


### * .find_charset

.find_charset <-
function()
{
    l10n <- l10n_info()
    enc <- if(l10n[["UTF-8"]]) "UTF-8" else utils::localeToCharset()
    cat("charset: ", enc, "\n", sep="")
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
        names(y)[ind] <- sapply(y[ind],as.character)
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
    paste(out, txt, sep = "")
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
            dq <- function(ch) paste('"', ch ,'"', sep='')
            rErr <- r[hasErr]
            pkgs <- r@package[hasErr]
            ## FIXME: This warning should not happen here when called
            ## from R CMD check, but rather be part of a new "check"
            ## there !
	    warning("Generics g in env = ", format(env),
		    " where hasMethods(g, env) errors: ",
		    paste(sQuote(rErr), collapse = ", "),
		    "\nMay need something like\n\n",
		    paste("  importFrom(", paste(dq(pkgs), dq(rErr), sep=", "),
                          ")\n", sep=''),
		    "\nin NAMESPACE.")
	    hasM <- hasM[!hasErr]
	}
	!all(ok <- unlist(hasM))
    }) {
	if(verbose)
	    message("Generics without methods in ", format(env), ": ",
		    paste(sQuote(r[!ok]), collapse = ", "))
	r[ok]
    }
    else as.vector(r)# for back-compatibility and current ..../tests/reg-S4.R
}

### ** .get_S4_methods_list

.get_S4_methods_list <-
function(g, env)
{
    ## For the QC computations, we really only want the S4 methods
    ## defined in a package, so we try to exclude derived default
    ## methods as well as methods inherited from other environments.

    env <- as.environment(env)
    mlist <- methods::findMethods(g, env)

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
        namespace <- .get_namespace_from_package_env(env)
        if(!is.null(namespace)) {
            mlist <- Filter(function(m)
                            identical(environment(m), namespace),
                            mlist)
        } else {
            mlist <- Filter(function(m)
                            environmentName(environment(m)) == "",
                            mlist)
        }
    }

    mlist
}

.get_namespace_from_package_env <-
function(env)
{
    package <-
        sub(".*:([^_]*).*", "\\1", attr(env, "name", exact = TRUE))
    ## (Ugly, but why not?)
    if(length(package) && nzchar(package)) {
        .Internal(getRegisteredNamespace(as.name(package)))
        ## Note that we could also use
        ##   getNamespace(package, error = function(e) NULL)
    }
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
                    "\\$", "\\[\\[?"),
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

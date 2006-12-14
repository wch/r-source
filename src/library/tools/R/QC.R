### * undoc

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
        if(length(package) != 1)
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
            .source_assignments_in_code_dir(code_dir, code_env)
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

    data_objs <- character(0)
    data_dir <- file.path(dir, "data")
    if(file_test("-d", data_dir)) {
        data_env <- new.env()
        files <- list_files_with_type(data_dir, "data")
        files <- unique(basename(file_path_sans_ext(files)))
        ## <FIXME>
        ## Argh.  When working on the source directory of a package in a
        ## bundle, or a base package, we (currently?) cannot simply use
        ## data().  In these cases, we only have a 'DESCRIPTION.in'
        ## file.  On the other hand, data() uses .find.package() to find
        ## the package paths from its 'package' and '.lib.loc'
        ## arguments, and .find.packages() is really for finding
        ## *installed* packages, and hence tests for the existence of a
        ## 'DESCRIPTION' file.  As a last resort, use the fact that
        ## data() can be made to for look data sets in the 'data'
        ## subdirectory of the current working directory ...
        package_name <- basename(dir)
        libPath <- dirname(dir)
        if(!file.exists(file.path(dir, "DESCRIPTION"))) {
            ## Hope that there is a 'DESCRIPTION.in', maybe we should
            ## check for this?
            package_name <- character()
            libPath <- NULL
            owd <- getwd()
            setwd(dir)
            on.exit(setwd(owd))
        }
        ## </FIXME>
        for(f in files) {
            ## <NOTE>
            ## Non-standard evaluation for argument 'package' to data()
            ## gone in R 1.9.0.
            .try_quietly(utils::data(list = f, package = package_name,
                                     lib.loc = libPath, envir = data_env))
            ## (We use .try_quietly() because a .R data file using scan()
            ## to read in data from some other place may do this without
            ## 'quiet = TRUE', giving output which R CMD check would
            ## think to indicate a problem.)
            ## </NOTE>
            new <- ls(envir = data_env, all.names = TRUE)
            data_objs <- c(data_objs, new)
            rm(list = new, envir = data_env)
        }
    }

    ## Undocumented objects?
    if(!missing(package)
       && (length(code_objs) == 0)
       && (length(data_objs) == 0))
        warning("neither code nor data objects found")
    ## When working on the sources, we will not get any code objects in
    ## case a package provides "just" S4 classes and methods.

    if(!is_base) {
        ## Code objects in add-on packages with names starting with a
        ## dot are considered 'internal' (not user-level) by
        ## convention.
        ## <FIXME>
        ## Not clear whether everyone believes in this convention.
        ## We used to have
        ##   allObjs[! allObjs %in% c(all_doc_topics,
        ##                            ".First.lib", ".Last.lib")]
        ## i.e., only exclude '.First.lib' and '.Last.lib'.
        code_objs <- grep("^[^.].*", code_objs, value = TRUE)
        ## Note that this also allows us to get rid of S4 meta objects
        ## (with names starting with '.__C__' or '.__M__'; well, as long
        ## as there are none in base).
        ## </FIXME>

        ## <FIXME>
        ## Need to do something about S4 generic functions 'created' by
        ## setGeneric() or setMethod() on 'ordinary' functions.
        ## The test below exempts objects that are generic functions
        ## which are 'derived', either by importing from another
        ## package or from a default method.
        ## In the long run we need dynamic documentation.
        if(.isMethodsDispatchOn()) {
            code_objs <-
                .filter(code_objs,
                        function(f) {
                            fdef <- get(f, envir = code_env)
                            if(methods::is(fdef, "genericFunction"))
                                fdef@package == pkgname
                            else
                                TRUE
                        })
        }
        ## </FIXME>

        ## Allow group generics to be undocumented other than in base.
        ## In particular, those from methods partially duplicate base
        ## and are documented in base's groupGenerics.Rd.
        code_objs <-
	    code_objs %w/o% c("Arith", "Compare", "Complex", "Logic",
			      "Math", "Math2", "Ops", "Summary")
    }

    ## <FIXME>
    ## Currently, loading data from an R file via sys.source() puts
    ## .required into the load environment if the R code has a call to
    ## require().
    data_objs <- data_objs %w/o% c(".required")
    ## </FIXME>

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
                               function(u) utils::topicName("class", u))
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
        methodsSignatures <- function(f) {
            mlist <- methods::getMethodsMetaData(f, code_env)
            meths <- methods::linearizeMlist(mlist, FALSE)
            classes <- methods::slot(meths, "classes")
            ## Don't look for doc on a generated default method.
            default <-
                as.logical(lapply(classes,
                                  function(x)
                                  identical(all(x == "ANY"), TRUE)))
            if(any(default)
               && methods::is(methods::finalDefaultMethod(mlist),
                              "derivedDefaultMethod")) {
                classes <- classes[!default]
            }
            ## Exclude methods inherited from the 'appropriate' parent
            ## environment.
            ## <NOTE>
            ## Keep this in sync with similar code in checkFF().
            penv <- .Internal(getRegisteredNamespace(as.name(package)))
            if(is.environment(penv))
                penv <- parent.env(penv)
            else
                penv <- parent.env(code_env)
            if((f %in% methods::getGenerics(penv))
               && !is.null(mlist_from_penv <-
                           methods::getMethodsMetaData(f, penv))) {
                classes_from_penv <-
                    methods::slot(methods::linearizeMlist(mlist_from_penv),
                                  "classes")
                ind <- is.na(match(.make_signatures(classes),
                                   .make_signatures(classes_from_penv)))
                classes <- classes[ind]
            }
            ## </NOTE>
            sigs <- sapply(classes, paste, collapse = ",")
            if(length(sigs))
                paste(f, ",", sigs, sep = "")
            else
                character()
        }
        S4_methods <-
            sapply(methods::getGenerics(code_env), methodsSignatures)
        S4_methods <-
            as.character(unlist(S4_methods, use.names = FALSE))
        ## The bad ones:
        S4_methods <-
            S4_methods[!sapply(S4_methods,
                               function(u)
                               utils::topicName("method", u))
                       %in% all_doc_topics]
        undoc_things <-
            c(undoc_things,
              list("S4 methods" =
                   unique(sub("([^,]*),(.*)",
                              "generic '\\1' and siglist '\\2'",
                              S4_methods))))
    }

    class(undoc_things) <- "undoc"
    undoc_things
}

print.undoc <-
function(x, ...)
{
    for(i in which(sapply(x, length) > 0)) {
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
                      gettextf("Undocumented %s:", tag))
        writeLines(msg)
        ## We avoid markup for indicating S4 methods, hence need to
        ## special-case output for these ...
        if(tag == "S4 methods")
            writeLines(strwrap(x[[i]], indent = 2, exdent = 4))
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
    ## <FIXME>
    ## Improvements worth considering:
    ## * Parallelize the actual checking (it is not necessary to loop
    ##   over the Rd files);
    ## </FIXME>

    has_namespace <- FALSE

    ## Argument handling.
    if(!missing(package)) {
        if(length(package) != 1)
            stop("argument 'package' must be of length 1")
        dir <- .find.package(package, lib.loc)
        ## Using package installed in @code{dir} ...
        code_dir <- file.path(dir, "R")
        if(!file_test("-d", code_dir))
            stop(gettextf("directory '%s' does not contain R code",
                          dir),
                 domain = NA)
        docs_dir <- file.path(dir, "man")
        if(!file_test("-d", docs_dir))
            stop(gettextf("directory '%s' does not contain Rd sources",
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
        docs_dir <- file.path(dir, "man")
        if(!file_test("-d", docs_dir))
            stop(gettextf("directory '%s' does not contain Rd sources",
                          dir),
                 domain = NA)
        package_name <- basename(dir)
        is_base <- package_name == "base"

        code_env <- new.env()
        .source_assignments_in_code_dir(code_dir, code_env)
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
            functions_in_S3Table <- character(0)
            ns_env <- code_env
            nsInfo <- parseNamespaceFile(basename(dir), dirname(dir))
            ## Look only at exported objects.
            OK <- intersect(objects_in_code, nsInfo$exports)
            for(p in nsInfo$exportPatterns)
                OK <- c(OK, grep(p, objects_in_code, value = TRUE))
            objects_in_code <- unique(OK)
        }
    }

    ## Find the function objects to work on.
    functions_in_code <-
        .filter(objects_in_code,
                function(f) {
                    f <- get(f, envir = code_env)
                    is.function(f) && (length(formals(f)) > 0)
                })
    ## <FIXME>
    ## Sourcing all R code files in the package is a problem for base,
    ## where this misses the .Primitive functions.  Hence, when checking
    ## base for objects shown in \usage but missing from the code, we
    ## get the primitive functions from the version of R we are using.
    ## Maybe one day we will have R code for the primitives as well ...
    if(is_base) {
        objects_in_base <-
            objects(envir = baseenv(), all.names = TRUE)
        objects_in_code <-
            c(objects_in_code,
              .filter(objects_in_base, .is_primitive, baseenv()),
              c(".First.lib", ".Last.lib", ".Random.seed",
                ".onLoad", ".onAttach", ".onUnload"))
        objects_in_code_or_namespace <- objects_in_code
    }
    ## </FIXME>

    ## Build a list with the formals of the functions in the code
    ## indexed by the names of the functions.
    function_args_in_code <-
        lapply(functions_in_code,
               function(f) formals(get(f, envir = code_env)))
    names(function_args_in_code) <- functions_in_code
    if(has_namespace) {
        functions_in_ns <-
            .filter(objects_in_ns,
                    function(f) {
                        f <- get(f, envir = ns_env)
                        is.function(f) && (length(formals(f)) > 0)
                    })
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
        lapply(methods::getGenerics(code_env),
               function(f) {
                   meths <-
                       methods::linearizeMlist(methods::getMethodsMetaData(f, code_env))
                   sigs <- sapply(methods::slot(meths, "classes"),
                                  paste, collapse = ",")
                   if(!length(sigs)) return()
                   args <- lapply(methods::slot(meths, "methods"),
                                  formals)
                   names(args) <-
                       paste("\\S4method{", f, "}{", sigs, "}",
                             sep = "")
                   function_args_in_code <<- c(function_args_in_code, args)
               })
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
                    ind <- nchar(as.character(ffd)) > 0
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

    db <- lapply(db, function(f) paste(Rd_pp(f), collapse = "\n"))
    names(db) <- db_names <- .get_Rd_names_from_Rd_db(db)

    ## pkg-defunct.Rd is not expected to list arguments
    ind <- db_names %in% paste(package_name, "defunct", sep="-")
    db <- db[!ind]
    db_names <- db_names[!ind]

    db_usage_texts <-
        .apply_Rd_filter_to_Rd_db(db, get_Rd_section, "usage")
    db_synopses <-
        .apply_Rd_filter_to_Rd_db(db, get_Rd_section, "synopsis")
    ind <- sapply(db_synopses, length) > 0
    db_usage_texts[ind] <- db_synopses[ind]
    with_synopsis <- as.character(db_names[ind])
    db_usages <- lapply(db_usage_texts, .parse_usage_as_much_as_possible)
    ind <- sapply(db_usages,
                  function(x) !is.null(attr(x, "bad_lines")))
    bad_lines <- lapply(db_usages[ind], attr, "bad_lines")

    ## <FIXME>
    ## Currently, there is no useful markup for S3 Ops group methods
    ## and S3 methods for subscripting and subassigning.  Hence, we
    ## cannot reliably distinguish between usage for the generic and
    ## that of a method ...
    functions_to_be_ignored <-
        c(.functions_to_be_ignored_from_usage(basename(dir)),
          .functions_with_no_useful_S3_method_markup())
    ## </FIXME>

    bad_doc_objects <- list()
    functions_in_usages <- character()
    variables_in_usages <- character()
    data_sets_in_usages <- character()
    functions_in_usages_not_in_code <- list()

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
                                 (length(e) == 2)
                                 && e[[1]] == as.symbol("data")))
        if(any(ind)) {
            data_sets_in_usages <-
                c(data_sets_in_usages,
                  sapply(exprs[ind], function(e) as.character(e[[2]])))
            exprs <- exprs[!ind]
        }
        functions <- sapply(exprs, function(e) as.character(e[[1]]))
        functions <- .transform_S3_method_markup(as.character(functions))
        ind <- (! functions %in% functions_to_be_ignored
                & functions %in% functions_in_code)
        bad_functions <-
            mapply(functions[ind],
                   exprs[ind],
                   FUN = function(x, y)
                   check_codoc(x, as.pairlist(as.alist.call(y[-1]))),
                   SIMPLIFY = FALSE)
        ## Replacement functions.
        ind <- as.logical(sapply(exprs,
                                 .is_call_from_replacement_function_usage))
        if(any(ind)) {
            exprs <- exprs[ind]
            replace_funs <-
                paste(sapply(exprs,
                             function(e) as.character(e[[2]][[1]])),
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
                                      as.pairlist(c(as.alist.call(y[[2]][-1]),
                                                    as.alist.symbol(y[[3]])))),
                           SIMPLIFY = FALSE)
                bad_functions <-
                    c(bad_functions, bad_replace_funs)
            }
        }

        bad_functions <- do.call("c", bad_functions)
        if(length(bad_functions) > 0)
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
        if(length(bad_functions) > 0)
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

    attr(bad_doc_objects, "objects_in_code_not_in_usages") <-
        objects_in_code_not_in_usages
    attr(bad_doc_objects, "functions_in_code_not_in_usages") <-
        functions_in_code_not_in_usages
    attr(bad_doc_objects, "functions_in_usages_not_in_code") <-
        functions_in_usages_not_in_code
    attr(bad_doc_objects, "function_args_in_code") <-
        function_args_in_code
    attr(bad_doc_objects, "has_namespace") <- has_namespace
    attr(bad_doc_objects, "with_synopsis") <- with_synopsis
    attr(bad_doc_objects, "bad_lines") <- bad_lines
    class(bad_doc_objects) <- "codoc"
    bad_doc_objects
}

print.codoc <-
function(x, ...)
{
    ## In general, functions in the code which only have an \alias but
    ## no \usage entry are not necessarily a problem---they might be
    ## mentioned in other parts of the Rd object documenting them, or be
    ## 'internal'.  However, if a package has a namespace (and this was
    ## used in the codoc() computations), then clearly all *exported*
    ## functions should have \usage entries.
    ## <FIXME>
    ## Things are not quite that simple.
    ## E.g., for generic functions with just a default and a formula
    ## method we typically do not have \usage for the generic itself.
    ## (This will change now with the new \method{}{} transformation.)
    ## Also, earlier versions of codoc() based on the defunct Perl code
    ## in extract-usage.pl (now removed) only dealt with the *functions*
    ## so all variables would come out as 'without usage information' ...
    ## As we can always access the information via
    ##    attr(codoc("foo"), "objects_in_code_not_in_usages")
    ## disable reporting this for the time being ...
    ## <COMMENT>
    ##     objects_in_code_not_in_usages <-
    ##         attr(x, "objects_in_code_not_in_usages")
    ##     if(length(objects_in_code_not_in_usages)
    ##        && identical(TRUE, attr(x, "has_namespace"))) {
    ##         if(length(objects_in_code_not_in_usages)) {
    ##             writeLines("Exported objects without usage information:")
    ##             .pretty_print(objects_in_code_not_in_usages)
    ##             writeLines("")
    ##         }
    ##     }
    ## </COMMENT>
    ## Hmm.  But why not mention the exported *functions* without \usage
    ## information?  Note that currently there is no useful markup for
    ## S3 Ops group methods and S3 methods for subscripting and
    ## subassigning, so the corresponding generics and methods cannot
    ## reliably be distinguished, and hence would need to be excluded
    ## here as well.
    ## <COMMENT>
    ##     functions_in_code_not_in_usages <-
    ##         attr(x, "functions_in_code_not_in_usages")
    ##     if(length(functions_in_code_not_in_usages)
    ##        && identical(TRUE, attr(x, "has_namespace"))) {
    ##         if(length(functions_in_code_not_in_usages)) {
    ##             writeLines("Exported functions without usage information:")
    ##             .pretty_print(functions_in_code_not_in_usages)
    ##             writeLines("")
    ##         }
    ##     }
    ## </COMMENT>
    ## </FIXME>

    functions_in_usages_not_in_code <-
        attr(x, "functions_in_usages_not_in_code")
    if(length(functions_in_usages_not_in_code) > 0) {
        for(fname in names(functions_in_usages_not_in_code)) {
            writeLines(gettextf("Functions/methods with usage in documentation object '%s' but not in code:",
                                fname))
            .pretty_print(unique(functions_in_usages_not_in_code[[fname]]))
            writeLines("")
        }
    }

    if(length(x) == 0)
        return(invisible(x))
    has_only_names <- is.character(x[[1]][[1]][["code"]])

    format_args <- function(s) {
        if(length(s) == 0)
            "function()"
        else if(has_only_names)
            paste("function(", paste(s, collapse = ", "), ")", sep = "")
        else {
            s <- paste(deparse(s), collapse = "")
            s <- gsub(" = ([,\\)])", "\\1", s)
            gsub("^list", "function", s)
        }
    }

    summarize_mismatches_in_names <- function(nfc, nfd) {
        if(length(nms <- nfc %w/o% nfd))
            writeLines(c(gettext("  Argument names in code not in docs:"),
                         strwrap(paste(nms, collapse = " "),
                                 indent = 4, exdent = 4)))
        if(length(nms <- nfd %w/o% nfc))
            writeLines(c(gettext("  Argument names in docs not in code:"),
                         strwrap(paste(nms, collapse = " "),
                                 indent = 4, exdent = 4)))
        len <- min(length(nfc), length(nfd))
        if(len) {
            len <- seq_len(len)
            nfc <- nfc[len]
            nfd <- nfd[len]
            ind <- which(nfc != nfd)
            len <- length(ind)
            if(len) {
                if(len > 3) {
                    writeLines(gettext("  Mismatches in argument names (first 3):"))
                    ind <- ind[1 : 3]
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
        vffc <- as.character(ffc[nms])
        vffd <- as.character(ffd[nms])
        ind <- which(vffc != vffd)
        len <- length(ind)
        if(len) {
            if(len > 3) {
                writeLines(gettext("  Mismatches in argument default values (first 3):"))
                ind <- ind[1 : 3]
            } else {
                writeLines(gettext("  Mismatches in argument default values:"))
            }
            for(i in ind) {
                writeLines(sprintf("    Name: %s Code: %s Docs: %s",
                                   nms[i], vffc[i], vffd[i]))
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
                                 indent = 2, exdent = 17),
                         strwrap(gettextf("Docs: %s", format_args(ffd)),
                                 indent = 2, exdent = 17)))
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

    bad_Rd_objects <- list()
    class(bad_Rd_objects) <- "codocClasses"

    ## Argument handling.
    if(length(package) != 1)
        stop("argument 'package' must be of length 1")
    dir <- .find.package(package, lib.loc)
    if(!file_test("-d", file.path(dir, "R")))
        stop(gettextf("directory '%s' does not contain R code", dir),
             domain = NA)
    if(!file_test("-d", file.path(dir, "man")))
        stop(gettextf("directory '%s' does not contain Rd sources",
                      dir),
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

    ## Build Rd data base.
    db <- Rd_db(package, lib.loc = dirname(dir))
    db <- lapply(db, Rd_pp)

    ## Need some heuristics now.  When does an Rd object document just
    ## one S4 class so that we can compare (at least) the slot names?
    ## Try the following:
    ## * \docType{} identical to "class";
    ## * just one \alias{} (could also check whether it ends in
    ##   "-class");
    ## * a non-empty user-defined section 'Slots'.

    ## As going through the db to extract sections can take some time,
    ## we do the vectorized metadata computations first, and try to
    ## subscript whenever possible.

    aliases <- lapply(db, .get_Rd_metadata_from_Rd_lines, "alias")
    idx <- (sapply(aliases, length) == 1)
    if(!any(idx)) return(bad_Rd_objects)
    db <- db[idx]; aliases <- aliases[idx]
    idx <- sapply(lapply(db, .get_Rd_metadata_from_Rd_lines, "docType"),
                  identical, "class")
    if(!any(idx)) return(bad_Rd_objects)
    db <- db[idx]; aliases <- aliases[idx]
    ## Now collapse.
    db <- lapply(db, paste, collapse = "\n")
    Rd_slots <-
        .apply_Rd_filter_to_Rd_db(db, get_Rd_section, "Slots", FALSE)
    idx <- !sapply(Rd_slots, identical, character())
    if(!any(idx)) return(bad_Rd_objects)
    db <- db[idx]
    aliases <- unlist(aliases[idx])
    Rd_slots <- Rd_slots[idx]

    names(db) <- .get_Rd_names_from_Rd_db(db)

    .get_slot_names_from_slot_section_text <- function(txt) {
        ## Get \describe (inside user-defined section 'Slots')
        txt <- unlist(sapply(txt, get_Rd_section, "describe"))
        ## Suppose this worked ...
        ## Get the \items inside \describe
        txt <- unlist(sapply(txt, get_Rd_items))
        if(!length(txt)) return(character())
        ## And now strip enclosing '\code{...}:'
        txt <- gsub("\\\\code\\{([^}]*)\\}:?", "\\1", as.character(txt))
        txt <- unlist(strsplit(txt, ", *"))
        txt <- sub("^[[:space:]]+", "", txt)
        txt <- sub("[[:space:]]+$", "", txt)
        txt
    }

    S4_classes_checked <- character()
    for(cl in S4_classes) {
        idx <- which(utils::topicName("class", cl) == aliases)
        if(length(idx) == 1) {
            ## Add sanity checking later ...
            S4_classes_checked <- c(S4_classes_checked, cl)
            slots_in_code <-
                sort(names(methods::slot(methods::getClass(cl, where =
                                                           code_env),
                                         "slots")))
            slots_in_docs <-
                sort(.get_slot_names_from_slot_section_text(Rd_slots[[idx]]))
            if(!identical(slots_in_code, slots_in_docs)) {
                bad_Rd_objects[[names(db)[idx]]] <-
                    list(name = cl,
                         code = slots_in_code,
                         docs = slots_in_docs)
            }
        }
    }

    attr(bad_Rd_objects, "S4_classes_checked") <-
        as.character(S4_classes_checked)
    bad_Rd_objects
}

print.codocClasses <-
function(x, ...)
{
    if (length(x) == 0)
        return(invisible(x))
    format_args <- function(s) paste(s, collapse = " ")
    for (docObj in names(x)) {
        writeLines(gettextf("S4 class codoc mismatches from documentation object '%s':",
                            docObj))
        docObj <- x[[docObj]]
        writeLines(c(gettextf("Slots for class '%s'", docObj[["name"]]),
                     strwrap(gettextf("Code: %s",
                                      format_args(docObj[["code"]])),
                             indent = 2, exdent = 8),
                     strwrap(gettextf("Docs: %s",
                                      format_args(docObj[["docs"]])),
                             indent = 2, exdent = 8)))
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

    bad_Rd_objects <- list()
    class(bad_Rd_objects) <- "codocData"

    ## Argument handling.
    if(length(package) != 1)
        stop("argument 'package' must be of length 1")

    dir <- .find.package(package, lib.loc)
    if(!file_test("-d", file.path(dir, "man")))
       stop(gettextf("directory '%s' does not contain Rd sources", dir),
            domain = NA)
    is_base <- basename(dir) == "base"
    has_namespace <- !is_base && packageHasNamespace(package, dirname(dir))

    ## Load package into code_env.
    if(!is_base)
        .load_package_quietly(package, lib.loc)
    code_env <- .package_env(package)
    if(has_namespace) ns_env <- asNamespace(package)

    ## Could check here whether the package has any variables or data
    ## sets (and return if not).

    ## Build Rd data base.
    db <- Rd_db(package, lib.loc = dirname(dir))
    db <- lapply(db, Rd_pp)

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
    aliases <- lapply(db, .get_Rd_metadata_from_Rd_lines, "alias")
    idx <- sapply(aliases, length) == 1
    if(!any(idx)) return(bad_Rd_objects)
    db <- db[idx]; aliases <- aliases[idx]
    ## Now collapse.
    db <- lapply(db, paste, collapse = "\n")
    names(db) <- .get_Rd_names_from_Rd_db(db)

    .get_data_frame_var_names_from_Rd_text <- function(txt) {
        txt <- get_Rd_section(txt, "format")
        ## Was there just one \format section?
        if(length(txt) != 1) return(character())
        ## What did it start with?
        if(!length(grep("^[ \n\t]*(A|This) data frame", txt)))
            return(character())
        ## Get \describe inside \format
        txt <- get_Rd_section(txt, "describe")
        ## Suppose this worked ...
        ## Get the \items inside \describe
        txt <- unlist(sapply(txt, get_Rd_items))
        if(!length(txt)) return(character())
        txt <- gsub("(.*):$", "\\1", as.character(txt))
        txt <- gsub("\\\\code\\{(.*)\\}:?", "\\1", txt)
        ## Argh.  Of course, variable names can have a '_', which needs
        ## to be escaped if not in \code{}, and the prompt() default is
        ## not to put variable names inside \code{}.
        txt <- gsub("\\\\_", "_", txt)
        txt <- unlist(strsplit(txt, ", *"))
        txt <- sub("^[[:space:]]+", "", txt)
        txt <- sub("[[:space:]]+$", "", txt)
        txt
    }

    Rd_var_names <-
        .apply_Rd_filter_to_Rd_db(db, .get_data_frame_var_names_from_Rd_text)
    idx <- (sapply(Rd_var_names, length) > 0)
    if(!length(idx)) return(bad_Rd_objects)
    aliases <- unlist(aliases[idx])
    Rd_var_names <- Rd_var_names[idx]

    db_names <- names(db)[idx]

    data_env <- new.env()
    data_dir <- file.path(dir, "data")
    ## with lazy data we have data() but don't need to use it.
    hasData <- file_test("-d", data_dir) &&
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
        } else if(hasData) {
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
                             indent = 2, exdent = 8),
                     strwrap(gettextf("Docs: %s",
                                   format_args(docObj[["docs"]])),
                             indent = 2, exdent = 8)))
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
        if(length(package) != 1)
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

    docs_dir <- file.path(dir, "man")
    if(!file_test("-d", docs_dir))
        stop(gettextf("directory '%s' does not contain Rd sources",
                      dir),
             domain = NA)

    db <- if(!missing(package))
        Rd_db(package, lib.loc = dirname(dir))
    else
        Rd_db(dir = dir)

    db <- lapply(db, Rd_pp)
    ## Do vectorized computations for metadata first.
    db_aliases <- lapply(db, .get_Rd_metadata_from_Rd_lines, "alias")
    db_keywords <- lapply(db, .get_Rd_metadata_from_Rd_lines, "keyword")
    ## Now collapse.
    db <- lapply(db, paste, collapse = "\n")
    db_names <- .get_Rd_names_from_Rd_db(db)
    names(db) <- names(db_aliases) <- db_names

    db_usage_texts <-
        .apply_Rd_filter_to_Rd_db(db, get_Rd_section, "usage")
    db_usages <- lapply(db_usage_texts, .parse_usage_as_much_as_possible)
    ind <- as.logical(sapply(db_usages,
                             function(x) !is.null(attr(x, "bad_lines"))))
    bad_lines <- lapply(db_usages[ind], attr, "bad_lines")

    ## Exclude internal objects from further computations.
    ind <- sapply(db_keywords,
                  function(x) any(grep("^ *internal *$", x)))
    if(any(ind)) {                      # exclude them
        db <- db[!ind]
        db_names <- db_names[!ind]
        db_aliases <- db_aliases[!ind]
    }

    db_argument_names <-
        .apply_Rd_filter_to_Rd_db(db, .get_Rd_argument_names)

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
                                 ((length(e) > 1) &&
                                  !((length(e) == 2)
                                    && e[[1]] == as.symbol("data")))))
        exprs <- exprs[ind]
        ## Ordinary functions.
        functions <- as.character(sapply(exprs,
                                         function(e)
                                         as.character(e[[1]])))
        ## (Note that as.character(sapply(exprs, "[[", 1)) does not do
        ## what we want due to backquotifying.)
        ind <- ! functions %in% functions_to_be_ignored
        functions <- functions[ind]
        arg_names_in_usage <-
            unlist(sapply(exprs[ind],
                          function(e) .arg_names_from_call(e[-1])))
        ## Replacement functions.
        ind <- as.logical(sapply(exprs,
                                 .is_call_from_replacement_function_usage))
        if(any(ind)) {
            replace_funs <-
                paste(sapply(exprs[ind],
                             function(e) as.character(e[[2]][[1]])),
                      "<-",
                      sep = "")
            functions <- c(functions, replace_funs)
            arg_names_in_usage <-
                c(arg_names_in_usage,
                  unlist(sapply(exprs[ind],
                                function(e)
                                c(.arg_names_from_call(e[[2]][-1]),
                                  .arg_names_from_call(e[[3]])))))
        }
        ## And finally transform the S3 \method{}{} markup into the
        ## usual function names ...
        ## <NOTE>
        ## If we were really picky, we would worry about possible
        ## namespace renaming.
        functions <- .transform_S3_method_markup(functions)
        ## </NOTE>

        ## Now analyze what we found.
        arg_names_in_usage_missing_in_arg_list <-
            arg_names_in_usage %w/o% arg_names_in_arg_list
        arg_names_in_arg_list_missing_in_usage <-
            arg_names_in_arg_list %w/o% arg_names_in_usage
        if(length(arg_names_in_arg_list_missing_in_usage) > 0) {
            usage_text <- db_usage_texts[[docObj]]
            bad_args <- character()
            ## In the case of 'over-documented' arguments, try to be
            ## defensive and reduce to arguments which either are not
            ## syntactically valid names of do not match the \usage text
            ## (modulo word boundaries).
            bad <- regexpr("^[[:alnum:]._]+$",
                           arg_names_in_arg_list_missing_in_usage) == -1
            if(any(bad)) {
                bad_args <- arg_names_in_arg_list_missing_in_usage[bad]
                arg_names_in_arg_list_missing_in_usage <-
                    arg_names_in_arg_list_missing_in_usage[!bad]
            }
            bad <- sapply(arg_names_in_arg_list_missing_in_usage,
                          function(x)
                          regexpr(paste("\\b", x, "\\b", sep = ""),
                                  usage_text) == -1)
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
        if(!any(grep("-deprecated$", aliases))) {
            ## Currently, there is no useful markup for S3 Ops group
            ## methods and S3 methods for subscripting and subassigning,
            ## so the corresponding generics and methods need to be
            ## excluded from this test (e.g., the usage for '+' in
            ## 'DateTimeClasses.Rd' ...).
            functions <-
                functions %w/o% .functions_with_no_useful_S3_method_markup()
            ## Argh.  There are good reasons for keeping \S4method{}{}
            ## as is, but of course this is not what the aliases use ...
            ## <FIXME>
            ## Should maybe use topicName(), but in any case, we should
            ## have functions for converting between the two forms, see
            ## also the code for undoc().
            aliases <- sub("([^,]+),(.+)-method$",
                           "\\\\S4method{\\1}{\\2}",
                           aliases)
            ## </FIXME>
            aliases <- gsub("\\\\%", "%", aliases)
            functions_not_in_aliases <- functions %w/o% aliases
        }
        else
            functions_not_in_aliases <- character()

        if((length(arg_names_in_usage_missing_in_arg_list) > 0)
           || any(duplicated(arg_names_in_arg_list))
           || (length(arg_names_in_arg_list_missing_in_usage) > 0)
           || (length(functions_not_in_aliases) > 0))
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
        if(length(arg_names_in_usage_missing_in_arg_list) > 0) {
            writeLines(gettextf("Undocumented arguments in documentation object '%s'",
                                doc_obj))
            .pretty_print(unique(arg_names_in_usage_missing_in_arg_list))
        }
        duplicated_args_in_arg_list <- x[[doc_obj]][["duplicated"]]
        if(length(duplicated_args_in_arg_list) > 0) {
            writeLines(gettextf("Duplicated \\argument entries in documentation object '%s':",
                                doc_obj))
            .pretty_print(duplicated_args_in_arg_list)
        }
        arg_names_in_arg_list_missing_in_usage <- x[[doc_obj]][["overdoc"]]
        if(length(arg_names_in_arg_list_missing_in_usage) > 0) {
            writeLines(gettextf("Documented arguments not in \\usage in documentation object '%s':",
                                doc_obj))
            .pretty_print(unique(arg_names_in_arg_list_missing_in_usage))
        }
        functions_not_in_aliases <- x[[doc_obj]][["unaliased"]]
        if(length(functions_not_in_aliases) > 0) {
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
        if(length(package) != 1)
            stop("argument 'package' must be of length 1")
        dir <- .find.package(package, lib.loc)
        ## Using package installed in 'dir' ...
        code_dir <- file.path(dir, "R")
        if(!file_test("-d", code_dir))
            stop(gettextf("directory '%s' does not contain R code",
                          dir),
                 domain = NA)
        docs_dir <- file.path(dir, "man")
        if(!file_test("-d", docs_dir))
            stop(gettextf("directory '%s' does not contain Rd sources",
                          dir),
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
            ns_S3_generics <- ns_S3_methods_db[, 1]
            ns_S3_methods <- ns_S3_methods_db[, 3]
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
        docs_dir <- file.path(dir, "man")
        if(!file_test("-d", docs_dir))
            stop(gettextf("directory '%s' does not contain Rd sources",
                          dir),
                 domain = NA)
        package_name <- basename(dir)
        is_base <- package_name == "base"

        code_env <- new.env()
        .source_assignments_in_code_dir(code_dir, code_env)
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
            ns_S3_generics <- ns_S3_methods_db[, 1]
            ns_S3_methods <- ns_S3_methods_db[, 3]
        }

    }

    ## Find the function objects in the given package.
    functions_in_code <-
        .filter(objects_in_code,
                function(f) is.function(get(f, envir = code_env)))

    ## Find all S3 generics "as seen from the package".
    all_S3_generics <-
        unique(c(.filter(functions_in_code, .is_S3_generic, code_env),
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
            functions_in_code[substr(functions_in_code, 1,
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

    db <- lapply(db, function(f) paste(Rd_pp(f), collapse = "\n"))
    names(db) <- db_names <- .get_Rd_names_from_Rd_db(db)

    ## Ignore pkg-deprecated.Rd and pkg-defunct.Rd.
    ind <- db_names %in% paste(package_name, c("deprecated", "defunct"),
                               sep = "-")
    db <- db[!ind]
    db_names <- db_names[!ind]

    db_usage_texts <-
        .apply_Rd_filter_to_Rd_db(db, get_Rd_section, "usage")
    db_usages <- lapply(db_usage_texts, .parse_usage_as_much_as_possible)
    ind <- sapply(db_usages,
                  function(x) !is.null(attr(x, "bad_lines")))
    bad_lines <- lapply(db_usages[ind], attr, "bad_lines")

    bad_doc_objects <- list()

    for(docObj in db_names) {

        ## Determine function names in the \usage.
        exprs <- db_usages[[docObj]]
        exprs <- exprs[sapply(exprs, length) > 1]
        ## Ordinary functions.
        functions <-
            as.character(sapply(exprs,
                                function(e) as.character(e[[1]])))
        ## (Note that as.character(sapply(exprs, "[[", 1)) does not do
        ## what we want due to backquotifying.)
        ## Replacement functions.
        ind <- as.logical(sapply(exprs,
                                 .is_call_from_replacement_function_usage))
        if(any(ind)) {
            replace_funs <-
                paste(sapply(exprs[ind],
                             function(e) as.character(e[[2]][[1]])),
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

        if((length(methods_with_generic) > 0) ||
           (length(methods_with_full_name > 0)))
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
        if(length(methods_with_full_name > 0)) {
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
    hasNamespace <- FALSE
    ## Argument handling.
    if(!missing(package)) {
        if(length(package) != 1)
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
                hasNamespace <- length(DLLs) > 0
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
        if(file.exists(file.path(dir, "NAMESPACE"))) {
            nm <- parseNamespaceFile(basename(dir), dirname(dir))
            hasNamespace <- length(nm$dynlibs)
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
    else if(missing(file)) {
        stop("you must specify 'package', 'dir' or 'file'")
    }

    if(missing(package) && !file_test("-f", file))
        stop(gettextf("file '%s' does not exist", file),
             domain = NA)

    ## <FIXME>
    ## Should there really be a 'verbose' argument?
    ## It may be useful to extract all foreign function calls but then
    ## we would want the calls back ...
    ## What we currently do is the following: if 'verbose' is true, we
    ## show all foreign function calls in abbreviated form with the line
    ## ending in either 'OK' or 'MISSING', and we return the list of
    ## 'bad' FF calls (i.e., where the 'PACKAGE' argument is missing)
    ## *invisibly* (so that output is not duplicated).
    ## Otherwise, if not verbose, we return the list of bad FF calls.
    ## </FIXME>

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
            if(deparse(e[[1]])[1] %in% FF_funs) {
                if(!is.character(e[[2]])) parg <- "Called with symbol"
                else {
                    parg <- e[["PACKAGE"]]
                    parg <- if(!is.null(parg) && (parg != "")) "OK"
                    else if(!hasNamespace) {
                        bad_exprs <<- c(bad_exprs, e)
                        "MISSING"
                    } else "MISSING but in a function in a namespace"
                }
                if(verbose)
                    cat(deparse(e[[1]]), "(", deparse(e[[2]]),
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
            for(f in methods::getGenerics(code_env)) {
                meths <-
                    methods::linearizeMlist(methods::getMethodsMetaData(f, code_env))
                bodies <- lapply(methods::slot(meths, "methods"), body)
                ## Exclude methods inherited from the 'appropriate'
                ## parent environment.
                ## <NOTE>
                ## Keep this in sync with similar code in undoc().
                ## Note that direct comparison of
                ##   lapply(methods::slot(meths, "methods"), environment)
                ## to code_env is not quite right ...
                penv <- .Internal(getRegisteredNamespace(as.name(package)))
                if(is.environment(penv))
                    penv <- parent.env(penv)
                else
                    penv <- parent.env(code_env)
                if((f %in% methods::getGenerics(penv))
                    && !is.null(mlist_from_penv <-
                                methods::getMethodsMetaData(f, penv))) {
                    classes_from_cenv <-
                        methods::slot(meths, "classes")
                    classes_from_penv <-
                        methods::slot(methods::linearizeMlist(mlist_from_penv),
                                      "classes")
                    ind <- is.na(match(.make_signatures(classes_from_cenv),
                                       .make_signatures(classes_from_penv)))
                    bodies <- bodies[ind]
                }
                ## </NOTE>
                exprs <- c(exprs, bodies)
            }
        }
    }
    else {
        exprs <- try(parse(file = file, n = -1))
        if(inherits(exprs, "try-error"))
            stop(gettextf("parse error in file '%s'", file),
                 domain = NA)
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
    if(length(x) > 0) {
        writeLines(gettextf("Foreign function calls without 'PACKAGE' argument:"))
        for(i in seq_along(x)) {
            writeLines(paste(deparse(x[[i]][[1]]),
                             "(",
                             deparse(x[[i]][[2]]),
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
    S3_reg <- character(0)

    ## Argument handling.
    if(!missing(package)) {
        if(length(package) != 1)
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
            ns_S3_generics <- ns_S3_methods_db[, 1]
            ns_S3_methods <- ns_S3_methods_db[, 3]
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
        .source_assignments_in_code_dir(code_dir, code_env)
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
            ns_S3_generics <- ns_S3_methods_db[, 1]
            ns_S3_methods <- ns_S3_methods_db[, 3]
        }

    }

    ## Find the function objects in the given package.
    functions_in_code <-
        .filter(objects_in_code,
                function(f) is.function(get(f, envir = code_env)))

    S3_group_generics <- .get_S3_group_generics()
    S3_primitive_generics <- .get_S3_primitive_generics()

    checkArgs <- function(g, m) {
        ## Do the arguments of method m (in code_env) 'extend' those of
        ## the generic g as seen from code_env?  The method must have all
        ## arguments the generic has, with positional arguments of g in
        ## the same positions for m.
        ## Exception: '...' in the method swallows anything.
        genfun <- get(g, envir = code_env)
        gArgs <- names(formals(genfun))
        if(g == "plot") gArgs <- gArgs[-2]
        ogArgs <- gArgs
        gm <- if(m %in% S3_reg) {
            ## See registerS3method() in namespace.R.
            defenv <-
                if (g %in% S3_group_generics || g %in% S3_primitive_generics)
                    .BaseNamespaceEnv
                else {
                    if(.isMethodsDispatchOn()
                       && methods::is(genfun, "genericFunction"))
                        genfun <-
                            methods::slot(genfun, "default")@methods$ANY
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
        if(length(grep("\\.formula$", m)) > 0) {
            gArgs <- gArgs[-1]
            mArgs <- mArgs[-1]
        }
        dotsPos <- which(gArgs == "...")
        ipos <- if(length(dotsPos) > 0)
            seq.int(from = 1, length = dotsPos[1] - 1)
        else
            seq_along(gArgs)

        ## careful, this could match multiply in incorrect funs.
        dotsPos <- which(mArgs == "...")
        if(length(dotsPos) > 0)
            ipos <- ipos[seq.int(from = 1, length = dotsPos[1] - 1)]
        posMatchOK <- identical(gArgs[ipos], mArgs[ipos])
        argMatchOK <- all(gArgs %in% mArgs) || length(dotsPos) > 0
        if(posMatchOK && argMatchOK)
            NULL
        else {
            l <- list(ogArgs, omArgs)
            names(l) <- c(g, m)
            list(l)
        }
    }

    all_S3_generics <-
        unique(c(.filter(functions_in_code, .is_S3_generic, code_env),
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
            functions_in_code[substr(functions_in_code, 1,
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
        writeLines(c(paste(names(entry)[1], ":", sep = ""),
                     strwrap(format_args(entry[[1]]),
                             indent = 2, exdent = 11),
                     paste(names(entry)[2], ":", sep = ""),
                     strwrap(format_args(entry[[2]]),
                             indent = 2, exdent = 11),
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
        if(length(package) != 1)
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
        .source_assignments_in_code_dir(code_dir, code_env)
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
        ns_S3_generics <- ns_S3_methods_db[, 1]
        ns_S3_methods <- ns_S3_methods_db[, 3]
        ## S3 replacement methods from namespace registration?
        idx <- grep("<-$", ns_S3_generics)
        if(any(idx)) replace_funs <- ns_S3_methods[idx]
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
        .filter(replace_funs,
                function(f) {
                    ## Always get the functions from code_env ...
                    ## Should maybe get S3 methods from the registry ...
                    f <- get(f, envir = code_env)
                    if(!is.function(f)) return(FALSE)
                    ! .check_last_formal_arg(f)
                })
    } else character(0)

    if(.isMethodsDispatchOn()) {
        S4_generics <- methods::getGenerics(code_env)
        ## Assume that the ones with names ending in '<-' are always
        ## replacement functions.
        S4_generics <- grep("<-$", S4_generics, value = TRUE)
        bad_S4_replace_methods <-
            sapply(S4_generics,
                   function(f) {
                       meths <- methods::linearizeMlist(methods::getMethodsMetaData(f, code_env))
                       ind <- !as.logical(sapply(methods::slot(meths,
                                                               "methods"),
                                                 .check_last_formal_arg))
                       if(!any(ind))
                           character()
                       else {
                           sigs <- sapply(methods::slot(meths,
                                                        "classes")[ind],
                                          paste, collapse = ",")
                           paste("\\S4method{", f, "}{", sigs, "}",
                                 sep = "")
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
    if(length(x) > 0) .pretty_print(unclass(x))
    invisible(x)
}

### * checkTnF

checkTnF <-
function(package, dir, file, lib.loc = NULL)
{
    code_files <- docs_files <- character(0)

    ## Argument handling.
    if(!missing(package)) {
        if(length(package) != 1)
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
        if(missing(txt)) {
            exprs <- try(parse(file = file, n = -1))
            if(inherits(exprs, "try-error"))
                stop(gettextf("parse error in file '%s'", file),
                     domain = NA)
        }
        else {
            exprs <- try(parse(text = txt))
            if(inherits(exprs, "try-error"))
                stop(gettextf("parse error in examples from file '%s'",
                              file),
                     domain = NA)
        }
        for(i in seq_along(exprs))
            find_bad_exprs(exprs[[i]], NULL)
        matches
    }

    bad_exprs <- list()
    for(file in code_files) {
        exprs <- find_TnF_in_code(file)
        if(length(exprs) > 0) {
            exprs <- list(exprs)
            names(exprs) <- file
            bad_exprs <- c(bad_exprs, exprs)
        }
    }
    for(file in docs_files) {
        txt <- paste(Rd_pp(.read_Rd_lines_quietly(file)),
                     collapse = "\n")
        txt <- .get_Rd_example_code(txt)
        exprs <- find_TnF_in_code(file, txt)
        if(length(exprs) > 0) {
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
                               exdent = 4))
        }
        writeLines("")
    }
    invisible(x)
}

### * .check_package_depends

## changed in 2.3.0 to refer to a source dir.

.check_package_depends <-
function(dir)
{
    if(length(dir) != 1)
        stop("argument 'package' must be of length 1")

    ## We definitely need a valid DESCRIPTION file.
    db <- .read_description(file.path(dir, "DESCRIPTION"))

    package_name <- basename(dir)
    ## (Should really use db["Package"], but then we need to check
    ## whether this is really there ...)
    depends <- .get_requires_from_package_db(db, "Depends")
    imports <- .get_requires_from_package_db(db, "Imports")
    suggests <- .get_requires_from_package_db(db, "Suggests")
    ## Need this to handle bundles ...
    contains <- .get_contains_from_package_db(db)

    standard_package_names <- .get_standard_package_names()

    bad_depends <- list()

    ## Are all packages listed in Depends/Suggests/Imports installed?
    ## Need to treat specially the former stub packages.
    reqs <- unique(c(depends,
                     imports,
                     if(!identical(as.logical(Sys.getenv("_R_CHECK_FORCE_SUGGESTS_")),
                                   FALSE))
                     suggests))
    reqs <- reqs %w/o% utils::installed.packages()[ , "Package"]
    m <- reqs %in% standard_package_names$stubs
    if(length(reqs[!m]))
        bad_depends$required_but_not_installed <- reqs[!m]
    if(length(reqs[m]))
        bad_depends$required_but_stub <- reqs[m]

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
        reqs <- reqs %w/o% c(contains, imports, depends, allowed_imports)
        ## Note that for bundles we currently cannot have package
        ## dependencies different from bundle ones, and clearly a bundle
        ## cannot depend on something it contains ...
        ## </FIXME>
        if(length(reqs))
            bad_depends$missing_namespace_depends <- reqs
    }

    class(bad_depends) <- "check_package_depends"
    bad_depends
}

print.check_package_depends <-
function(x, ...)
{
    if(length(bad <- x$required_but_not_installed)) {
        writeLines(gettext("Packages required but not available:"))
        .pretty_print(bad)
        writeLines("")
    }
    if(length(bad <- x$required_but_stub)) {
        writeLines(gettext("Former standard packages required but now defunct:"))
        .pretty_print(bad)
        writeLines("")
    }
    if(length(bad <- x$missing_vignette_depends)) {
        writeLines(gettext("Vignette dependencies not required:"))
        .pretty_print(bad)
        msg <- gettext("Vignette dependencies (\\VignetteDepends{} entries) must be contained in the DESCRIPTION Depends/Suggests/Imports entries.")
        writeLines(strwrap(msg))
        writeLines("")
    }
    if(length(bad <- x$missing_namespace_depends)) {
        writeLines(gettext("Namespace dependencies not required:"))
        .pretty_print(bad)
        writeLines("")
    }
    invisible(x)
}

### * check_Rd_files_in_package

## </NOTE>
## We currently have two (internal) check_Rd_files* functions.
##
## The primary one is check_Rd_files_in_man_dir, as this always works,
## but note that its 'dir' argument really is a directory containing Rd
## source files, and not a package top-level source subdirectory (as
## indicated by 'man_dir' in the function name).
##
## Function check_Rd_files_in_package only works for packages installed
## with R 2.0 or better (as it requires that the installed Rd sources
## have the Rd file names preserved).
##
## So perhaps eventually unify these functions for 2.1?  Currently, it
## seems a bad idea to have check_Rd_files(dir, package, lib.loc) which
## has a different interface than the other QC functions ...
##
## Of course, all of this is conditional on not moving away from Rd
## format ...
## </NOTE>

check_Rd_files_in_package <-
function(package, lib.loc = NULL)
{
    if(length(package) != 1)
        stop("argument 'package' must be of length 1")
    ## (Actually, Rd_db() would check on this too ...)
    db <- Rd_db(package, lib.loc)
    if(is.null(names(db)))
        stop("Package Rd sources were installed without preserving Rd file names.\n",
             "Please reinstall using a current version of R.")
    .check_Rd_files_in_Rd_db(db)
}

### * check_Rd_files_in_man_dir

check_Rd_files_in_man_dir <-
function(dir)
{
    if(!file_test("-d", dir))
        stop(gettextf("directory '%s' does not exist", dir),
             domain = NA)
    dir <- file_path_as_absolute(dir)
    ## Argh.  We cannot call Rd_db() directly, because this works on
    ## the top-level package source directory ...
    Rd_files <- list_files_with_type(file.path(dir), "docs")
    db <- lapply(Rd_files, .read_Rd_lines_quietly)
    names(db) <- Rd_files
    .check_Rd_files_in_Rd_db(db)
}

### * .check_Rd_files_in_Rd_db

.check_Rd_files_in_Rd_db <-
function(db)
{
    standard_keywords <- .get_standard_Rd_keywords()
    mandatory_tags <- c("name", "title", "description")
    ## We also need
    ##   alias keyword
    ## but we handle these differently ...
    unique_tags <-
        c("name", "title", "description", "usage", "arguments",
          "format", "details", "value", "references", "source",
          "seealso", "examples", "note", "author", "synopsis",
          "docType", "encoding")
    known_tags <- c(unique_tags,
                    "section",
                    ## Allow for empty keywords (these do not make it
                    ## into the metadata).
                    "keyword",
                    ## Keep this for back-compatibility ...
                    "non_function")
    ## Note that we treat \alias and \keyword entries as metadata.

    files_with_surely_bad_Rd <- list()
    files_with_likely_bad_Rd <- list()
    files_with_unknown_encoding <- NULL
    files_with_non_ASCII_metadata <- NULL
    files_with_non_ASCII_section_titles <- NULL
    files_with_missing_mandatory_tags <- NULL
    files_with_duplicated_unique_tags <- NULL
    files_with_unknown_tags <- NULL
    files_with_bad_name <- files_with_bad_title <- NULL
    files_with_bad_keywords <- NULL

    db_aliases <- vector("list", length(db))
    names(db_aliases) <- names(db)

    for(f in names(db)) {
        x <- tryCatch(Rd_parse(text = db[[f]]), error = .identity)

        if(inherits(x, "error")) {
            files_with_surely_bad_Rd[[f]] <- conditionMessage(x)
            next
        }

        db_aliases[[f]] <- unique(x$meta$aliases)
        if(length(x$rest))
            files_with_likely_bad_Rd[[f]] <- x$rest

        if(length(x$meta$encoding) && is.na(x$meta$encoding))
            files_with_unknown_encoding <-
                c(files_with_unknown_encoding, f)

        for(tag in c("aliases", "doc_type", "encoding")) {
            if(any(ind <- !.is_ASCII(x$meta[[tag]])))
                files_with_non_ASCII_metadata <-
                    rbind(files_with_non_ASCII_metadata,
                          cbind(f, tag, x$meta[[tag]][ind]))
        }

        ## Non-ASCII user-defined section titles.
        ## <NOTE>
        ## Rd_parse() re-encodes these if necessary (and possible), but
        ## we should still be able to catch the non-ASCII ones provided
        ## that the native encoding extends ASCII.
        user_defined_section_titles <- sapply(x$data$tags, "[", 2)
        if(any(ind <- !.is_ASCII(user_defined_section_titles)))
            files_with_non_ASCII_section_titles <-
                rbind(files_with_non_ASCII_section_titles,
                      cbind(f, user_defined_section_titles[ind]))
        ## </NOTE>

        tags <- sapply(x$data$tags, "[[", 1)
        ## Let's not worry about named sections for the time being ...
        bad_tags <- c(mandatory_tags %w/o% tags,
                      if(!length(x$meta$aliases)) "alias",
                      ## Allow for empty keywords (these do not make it
                      ## into the metadata).
                      if(!(length(x$meta$keywords)
                           || any(grep("^[[:space:]]*$",
                                       x$data$vals[tags == "keyword"]))))
                      "keyword")
        if(length(bad_tags))
            files_with_missing_mandatory_tags <-
                rbind(files_with_missing_mandatory_tags,
                      cbind(f, bad_tags))

        ind <- which(tags == "name")[1]
        if(is.na(ind))
            files_with_bad_name <- c(files_with_bad_name, f)

        ind <- which(tags == "title")[1]
        if(is.na(ind) ||
           (regexpr("^[[:space:]]*$", x$data$vals[[ind]]) != -1))
            files_with_bad_title <- c(files_with_bad_title, f)

        bad_tags <- intersect(tags[duplicated(tags)], unique_tags)
        if(length(bad_tags))
            files_with_duplicated_unique_tags <-
                rbind(files_with_duplicated_unique_tags,
                      cbind(f, bad_tags))

        bad_tags <- unique(tags) %w/o% known_tags
        if(length(bad_tags))
            files_with_unknown_tags <-
                rbind(files_with_unknown_tags,
                      cbind(f, bad_tags))

        bad_keywords <- x$meta$keywords %w/o% standard_keywords
        if(length(bad_keywords))
            files_with_bad_keywords <-
                rbind(files_with_bad_keywords,
                      cbind(f, bad_keywords))
    }

    db_aliases_by_db_names <-
        split(rep(names(db_aliases), sapply(db_aliases, length)),
              unlist(db_aliases, use.names = FALSE))
    files_with_duplicated_aliases <-
        db_aliases_by_db_names[sapply(db_aliases_by_db_names,
                                      length) > 1]

    val <- list(files_with_surely_bad_Rd,
                files_with_likely_bad_Rd,
                files_with_unknown_encoding,
                files_with_non_ASCII_metadata,
                files_with_non_ASCII_section_titles,
                files_with_missing_mandatory_tags,
                files_with_duplicated_unique_tags,
                files_with_unknown_tags,
                files_with_bad_name,
                files_with_bad_title,
                files_with_bad_keywords,
                files_with_duplicated_aliases)
    names(val) <-
        c("files_with_surely_bad_Rd",
          "files_with_likely_bad_Rd",
          "files_with_unknown_encoding",
          "files_with_non_ASCII_metadata",
          "files_with_non_ASCII_section_titles",
          "files_with_missing_mandatory_tags",
          "files_with_duplicated_unique_tags",
          "files_with_unknown_tags",
          "files_with_bad_name",
          "files_with_bad_title",
          "files_with_bad_keywords",
          "files_with_duplicated_aliases")
    class(val) <- "check_Rd_files_in_Rd_db"
    val
}

print.check_Rd_files_in_Rd_db <-
function(x, ...)
{
    if(length(x$files_with_surely_bad_Rd)) {
        writeLines(gettext("Rd files with syntax errors:"))
        bad <- x$files_with_surely_bad_Rd
        for(i in seq_along(bad)) {
            writeLines(c(paste("  ", names(bad)[i], ":", sep = ""),
                         strwrap(bad[[i]], indent = 4, exdent = 4)))
        }
        writeLines("")
    }

    if(length(x$files_with_likely_bad_Rd)) {
        bad <- x$files_with_likely_bad_Rd
        ## Do not warn about stray top-level text which is just
        ## whitespace and closing braces (i.e., "too many" closing
        ## braces at top level).  These are not quite correct Rd, but
        ## can safely be ignored, as Rdconv does.
        bad <- lapply(bad,
                      function(x) x[regexpr("^[[:space:]}]*$", x) == -1])
        bad <- bad[sapply(bad, length) > 0]
        if(length(bad)) {
            writeLines(gettext("Rd files with likely Rd problems:"))
            for(i in seq_along(bad)) {
                writeLines(gettextf("Unaccounted top-level text in file '%s':",
                                    names(bad)[i]))
                tags <- names(bad[[i]])
                if(any(ind <- tags != ""))
                    tags[ind] <- gettextf("Following section '%s'",
                                          tags[ind])
                tags[!ind] <- "Preceding all sections"
                vals <- as.character(bad[[i]])
                long <- nchar(vals, type="c") >= 128  # Why 128?  Why not?
                vals <- paste(sapply(substr(vals, 1, 127), deparse, 128),
                              ifelse(long, " [truncated]", ""), sep = "")
                writeLines(c(paste(tags, vals, sep = c(":\n", "\n")), ""))
            }
        }
    }

    if(length(x$files_with_unknown_encoding)) {
        writeLines(c(gettext("Rd files with unknown encoding:"),
                     paste(" ", x$files_with_unknown_encoding),
                     ""))
    }

    if(length(x$files_with_non_ASCII_metadata)) {
        writeLines(gettext("Rd files with invalid non-ASCII metadata:"))
        bad <- x$files_with_non_ASCII_metadata
        ## Reinstate the Rd markup tags for better intelligibility.
        bad[ , 2] <- sub("aliases", "\\\\alias", bad[ , 2])
        bad[ , 2] <- sub("doc_type", "\\\\docType", bad[ , 2])
        bad[ , 2] <- sub("encoding", "\\\\encoding", bad[ , 2])
        ind <- split(seq_len(NROW(bad)), bad[, 1])
        for(i in seq_along(ind)) {
            writeLines(c(paste(" ", paste(names(ind)[i], ":", sep = "")),
                         paste("   ",
                               apply(bad[ind[[i]], -1, drop = FALSE],
                                     1, paste, collapse = " "))))
        }
        writeLines("")
    }

    if(length(x$files_with_non_ASCII_section_titles)) {
        writeLines(gettext("Rd files with non-ASCII section titles:"))
        bad <- x$files_with_non_ASCII_section_titles
        bad <- split(bad[, 2], bad[, 1])
        for(i in seq_along(bad)) {
            writeLines(c(paste(" ", paste(names(bad)[i], ":", sep = "")),
                         strwrap(bad[[i]], indent = 4, exdent = 6),
                         ""))
        }
    }

    if(length(x$files_with_bad_name)) {
        writeLines(c(gettextf("Rd files with missing or empty '\\name':"),
                     paste(" ", x$files_with_bad_name),
                     ""))
    }

    if(length(x$files_with_bad_title)) {
        writeLines(c(gettextf("Rd files with missing or empty '\\title':"),
                     paste(" ", x$files_with_bad_title),
                     ""))
    }

    if(length(x$files_with_missing_mandatory_tags)) {
        bad <- x$files_with_missing_mandatory_tags
        bad <- split(bad[, 1], bad[, 2])
        for(i in seq_along(bad)) {
            writeLines(c(gettextf("Rd files without '%s':",
                                  names(bad)[i]),
                         paste(" ", bad[[i]])))
        }
        writeLines(gettext("These entries are required in an Rd file.\n"))
    }

    if(length(x$files_with_duplicated_unique_tags)) {
        bad <- x$files_with_duplicated_unique_tags
        bad <- split(bad[, 1], bad[, 2])
        for(i in seq_along(bad)) {
            writeLines(c(gettextf("Rd files with duplicate '%s':",
                                  names(bad)[i]),
                         paste(" ", bad[[i]])))
        }
        writeLines(gettext("These entries must be unique in an Rd file.\n"))
    }

    if(length(x$files_with_unknown_tags)) {
        writeLines(gettextf("Rd files with unknown sections:"))
        bad <- x$files_with_unknown_tags
        bad <- split(bad[, 2], bad[, 1])
        for(i in seq_along(bad)) {
            writeLines(strwrap(paste(names(bad)[i], ": ",
                                     paste(bad[[i]], collapse = " "),
                                     "\n", sep = ""),
                               indent = 2, exdent = 4))
        }
        writeLines("")
    }

    if(length(x$files_with_bad_keywords)) {
        writeLines(gettext("Rd files with non-standard keywords:"))
        bad <- x$files_with_bad_keywords
        bad <- split(bad[, 2], bad[, 1])
        for(i in seq_along(bad)) {
            writeLines(strwrap(paste(names(bad)[i], ": ",
                                     paste(bad[[i]], collapse = " "),
                                     "\n", sep = ""),
                               indent = 2, exdent = 4))
        }
        msg <-
        gettext("Each '\\keyword' entry should specify one of the standard keywords (as listed in file 'KEYWORDS' in the R documentation directory).")
        writeLines(c(strwrap(msg), ""))
    }

    if(length(x$files_with_duplicated_aliases)) {
        bad <- x$files_with_duplicated_aliases
        for(alias in names(bad)) {
            writeLines(gettextf("Rd files with duplicated alias '%s':", alias))
            .pretty_print(bad[[alias]])
        }
        writeLines("")
    }

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
    ## must be ASCII we well (so that the RPM works in a C locale).
    ASCII_fields <- c("Package", "Version", "Depends", "Suggests",
                      "Imports", "Priority", "Encoding")
    ASCII_fields <- intersect(ASCII_fields, names(db))
    if(any(ind <- !.is_ASCII(db[ASCII_fields])))
        out$fields_with_non_ASCII_values <- ASCII_fields[ind]

    ## Determine encoding and re-encode if necessary and possible.
    if("Encoding" %in% names(db)) {
        encoding <- db["Encoding"]
        if((Sys.getlocale("LC_CTYPE") != "C")
           && capabilities("iconv"))
            db <- iconv(db, encoding, "")
    }
    else if(!all(.is_ISO_8859(db))) {
        ## No valid Encoding metadata.
        ## Determine whether we can assume Latin1.
        out$missing_encoding <- TRUE
    }

    if(any(is.na(nchar(db, "c")))) {
        ## Ouch, invalid in the current locale.
        ## (Can only happen in a MBCS locale.)
        ## Try re-encoding from Latin1.
        if(capabilities("iconv"))
            db <- iconv(db, "latin1", "")
        else
            stop("Found invalid multi-byte character data.", "\n",
                 "Cannot re-encode because iconv is not available.", "\n",
                 "Try running R in a single-byte locale.")

    }

    ## Mandatory entries in DESCRIPTION:
    ##   Package, Version, License, Description, Title, Author,
    ##   Maintainer.
    required_fields <- c("Package", "Version", "License", "Description",
                         "Title", "Author", "Maintainer")
    if(any(i <- which(is.na(match(required_fields, names(db))) |
                      is.na(db[required_fields]))))
        out$missing_required_fields <- required_fields[i]

    val <- package_name <- db["Package"]
    if(!is.na(val)) {
        tmp <- character()
        if(regexpr(sprintf("^%s$", valid_package_name_regexp), val) == -1
           && regexpr("^Translation-[[:alnum:].]+$", val) == -1)
            tmp <- c(tmp, gettext("Malformed package name"))
        ## <FIXME>
        ## Not clear if we really want to do this.  The Perl code still
        ## seemed to assume that when checking a package, package name
        ## and 'directory' (i.e., the base name of the directory with
        ## the DESCRIPTION metadata) need to be the same.
        ## if(val != basename(dirname(dfile)))
        ##     tmp <- c(tmp, "Package name differs from dir name.")
        ## </FIXME>
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
       && (regexpr(sprintf("^%s$", valid_package_version_regexp),
                   val) == -1))
        out$bad_version <- val
    if(!is.na(val <- db["Maintainer"])
       && (regexpr("(^[^<>]*<[^<>@]+@[^<>@]+> *$|ORPHANED)", val)
           == -1))
        out$bad_maintainer <- val

    ## Optional entries in DESCRIPTION:
    ##   Depends/Suggests/Imports, Namespace, Priority.
    ## These must be correct if present.

    val <- db[match(c("Depends", "Suggests", "Imports", "Enhances"),
                    names(db), nomatch = 0)]
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
            if(regexpr(dep_regexp, dep) == -1) {
                ## Entry does not match the regexp.
                bad_dep_entry <- c(bad_dep_entry, dep)
                next
            }
            if(nchar(sub(dep_regexp, "\\2", dep))) {
                ## If not just a valid package name ...
                if(!sub(dep_regexp, "\\3", dep) %in% c("<=", ">="))
                    bad_dep_op <- c(bad_dep_op, dep)
                else if(regexpr(sprintf("^%s$",
                                        valid_package_version_regexp),
                                sub(dep_regexp, "\\4", dep)) == -1)
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

    if(any(as.integer(sapply(x$bad_depends_or_suggests_or_imports, length)))) {
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

    if(any(as.integer(sapply(x, length))))
        writeLines(c(strwrap(gettextf("See the information on DESCRIPTION files in section 'Creating R packages' of the 'Writing R Extensions' manual.")),
                     ""))

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
    mfile <- paths[1]

    lines <-
        tryCatch(system(sprintf("%s -f %s -f %s",
                                Sys.getenv("MAKE"),
                                shQuote(mfile),
                                shQuote(file.path(R.home("share"),
                                                  "make", "check.mk"))),
                        intern = TRUE,
                        if(identical(.Platform$OS.type, "unix"))
                        ignore.stderr = TRUE),
                 error = .identity)
    if(!length(lines) || inherits(lines, "error"))
        return(bad_flags)

    ## Try to be careful ...
    lines <- lines[regexpr("^PKG_[A-Z]*FLAGS: ", lines) > -1]
    names <- sub(":.*", "", lines)
    lines <- sub("^PKG_[A-Z]*FLAGS: ", "", lines)
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
                        "Wall", "ansi", "pedantic", "traditiona",
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
    if(length(x) > 0) {
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
    if(!is_base)
        .load_package_quietly(package, lib.loc)

    ## A simple function for catching the output from the codetools
    ## analysis using the checkUsage report mechanism.
    out <- character()
    foo <- function(x) out <<- c(out, x)
    ## (Simpler than using a variant of capture.output().)
    ## Of course, it would be nice to return a suitably structured
    ## result, but we can always do this by suitably splitting the
    ## messages on the double colons ...

    ## <NOTE>
    ## Eventually, we should be able to specify a codetools "profile"
    ## for checking.
    ## </NOTE>

    suppressMessages(codetools::checkUsagePackage(package,
                                                  report = foo,
                                                  suppressLocalUnused = TRUE,
                                                  skipWith = TRUE))
    class(out) <- "check_code_usage_in_package"
    out
}

print.check_code_usage_in_package <-
function(x, ...)
{
    if(length(x) > 0)
        writeLines(strwrap(x, indent = 0, exdent = 2))
    invisible(x)
}

### * .check_Rd_xrefs

.check_Rd_xrefs <-
function(package, dir, lib.loc = NULL)
{
    ## Build a db with all possible link targets (aliases) in the base
    ## and recommended packages.
    aliases <-
        lapply(unlist(.get_standard_package_names()[c("base",
                                                      "recommended")],
                      use.names = FALSE),
               Rd_aliases, lib.loc = NULL)
    ## (Don't use lib.loc = .Library, as recommended packages may have
    ## been installed to a different place.)

    ## Add the aliases from the package itself, and build a db with all
    ## \link xrefs in the package Rd objects.
    if(!missing(package)) {
        aliases <-
            c(aliases, list(Rd_aliases(package, lib.loc = lib.loc)))
        db <- .build_Rd_xref_db(package, lib.loc = lib.loc)
    }
    else {
        aliases <-
            c(aliases, list(Rd_aliases(dir = dir)))
        db <- .build_Rd_xref_db(dir = dir)
    }

    ## Flatten the xref db into one big matrix.
    db <- cbind(do.call("rbind", db), rep(names(db), sapply(db, NROW)))

    ## Take the targets from the non-anchored xrefs.
    db <- db[db[, 2] == "", -2, drop = FALSE]

    ## The bad ones:
    db <- db[! db[, 1] %in% unlist(aliases), , drop = FALSE]
    structure(split(db[, 1], db[, 2]), class = "check_Rd_xrefs")
}

print.check_Rd_xrefs <-
function(x, ...)
{
    if(length(x) > 0) {
        for(i in seq_along(x)) {
            writeLines(gettextf("Missing link(s) in documentation object '%s':",
                                names(x)[i]))
            .pretty_print(x[[i]])
            writeLines("")
        }
        ## <FIXME>
        ## Add some explanatory message and a pointer to R-exts
        ## eventually ...
        ## </FIXME>
    }
    x
}

### * as.alist.call

as.alist.call <-
function(x)
{
    y <- as.list(x)
    ind <- if(is.null(names(y)))
        seq_along(y)
    else
        which(names(y) == "")
    if(any(ind)) {
        names(y)[ind] <- sapply(y[ind],as.character)
        y[ind] <- rep.int(list(alist(irrelevant = )[[1]]), length(ind))
    }
    y
}

### * as.alist.symbol

as.alist.symbol <-
function(x)
{
    as.alist.call(call(as.character(x)))
}

### * .arg_names_from_call

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

### * .functions_to_be_ignored_from_usage

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

### * .functions_with_no_useful_S3_method_markup

.functions_with_no_useful_S3_method_markup <-
function()
{
    ## Once upon a time ... there was no useful markup for S3 methods
    ## for subscripting/subassigning and binary operators.  There is
    ## still no such markup for *unary* operators, and, strictly
    ## speaking, for S3 Ops group methods for binary operators [but it
    ## seems that people do not want to provide explicit documentation
    ## for these].
    ##
    ## Support for S3 methods for subscripting/subassigning was added
    ## for R 2.1, and for S3 methods for binary operators in 2.2.
    ## Markup for the former is a bit controversial, as some legacy docs
    ## have non-synopsis-style \usage entries for these methods.  E.g.,
    ## as of 2005-05-21, \link[base]{Extract.data.frame} has
    ##   x[i]
    ##   x[i] <- value
    ##   x[i, j, drop = TRUE]
    ##   x[i, j] <- value
    ## Hence, we provide internal environment variables for controlling
    ## what should be ignored.
    c(if(!identical(as.logical(Sys.getenv("_R_CHECK_RD_USAGE_METHOD_SUBSET_")),
                    TRUE))
      c("[", "[[", "$", "[<-", "[[<-", "$<-"),
      if(identical(as.logical(Sys.getenv("_R_CHECK_RD_USAGE_METHOD_BINOPS_")),
                   FALSE))
      c("+", "-", "*", "/", "^", "<", ">", "<=", ">=", "!=", "==", "%%",
        "%/%", "&", "|"),
      ## Current, nothing for unary operators.
      "!")
}

### * .is_call_from_replacement_function_usage

.is_call_from_replacement_function_usage <-
function(x)
{
    ((length(x) == 3)
     && (identical(x[[1]], as.symbol("<-")))
     && (length(x[[2]]) > 1)
     && is.symbol(x[[3]]))
}

### * .make_signatures

.make_signatures <-
function(cls)
{
    ## Note that (thanks JMC), when comparing signatures, the signature
    ## has to be stripped of trailing "ANY" elements (which are always
    ## implicit) or padded to a fixed length.
    sub("(#ANY)*$", "", unlist(lapply(cls, paste, collapse = "#")))
}

### * .package_env

.package_env <-
function(package_name)
{
    as.environment(paste("package", package_name, sep = ":"))
}

### * .parse_text_as_much_as_possible

.parse_text_as_much_as_possible <-
function(txt)
{
    exprs <- try(parse(text = txt), silent = TRUE)
    if(!inherits(exprs, "try-error")) return(exprs)
    exprs <- expression()
    lines <- unlist(strsplit(txt, "\n"))
    bad_lines <- character()
    while((n <- length(lines)) > 0) {
        i <- 1; txt <- lines[1]
        while(inherits(yy <- try(parse(text = txt), silent = TRUE),
                       "try-error")
              && (i < n)) {
            i <- i + 1; txt <- paste(txt, lines[i], collapse = "\n")
        }
        if(inherits(yy, "try-error")) {
            bad_lines <- c(bad_lines, lines[1])
            lines <- lines[-1]
        }
        else {
            exprs <- c(exprs, yy)
            lines <- lines[-seq_len(i)]
        }
    }
    attr(exprs, "bad_lines") <- bad_lines
    exprs
}

### * .parse_usage_as_much_as_possible

.parse_usage_as_much_as_possible <-
function(txt)
{
    txt <- gsub("\\\\l?dots", "...", txt)
    txt <- gsub("\\\\%", "%", txt)
    txt <- .Rd_transform_command(txt, "special", function(u) NULL)
    txt <- gsub(.S3_method_markup_regexp, "\"\\\\\\1\"", txt)
    txt <- gsub(.S4_method_markup_regexp, "\"\\\\\\1\"", txt)
    ## Transform <<see below>> style markup so that we can catch and
    ## throw it, rather than "basically ignore" it by putting it in the
    ## bad_lines attribute.
    txt <- gsub("(<<?see below>>?)", "`\\1`", txt)
    ## 'LanguageClasses.Rd' in package methods has '"\{"' in its usage:
    ## the docs say that unpaired braces in \code need to be escaped, so
    ## let's assume that this is also true for \usage.
    txt <- gsub("\\\\\\{", "{", txt)
    txt <- gsub("\\\\\\}", "}", txt)
    .parse_text_as_much_as_possible(txt)
}

### * .pretty_print

.pretty_print <-
function(x)
{
    writeLines(strwrap(paste(x, collapse = " "),
                       indent = 2, exdent = 2))
}

### * .transform_S3_method_markup

.transform_S3_method_markup <-
function(x)
{
    ## Note how we deal with S3 replacement methods found.
    ## These come out named "\method{GENERIC}{CLASS}<-" which we
    ## need to turn into 'GENERIC<-.CLASS'.
    sub(sprintf("%s(<-)?", .S3_method_markup_regexp),
        "\\3\\5.\\4",
        x)
}

### * .S3_method_markup_regexp

## For matching \(S3)?method{GENERIC}{CLASS}.
## GENERIC can be
## * a syntactically valid name
## * one of $ [ [[
## * one of the binary operators
##   + - * / ^ < <= > >= != == | & %something%
## (as supported by Rdconv).
## See also .functions_with_no_useful_S3_method_markup.

.S3_method_markup_regexp <-
    sprintf("(\\\\(S3)?method\\{(%s)\\}\\{(%s)\\})",
            paste(c("[._[:alnum:]]*",
                    ## Subscripting
                    "\\$", "\\[\\[?",
                    ## Binary operators
                    "\\+", "\\-", "\\*", "\\/", "\\^", "<=?", ">=?",
                    "!=", "==", "\\&", "\\|",
                    "\\%[[:alnum:][:punct:]]*\\%"),
                  collapse = "|"),
            "[._[:alnum:]]*")

### * .S4_method_markup_regexp

## For matching \S4method{GENERIC}{SIGLIST}.

.S4_method_markup_regexp <-
    sprintf("(\\\\S4method\\{(%s)\\}\\{(%s)\\})",
            "[._[:alnum:]]*",
            "[._[:alnum:],]*")


### * .check_package_subdirs

.check_package_subdirs <- function(dir, doDelete = FALSE)
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
        d[sapply(file.path(dir, d), function(x) file_test("-f", x))]
    }

    if(!file_test("-d", dir))
        stop(gettextf("directory '%s' does not exist", dir), domain = NA)
    else
        dir <- file_path_as_absolute(dir)

    wrong_things <- list(R = character(0), man = character(0),
                         demo = character(0), `inst/doc` = character(0))

    code_dir <- file.path(dir, "R")
    if(file_test("-d", code_dir)) {
        all_files <- mydir(code_dir)
        ## Under Windows, need a Makefile for methods.
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
        ## Assume here this is run in the C locale, as it is by R CMD
        ## check.
        OK <- grep("^[[:alpha:]][[:alnum:]._-]+$", vignettes)
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
    for(i in which(sapply(x, length) > 0)) {
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
    wrong_things <- character(0)
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
    for(f in list_files_with_type(dir, "code",
                                  OS_subdirs = c("unix", "windows")))
        tryCatch(parse(f),
                 error = function(e)
                 writeLines(c(sprintf("File '%s':", f),
                              sprintf("  %s",
                                      unlist(strsplit(conditionMessage(e),
                                                      "\n"))))))
}

### * .check_package_code_shlib

.check_package_code_shlib <-
function(dir) {
    ## <NOTE>
    ## This is very similar to what happens with checkTnF() etc.
    ## We should really have a more general-purpose tree walker.
    ## </NOTE>

    ## Workhorse function.
    filter <- function(file) {
        matches <- list()
        walker <- function(e) {
            if((length(e) > 1)
               && is.call(e)
               && as.character(e[[1]]) %in% c("library.dynam",
                                              "library.dynam.unload")
               && is.character(e[[2]])
               && (regexpr("\\.(so|sl|dll)$", e[[2]]) > -1)
               )
                matches <<- c(matches, list(e))
            if(is.recursive(e))
                for(i in seq_along(e)) Recall(e[[i]])
        }
        exprs <- parse(file)
        for(i in seq_along(exprs)) walker(exprs[[i]])
        matches
    }

    code_files <-
        list_files_with_type(dir, "code",
                             OS_subdirs = c("unix", "windows"))
    x <- lapply(code_files, filter)
    names(x) <- code_files
    x <- x[sapply(x, length) > 0]

    ## Because we really only need this for calling from R CMD check, we
    ## produce output here in case we found something.
    for(fname in names(x)) {
        writeLines(gettextf("File '%s':", fname))
        xfname <- x[[fname]]
        for(i in seq_along(xfname)) {
            writeLines(strwrap(gettextf("found %s",
                                        paste(deparse(xfname[[i]]),
                                              collapse = "")),
                               indent = 2, exdent = 4))
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
        if(length(package) != 1)
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
        ## we need to check for a bundle here
        ## Need this to handle bundles ...
        contains <- .get_contains_from_package_db(db)
        if(length(contains)) {
            file <- tempfile()
            on.exit(unlink(file))
            if(!file.create(file)) stop("unable to create ", file)
            for(pkg in contains) {
                code_dir <- file.path(dir, pkg, "R")
                if(file_test("-d", code_dir)) {
                    if(!all(.file_append_ensuring_LFs(file,
                                                      list_files_with_type(code_dir,
                                                                           "code"))))
                        stop("unable to write code files")
                }
            }
        } else {
            code_dir <- file.path(dir, "R")
            if(file_test("-d", code_dir)) {
                file <- tempfile()
                on.exit(unlink(file))
                if(!file.create(file)) stop("unable to create ", file)
                if(!all(.file_append_ensuring_LFs(file,
                                                  list_files_with_type(code_dir,
                                                                       "code"))))
                    stop("unable to write code files")
            } else {
                return(invisible())
            }
        }
    }
    pkg_name <- db["Package"]
    depends <- .get_requires_from_package_db(db, "Depends")
    imports <- .get_requires_from_package_db(db, "Imports")
    suggests <- .get_requires_from_package_db(db, "Suggests")
    enhances <- .get_requires_from_package_db(db, "Enhances")

    ## Need this to handle bundles ...
    contains <- .get_contains_from_package_db(db)

    ## it is OK to refer to yourself and non-S4 standard packages
    standard_package_names <-
        .get_standard_package_names()$base %w/o% c("methods", "stats4")
    depends_suggests <- c(depends, suggests, pkg_name, contains,
                          standard_package_names)
    imports <- c(imports, depends_suggests, enhances)
    ## the first argument could be named, or could be a variable name.
    ## we just have a stop list here.
    common_names <- c("pkg", "pkgName", "package", "pos")

    bad_exprs <- character()
    bad_imports <- character()
    uses_methods <- FALSE
    find_bad_exprs <- function(e) {
        if(is.call(e) || is.expression(e)) {
            Call <- deparse(e[[1]])[1]
            if(length(e) >= 2) pkg <- deparse(e[[2]])
            if(Call %in% c("library", "require")) {
                ## Zelig has library()
                if(length(e) >= 2) {
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
                       && !identical(class(e[[2]]), "character"))
                        dunno <- TRUE
                    ## <NOTE>
                    ## </NOTE>
                    ## <FIXME> could be inside substitute or a variable
                    ## and is in e.g. R.oo
                    if(! dunno
                       && ! pkg %in% c(depends_suggests, common_names))
                        bad_exprs <<- c(bad_exprs, pkg)
                }
            } else if(Call %in%  "::") {
                ## <FIXME> fathom out if this package has a namespace
                if(! pkg %in% imports)
                    bad_imports <<- c(bad_imports, pkg)
            } else if(Call %in%  ":::") {
                if(! pkg %in% imports)
                    bad_imports <<- c(bad_imports, pkg)
            } else if(Call %in% c("setClass", "setMethod")) {
                uses_methods <<- TRUE
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
            ## This may find things twice.
            for(f in methods::getGenerics(code_env)) {
                meths <-
                    methods::linearizeMlist(methods::getMethodsMetaData(f, code_env))
                bodies <- lapply(methods::slot(meths, "methods"), body)
                ## Exclude methods inherited from the 'appropriate'
                ## parent environment.
                ## <NOTE>
                ## Keep this in sync with similar code in undoc().
                ## Note that direct comparison of
                ##   lapply(methods::slot(meths, "methods"), environment)
                ## to code_env is not quite right ...
                penv <- .Internal(getRegisteredNamespace(as.name(package)))
                if(is.environment(penv))
                    penv <- parent.env(penv)
                else
                    penv <- parent.env(code_env)
                if((f %in% methods::getGenerics(penv))
                    && !is.null(mlist_from_penv <-
                                methods::getMethodsMetaData(f, penv))) {
                    classes_from_cenv <-
                        methods::slot(meths, "classes")
                    classes_from_penv <-
                        methods::slot(methods::linearizeMlist(mlist_from_penv),
                                      "classes")
                    ind <- is.na(match(.make_signatures(classes_from_cenv),
                                       .make_signatures(classes_from_penv)))
                    bodies <- bodies[ind]
                }
                ## </NOTE>
                exprs <- c(exprs, bodies)
            }
        }
    }
    else {
        exprs <- try(parse(file = file, n = -1))
        if(inherits(exprs, "try-error"))
            stop(gettextf("parse error in file '%s'", file),
                 domain = NA)
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
    if(length(x$imports) > 0) {
        writeLines(gettext("'::' or ':::' imports not declared from:"))
        .pretty_print(x$imports)
    }
    if(length(x$others) > 0) {
        writeLines(gettext("'library' or 'require' calls not declared from:"))
        .pretty_print(x$others)
    }
    if(nchar(x$methods_message))
        writeLines(x$methods_message)
    invisible(x)
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
    bad_closures <- character()
    bad_examples <- character()

    find_bad_closures <- function(env) {
        objects_in_env <- objects(env, all = TRUE)
        x <- lapply(objects_in_env,
                    function(o) {
                        v <- get(o, env = env)
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
                        eval(parse(text =
                                   paste("FOO <- function() {",
                                         paste(txt, collapse = "\n"),
                                         "}",
                                         collapse = "\n")),
                             env)
                        find_bad_closures(env)
                    })
        names(txts)[sapply(x, length) > 0]
    }

    if(!missing(package)) {
        if(length(package) != 1)
            stop("argument 'package' must be of length 1")
        dir <- .find.package(package, lib.loc)
        if((package != "base")
           && !packageHasNamespace(package, dirname(dir))) {
            .load_package_quietly(package, lib.loc)
            code_env <- .package_env(package)
            bad_closures <- find_bad_closures(code_env)
        }
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
            .source_assignments_in_code_dir(code_dir, code_env)
            bad_closures <- find_bad_closures(code_env)
        }
        example_texts <- .get_example_texts_from_source_dir(dir)
    }

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
    sapply(Rd_db(dir = dir),
           function(s) {
               .get_Rd_example_code(paste(Rd_pp(s), collapse = "\n"))
           })
}

print.check_T_and_F <- function(x, ...) {
    if(length(x$bad_closures)) {
        msg <- gettext("Found possibly global T or F in the following functions:")
        writeLines(strwrap(msg))
        .pretty_print(x$bad_closures)
    }
    if(length(x$bad_examples)) {
        msg <- gettext("Found possibly global T or F in the following Rd example files:")
        writeLines(strwrap(msg))
        writeLines(paste(" ", x$bad_examples))
    }
    invisible(x)
}


### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "### [*]+" ***
### End: ***

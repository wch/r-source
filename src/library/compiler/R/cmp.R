#  Automatically generated from ../noweb/compiler.nw.
#
#  File src/library/compiler/R/cmp.R
#  Part of the R package, https://www.R-project.org
#  Copyright (C) 2001-2014 Luke Tierney
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

##
## Compiler options
##

compilerOptions <- new.env(hash = TRUE, parent = emptyenv())
compilerOptions$optimize <- 2
compilerOptions$suppressAll <- TRUE
compilerOptions$suppressNoSuperAssignVar <- FALSE
compilerOptions$suppressUndefined <-
    c(".Generic", ".Method", ".Random.seed", ".self")

getCompilerOption <- function(name, options = NULL) {
    if (name %in% names(options))
        options[[name]]
    else
        get(name, compilerOptions)
}


##
## General Utilities
##

pasteExpr <- function(e, prefix = "\n    ") {
    de <- deparse(e)
    if (length(de) == 1) sQuote(de)
    else paste(prefix, deparse(e), collapse="")
}

dots.or.missing <- function(args) {
    for (i in 1:length(args)) {
        a <-args[[i]]
        if (missing(a)) return(TRUE) #**** better test?
        if (typeof(a) == "symbol" && a == "...") return(TRUE)
    }
    return(FALSE)
}

any.dots <- function(args) {
    for (i in 1:length(args)) {
        a <-args[[i]]
        if (! missing(a) && typeof(a) == "symbol" && a == "...")
            return(TRUE)
    }
    return(FALSE)
}

is.ddsym <- function(name) {
    (is.symbol(name) || is.character(name)) &&
    length(grep("^\\.\\.[0-9]+$", as.character(name))) != 0
}


missingArgs <- function(args) {
    val <- logical(length(args))
    for (i in seq_along(args)) {
        a <- args[[i]]
        if (missing(a))
            val[i] <- TRUE
        else
            val[i] <- FALSE
    }
    val
}


##
## Environment utilities
##

frameTypes <- function(env) {
    top <- topenv(env)
    empty <- emptyenv()
    nl <- 0
    while (! identical(env, top)) {
        if (isNamespace(env))
            stop("namespace found within local environments")
        env <- parent.env(env)
        nl <- nl + 1
        if (identical(env, empty))
            stop("not a proper evaluation environment")
    }
    nn <- 0
    if (isNamespace(env)) {
        while (! identical(env, .GlobalEnv)) {
            if (!isNamespace(env)) {
                name <- attr(env, "name")
                if (!is.character(name) || !startsWith(name, "imports:"))
                    stop("non-namespace found within namespace environments")
            }
            env <- parent.env(env)
            nn <- nn + 1
            if (identical(env, empty))
                stop("not a proper evaluation environment")
        }
    }
    ng <- 0
    while (! identical(env, empty)) {
        if (isNamespace(env))
            stop("namespace found within global environments")
        env <- parent.env(env)
        ng <- ng + 1
    }
    rep(c("local", "namespace", "global"), c(nl, nn, ng))
}

## Given a symbol name and a namespace environment (or a namespace
## imports environment) find the namespace in which the symbol's value
## was originally defined. Returns NULL if the symbol is not found via
## the namespace.
findHomeNS <- function(sym, ns, cntxt) {
    if (! isNamespace(ns)) {
        ## As a convenience this allows for 'ns' to be the imports fame
        ## of a namespace. It appears that these now have a 'name'
        ## attribute of the form 'imports:foo' if 'foo' is the
        ## namespace.
        name <- attr(ns, "name")
        if (is.null(name))
            cntxt$stop("'ns' must be a namespace or a namespace imports environment",
                cntxt)
        ns <- getNamespace(sub("imports:", "", attr(ns, "name")))
    }
    if (exists(sym, ns, inherits = FALSE))
        ns
    else if (exists(".__NAMESPACE__.", ns, inherits = FALSE)) {
        imports <- get(".__NAMESPACE__.", ns)$imports
        for (i in rev(seq_along(imports))) {
            iname <- names(imports)[i]
            ins <- getNamespace(iname)
            if (identical(imports[[i]], TRUE)) {
                if (identical(ins, .BaseNamespaceEnv))
                    exports <- .BaseNamespaceEnv
                else
                    exports <- get(".__NAMESPACE__.", ins)$exports
                if (exists(sym, exports, inherits = FALSE))
                    return(findHomeNS(sym, ins, cntxt))
            }
            else {
                exports <- imports[[i]]
                pos <- match(sym, names(exports), 0)
                if (pos) {
                    ## If renaming has been used things get too
                    ## confusing so return NULL. (It is not clear if
                    ## renaming this is still supported by the
                    ## namespace code.)
                    if (sym == exports[pos])
                        return(findHomeNS(sym, ins, cntxt))
                    else
                        return(NULL)
                }
            }
        }
        NULL
    }
    else NULL
}

packFrameName <- function(frame) {
    fname <- attr(frame, "name")
    if (is.character(fname))
        sub("package:", "", fname)
    else if (identical(frame , baseenv()))
        "base"
    else ""
}

nsName <- function(ns) {
    if (identical(ns, .BaseNamespaceEnv))
        "base"
    else {
        name <- ns$.__NAMESPACE__.$spec["name"]
        if (is.character(name))
            as.character(name) ## strip off names
        else ""
    }
}


##
## Finding possible local variables
##

getAssignedVar <- function(e, cntxt) {
    v <- e[[2]]
    if (missing(v))
        cntxt$stop(gettextf("bad assignment: %s", pasteExpr(e)), cntxt)
    else if (typeof(v) %in% c("symbol", "character"))
        as.character(v)
    else {
        while (typeof(v) == "language") {
            if (length(v) < 2)
                cntxt$stop(gettextf("bad assignment: %s", pasteExpr(e)), cntxt)
            v <- v[[2]]
            if (missing(v))
                cntxt$stop(gettextf("bad assignment: %s", pasteExpr(e)), cntxt)
        }
        if (typeof(v) != "symbol")
            cntxt$stop(gettextf("bad assignment: %s", pasteExpr(e)), cntxt)
        as.character(v)
    }
}

addVar <- function(v, vars) assign(v, 1, envir = vars)
findLocals1 <- function(e, shadowed = character(0), cntxt, vars) {
    if (typeof(e) == "language") {
        if (typeof(e[[1]]) %in% c("symbol", "character")) {
            v <- as.character(e[[1]])
            switch(v,
                   "=" =,
                   "<-" = { addVar(getAssignedVar(e, cntxt), vars); e[-1] },

                   "for" = { addVar(as.character(e[2]), vars); e[-2] },

                   "delayedAssign" =,
                   "assign" = if (length(e) == 3 &&
                                  is.character(e[[2]]) &&
                                  length(e[[2]]) == 1) {

                                  addVar(e[[2]], vars); list(e[[3]])
                              }
                              else e[1],
                   "function" = character(0),
                   "~" = character(0),
                   "local" = if (! v %in% shadowed && length(e) == 2)
                                 NULL
                             else e[-1],
                   "expression" =,
                   "quote" = if (! v %in% shadowed)
                                 NULL
                             else e[-1],
                   e[-1])
        }
        else e
    }
    else NULL
}

findLocalsList1 <- function(elist, shadowed, cntxt) {
    todo <- elist
    vars <- new.env()
    while(length(todo) > 0) {
        newtodo <- list()
        lapply(todo, function(e)
            lapply(findLocals1(e, shadowed, cntxt, vars),
                   function(x)
                       if (typeof(x) == "language")
                           newtodo <<- append(newtodo, x))
        )
        todo <- newtodo
    }
    ls(vars, all.names=T)
}


findLocals <- function(e, cntxt)
    findLocalsList(list(e), cntxt)

findLocalsList <- function(elist, cntxt) {
    initialShadowedFuns <- c("expression", "local", "quote")
    shadowed <- Filter(function(n) ! isBaseVar(n, cntxt), initialShadowedFuns)
    specialSyntaxFuns <- c("~", "<-", "=", "for", "function")
    sf <- initialShadowedFuns
    nsf <- length(sf)
    repeat {
        vals <- findLocalsList1(elist, sf, cntxt)
        redefined <- sf %in% vals
        last.nsf <- nsf
        sf <- unique(c(shadowed, sf[redefined]))
        nsf <- length(sf)
        ## **** need to fix the termination condition used in codetools!!!
        if (last.nsf == nsf) {
            rdsf <- vals %in% specialSyntaxFuns
            if (any(rdsf))
                ## cannot get location info (source reference) here
                notifyAssignSyntacticFun(vals[rdsf], cntxt)
            return(vals)
        }
    }
}


##
## Compilation environment implementation
##

## Create a new compiler environment
## **** need to explain the structure
makeCenv <- function(env) {
    structure(list(extra = list(character(0)),
                   env = env,
                   ftypes = frameTypes(env)),
              class = "compiler_environment")
}

## Add vars to the top compiler environment frame
addCenvVars <- function(cenv, vars) {
    cenv$extra[[1]] <- union(cenv$extra[[1]], vars)
    cenv
}

## Add a new frame to a compiler environment
addCenvFrame <- function(cenv, vars) {
    cenv$extra <- c(list(character(0)), cenv$extra)
    cenv$env <- new.env(parent = cenv$env)
    cenv$ftypes <- c("local", cenv$ftypes)
    if (missing(vars))
        cenv
    else
        addCenvVars(cenv, vars)
}

## Find binding information for a variable (character or name).
## If a binding is found, return a list containing components
##   ftype -- one of "local", "namespace", "global"
##   value -- current value if available
##   frame -- frame containing the binding (not useful for "local" variables)
##   index -- index of the frame (1 for top, 2, for next one, etc.)
## Return NULL if no binding is found.
## **** drop the index, maybe value, to reduce cost? (query as needed?)
findCenvVar <- function(var, cenv) {
    if (typeof(var) == "symbol")
        var <- as.character(var)
    extra <- cenv$extra
    env <- cenv$env
    frame <- NULL
    for (i in seq_along(cenv$extra)) {
        if (var %in% extra[[i]] || exists(var, env, inherits = FALSE)) {
            frame <- env
            break
        }
        else
            env <- parent.env(env)
    }
    if (is.null(frame)) {
        empty <- emptyenv()
        while (! identical(env, empty)) {
            i <- i + 1
            if (exists(var, env, inherits = FALSE)) {
                frame <- env
                break
            }
            else
                env <- parent.env(env)
        }
    }
    if (! is.null(frame)) {
        if (exists(var, frame, inherits = FALSE) && var != "...") {
            value <- new.env(parent = emptyenv())
            delayedAssign("value", get(var, frame, inherits = FALSE),
                          assign.env = value)
        }
        else
            value <- NULL
        list(frame = frame, ftype = cenv$ftypes[i], value = value, index = i)
    }
    else
        NULL
}

isBaseVar <- function(var, cntxt) {
    info <- getInlineInfo(var, cntxt)
    (! is.null(info) &&
     (identical(info$frame, .BaseNamespaceEnv) ||
      identical(info$frame, baseenv())))
}

## augment compiler environment with function args and locals
funEnv <- function(forms, body, cntxt) {
    cntxt$env <- addCenvFrame(cntxt$env, names(forms))
    locals <- findLocalsList(c(forms, body), cntxt)
    addCenvVars(cntxt$env, locals)
}

## test whether a local version of a variable might exist
findLocVar <- function(var, cntxt) {
    cenv <- cntxt$env
    info <- findCenvVar(var, cenv)
    ! is.null(info) && info$ftype == "local"
}

## **** should this check for local functions as well?
findFunDef <- function(fun, cntxt) {
    cenv <- cntxt$env
    info <- findCenvVar(fun, cenv)
    if (! is.null(info$value) && is.function(info$value$value))
        info$value$value
    else
        NULL
}

findVar <- function(var, cntxt) {
    cenv <- cntxt$env
    info <- findCenvVar(var, cenv)
    ! is.null(info)
}


##
## Constant folding
##

maxConstSize <- 10

constModes <- c("numeric", "logical", "NULL", "complex", "character")

constNames <- c("pi", "T", "F")

checkConst <- function(e) {
    if (mode(e) %in% constModes && length(e) <= maxConstSize)
        list(value = e)
    else
        NULL
}

## Assumes all constants will be defined in base.
## Eventually allow other packages to define constants.
## Any variable with locked binding could be used if type is right.
## Allow local declaration of optimize, notinline declaration.
constantFoldSym <- function(var, cntxt) {
    var <- as.character(var)
    if (var %in% constNames && isBaseVar(var, cntxt))
        checkConst(get(var, .BaseNamespaceEnv))
    else NULL
}

## For now assume all foldable functions are in base
getFoldFun <- function(var, cntxt) {
    var <- as.character(var)
    if (var %in% foldFuns && isBaseVar(var, cntxt)) {
        val <- get(var, .BaseNamespaceEnv)
        if (is.function(val))
            val
        else
            NULL
    }
    else NULL
}

constantFoldCall <- function(e, cntxt) {
    fun <- e[[1]]
    if (typeof(fun) == "symbol") {
        ffun <- getFoldFun(fun, cntxt)
        if (! is.null(ffun)) {
            args <- as.list(e[-1])
            for (i in seq_along(args)) {
                a <- args[[i]]
                if (missing(a))
                    return(NULL)
                val <- constantFold(a, cntxt)
                if (! is.null(val))
                    args[i] <- list(val$value) ## **** in case value is NULL
                else return(NULL)
            }
            modes <- unlist(lapply(args, mode))
            if (all(modes %in% constModes)) {
                tryCatch(checkConst(do.call(ffun, args)),
                         error = function(e) NULL, warning = function(w) NULL)
                ## **** issue warning??
            }
            else NULL
        }
        else NULL
    }
    else NULL
}

## **** rewrite using switch??
constantFold <- function(e, cntxt, loc = NULL) {
    type = typeof(e)
    if (type == "language")
        constantFoldCall(e, cntxt)
    else if (type == "symbol")
        constantFoldSym(e, cntxt)
    else if (type == "promise")
        cntxt$stop(gettext("cannot constant fold literal promises"),
                   cntxt, loc = loc)
    else if (type == "bytecode")
        cntxt$stop(gettext("cannot constant fold literal bytecode objects"),
                   cntxt, loc = loc)
    else checkConst(e)
}

foldFuns <- c("+", "-", "*", "/", "^", "(",
              ">", ">=", "==", "!=", "<", "<=", "||", "&&", "!",
              "|", "&", "%%",
              "c", "rep", ":",
              "abs", "acos", "acosh", "asin", "asinh", "atan", "atan2",
              "atanh", "ceiling", "choose", "cos", "cosh", "exp", "expm1",
              "floor", "gamma", "lbeta", "lchoose", "lgamma", "log", "log10",
              "log1p", "log2", "max", "min", "prod", "range", "round",
              "seq_along", "seq.int", "seq_len", "sign", "signif",
              "sin", "sinh", "sqrt", "sum", "tan", "tanh", "trunc",
              "baseenv", "emptyenv", "globalenv",
              "Arg", "Conj", "Im", "Mod", "Re",
              "is.R")

languageFuns <- c("^", "~", "<", "<<-", "<=", "<-", "=", "==", ">", ">=",
                  "|", "||", "-", ":", "!", "!=", "/", "(", "[", "[<-", "[[",
                  "[[<-", "{", "@", "$", "$<-", "*", "&", "&&", "%/%", "%*%",
                  "%%", "+",
                  "::", ":::", "@<-",
                  "break", "for", "function", "if", "next", "repeat", "while",
                  "local", "return", "switch")


##
## Opcode constants
##

Opcodes.argc <- list(
BCMISMATCH.OP = 0,
RETURN.OP = 0,
GOTO.OP = 1,
BRIFNOT.OP = 2,
POP.OP = 0,
DUP.OP = 0,
PRINTVALUE.OP = 0,
STARTLOOPCNTXT.OP = 2,
ENDLOOPCNTXT.OP = 1,
DOLOOPNEXT.OP = 0,
DOLOOPBREAK.OP = 0,
STARTFOR.OP = 3,
STEPFOR.OP = 1,
ENDFOR.OP = 0,
SETLOOPVAL.OP = 0,
INVISIBLE.OP = 0,
LDCONST.OP = 1,
LDNULL.OP = 0,
LDTRUE.OP = 0,
LDFALSE.OP = 0,
GETVAR.OP = 1,
DDVAL.OP = 1,
SETVAR.OP = 1,
GETFUN.OP = 1,
GETGLOBFUN.OP = 1,
GETSYMFUN.OP = 1,
GETBUILTIN.OP = 1,
GETINTLBUILTIN.OP = 1,
CHECKFUN.OP = 0,
MAKEPROM.OP = 1,
DOMISSING.OP = 0,
SETTAG.OP = 1,
DODOTS.OP = 0,
PUSHARG.OP = 0,
PUSHCONSTARG.OP = 1,
PUSHNULLARG.OP = 0,
PUSHTRUEARG.OP = 0,
PUSHFALSEARG.OP = 0,
CALL.OP = 1,
CALLBUILTIN.OP = 1,
CALLSPECIAL.OP = 1,
MAKECLOSURE.OP = 1,
UMINUS.OP = 1,
UPLUS.OP = 1,
ADD.OP = 1,
SUB.OP = 1,
MUL.OP = 1,
DIV.OP = 1,
EXPT.OP = 1,
SQRT.OP = 1,
EXP.OP = 1,
EQ.OP = 1,
NE.OP = 1,
LT.OP = 1,
LE.OP = 1,
GE.OP = 1,
GT.OP = 1,
AND.OP = 1,
OR.OP = 1,
NOT.OP = 1,
DOTSERR.OP = 0,
STARTASSIGN.OP = 1,
ENDASSIGN.OP = 1,
STARTSUBSET.OP = 2,
DFLTSUBSET.OP = 0,
STARTSUBASSIGN.OP = 2,
DFLTSUBASSIGN.OP = 0,
STARTC.OP = 2,
DFLTC.OP = 0,
STARTSUBSET2.OP = 2,
DFLTSUBSET2.OP = 0,
STARTSUBASSIGN2.OP = 2,
DFLTSUBASSIGN2.OP = 0,
DOLLAR.OP = 2,
DOLLARGETS.OP = 2,
ISNULL.OP = 0,
ISLOGICAL.OP = 0,
ISINTEGER.OP = 0,
ISDOUBLE.OP = 0,
ISCOMPLEX.OP = 0,
ISCHARACTER.OP = 0,
ISSYMBOL.OP = 0,
ISOBJECT.OP = 0,
ISNUMERIC.OP = 0,
VECSUBSET.OP = 1,
MATSUBSET.OP = 1,
VECSUBASSIGN.OP = 1,
MATSUBASSIGN.OP = 1,
AND1ST.OP = 2,
AND2ND.OP = 1,
OR1ST.OP = 2,
OR2ND.OP = 1,
GETVAR_MISSOK.OP = 1,
DDVAL_MISSOK.OP = 1,
VISIBLE.OP = 0,
SETVAR2.OP = 1,
STARTASSIGN2.OP = 1,
ENDASSIGN2.OP = 1,
SETTER_CALL.OP = 2,
GETTER_CALL.OP = 1,
SWAP.OP = 0,
DUP2ND.OP = 0,
SWITCH.OP = 4,
RETURNJMP.OP = 0,
STARTSUBSET_N.OP = 2,
STARTSUBASSIGN_N.OP = 2,
VECSUBSET2.OP = 1,
MATSUBSET2.OP = 1,
VECSUBASSIGN2.OP = 1,
MATSUBASSIGN2.OP = 1,
STARTSUBSET2_N.OP = 2,
STARTSUBASSIGN2_N.OP = 2,
SUBSET_N.OP = 2,
SUBSET2_N.OP = 2,
SUBASSIGN_N.OP = 2,
SUBASSIGN2_N.OP = 2,
LOG.OP = 1,
LOGBASE.OP = 1,
MATH1.OP = 2,
DOTCALL.OP = 2,
COLON.OP = 1,
SEQALONG.OP = 1,
SEQLEN.OP = 1,
BASEGUARD.OP = 2,
INCLNK.OP = 0,
DECLNK.OP = 0,
DECLNK_N.OP = 1
)

Opcodes.names <- names(Opcodes.argc)

BCMISMATCH.OP <- 0
RETURN.OP <- 1
GOTO.OP <- 2
BRIFNOT.OP <- 3
POP.OP <- 4
DUP.OP <- 5
PRINTVALUE.OP <- 6
STARTLOOPCNTXT.OP <- 7
ENDLOOPCNTXT.OP <- 8
DOLOOPNEXT.OP <- 9
DOLOOPBREAK.OP <- 10
STARTFOR.OP <- 11
STEPFOR.OP <- 12
ENDFOR.OP <- 13
SETLOOPVAL.OP <- 14
INVISIBLE.OP <- 15
LDCONST.OP <- 16
LDNULL.OP <- 17
LDTRUE.OP <- 18
LDFALSE.OP <- 19
GETVAR.OP <- 20
DDVAL.OP <- 21
SETVAR.OP <- 22
GETFUN.OP <- 23
GETGLOBFUN.OP <- 24
GETSYMFUN.OP <- 25
GETBUILTIN.OP <- 26
GETINTLBUILTIN.OP <- 27
CHECKFUN.OP <- 28
MAKEPROM.OP <- 29
DOMISSING.OP <- 30
SETTAG.OP <- 31
DODOTS.OP <- 32
PUSHARG.OP <- 33
PUSHCONSTARG.OP <- 34
PUSHNULLARG.OP <- 35
PUSHTRUEARG.OP <- 36
PUSHFALSEARG.OP <- 37
CALL.OP <- 38
CALLBUILTIN.OP <- 39
CALLSPECIAL.OP <- 40
MAKECLOSURE.OP <- 41
UMINUS.OP <- 42
UPLUS.OP <- 43
ADD.OP <- 44
SUB.OP <- 45
MUL.OP <- 46
DIV.OP <- 47
EXPT.OP <- 48
SQRT.OP <- 49
EXP.OP <- 50
EQ.OP <- 51
NE.OP <- 52
LT.OP <- 53
LE.OP <- 54
GE.OP <- 55
GT.OP <- 56
AND.OP <- 57
OR.OP <- 58
NOT.OP <- 59
DOTSERR.OP <- 60
STARTASSIGN.OP <- 61
ENDASSIGN.OP <- 62
STARTSUBSET.OP <- 63
DFLTSUBSET.OP <- 64
STARTSUBASSIGN.OP <- 65
DFLTSUBASSIGN.OP <- 66
STARTC.OP <- 67
DFLTC.OP <- 68
STARTSUBSET2.OP <- 69
DFLTSUBSET2.OP <- 70
STARTSUBASSIGN2.OP <- 71
DFLTSUBASSIGN2.OP <- 72
DOLLAR.OP <- 73
DOLLARGETS.OP <- 74
ISNULL.OP <- 75
ISLOGICAL.OP <- 76
ISINTEGER.OP <- 77
ISDOUBLE.OP <- 78
ISCOMPLEX.OP <- 79
ISCHARACTER.OP <- 80
ISSYMBOL.OP <- 81
ISOBJECT.OP <- 82
ISNUMERIC.OP <- 83
VECSUBSET.OP <- 84
MATSUBSET.OP <- 85
VECSUBASSIGN.OP <- 86
MATSUBASSIGN.OP <- 87
AND1ST.OP <- 88
AND2ND.OP <- 89
OR1ST.OP <- 90
OR2ND.OP <- 91
GETVAR_MISSOK.OP <- 92
DDVAL_MISSOK.OP <- 93
VISIBLE.OP <- 94
SETVAR2.OP <- 95
STARTASSIGN2.OP <- 96
ENDASSIGN2.OP <- 97
SETTER_CALL.OP <- 98
GETTER_CALL.OP <- 99
SWAP.OP <- 100
DUP2ND.OP <- 101
SWITCH.OP <- 102
RETURNJMP.OP <- 103
STARTSUBSET_N.OP <- 104
STARTSUBASSIGN_N.OP <- 105
VECSUBSET2.OP <- 106
MATSUBSET2.OP <- 107
VECSUBASSIGN2.OP <- 108
MATSUBASSIGN2.OP <- 109
STARTSUBSET2_N.OP <- 110
STARTSUBASSIGN2_N.OP <- 111
SUBSET_N.OP <- 112
SUBSET2_N.OP <- 113
SUBASSIGN_N.OP <- 114
SUBASSIGN2_N.OP <-115
LOG.OP <- 116
LOGBASE.OP <- 117
MATH1.OP <- 118
DOTCALL.OP <- 119
COLON.OP <- 120
SEQALONG.OP <- 121
SEQLEN.OP <- 122
BASEGUARD.OP <- 123
INCLNK.OP <- 124
DECLNK.OP <- 125
DECLNK_N.OP <- 126


##
## Code buffer implementation
##

extractSrcref <- function(sref, idx) {
    if (is.list(sref) && length(sref) >= idx)
        sref[[idx]]
    else if (is.integer(sref) && length(sref) >= 6)
        sref
    else
        NULL
}
getExprSrcref <- function(expr) {
    sattr <- attr(expr, "srcref")
    extractSrcref(sattr, 1)
}
# if block is a block srcref, get its idx'th entry
# if block is a single srcref, return this srcref
getBlockSrcref <- function(block, idx) {
  extractSrcref(block, idx)
}
addLocString <- function(msg, loc) {
    if (is.null(loc$srcref))
        msg
    else
        paste0(msg, " at ", utils::getSrcFilename(loc$srcref), ":",
               utils::getSrcLocation(loc$srcref, "line"))
}

make.codeBuf <- function(expr, loc = NULL) {
    exprTrackingOn <- TRUE
    srcrefTrackingOn <- TRUE

    if (is.null(loc)) {
        curExpr <- expr
        curSrcref <- getExprSrcref(expr)
    } else {
        curExpr <- loc$expr
        curSrcref <- loc$srcref
    }

    if (is.null(curSrcref))
        ## when top-level srcref is null, we speculate there will be no
        ##   source references within the compiled expressions either,
        ##   disabling the tracking makes the resulting constant pool
        ##   smaller
        srcrefTrackingOn <- FALSE

    exprBuf <- NA   ## exprBuf will have the same length as codeBuf
    srcrefBuf <- NA ## srcrefBuf will have the same length as codeBuf

    if (!exprTrackingOn) {
        curExpr <- NULL
        exprBuf <- NULL
    }
    if (!srcrefTrackingOn) {
        curSrcref <- NULL
        srcrefBuf <- NULL
    }

    ## set the current expression
    ## also update the srcref according to expr, if expr has srcref attribute
    ##   (note: never clears current srcref)
    setcurexpr <- function(expr) {
        if (exprTrackingOn) {
            curExpr <<- expr
        }
        if (srcrefTrackingOn) {
            sref <- getExprSrcref(expr)
            if (!is.null(sref) && srcrefTrackingOn)
                curSrcref <<- sref
         }
    }
    ## unconditionally sets the current expression and srcrefs
    setcurloc <- function(expr, sref) {
        if (exprTrackingOn)
            curExpr <<- expr
        if (srcrefTrackingOn)
            curSrcref <<- sref
    }
    ## add location information (current expressions, srcrefs) to the constant pool
    commitlocs <- function() {
        if (exprTrackingOn) {
          exprs <- exprBuf[1:codeCount]
          class(exprs) <- "expressionsIndex"
          putconst(exprs)
        }

        if (srcrefTrackingOn) {
          srefs <- srcrefBuf[1:codeCount]
          class(srefs) <- "srcrefsIndex"
          putconst(srefs)
        }

        ## these entries will be at the end of the constant pool, assuming only the compiler
        ## uses these two classes
        NULL
    }
    savecurloc <- function() {
        list(expr = curExpr, srcref = curSrcref)
    }
    restorecurloc <- function(saved) {
        if (exprTrackingOn) curExpr <<- saved$expr
        if (srcrefTrackingOn) curSrcref <<- saved$srcref
    }
    codeBuf <- list(.Internal(bcVersion()))
    codeCount <- 1
    putcode <- function(...) {
        new <- list(...)
        newLen <- length(new)
        while (codeCount + newLen > length(codeBuf)) {
            codeBuf <<- c(codeBuf, vector("list", length(codeBuf)))
            if (exprTrackingOn)
                exprBuf <<- c(exprBuf, vector("integer", length(exprBuf)))
            if (srcrefTrackingOn)
                srcrefBuf <<- c(srcrefBuf, vector("integer", length(srcrefBuf)))
        }
        codeRange <- (codeCount + 1) : (codeCount + newLen)
        codeBuf[codeRange] <<- new

        if (exprTrackingOn) {   ## put current expression into the constant pool
            ei <- putconst(curExpr)
            exprBuf[codeRange] <<- ei
        }
        if (srcrefTrackingOn) { ## put current srcref into the constant pool
            si <- putconst(curSrcref)
            srcrefBuf[codeRange] <<- si
        }

        codeCount <<- codeCount + newLen
    }
    getcode <- function() as.integer(codeBuf[1 : codeCount])
    constBuf <- vector("list", 1)
    constCount <- 0
    putconst <- function(x) {
        if (constCount == length(constBuf))
            constBuf <<- .Internal(growconst(constBuf))
        i <- .Internal(putconst(constBuf, constCount, x))
        if (i == constCount)
            constCount <<- constCount + 1
        i
    }
    getconst <- function()
        .Internal(getconst(constBuf, constCount))
    idx <- 0
    labels <- vector("list")
    makelabel <- function() { idx <<- idx + 1; paste0("L", idx) }
    putlabel <- function(name) labels[[name]] <<- codeCount
    patchlabels <- function(cntxt) {
        offset <- function(lbl) {
            if (is.null(labels[[lbl]]))
                cntxt$stop(gettextf("no offset recorded for label \"%s\"", lbl),
                           cntxt)
            labels[[lbl]]
        }
        for (i in 1 : codeCount) {
            v <- codeBuf[[i]]
            if (is.character(v))
                codeBuf[[i]] <<- offset(v)
            else if (typeof(v) == "list") {
                off <- as.integer(lapply(v, offset))
                ci <- putconst(off)
                codeBuf[[i]] <<- ci
            }
        }
    }
    cb <- list(code = getcode,
               const = getconst,
               putcode = putcode,
               putconst = putconst,
               makelabel = makelabel,
               putlabel = putlabel,
               patchlabels = patchlabels,
               setcurexpr = setcurexpr,
               setcurloc = setcurloc,
               commitlocs = commitlocs,
               savecurloc = savecurloc,
               restorecurloc = restorecurloc)
    cb$putconst(expr) ## insert expression as first constant.
      ## NOTE: this will also insert the srcref directly into the constant
      ## pool
    cb
}

codeBufCode <- function(cb, cntxt) {
    cb$patchlabels(cntxt)
    cb$commitlocs()
    .Internal(mkCode(cb$code(), cb$const()))
}

genCode <- function(e, cntxt, gen = NULL, loc = NULL) {
    cb <- make.codeBuf(e, loc)
    if (is.null(gen))
        cmp(e, cb, cntxt, setloc = FALSE)
    else
        gen(cb, cntxt)
    codeBufCode(cb, cntxt)
}


##
## Compiler contexts
##

make.toplevelContext <- function(cenv, options = NULL)
    structure(list(toplevel = TRUE,
                   tailcall = TRUE,
                   needRETURNJMP = FALSE,
                   env = cenv,
                   optimize = getCompilerOption("optimize", options),
                   suppressAll = getCompilerOption("suppressAll", options),
                   suppressNoSuperAssignVar =
                       getCompilerOption("suppressNoSuperAssignVar", options),
                   suppressUndefined = getCompilerOption("suppressUndefined",
                                                         options),
                   call = NULL,
                   stop = function(msg, cntxt, loc = NULL)
                       stop(simpleError(addLocString(msg, loc), cntxt$call)),
                   warn = function(x, cntxt, loc = NULL)
                       cat(paste("Note:", addLocString(x, loc), "\n"))
              ),
              class = "compiler_context")

make.callContext <- function(cntxt, call) {
    cntxt$call <- call
    cntxt
}

make.promiseContext <- function(cntxt) {
    cntxt$toplevel <- FALSE
    cntxt$tailcall <- TRUE
    cntxt$needRETURNJMP <- TRUE
    if (! is.null(cntxt$loop))
        cntxt$loop$gotoOK <- FALSE
    cntxt
}

make.functionContext <- function(cntxt, forms, body) {
    nenv <- funEnv(forms, body, cntxt)
    ncntxt <- make.toplevelContext(nenv)
    ncntxt$optimize <- cntxt$optimize
    ncntxt$suppressAll <- cntxt$suppressAll
    ncntxt$suppressNoSuperAssignVar <- cntxt$suppressNoSuperAssignVar
    ncntxt$suppressUndefined <- cntxt$suppressUndefined
    ncntxt
}

make.nonTailCallContext <- function(cntxt) {
    cntxt$tailcall <- FALSE
    cntxt
}

make.argContext <- function(cntxt) {
    cntxt$toplevel <- FALSE
    cntxt$tailcall <- FALSE
    if (! is.null(cntxt$loop))
        cntxt$loop$gotoOK <- FALSE
    cntxt
}

make.noValueContext <- function(cntxt) {
    cntxt$tailcall <- FALSE
    cntxt
}

make.loopContext <- function(cntxt, loop.label, end.label) {
    ncntxt <- make.noValueContext(cntxt)
    ncntxt$loop <- list(loop = loop.label, end = end.label, gotoOK = TRUE)
    ncntxt
}


##
## Compiler top level
##

cmp <- function(e, cb, cntxt, missingOK = FALSE, setloc = TRUE) {
    if (setloc) {
        sloc <- cb$savecurloc()
        cb$setcurexpr(e)
    }
    ce <- constantFold(e, cntxt, loc = cb$savecurloc())
    if (is.null(ce)) {
        if (typeof(e) == "language")
            cmpCall(e, cb, cntxt)
        else if (typeof(e) == "symbol")
            cmpSym(e, cb, cntxt, missingOK)
        else if (typeof(e) == "bytecode")
            cntxt$stop(gettext("cannot compile byte code literals in code"),
                       cntxt, loc = cb$savecurloc())
        else if (typeof(e) == "promise")
            cntxt$stop(gettext("cannot compile promise literals in code"),
                       cntxt, loc = cb$savecurloc())
        else
            cmpConst(e, cb, cntxt)
    }
    else
        cmpConst(ce$value, cb, cntxt)
    if (setloc)
        cb$restorecurloc(sloc)
}

cmpConst <- function(val, cb, cntxt) {
    if (identical(val, NULL))
        cb$putcode(LDNULL.OP)
    else if (identical(val, TRUE))
        cb$putcode(LDTRUE.OP)
    else if (identical(val, FALSE))
        cb$putcode(LDFALSE.OP)
    else {
        ci <- cb$putconst(val)
        cb$putcode(LDCONST.OP, ci)
    }
    if (cntxt$tailcall) cb$putcode(RETURN.OP)
}

cmpSym <- function(sym, cb, cntxt, missingOK = FALSE) {
    if (sym == "...") {
        notifyWrongDotsUse("...", cntxt, loc = cb$savecurloc())
        cb$putcode(DOTSERR.OP)
    }
    else if (is.ddsym(sym)) {
        if (! findLocVar("...", cntxt))
            notifyWrongDotsUse(sym, cntxt, loc = cb$savecurloc())
        ci <- cb$putconst(sym)
        if (missingOK)
            cb$putcode(DDVAL_MISSOK.OP, ci)
        else
            cb$putcode(DDVAL.OP, ci)
        if (cntxt$tailcall) cb$putcode(RETURN.OP)
    }
    else {
        if (! findVar(sym, cntxt))
            notifyUndefVar(sym, cntxt, loc = cb$savecurloc())
        ci <- cb$putconst(sym)
        if (missingOK)
            cb$putcode(GETVAR_MISSOK.OP, ci)
        else
            cb$putcode(GETVAR.OP, ci)
        if (cntxt$tailcall) cb$putcode(RETURN.OP)
    }
}

cmpCall <- function(call, cb, cntxt, inlineOK = TRUE) {
    sloc <- cb$savecurloc()
    cb$setcurexpr(call)
    cntxt <- make.callContext(cntxt, call)
    fun <- call[[1]]
    args <- call[-1]
    if (typeof(fun) == "symbol") {
        if (! (inlineOK && tryInline(call, cb, cntxt))) {
            if (findLocVar(fun, cntxt))
                notifyLocalFun(fun, cntxt, loc = cb$savecurloc())
            else {
                def <- findFunDef(fun, cntxt)
                if (is.null(def))
                    notifyUndefFun(fun, cntxt, loc = cb$savecurloc())
                else
                    checkCall(def, call,
                              function(w) notifyBadCall(w, cntxt, loc = cb$savecurloc()))
            }
            cmpCallSymFun(fun, args, call, cb, cntxt)
        }
    }
    else {
        ## **** this hack is needed for now because of the way the
        ## **** parser handles break() and next() calls
        if (typeof(fun) == "language" && typeof(fun[[1]]) == "symbol" &&
            as.character(fun[[1]]) %in% c("break", "next"))
            return(cmp(fun, cb, cntxt))
        cmpCallExprFun(fun, args, call, cb, cntxt)
    }
    cb$restorecurloc(sloc)
}

maybeNSESymbols <- c("bquote")
cmpCallSymFun <- function(fun, args, call, cb, cntxt) {
    ci <- cb$putconst(fun)
    cb$putcode(GETFUN.OP, ci)
    nse <- as.character(fun) %in% maybeNSESymbols
    cmpCallArgs(args, cb, cntxt, nse)
    ci <- cb$putconst(call)
    cb$putcode(CALL.OP, ci)
    if (cntxt$tailcall) cb$putcode(RETURN.OP)
}

cmpCallExprFun <- function(fun, args, call, cb, cntxt) {
    ncntxt <- make.nonTailCallContext(cntxt)
    cmp(fun, cb, ncntxt)
    cb$putcode(CHECKFUN.OP)
    nse <- FALSE
    cmpCallArgs(args, cb, cntxt, nse)
    ci <- cb$putconst(call)
    cb$putcode(CALL.OP, ci)
    if (cntxt$tailcall) cb$putcode(RETURN.OP)
}

cmpCallArgs <- function(args, cb, cntxt, nse = FALSE) {
    names <- names(args)
    pcntxt <- make.promiseContext(cntxt)
    for (i in seq_along(args)) {
        a <- args[[i]]
        n <- names[[i]]
        if (missing(a)) { ## better test for missing??
            cb$putcode(DOMISSING.OP)
            cmpTag(n, cb)
        }
        else if (is.symbol(a) && a == "...") {
            if (! findLocVar("...", cntxt))
                notifyWrongDotsUse("...", cntxt, loc = cb$savecurloc())
            cb$putcode(DODOTS.OP)
        }
        else if (typeof(a) == "bytecode")
            cntxt$stop(gettext("cannot compile byte code literals in code"),
                       cntxt, loc = cb$savecurloc())
        else if (typeof(a) == "promise")
            cntxt$stop(gettext("cannot compile promise literals in code"),
                       cntxt, loc = cb$savecurloc())
        else {
            if (is.symbol(a) || typeof(a) == "language") {
                if (nse)
                      ci <- cb$putconst(a)
                else
                      ci <- cb$putconst(genCode(a, pcntxt, loc = cb$savecurloc()))
                cb$putcode(MAKEPROM.OP, ci)
            }
            else
                cmpConstArg(a, cb, cntxt)
            cmpTag(n, cb)
        }
    }
}

cmpConstArg <- function(a, cb, cntxt) {
    if (identical(a, NULL))
        cb$putcode(PUSHNULLARG.OP)
    else if (identical(a, TRUE))
        cb$putcode(PUSHTRUEARG.OP)
    else if (identical(a, FALSE))
        cb$putcode(PUSHFALSEARG.OP)
    else {
        ci <- cb$putconst(a)
        cb$putcode(PUSHCONSTARG.OP, ci)
    }
}

## **** figure out how to handle multi-line deparses
## ****     e.g. checkCall(`{`, quote({}))
## **** better design would capture error object, wrap it up, and pass it on
## **** use approach from codetools to capture partial argument match
## ****     warnings if enabled?
checkCall <- function(def, call, signal = warning) {
    if (typeof(def) %in% c("builtin", "special"))
        def <- args(def)
    if (typeof(def) != "closure" || any.dots(call))
        NA
    else {
        msg <- tryCatch({match.call(def, call); NULL},
                        error = function(e) conditionMessage(e))
        if (! is.null(msg)) {
            emsg <- gettextf("possible error in '%s': %s",
                             deparse(call, 20)[1], msg)
            if (! is.null(signal)) signal(emsg)
            FALSE
        }
        else TRUE
    }
}

## **** need to handle ... and ..n arguments specially
## **** separate call opcode for calls with named args?
## **** for (a in e[[-1]]) ... goes into infinite loop

cmpTag <- function(n, cb) {
    if (! is.null(n) && n != "") {
        ci <- cb$putconst(as.name(n))
        cb$putcode(SETTAG.OP, ci)
    }
}

mayCallBrowser <- function(e, cntxt) {
    if (typeof(e) == "language") {
        fun <- e[[1]]
        if (typeof(fun) == "symbol") {
            fname <- as.character(fun)
            if (fname == "browser") ## not checking isBaseVar to err on the
                                    ## positive
                TRUE
            else if (fname == "function" && isBaseVar(fname, cntxt))
                FALSE
            else
                mayCallBrowserList(e[-1], cntxt)
        }
        else
            mayCallBrowserList(e, cntxt)
    }
    else FALSE
}

mayCallBrowserList <- function(elist, cntxt) {
    for (a in as.list(elist))
        if (! missing(a) && mayCallBrowser(a, cntxt))
            return(TRUE)
    FALSE
}

##
## Inlining mechanism
##

inlineHandlers <- new.env(hash = TRUE, parent = emptyenv())

setInlineHandler <- function(name, h, package = "base") {
    if (exists(name, inlineHandlers, inherits = FALSE)) {
        entry <- get(name, inlineHandlers)
        if (entry$package != package) {
            fmt <- "handler for '%s' is already defined for another package"
            stop(gettextf(fmt, name), domain = NA)
        }
    }
    entry <- list(handler = h, package = package)
    assign(name, entry, inlineHandlers)
}

getInlineHandler <- function(name, package = "base") {
    if (exists(name, inlineHandlers, inherits = FALSE)) {
        hinfo <- get(name, inlineHandlers)
        if (hinfo$package == package)
            hinfo$handler
        else NULL
    }
    else NULL
}

haveInlineHandler <- function(name, package = "base") {
    if (exists(name, inlineHandlers, inherits = FALSE)) {
        hinfo <- get(name, inlineHandlers)
        package == hinfo$package
    }
    else FALSE
}

## tryInline implements the rule permitting inlining as they stand now:
## Inlining is controlled by the optimize compiler option, with possible
## values 0, 1, 2, 3.

noInlineSymbols <- c("standardGeneric")

getInlineInfo <- function(name, cntxt, guardOK = FALSE) {
    optimize <- cntxt$optimize
    if (optimize > 0 && ! (name %in% noInlineSymbols)) {
        info <- findCenvVar(name, cntxt$env)
        if (is.null(info))
            NULL
        else {
            ftype <- info$ftype
            frame <- info$frame
            if (ftype == "namespace") {
                if (! isNamespace(frame)) {
                    ## should be the import frame of the current topenv
                    top <- topenv(cntxt$env$env)
                    if (! isNamespace(top) ||
                        ! identical(frame, parent.env(top)))
                        cntxt$stop(gettext("bad namespace import frame"))
                    frame <- top
                }
                info$package <- nsName(findHomeNS(name, frame, cntxt))
                info$guard <- FALSE
                info
            }
            else if (ftype == "global" &&
                     (optimize >= 3 ||
                      (optimize >= 2 && name %in% languageFuns))) {
                info$package <- packFrameName(frame)
                info$guard <- FALSE
                info
            }
            else if (guardOK && ftype == "global" &&
                     packFrameName(frame) == "base") {
                info$package <- packFrameName(frame)
                info$guard <- TRUE
                info
            }
            else NULL
        }
    }
    else NULL
}

tryInline <- function(e, cb, cntxt) {
    name <- as.character(e[[1]])
    info <- getInlineInfo(name, cntxt, guardOK = TRUE)
    if (is.null(info))
        FALSE
    else {
        h <- getInlineHandler(name, info$package)
        if (! is.null(h)) {
            if (info$guard) {
                tailcall <- cntxt$tailcall
                if (tailcall) cntxt$tailcall <- FALSE
                expridx <- cb$putconst(e)
                endlabel <- cb$makelabel()
                cb$putcode(BASEGUARD.OP, expridx, endlabel)
                if (! h(e, cb, cntxt))
                    cmpCall(e, cb, cntxt, inlineOK = FALSE)
                cb$putlabel(endlabel)
                if (tailcall) cb$putcode(RETURN.OP)
                TRUE
            }
            else h(e, cb, cntxt)
        }
        else FALSE
    }
}


##
## Inline handlers for some SPECIAL functions
##

setInlineHandler("function", function(e, cb, cntxt) {
    forms <- e[[2]]
    body <- e[[3]]
    sref <- e[[4]]
    ncntxt <- make.functionContext(cntxt, forms, body)
    if (mayCallBrowser(body, cntxt))
        return(FALSE)
    cbody <- genCode(body, ncntxt, loc = cb$savecurloc())
    ci <- cb$putconst(list(forms, cbody, sref))
    cb$putcode(MAKECLOSURE.OP, ci)
    if (cntxt$tailcall) cb$putcode(RETURN.OP)
    TRUE
})

setInlineHandler("{", function(e, cb, cntxt) {
    n <- length(e)
    if (n == 1)
        cmp(NULL, cb, cntxt)
    else {
        sloc <- cb$savecurloc()
        bsrefs <- attr(e, "srcref")
        if (n > 2) {
            ncntxt <- make.noValueContext(cntxt)
            for (i in 2 : (n - 1)) {
                subexp <- e[[i]]
                cb$setcurloc(subexp, getBlockSrcref(bsrefs, i))
                cmp(subexp, cb, ncntxt, setloc = FALSE)
                cb$putcode(POP.OP)
            }
        }
        subexp <- e[[n]]
        cb$setcurloc(subexp, getBlockSrcref(bsrefs, n))
        cmp(subexp, cb, cntxt, setloc = FALSE)
        cb$restorecurloc(sloc)
    }
    TRUE
})

setInlineHandler("if", function(e, cb, cntxt) {
    ## **** test for missing, ...
    test <- e[[2]]
    then.expr <- e[[3]]
    if (length(e) == 4) {
        have.else.expr <- TRUE
        else.expr <- e[[4]]
    }
    else have.else.expr <- FALSE
    ct <- constantFold(test, cntxt, loc = cb$savecurloc())
    if (! is.null(ct) && is.logical(ct$value) && length(ct$value) == 1
        && ! is.na(ct$value)) {
        if (ct$value)
            cmp(then.expr, cb, cntxt)
        else if (have.else.expr)
            cmp(else.expr, cb, cntxt)
        else if (cntxt$tailcall) {
            cb$putcode(LDNULL.OP)
            cb$putcode(INVISIBLE.OP)
            cb$putcode(RETURN.OP)
        }
        else cb$putcode(LDNULL.OP)
        return(TRUE)
    }
    ncntxt <- make.nonTailCallContext(cntxt)
    cmp(test, cb, ncntxt)
    callidx <- cb$putconst(e)
    else.label <- cb$makelabel()
    cb$putcode(BRIFNOT.OP, callidx, else.label)
    cmp(then.expr, cb, cntxt)
    if (cntxt$tailcall) {
        cb$putlabel(else.label)
        if (have.else.expr)
            cmp(else.expr, cb, cntxt)
        else {
            cb$putcode(LDNULL.OP)
            cb$putcode(INVISIBLE.OP)
            cb$putcode(RETURN.OP)
        }
    }
    else {
        end.label <- cb$makelabel()
        cb$putcode(GOTO.OP, end.label)
        cb$putlabel(else.label)
        if (have.else.expr)
            cmp(else.expr, cb, cntxt)
        else
            cb$putcode(LDNULL.OP)
        cb$putlabel(end.label)
    }
    TRUE
})

setInlineHandler("&&", function(e, cb, cntxt) {
    ## **** arity check??
    ncntxt <- make.argContext(cntxt)
    callidx <- cb$putconst(e)
    label <- cb$makelabel()
    cmp(e[[2]], cb, ncntxt)
    cb$putcode(AND1ST.OP, callidx, label)
    cmp(e[[3]], cb, ncntxt)
    cb$putcode(AND2ND.OP, callidx)
    cb$putlabel(label)
    if (cntxt$tailcall)
        cb$putcode(RETURN.OP)
    TRUE
})

setInlineHandler("||", function(e, cb, cntxt) {
    ## **** arity check??
    ncntxt <- make.argContext(cntxt)
    callidx <- cb$putconst(e)
    label <- cb$makelabel()
    cmp(e[[2]], cb, ncntxt)
    cb$putcode(OR1ST.OP, callidx, label)
    cmp(e[[3]], cb, ncntxt)
    cb$putcode(OR2ND.OP, callidx)
    cb$putlabel(label)
    if (cntxt$tailcall)
        cb$putcode(RETURN.OP)
    TRUE
})


##
## Inline handlers for assignment expressions
##

setterInlineHandlers <- new.env(hash = TRUE, parent = emptyenv())

setSetterInlineHandler <- function(name, h, package = "base") {
    if (exists(name, setterInlineHandlers, inherits = FALSE)) {
        entry <- get(name, setterInlineHandlers)
        if (entry$package != package) {
            fmt <- "handler for '%s' is already defined for another package"
            stop(gettextf(fmt, name), domain = NA)
        }
    }
    entry <- list(handler = h, package = package)
    assign(name, entry, setterInlineHandlers)
}

getSetterInlineHandler <- function(name, package = "base") {
    if (exists(name, setterInlineHandlers, inherits = FALSE)) {
        hinfo <- get(name, setterInlineHandlers)
        if (hinfo$package == package)
            hinfo$handler
        else NULL
    }
    else NULL
}

trySetterInline <- function(afun, place, origplace, call, cb, cntxt) {
    name <- as.character(afun)
    info <- getInlineInfo(name, cntxt)
    if (is.null(info))
        FALSE
    else {
        h <- getSetterInlineHandler(name, info$package)
        if (! is.null(h))
            h(afun, place, origplace, call, cb, cntxt)
        else FALSE
    }
}

getterInlineHandlers <- new.env(hash = TRUE, parent = emptyenv())

setGetterInlineHandler <- function(name, h, package = "base") {
    if (exists(name, getterInlineHandlers, inherits = FALSE)) {
        entry <- get(name, getterInlineHandlers)
        if (entry$package != package) {
            fmt <- "handler for '%s' is already defined for another package"
            stop(gettextf(fmt, name), domain = NA)
        }
    }
    entry <- list(handler = h, package = package)
    assign(name, entry, getterInlineHandlers)
}

getGetterInlineHandler <- function(name, package = "base") {
    if (exists(name, getterInlineHandlers, inherits = FALSE)) {
        hinfo <- get(name, getterInlineHandlers)
        if (hinfo$package == package)
            hinfo$handler
        else NULL
    }
    else NULL
}

tryGetterInline <- function(call, cb, cntxt) {
    name <- as.character(call[[1]])
    info <- getInlineInfo(name, cntxt)
    if (is.null(info))
        FALSE
    else {
        h <- getGetterInlineHandler(name, info$package)
        if (! is.null(h))
            h(call, cb, cntxt)
        else FALSE
    }
}

cmpAssign <- function(e, cb, cntxt) {
    ## if (! cntxt$toplevel)
    ##    return(cmpSpecial(e, cb, cntxt))
    if (! checkAssign(e, cntxt, loc = cb$savecurloc()))
        return(cmpSpecial(e, cb, cntxt))
    superAssign <- as.character(e[[1]]) == "<<-"
    lhs <- e[[2]]
    value <- e[[3]]
    symbol <- as.name(getAssignedVar(e, cntxt))
    if (superAssign && ! findVar(symbol, cntxt))
        notifyNoSuperAssignVar(symbol, cntxt, loc = cb$savecurloc())
    if (is.name(lhs) || is.character(lhs))
        cmpSymbolAssign(symbol, value, superAssign, cb, cntxt)
    else if (typeof(lhs) == "language")
        cmpComplexAssign(symbol, lhs, value, superAssign, cb, cntxt)
    else cmpSpecial(e, cb, cntxt) # punt for now
}

flattenPlace <- function(place, cntxt, loc = NULL) {
    places <- NULL
    origplaces <- NULL
    while (typeof(place) == "language") {
        if (length(place) < 2)
            cntxt$stop(gettext("bad assignment 1"), cntxt, loc = loc)
        origplaces <- c(origplaces, list(place))
        tplace <- place
        tplace[[2]] <- as.name("*tmp*")
        places <- c(places, list(tplace))
        place <- place[[2]]
    }
    if (typeof(place) != "symbol")
        cntxt$stop(gettext("bad assignment 2"), cntxt, loc = loc)
    list(places = places, origplaces = origplaces)
}

cmpGetterCall <- function(place, origplace, cb, cntxt) {
    ncntxt <- make.callContext(cntxt, place)
    sloc <- cb$savecurloc()
    cb$setcurexpr(origplace)
    fun <- place[[1]]
    if (typeof(fun) == "symbol") {
        if (! tryGetterInline(place, cb, ncntxt)) {
            ci <- cb$putconst(fun)
            cb$putcode(GETFUN.OP, ci)
            cb$putcode(PUSHNULLARG.OP)
            cmpCallArgs(place[-c(1, 2)], cb, ncntxt)
            cci <- cb$putconst(place)
            cb$putcode(GETTER_CALL.OP, cci)
            cb$putcode(SWAP.OP)
        }
    }
    else {
        cmp(fun, cb, ncntxt)
        cb$putcode(CHECKFUN.OP)
        cb$putcode(PUSHNULLARG.OP)
        cmpCallArgs(place[-c(1, 2)], cb, ncntxt)
        cci <- cb$putconst(place)
        cb$putcode(GETTER_CALL.OP, cci)
        cb$putcode(SWAP.OP)
    }
    cb$restorecurloc(sloc)
}

checkAssign <- function(e, cntxt, loc = NULL) {
    if (length(e) != 3)
        FALSE
    else {
        place <- e[[2]]
        if (typeof(place) == "symbol" ||
            (typeof(place) == "character" && length(place) == 1))
            TRUE
        else {
            while (typeof(place) == "language") {
                fun <- place[[1]]
                if (typeof(fun) != "symbol" &&
                    ! (typeof(fun) == "language" && length(fun) == 3 &&
                       typeof(fun[[1]]) == "symbol" &&
                       as.character(fun[[1]]) %in% c("::", ":::"))) {
                    notifyBadAssignFun(fun, cntxt, loc)
                    return(FALSE)
                }
                place = place[[2]]
            }
            if (typeof(place) == "symbol")
                TRUE
            else FALSE
        }
    }
}

cmpSymbolAssign <- function(symbol, value, superAssign, cb, cntxt) {
    ncntxt <- make.nonTailCallContext(cntxt)
    cmp(value, cb, ncntxt)
    ci <- cb$putconst(symbol)
    if (superAssign)
        cb$putcode(SETVAR2.OP, ci)
    else
        cb$putcode(SETVAR.OP, ci)
    if (cntxt$tailcall) {
        cb$putcode(INVISIBLE.OP)
        cb$putcode(RETURN.OP)
    }
    TRUE
}

cmpComplexAssign <- function(symbol, lhs, value, superAssign, cb, cntxt) {
    if (superAssign) {
        startOP <- STARTASSIGN2.OP
        endOP <- ENDASSIGN2.OP
    }
    else {
        if (! findVar(symbol, cntxt))
            notifyUndefVar(symbol, cntxt, loc = cb$savecurloc())
        startOP <- STARTASSIGN.OP
        endOP <- ENDASSIGN.OP
    }
    ncntxt <- make.nonTailCallContext(cntxt)
    cmp(value, cb, ncntxt)
    csi <- cb$putconst(symbol)
    cb$putcode(startOP, csi)

    ncntxt <- make.argContext(cntxt)
    flat <- flattenPlace(lhs, cntxt, loc = cb$savecurloc())
    flatOrigPlace <- flat$origplaces
    flatPlace <- flat$places
    flatPlaceIdxs <- seq_along(flatPlace)[-1]
    for (i in rev(flatPlaceIdxs))
        cmpGetterCall(flatPlace[[i]], flatOrigPlace[[i]], cb, ncntxt)
    cmpSetterCall(flatPlace[[1]], flatOrigPlace[[1]], value, cb, ncntxt)
    for (i in flatPlaceIdxs)
        cmpSetterCall(flatPlace[[i]], flatOrigPlace[[i]], as.name("*vtmp*"), cb, ncntxt)

    cb$putcode(endOP, csi)
    if (cntxt$tailcall) {
        cb$putcode(INVISIBLE.OP)
        cb$putcode(RETURN.OP)
    }
    TRUE;
}

cmpSetterCall <- function(place, origplace, vexpr, cb, cntxt) {
    afun <- getAssignFun(place[[1]])
    acall <- as.call(c(afun, as.list(place[-1]), list(value = vexpr)))
    acall[[2]] <- as.name("*tmp*")
    ncntxt <- make.callContext(cntxt, acall)
    sloc <- cb$savecurloc()
    cexpr <- as.call(c(afun, as.list(origplace[-1]), list(value = vexpr)))
    cb$setcurexpr(cexpr)
    if (is.null(afun))
        ## **** warn instead and arrange for cmpSpecial?
        ## **** or generate code to signal runtime error?
        cntxt$stop(gettext("invalid function in complex assignment"),
                   loc = cb$savecurloc())
    else if (typeof(afun) == "symbol") {
        if (! trySetterInline(afun, place, origplace, acall, cb, ncntxt)) {
            ci <- cb$putconst(afun)
            cb$putcode(GETFUN.OP, ci)
            cb$putcode(PUSHNULLARG.OP)
            cmpCallArgs(place[-c(1, 2)], cb, ncntxt)
            cci <- cb$putconst(acall)
            cvi <- cb$putconst(vexpr)
            cb$putcode(SETTER_CALL.OP, cci, cvi)
        }
    }
    else {
        cmp(afun, cb, ncntxt)
        cb$putcode(CHECKFUN.OP)
        cb$putcode(PUSHNULLARG.OP)
        cmpCallArgs(place[-c(1, 2)], cb, ncntxt)
        cci <- cb$putconst(acall)
        cvi <- cb$putconst(vexpr)
        cb$putcode(SETTER_CALL.OP, cci, cvi)
    }
    cb$restorecurloc(sloc)
}

getAssignFun <- function(fun) {
    if (typeof(fun) == "symbol")
        as.name(paste0(fun, "<-"))
    else {
        ## check for and handle foo::bar(x) <- y assignments here
        if (typeof(fun) == "language" && length(fun) == 3 &&
            (as.character(fun[[1]]) %in% c("::", ":::")) &&
            typeof(fun[[2]]) == "symbol" && typeof(fun[[3]]) == "symbol") {
            afun <- fun
            afun[[3]] <- as.name(paste0(fun[[3]],"<-"))
            afun
        }
        else NULL
    }
}

cmpSetterDispatch <- function(start.op, dflt.op, afun, place, call, cb, cntxt) {
    if (any.dots(place))
        FALSE ## punt
    else {
        ci <- cb$putconst(call)
        end.label <- cb$makelabel()
        cb$putcode(start.op, ci, end.label)
        if (length(place) > 2) {
            args <- place[-(1:2)]
            cmpBuiltinArgs(args, names(args), cb, cntxt, TRUE)
        }
        cb$putcode(dflt.op)
        cb$putlabel(end.label)
        TRUE
    }
}

setInlineHandler("<-", cmpAssign)
setInlineHandler("=", cmpAssign)
setInlineHandler("<<-", cmpAssign)

setSetterInlineHandler("$<-", function(afun, place, origplace, call, cb, cntxt) {
    if (any.dots(place) || length(place) != 3)
        FALSE
    else {
        sym <- place[[3]]
        if (is.character(sym))
            sym <- as.name(sym)
        if (is.name(sym)) {
            ci <- cb$putconst(call)
            csi <- cb$putconst(sym)
            cb$putcode(DOLLARGETS.OP, ci, csi)
            TRUE
        }
        else FALSE
    }
})

# **** this is now handled differently; see "Improved subset ..."
# setSetterInlineHandler("[<-", function(afun, place, origplace, call, cb, cntxt)
#     cmpSetterDispatch(STARTSUBASSIGN.OP, DFLTSUBASSIGN.OP,
#                       afun, place, call, cb, cntxt))

# setSetterInlineHandler("[[<-", function(afun, place, origplace, call, cb, cntxt)
#     cmpSetterDispatch(STARTSUBASSIGN2.OP, DFLTSUBASSIGN2.OP,
#                       afun, place, call, cb, cntxt))

cmpGetterDispatch <- function(start.op, dflt.op, call, cb, cntxt) {
    if (any.dots(call))
        FALSE ## punt
    else {
        ci <- cb$putconst(call)
        end.label <- cb$makelabel()
        cb$putcode(DUP2ND.OP)
        cb$putcode(start.op, ci, end.label)
        if (length(call) > 2) {
            args <- call[-(1:2)]
            cmpBuiltinArgs(args, names(args), cb, cntxt, TRUE)
        }
        cb$putcode(dflt.op)
        cb$putlabel(end.label)
        cb$putcode(SWAP.OP)
        TRUE
    }
}

setGetterInlineHandler("$", function(call, cb, cntxt) {
    if (any.dots(call) || length(call) != 3)
        FALSE
    else {
        sym <- call[[3]]
        if (is.character(sym))
            sym <- as.name(sym)
        if (is.name(sym)) {
            ci <- cb$putconst(call)
            csi <- cb$putconst(sym)
            cb$putcode(DUP2ND.OP)
            cb$putcode(DOLLAR.OP, ci, csi)
            cb$putcode(SWAP.OP)
            TRUE
        }
        else FALSE
    }
})

# **** this is now handled differently; see "Improved subset ..."
# setGetterInlineHandler("[", function(call, cb, cntxt)
#     cmpGetterDispatch(STARTSUBSET.OP, DFLTSUBSET.OP, call, cb, cntxt))

# setGetterInlineHandler("[[", function(call, cb, cntxt)
#     cmpGetterDispatch(STARTSUBSET2.OP, DFLTSUBSET2.OP, call, cb, cntxt))


##
## Inline handlers for loops
##

setInlineHandler("break", function(e, cb, cntxt) {
    if (is.null(cntxt$loop)) {
        notifyWrongBreakNext("break", cntxt, loc = cb$savecurloc())
        cmpSpecial(e, cb, cntxt)
    }
    else if (cntxt$loop$gotoOK) {
        cb$putcode(GOTO.OP, cntxt$loop$end)
        TRUE
    }
    else cmpSpecial(e, cb, cntxt)
})

setInlineHandler("next", function(e, cb, cntxt) {
    if (is.null(cntxt$loop)) {
        notifyWrongBreakNext("next", cntxt, loc = cb$savecurloc())
        cmpSpecial(e, cb, cntxt)
    }
    else if (cntxt$loop$gotoOK) {
        cb$putcode(GOTO.OP, cntxt$loop$loop)
        TRUE
    }
    else cmpSpecial(e, cb, cntxt)
})

isLoopStopFun <- function(fname, cntxt)
    (fname %in% c("function", "for", "while", "repeat") &&
     isBaseVar(fname, cntxt))

isLoopTopFun <- function(fname, cntxt)
    (fname %in% c("(", "{", "if") &&
     isBaseVar(fname, cntxt))

checkSkipLoopCntxtList <- function(elist, cntxt, breakOK) {
    for (a in as.list(elist))
        if (! missing(a) && ! checkSkipLoopCntxt(a, cntxt, breakOK))
            return(FALSE)
    TRUE
}

checkSkipLoopCntxt <- function(e, cntxt, breakOK = TRUE) {
    if (typeof(e) == "language") {
        fun <- e[[1]]
        if (typeof(fun) == "symbol") {
            fname <- as.character(fun)
            if (! breakOK && fname %in% c("break", "next"))
                FALSE
            else if (isLoopStopFun(fname, cntxt))
                TRUE
            else if (isLoopTopFun(fname, cntxt))
                checkSkipLoopCntxtList(e[-1], cntxt, breakOK)
            else if (fname %in% c("eval", "evalq", "source"))
                FALSE
            else
                checkSkipLoopCntxtList(e[-1], cntxt, FALSE)
        }
        else
            checkSkipLoopCntxtList(e, cntxt, FALSE)
    }
    else TRUE
}

setInlineHandler("repeat", function(e, cb, cntxt) {
    body <- e[[2]]
    if (checkSkipLoopCntxt(body, cntxt))
        cmpRepeatBody(body, cb, cntxt)
    else {
        cntxt$needRETURNJMP <- TRUE ## **** do this a better way
        ljmpend.label <- cb$makelabel()
        cb$putcode(STARTLOOPCNTXT.OP, 0, ljmpend.label)
        cmpRepeatBody(body, cb, cntxt)
        cb$putlabel(ljmpend.label)
        cb$putcode(ENDLOOPCNTXT.OP, 0)
    }
    cb$putcode(LDNULL.OP)
    if (cntxt$tailcall) {
        cb$putcode(INVISIBLE.OP)
        cb$putcode(RETURN.OP)
    }
    TRUE
})

cmpRepeatBody <- function(body, cb, cntxt) {
    loop.label <- cb$makelabel()
    end.label <- cb$makelabel()
    cb$putlabel(loop.label)
    lcntxt <- make.loopContext(cntxt, loop.label, end.label)
    cmp(body, cb, lcntxt)
    cb$putcode(POP.OP)
    cb$putcode(GOTO.OP, loop.label)
    cb$putlabel(end.label)
}

setInlineHandler("while", function(e, cb, cntxt) {
    cond <- e[[2]]
    body <- e[[3]]
    if (checkSkipLoopCntxt(cond, cntxt) && checkSkipLoopCntxt(body, cntxt))
        cmpWhileBody(e, cond, body, cb, cntxt)
    else {
        cntxt$needRETURNJMP <- TRUE ## **** do this a better way
        ljmpend.label <- cb$makelabel()
        cb$putcode(STARTLOOPCNTXT.OP, 0, ljmpend.label)
        cmpWhileBody(e, cond, body, cb, cntxt)
        cb$putlabel(ljmpend.label)
        cb$putcode(ENDLOOPCNTXT.OP, 0)
    }
    cb$putcode(LDNULL.OP)
    if (cntxt$tailcall) {
        cb$putcode(INVISIBLE.OP)
        cb$putcode(RETURN.OP)
    }
    TRUE
})

cmpWhileBody <- function(call, cond, body, cb, cntxt) {
    loop.label <- cb$makelabel()
    end.label <- cb$makelabel()
    cb$putlabel(loop.label)
    lcntxt <- make.loopContext(cntxt, loop.label, end.label)
    cmp(cond, cb, lcntxt)
    callidx <- cb$putconst(call)
    cb$putcode(BRIFNOT.OP, callidx, end.label)
    cmp(body, cb, lcntxt)
    cb$putcode(POP.OP)
    cb$putcode(GOTO.OP, loop.label)
    cb$putlabel(end.label)
}

setInlineHandler("for", function(e, cb, cntxt) {
    sym <- e[[2]]
    seq <- e[[3]]
    body <- e[[4]]
    if (! is.name(sym)) {
        ## not worth warning here since the parser should not allow this
        return(FALSE)
    }
    ncntxt <- make.nonTailCallContext(cntxt)
    cmp(seq, cb, ncntxt)
    ci <- cb$putconst(sym)
    callidx <- cb$putconst(e)
    if (checkSkipLoopCntxt(body, cntxt))
        cmpForBody(callidx, body, ci, cb, cntxt)
    else {
        cntxt$needRETURNJMP <- TRUE ## **** do this a better way
        ctxt.label <- cb$makelabel()
        cb$putcode(STARTFOR.OP, callidx, ci, ctxt.label)
        cb$putlabel(ctxt.label)
        ljmpend.label <- cb$makelabel()
        cb$putcode(STARTLOOPCNTXT.OP, 1, ljmpend.label)
        cmpForBody(NULL, body, NULL, cb, cntxt)
        cb$putlabel(ljmpend.label)
        cb$putcode(ENDLOOPCNTXT.OP, 1)
    }
    cb$putcode(ENDFOR.OP)
    if (cntxt$tailcall) {
        cb$putcode(INVISIBLE.OP)
        cb$putcode(RETURN.OP)
    }
    TRUE
})

cmpForBody <- function(callidx, body, ci, cb, cntxt) {
    body.label <- cb$makelabel()
    loop.label <- cb$makelabel()
    end.label <- cb$makelabel()
    if (is.null(ci))
        cb$putcode(GOTO.OP, loop.label)
    else
        cb$putcode(STARTFOR.OP, callidx, ci, loop.label)
    cb$putlabel(body.label)
    lcntxt <- make.loopContext(cntxt, loop.label, end.label)
    cmp(body, cb, lcntxt)
    cb$putcode(POP.OP)
    cb$putlabel(loop.label)
    cb$putcode(STEPFOR.OP, body.label)
    cb$putlabel(end.label)
}


##
## Inline handlers for one and two argument primitives
##

cmpPrim1 <- function(e, cb, op, cntxt) {
    if (dots.or.missing(e[-1]))
        cmpBuiltin(e, cb, cntxt)
    else if (length(e) != 2) {
        notifyWrongArgCount(e[[1]], cntxt, loc = cb$savecurloc())
        cmpBuiltin(e, cb, cntxt)
    }
    else {
        ncntxt <- make.nonTailCallContext(cntxt)
        cmp(e[[2]], cb, ncntxt);
        ci <- cb$putconst(e)
        cb$putcode(op, ci)
        if (cntxt$tailcall)
            cb$putcode(RETURN.OP)
        TRUE
    }
}

checkNeedsInc <- function(e, cntxt) {
    type <- typeof(e)
    if (type %in% c("language", "bytecode", "symbol", "promise"))
        TRUE
    else if (type == "symbol" && ! findLocVar(e, cntxt))
        TRUE
    else FALSE
}

cmpPrim2 <- function(e, cb, op, cntxt) {
    if (dots.or.missing(e[-1]))
        cmpBuiltin(e, cb, cntxt)
    else if (length(e) != 3) {
        notifyWrongArgCount(e[[1]], cntxt, loc = cb$savecurloc())
        cmpBuiltin(e, cb, cntxt)
    }
    else {
        needInc <- checkNeedsInc(e[[3]], cntxt)
        ncntxt <- make.nonTailCallContext(cntxt)
        cmp(e[[2]], cb, ncntxt);
        if (needInc) cb$putcode(INCLNK.OP)
        ncntxt <- make.argContext(cntxt)
        cmp(e[[3]], cb, ncntxt)
        if (needInc) cb$putcode(DECLNK.OP)
        ci <- cb$putconst(e)
        cb$putcode(op, ci)
        if (cntxt$tailcall)
            cb$putcode(RETURN.OP)
        TRUE
    }
}

setInlineHandler("+", function(e, cb, cntxt) {
    if (length(e) == 3)
        cmpPrim2(e, cb, ADD.OP, cntxt)
    else
        cmpPrim1(e, cb, UPLUS.OP, cntxt)
})

setInlineHandler("-", function(e, cb, cntxt) {
    if (length(e) == 3)
        cmpPrim2(e, cb, SUB.OP, cntxt)
    else
        cmpPrim1(e, cb, UMINUS.OP, cntxt)
})

setInlineHandler("*", function(e, cb, cntxt)
    cmpPrim2(e, cb, MUL.OP, cntxt))

setInlineHandler("/", function(e, cb, cntxt)
    cmpPrim2(e, cb, DIV.OP, cntxt))

setInlineHandler("^", function(e, cb, cntxt)
    cmpPrim2(e, cb, EXPT.OP, cntxt))

setInlineHandler("exp", function(e, cb, cntxt)
    cmpPrim1(e, cb, EXP.OP, cntxt))

setInlineHandler("sqrt", function(e, cb, cntxt)
    cmpPrim1(e, cb, SQRT.OP, cntxt))

setInlineHandler("log", function(e, cb, cntxt) {
    if (dots.or.missing(e) || ! is.null(names(e)) ||
        length(e) < 2 || length(e) > 3)
        cmpSpecial(e, cb, cntxt)
    else {
        ci <- cb$putconst(e)
        ncntxt <- make.nonTailCallContext(cntxt)
        cmp(e[[2]], cb, ncntxt);
        if (length(e) == 2)
            cb$putcode(LOG.OP, ci)
        else {
            needInc <- checkNeedsInc(e[[3]])
            if (needInc) cb$putcode(INCLNK.OP)
            ncntxt <- make.argContext(cntxt)
            cmp(e[[3]], cb, ncntxt)
            if (needInc) cb$putcode(DECLNK.OP)
            cb$putcode(LOGBASE.OP, ci)
        }
        if (cntxt$tailcall)
            cb$putcode(RETURN.OP)
        TRUE
    }
})

## Keep the order consistent with the order in the internal byte code
## interpreter!
math1funs <- c("floor", "ceiling", "sign",
               "expm1", "log1p",
               "cos", "sin", "tan", "acos", "asin", "atan",
               "cosh", "sinh", "tanh", "acosh", "asinh", "atanh",
               "lgamma", "gamma", "digamma", "trigamma",
               "cospi", "sinpi", "tanpi")

cmpMath1 <- function(e, cb, cntxt) {
    if (dots.or.missing(e[-1]))
        cmpBuiltin(e, cb, cntxt)
    else if (length(e) != 2) {
        notifyWrongArgCount(e[[1]], cntxt, loc = cb$savecurloc())
        cmpBuiltin(e, cb, cntxt)
    }
    else {
        name <- as.character(e[[1]])
        idx <- match(name, math1funs) - 1
        if (is.na(idx))
            cntxt$stop(
                paste(sQuote(name), "is not a registered math1 function"),
                cntxt, loc = cb$savecurloc())
        ncntxt <- make.nonTailCallContext(cntxt)
        cmp(e[[2]], cb, ncntxt);
        ci <- cb$putconst(e)
        cb$putcode(MATH1.OP, ci, idx)
        if (cntxt$tailcall)
            cb$putcode(RETURN.OP)
        TRUE
    }
}

for (name in math1funs)
    setInlineHandler(name, cmpMath1)

setInlineHandler("==", function(e, cb, cntxt)
   cmpPrim2(e, cb, EQ.OP, cntxt))

setInlineHandler("!=", function(e, cb, cntxt)
   cmpPrim2(e, cb, NE.OP, cntxt))

setInlineHandler("<", function(e, cb, cntxt)
   cmpPrim2(e, cb, LT.OP, cntxt))

setInlineHandler("<=", function(e, cb, cntxt)
   cmpPrim2(e, cb, LE.OP, cntxt))

setInlineHandler(">=", function(e, cb, cntxt)
   cmpPrim2(e, cb, GE.OP, cntxt))

setInlineHandler(">", function(e, cb, cntxt)
   cmpPrim2(e, cb, GT.OP, cntxt))

setInlineHandler("&", function(e, cb, cntxt)
   cmpPrim2(e, cb, AND.OP, cntxt))

setInlineHandler("|", function(e, cb, cntxt)
   cmpPrim2(e, cb, OR.OP, cntxt))

setInlineHandler("!", function(e, cb, cntxt)
   cmpPrim1(e, cb, NOT.OP, cntxt))


##
## Inline handlers for the left parenthesis function
##

setInlineHandler("(", function(e, cb, cntxt) {
    if (any.dots(e))
        cmpBuiltin(e, cb, cntxt) ## punt
    else if (length(e) != 2) {
        notifyWrongArgCount("(", cntxt, loc = cb$savecurloc())
        cmpBuiltin(e, cb, cntxt) ## punt
    }
    else if (cntxt$tailcall) {
        ncntxt <- make.nonTailCallContext(cntxt)
        cmp(e[[2]], cb, ncntxt)
        cb$putcode(VISIBLE.OP)
        cb$putcode(RETURN.OP)
        TRUE
    }
    else {
        cmp(e[[2]], cb, cntxt)
        TRUE
    }
})


##
## Inline handlers for general BUILTIN and SPECIAL functions
##

cmpBuiltin <- function(e, cb, cntxt, internal = FALSE) {
    fun <- e[[1]]
    args <- e[-1]
    names <- names(args)
    if (dots.or.missing(args))
        FALSE
    else {
        ci <- cb$putconst(fun)
        if (internal)
            cb$putcode(GETINTLBUILTIN.OP, ci)
        else
            cb$putcode(GETBUILTIN.OP, ci)
        cmpBuiltinArgs(args, names, cb, cntxt)
        ci <- cb$putconst(e)
        cb$putcode(CALLBUILTIN.OP, ci)
        if (cntxt$tailcall) cb$putcode(RETURN.OP)
        TRUE
    }
}

cmpBuiltinArgs <- function(args, names, cb, cntxt, missingOK = FALSE) {
    ncntxt <- make.argContext(cntxt)
    for (i in seq_along(args)) {
        a <- args[[i]]
        n <- names[[i]]
        if (missing(a)) {
            if (missingOK) {
                cb$putcode(DOMISSING.OP)
                cmpTag(n, cb)
            }
            else
                cntxt$stop(gettext("missing arguments are not allowed"), cntxt,
                           loc = cb$savecurloc())
        }
        ## **** handle ... here ??
        else if (typeof(a) == "bytecode")
            cntxt$stop(gettext("cannot compile byte code literals in code"),
                       cntxt, loc = cb$savecurloc())
        else if (typeof(a) == "promise")
            cntxt$stop(gettext("cannot compile promise literals in code"),
                       cntxt, loc = cb$savecurloc())
        else {
            if (is.symbol(a)) {
                ca <- constantFold(a, cntxt, loc = cb$savecurloc())
                if (is.null(ca)) {
                    cmpSym(a, cb, ncntxt, missingOK)
                    cb$putcode(PUSHARG.OP)
                }
                else
                    cmpConstArg(ca$value, cb, cntxt)
            }
            else if (typeof(a) == "language") {
                cmp(a, cb, ncntxt)
                cb$putcode(PUSHARG.OP)
            }
            else
                cmpConstArg(a, cb, cntxt)
            cmpTag(n, cb)
        }
    }
}

cmpSpecial <- function(e, cb, cntxt) {
    fun <- e[[1]]
    if (typeof(fun) == "character")
        fun <- as.name(fun)
    ci <- cb$putconst(e)
    cb$putcode(CALLSPECIAL.OP, ci)
    if (cntxt$tailcall)
        cb$putcode(RETURN.OP)
    TRUE
}

cmpDotInternalCall <- function(e, cb, cntxt) {
    ee <- e[[2]]
    sym <- ee[[1]]
    if (.Internal(is.builtin.internal(sym)))
        cmpBuiltin(ee, cb, cntxt, internal = TRUE)
    else
        cmpSpecial(e, cb, cntxt)
}

setInlineHandler(".Internal", cmpDotInternalCall)


##
## Inline handlers for subsetting and related operators
##

cmpDispatch <- function(start.op, dflt.op, e, cb, cntxt, missingOK = TRUE) {
    if ((missingOK && any.dots(e)) ||
        (! missingOK && dots.or.missing(e)) ||
        length(e) == 1)
        cmpSpecial(e, cb, cntxt) ## punt
    else {
        ne <- length(e)
        oe <- e[[2]]
        if (missing(oe))
            cmpSpecial(e, cb, cntxt) ## punt
        else {
            ncntxt <- make.argContext(cntxt)
            cmp(oe, cb, ncntxt)
            ci <- cb$putconst(e)
            end.label <- cb$makelabel()
            cb$putcode(start.op, ci, end.label)
            if (ne > 2)
                cmpBuiltinArgs(e[-(1:2)], names(e)[-(1:2)], cb, cntxt,
                               missingOK)
            cb$putcode(dflt.op)
            cb$putlabel(end.label)
            if (cntxt$tailcall) cb$putcode(RETURN.OP)
            TRUE
        }
    }
}

# **** this is now handled differently; see "Improved subset ..."
# setInlineHandler("[", function(e, cb, cntxt)
#     cmpDispatch(STARTSUBSET.OP, DFLTSUBSET.OP, e, cb, cntxt))

# **** c() is now a BUILTIN
# setInlineHandler("c", function(e, cb, cntxt)
#     cmpDispatch(STARTC.OP, DFLTC.OP, e, cb, cntxt, FALSE))

# **** this is now handled differently; see "Improved subset ..."
# setInlineHandler("[[", function(e, cb, cntxt)
#     cmpDispatch(STARTSUBSET2.OP, DFLTSUBSET2.OP, e, cb, cntxt))

setInlineHandler("$", function(e, cb, cntxt) {
    if (any.dots(e) || length(e) != 3)
        cmpSpecial(e, cb, cntxt)
    else {
        sym <- if (is.character(e[[3]]) && length(e[[3]]) == 1
                   && e[[3]] != "")
            as.name(e[[3]]) else e[[3]]
        if (is.name(sym)) {
            ncntxt <- make.argContext(cntxt)
            cmp(e[[2]], cb, ncntxt)
            ci <- cb$putconst(e)
            csi <- cb$putconst(sym)
            cb$putcode(DOLLAR.OP, ci, csi)
            if (cntxt$tailcall) cb$putcode(RETURN.OP)
            TRUE
        }
        else cmpSpecial(e, cb, cntxt)
    }
})


##
## Inline handler for local() and return() functions
##

setInlineHandler("local", function(e, cb, cntxt) {
    if (length(e) == 2) {
        ee <- as.call(list(as.call(list(
            as.name("function"), NULL, e[[2]], NULL))))
        cmp(ee, cb, cntxt)
        TRUE
    }
    else FALSE
})

setInlineHandler("return", function(e, cb, cntxt) {
    if (dots.or.missing(e) || length(e) > 2)
        cmpSpecial(e, cb, cntxt) ## **** punt for now
    else {
        if (length(e) == 1)
            val <- NULL
        else
            val <- e[[2]]
        ncntxt <- make.nonTailCallContext(cntxt)
        cmp(val, cb, ncntxt)
        if (cntxt$needRETURNJMP)
            cb$putcode(RETURNJMP.OP)
        else
            cb$putcode(RETURN.OP)
    }
    TRUE
})


##
## Inline handlers for the family of is.xyz primitives
##

cmpIs <- function(op, e, cb, cntxt) {
    if (any.dots(e) || length(e) != 2)
        cmpBuiltin(e, cb, cntxt)
    else {
        ## **** check that the function is a builtin somewhere??
        s<-make.argContext(cntxt)
        cmp(e[[2]], cb, s)
        cb$putcode(op)
        if (cntxt$tailcall) cb$putcode(RETURN.OP)
        TRUE
    }
}

setInlineHandler("is.character", function(e, cb, cntxt)
    cmpIs(ISCHARACTER.OP, e, cb, cntxt))
setInlineHandler("is.complex", function(e, cb, cntxt)
    cmpIs(ISCOMPLEX.OP, e, cb, cntxt))
setInlineHandler("is.double", function(e, cb, cntxt)
    cmpIs(ISDOUBLE.OP, e, cb, cntxt))
setInlineHandler("is.integer", function(e, cb, cntxt)
    cmpIs(ISINTEGER.OP, e, cb, cntxt))
setInlineHandler("is.logical", function(e, cb, cntxt)
    cmpIs(ISLOGICAL.OP, e, cb, cntxt))
setInlineHandler("is.name", function(e, cb, cntxt)
     cmpIs(ISSYMBOL.OP, e, cb, cntxt))
setInlineHandler("is.null", function(e, cb, cntxt)
    cmpIs(ISNULL.OP, e, cb, cntxt))
setInlineHandler("is.object", function(e, cb, cntxt)
    cmpIs(ISOBJECT.OP, e, cb, cntxt))
setInlineHandler("is.symbol", function(e, cb, cntxt)
    cmpIs(ISSYMBOL.OP, e, cb, cntxt))


##
## Default inline handlers for BUILTIN and SPECIAL functions
##

local({
    basevars <- ls('package:base', all = TRUE)
    types <- sapply(basevars, function(n) typeof(get(n)))
    for (s in basevars[types == "special"])
        if (! haveInlineHandler(s, "base"))
            setInlineHandler(s, cmpSpecial)
    for (b in basevars[types == "builtin"])
        if (! haveInlineHandler(b, "base"))
            setInlineHandler(b, cmpBuiltin)
})


##
## Inline handlers for some .Internal functions
##

simpleFormals <- function(def) {
    forms <- formals(def)
    if ("..." %in% names(forms))
        return(FALSE)
    for (d in as.list(forms)) {
        if (! missing(d)) {
            ## **** check constant folding
            if (typeof(d) %in% c("symbol", "language", "promise", "bytecode"))
                return(FALSE)
        }
    }
    TRUE
}

simpleArgs <- function(icall, fnames) {
    for (a in as.list(icall[-1])) {
        if (missing(a))
            return(FALSE)
        else if (typeof(a) == "symbol") {
            if (! (as.character(a) %in% fnames))
                return(FALSE)
        }
        else if (typeof(a) %in% c("language", "promise", "bytecode"))
            return(FALSE)
    }
    TRUE
}

is.simpleInternal <- function(def) {
    if (typeof(def) == "closure" && simpleFormals(def)) {
        b <- body(def)
        if (typeof(b) == "language" && length(b) == 2 && b[[1]] == "{")
            b <- b[[2]]
        if (typeof(b) == "language" &&
            typeof(b[[1]]) == "symbol" &&
            b[[1]] == ".Internal") {
            icall <- b[[2]]
            ifun <- icall[[1]]
            typeof(ifun) == "symbol" &&
            .Internal(is.builtin.internal(as.name(ifun))) &&
            simpleArgs(icall, names(formals(def)))
        }
        else FALSE
    }
    else FALSE
}

inlineSimpleInternalCall <- function(e, def) {
    if (! dots.or.missing(e) && is.simpleInternal(def)) {
        forms <- formals(def)
        b <- body(def)
        if (typeof(b) == "language" && length(b) == 2 && b[[1]] == "{")
            b <- b[[2]]
        icall <- b[[2]]
        defaults <- forms ## **** could strip missings but OK not to?
        cenv <- c(as.list(match.call(def, e, F))[-1], defaults)
        subst <- function(n)
            if (typeof(n) == "symbol") cenv[[as.character(n)]] else n
        args <- lapply(as.list(icall[-1]), subst)
        as.call(list(quote(.Internal), as.call(c(icall[[1]], args))))
    }
    else NULL
}

cmpSimpleInternal <- function(e, cb, cntxt) {
    if (any.dots(e))
        FALSE
    else {
        name <- as.character(e[[1]])
        def <- findFunDef(name, cntxt)
        if (! checkCall(def, e, NULL)) return(FALSE)
        call <- inlineSimpleInternalCall(e, def)
        if (is.null(call))
            FALSE
        else
            cmpDotInternalCall(call, cb, cntxt)
    }
}

safeBaseInternals <- c("atan2", "besselY", "beta", "choose",
                       "drop", "inherits", "is.vector", "lbeta", "lchoose",
                       "nchar", "polyroot", "typeof", "vector", "which.max",
                       "which.min", "is.loaded", "identical",
                       "match", "rep.int", "rep_len")

for (i in safeBaseInternals) setInlineHandler(i,  cmpSimpleInternal)

safeStatsInternals <- c("dbinom", "dcauchy", "dgeom", "dhyper", "dlnorm",
                        "dlogis", "dnorm", "dpois", "dunif", "dweibull",
                        "fft", "mvfft", "pbinom", "pcauchy",
                        "pgeom", "phyper", "plnorm", "plogis", "pnorm",
                        "ppois", "punif", "pweibull", "qbinom", "qcauchy",
                        "qgeom", "qhyper", "qlnorm", "qlogis", "qnorm",
                        "qpois", "qunif", "qweibull", "rbinom", "rcauchy",
                        "rgeom", "rhyper", "rlnorm", "rlogis", "rnorm",
                        "rpois", "rsignrank",  "runif", "rweibull",
                        "rwilcox", "ptukey", "qtukey")

for (i in safeStatsInternals) setInlineHandler(i,  cmpSimpleInternal, "stats")


##
## Inline handler for switch
##

findActionIndex <- function(name, nm, miss) {
    start <- match(name, nm)
    aidx <- c(which(! miss), length(nm) + 1)
    min(aidx[aidx >= start])
}

setInlineHandler("switch", function(e, cb, cntxt) {
    if (length(e) < 2 || any.dots(e))
        cmpSpecial(e, cb, cntxt)
    else {
        ## **** check name on EXPR, if any, partially matches EXPR?
        expr <- e[[2]]
        cases <-e[-c(1, 2)]

        if (is.null(cases))
            notifyNoSwitchcases(cntxt, loc = cb$savecurloc())

        miss <- missingArgs(cases)
        nm <- names(cases)

        ## allow for corner cases like switch(x, 1) which always
        ## returns 1 if x is a character scalar.
        if (is.null(nm) && length(cases) == 1)
            nm <- ""

        ## collect information on named alternatives and check for
        ## multiple default cases.
        if (! is.null(nm)) {
            haveNames <- TRUE
            ndflt <- sum(nm == "")
            if (ndflt > 1) {
                notifyMultipleSwitchDefaults(ndflt, cntxt, loc = cb$savecurloc())
                ## **** punt back to interpreted version for now to get
                ## **** runtime error message for multiple defaults
                cmpSpecial(e, cb, cntxt)
                return(TRUE)
            }
            if (ndflt > 0)
                haveCharDflt <- TRUE
            else
                haveCharDflt <- FALSE
        }
        else {
            haveNames <- FALSE
            haveCharDflt <- FALSE
        }

        ## create the labels
        if (any(miss))
            missLabel <- cb$makelabel()
        dfltLabel <- cb$makelabel()

        lab <- function(m)
            if (m) missLabel
            else cb$makelabel()
        labels <- c(lapply(miss, lab), list(dfltLabel))

        if (! cntxt$tailcall)
            endLabel <- cb$makelabel()

        ## create the map from names to labels for a character switch
        if (haveNames) {
            unm <- unique(nm[nm != ""])
            if (haveCharDflt)
                unm <- c(unm, "")
            nlabels <- labels[unlist(lapply(unm, findActionIndex, nm, miss))]
            ## if there is no unnamed case to act as a default for a
            ## character switch then the numeric default becomes the
            ## character default as well.
            if (! haveCharDflt) {
                unm <- c(unm, "")
                nlabels <- c(nlabels, list(dfltLabel))
            }
        }
        else {
            unm <- NULL
            nlabels <- NULL
        }

        ## compile the EXPR argument
        ncntxt <- make.nonTailCallContext(cntxt)
        cmp(expr, cb, ncntxt)

        ## emit the SWITCH instruction
        cei <- cb$putconst(e)
        if (haveNames) {
            cni <- cb$putconst(unm)
            cb$putcode(SWITCH.OP, cei, cni, nlabels, labels)
        }
        else {
            cni <- cb$putconst(NULL)
            cb$putcode(SWITCH.OP, cei, cni, cni, labels)
        }

        ## emit code to signal an error if a numeric switch hist an
        ## empty alternative (fall through, as for character, might
        ## make more sense but that isn't the way switch() works)
        if (any(miss)) {
            cb$putlabel(missLabel)
            cmp(quote(stop("empty alternative in numeric switch")), cb, cntxt)
        }

        ## emit code for the default case
        cb$putlabel(dfltLabel)
        cb$putcode(LDNULL.OP)
        if (cntxt$tailcall) {
            cb$putcode(INVISIBLE.OP)
            cb$putcode(RETURN.OP)
        }
        else
            cb$putcode(GOTO.OP, endLabel)

        ## emit code for the non-empty alternatives
        for (i in seq_along(cases)) {
            if (! miss[i]) {
                cb$putlabel(labels[[i]])
                cmp(cases[[i]], cb, cntxt)
                if (! cntxt$tailcall)
                    cb$putcode(GOTO.OP, endLabel)
            }
        }

        if (! cntxt$tailcall)
            cb$putlabel(endLabel)
    }
    TRUE
})


##
## Inline handler for .Call
##

setInlineHandler(".Call", function(e, cb, cntxt) {
    nargsmax <- 16 ## should match DOTCALL_MAX in eval.c
    if (dots.or.missing(e[-1]) || ! is.null(names(e)) ||
        length(e) < 2 || length(e) > nargsmax + 2)
        cmpBuiltin(e, cb, cntxt) ## punt
    else {
        ncntxt <- make.nonTailCallContext(cntxt)
        cmp(e[[2]], cb, ncntxt);
        nargs <- length(e) - 2
        if (nargs > 0) {
            ncntxt <- make.argContext(cntxt)
            for (a in as.list(e[-(1:2)]))
                cmp(a, cb, ncntxt);
        }
        ci <- cb$putconst(e)
        cb$putcode(DOTCALL.OP, ci, nargs)
        if (cntxt$tailcall)
            cb$putcode(RETURN.OP)
        TRUE
    }
})


##
## Inline handlers for generating integer sequences
##

setInlineHandler(":", function(e, cb, cntxt)
    cmpPrim2(e, cb, COLON.OP, cntxt))

setInlineHandler("seq_along", function(e, cb, cntxt)
    cmpPrim1(e, cb, SEQALONG.OP, cntxt))

setInlineHandler("seq_len", function(e, cb, cntxt)
    cmpPrim1(e, cb, SEQLEN.OP, cntxt))


##
## Inline handlers to control warnings
##

cmpMultiColon <- function(e, cb, cntxt) {
    if (! dots.or.missing(e) && length(e) == 3) {
        goodType <- function(a)
            typeof(a) == "symbol" ||
            (typeof(a) == "character" && length(a) == 1)
        fun <- e[[1]]
        x <- e[[2]]
        y <- e[[3]]
        if (goodType(x) && goodType(y)) {
            args <- list(as.character(x), as.character(y))
            cmpCallSymFun(fun, args, e, cb, cntxt)
            TRUE
        }
        else FALSE
    }
    else FALSE
}

setInlineHandler("::", cmpMultiColon)
setInlineHandler(":::", cmpMultiColon)

setSetterInlineHandler("@<-", function(afun, place, origplace, acall, cb, cntxt) {
    if (! dots.or.missing(place) && length(place) == 3 &&
        typeof(place[[3]]) == "symbol") {
        place[[3]] <- as.character(place[[3]])
        vexpr <- acall[[length(acall)]]
        cmpSetterCall(place, origplace, vexpr, cb, cntxt)
        TRUE
    }
    else FALSE
})

setInlineHandler("with", function(e, cb, cntxt) {
    cntxt$suppressUndefined <- TRUE
    cmpCallSymFun(e[[1]], e[-1], e, cb, cntxt)
    TRUE
})

setInlineHandler("require", function(e, cb, cntxt) {
    cntxt$suppressUndefined <- TRUE
    cmpCallSymFun(e[[1]], e[-1], e, cb, cntxt)
    TRUE
})


##
## Compiler warnings
##

suppressAll <- function(cntxt)
    identical(cntxt$suppressAll, TRUE)

suppressNoSuperAssignVar <- function(cntxt)
    isTRUE(cntxt$suppressNoSuperAssignVar)

suppressUndef <- function(name, cntxt) {
    if (identical(cntxt$suppressAll, TRUE))
        TRUE
    else {
        suppress <- cntxt$suppressUndefined
        if (is.null(suppress))
            FALSE
        else if (identical(suppress, TRUE))
            TRUE
        else if (is.character(suppress) && as.character(name) %in% suppress)
            TRUE
        else FALSE
    }
}

notifyLocalFun <- function(fun, cntxt, loc = NULL) {
    if (! suppressAll(cntxt))
        NULL
}

notifyUndefFun <- function(fun, cntxt, loc = NULL) {
    if (! suppressUndef(fun, cntxt)) {
        msg <- gettextf("no visible global function definition for '%s'",
                        as.character(fun))
        cntxt$warn(msg, cntxt, loc)
    }
}

notifyUndefVar <- function(var, cntxt, loc = NULL) {
    if (! suppressUndef(var, cntxt)) {
        msg <- gettextf("no visible binding for global variable '%s'",
                        as.character(var))
        cntxt$warn(msg, cntxt, loc)
    }
}

notifyNoSuperAssignVar <- function(symbol, cntxt, loc = NULL) {
    if (! suppressAll(cntxt) && ! suppressNoSuperAssignVar(cntxt)) {
        msg <- gettextf("no visible binding for '<<-' assignment to '%s'",
                        as.character(symbol))
        cntxt$warn(msg, cntxt, loc)
    }
}

notifyWrongArgCount <- function(fun, cntxt, loc = NULL) {
    if (! suppressAll(cntxt)) {
        msg <- gettextf("wrong number of arguments to '%s'",
                        as.character(fun))
        cntxt$warn(msg, cntxt, loc)
    }
}

notifyWrongDotsUse <- function(var, cntxt, loc = NULL) {
    if (! suppressAll(cntxt)) {
        msg <- paste(var, "may be used in an incorrect context")
        cntxt$warn(msg, cntxt, loc)
    }
}

notifyWrongBreakNext <- function(fun, cntxt, loc = NULL) {
    if (! suppressAll(cntxt)) {
        msg <- paste(fun, "used in wrong context: no loop is visible")
        cntxt$warn(msg, cntxt, loc)
    }
}

notifyBadCall <- function(w, cntxt, loc = NULL) {
    if (! suppressAll(cntxt))
        cntxt$warn(w, cntxt, loc)
}

notifyBadAssignFun <- function(fun, cntxt, loc = NULL) {
    if (! suppressAll(cntxt)) {
        msg <- gettext("invalid function in complex assignment")
        cntxt$warn(msg, cntxt, loc)
    }
}

notifyMultipleSwitchDefaults <- function(ndflt, cntxt, loc = NULL)
    if (! suppressAll(cntxt)) {
        msg <- gettext("more than one default provided in switch() call")
        cntxt$warn(msg, cntxt, loc)
    }

notifyNoSwitchcases <- function(cntxt, loc = NULL)
    if (! suppressAll(cntxt)) {
        msg <- gettext("'switch' with no alternatives")
        cntxt$warn(msg, cntxt, loc)
    }

notifyAssignSyntacticFun <- function(funs, cntxt, loc = NULL) {
    if (! suppressAll(cntxt)) {
        msg <- ngettext(length(funs),
            "local assignment to syntactic function: ",
            "local assignments to syntactic functions: ")
        cntxt$warn(paste(msg, paste(funs, collapse = ", ")),
                   cntxt, loc)
    }
}

notifyCompilerError <- function(msg)
    if (!compilerOptions$suppressAll)
        cat(paste(gettext("Error: compilation failed - "), msg, "\n"))


##
## Compiler interface
##

compile <- function(e, env = .GlobalEnv, options = NULL, srcref = NULL) {
    cenv <- makeCenv(env)
    cntxt <- make.toplevelContext(cenv, options)
    cntxt$env <- addCenvVars(cenv, findLocals(e, cntxt))
    if (mayCallBrowser(e, cntxt))
        ## NOTE: compilation will be attempted repeatedly
        e
    else if (is.null(srcref))
        genCode(e, cntxt)
    else
        genCode(e, cntxt, loc = list(expr = e, srcref = srcref))
}

cmpfun <- function(f, options = NULL) {
    type <- typeof(f)
    if (type == "closure") {
        cntxt <- make.toplevelContext(makeCenv(environment(f)), options)
        ncntxt <- make.functionContext(cntxt, formals(f), body(f))
        if (mayCallBrowser(body(f), ncntxt))
            return(f)
        if (typeof(body(f)) != "language" || body(f)[1] != "{")
            loc <- list(expr = body(f), srcref = getExprSrcref(f))
        else
            loc <- NULL
        b <- genCode(body(f), ncntxt, loc = loc)
        val <- .Internal(bcClose(formals(f), b, environment(f)))
        attrs <- attributes(f)
        if (! is.null(attrs))
            attributes(val) <- attrs
        if (isS4(f)) ## **** should this really be needed??
            val <- asS4(val)
        val
    }
    else if (type == "builtin" || type == "special")
        f
    else stop("cannot compile a non-function")
}

tryCmpfun <- function(f)
    tryCatch(cmpfun(f), error = function(e) {
        notifyCompilerError(paste(e$message, "at", deparse(e$call)))
        f
    })

tryCompile <- function(e, ...)
    tryCatch(compile(e, ...), error = function(err) {
        notifyCompilerError(paste(err$message, "at", deparse(err$call)))
        e
    })

cmpframe <- function(inpos, file) {
    expr.needed <- 1000
    expr.old <- getOption("expressions")
    if (expr.old < expr.needed)
       options(expressions = expr.needed)
    on.exit(options(expressions = expr.old))

    attach(NULL, name="<compiled>")
    inpos <- inpos + 1
    outpos <- 2
    on.exit(detach(pos=outpos), add=TRUE)

    for (f in ls(pos = inpos, all.names = TRUE)) {
        def <- get(f, pos = inpos)
        if (typeof(def) == "closure") {
                cat(gettextf("compiling '%s'", f), "\n", sep = "")
                fc <- cmpfun(def)
                assign(f, fc, pos=outpos)
        }
    }
    cat(gettextf("saving to file \"%s\" ... ", file))
    save(list = ls(pos = outpos, all.names = TRUE), file = file)
    cat(gettext("done"), "\n", sep = "")
}

cmplib <- function(package, file) {
    package <- as.character(substitute(package))
    pkgname <- paste("package", package, sep = ":")
    pos <- match(pkgname, search());
    if (missing(file))
        file <- paste0(package,".Rc")
    if (is.na(pos)) {
        library(package, character.only = TRUE)
        pos <- match(pkgname, search());
        on.exit(detach(pos=match(pkgname, search())))
    }
    cmpframe(pos, file)
}

cmpfile <- function(infile, outfile, ascii = FALSE, env = .GlobalEnv,
                    verbose = FALSE, options = NULL, version = NULL) {
    if (! is.environment(env) || ! identical(env, topenv(env)))
        stop("'env' must be a top level environment")
    if (missing(outfile)) {
        basename <- sub("\\.[a-zA-Z0-9]$", "", infile)
        outfile <- paste0(basename, ".Rc")
    }
    if (infile == outfile)
        stop("input and output file names are the same")
    forms <- parse(infile)
    nforms <- length(forms)
    srefs <- attr(forms, "srcref")
    if (nforms > 0) {
        expr.needed <- 1000
        expr.old <- getOption("expressions")
        if (expr.old < expr.needed) {
            options(expressions = expr.needed)
            on.exit(options(expressions = expr.old))
        }
        cforms <- vector("list", nforms)
        cenv <- makeCenv(env)
        cntxt <- make.toplevelContext(cenv, options)
        cntxt$env <- addCenvVars(cenv, findLocalsList(forms, cntxt))
        for (i in 1:nforms) {
            e <- forms[[i]]
            sref <- srefs[[i]]
            if (verbose) {
                if (typeof(e) == "language" && e[[1]] == "<-" &&
                    typeof(e[[3]]) == "language" && e[[3]][[1]] == "function")
                    cat(paste0("compiling function \"", e[[2]], "\"\n"))
                else
                    cat(paste("compiling expression", deparse(e, 20)[1],
                              "...\n"))
            }
            if (!mayCallBrowser(e, cntxt))
                cforms[[i]] <- genCode(e, cntxt,
                                       loc = list(expr = e, srcref = sref))
        }
        cat(gettextf("saving to file \"%s\" ... ", outfile))
        .Internal(save.to.file(cforms, outfile, ascii, version))
        cat(gettext("done"), "\n", sep = "")
    }
    else warning("empty input file; no output written");
    invisible(NULL)
}

loadcmp <- function (file, envir = .GlobalEnv, chdir = FALSE) {
    if (!(is.character(file) && file.exists(file)))
        stop(gettextf("file '%s' does not exist", file), domain = NA)
    exprs <- .Internal(load.from.file(file))
    if (length(exprs) == 0)
        return(invisible())
    if (chdir && (path <- dirname(file)) != ".") {
        owd <- getwd()
        on.exit(setwd(owd), add = TRUE)
        setwd(path)
    }
    for (i in exprs) {
        eval(i, envir)
    }
    invisible()
}

enableJIT <- function(level)
    .Internal(enableJIT(level))

compilePKGS <- function(enable)
    .Internal(compilePKGS(enable))

setCompilerOptions <- function(...) {
    options <- list(...)
    nm <- names(options)
    for (n in nm)
        if (! exists(n, compilerOptions))
            stop(gettextf("'%s' is not a valid compiler option", n),
                 domain = NA)
    old <- list()
    newOptions <- as.list(compilerOptions) # copy options
    for (n in nm) {
        op <- options[[n]]
        switch(n,
               optimize = {
                   op <- as.integer(op)
                   if (length(op) == 1 && 0 <= op && op <= 3) {
                       old <- c(old, list(optimize =
                                          compilerOptions$optimize))
                       newOptions$optimize <- op
                   }
               },
               suppressAll = {
                   if (identical(op, TRUE) || identical(op, FALSE)) {
                       old <- c(old, list(suppressAll =
                                          compilerOptions$suppressAll))
                       newOptions$suppressAll <- op
                   }
               },
               suppressNoSuperAssignVar = {
                   if (isTRUE(op) || isFALSE(op)) {
                       old <- c(old, list(
                           suppressNoSuperAssignVar =
                               compilerOptions$suppressNoSuperAssignVar))
                       newOptions$suppressNoSuperAssignVar <- op
                   }
               },
               suppressUndefined = {
                   if (identical(op, TRUE) || identical(op, FALSE) ||
                       is.character(op)) {
                       old <- c(old, list(suppressUndefined =
                                          compilerOptions$suppressUndefined))
                       newOptions$suppressUndefined <- op
                   }
               })
    }
    jitEnabled <- enableJIT(-1)
    if (checkCompilerOptions(jitEnabled, newOptions))
        for(n in names(newOptions)) # commit the new options
            assign(n, newOptions[[n]], compilerOptions)
    invisible(old)
}

.onLoad <- function(libname, pkgname) {
    envAsLogical <- function(varName) {
        value = Sys.getenv(varName)
        if (value == "")
            NA
        else
            switch(value,
                "1"=, "TRUE"=, "true"=, "True"=, "yes"=, "Yes"= TRUE,
                "0"=, "FALSE"=,"false"=,"False"=, "no"=, "No" = FALSE,
                stop(gettextf("invalid environment variable value: %s==%s",
                    varName, value)))
    }
    val <- envAsLogical("R_COMPILER_SUPPRESS_ALL")
    if (!is.na(val))
        setCompilerOptions(suppressAll = val)
    val <- envAsLogical("R_COMPILER_SUPPRESS_UNDEFINED")
    if (!is.na(val))
        setCompilerOptions(suppressUndefined = val)
    val <- envAsLogical("R_COMPILER_SUPPRESS_NO_SUPER_ASSIGN_VAR")
    if (!is.na(val))
        setCompilerOptions(suppressNoSuperAssignVar = val)
    if (Sys.getenv("R_COMPILER_OPTIMIZE") != "")
        tryCatch({
            lev <- as.integer(Sys.getenv("R_COMPILER_OPTIMIZE"))
            if (0 <= lev && lev <= 3)
                setCompilerOptions(optimize = lev)
        }, error = function(e) e, warning = function(w) w)
}

checkCompilerOptions <- function(jitEnabled, options = NULL) {
    optimize <- getCompilerOption("optimize", options)
    if (jitEnabled <= 2 || optimize >= 2)
        TRUE
    else {
        stop(gettextf(
            "invalid compiler options: optimize(==%d)<2 and jitEnabled(==%d)>2",
            optimize, jitEnabled))
        FALSE
    }
}


##
## Disassembler
##

bcDecode <- function(code) {
    n <- length(code)
    ncode <- vector("list", n)
    ncode[[1]] <- code[1] # version number
    i <- 2
    while (i <= n) {
        name<-Opcodes.names[code[i]+1]
        argc<-Opcodes.argc[[code[i]+1]]
        ncode[[i]] <- as.name(name)
        i<-i+1
        if (argc > 0)
            for (j in 1:argc) {
                ncode[[i]]<-code[i]
                i<-i+1
            }
    }
    ncode
}

disassemble <- function(code) {
    .CodeSym <- as.name(".Code")
    disasm.const<-function(x)
        if (typeof(x)=="list" && length(x) > 0 && identical(x[[1]], .CodeSym))
            disasm(x) else x
    disasm <-function(code) {
        code[[2]]<-bcDecode(code[[2]])
        code[[3]]<-lapply(code[[3]], disasm.const)
        code
    }
    if (typeof(code)=="closure") {
        code <- .Internal(bodyCode(code))
        if (typeof(code) != "bytecode")
            stop("function is not compiled")
    }
    dput(disasm(.Internal(disassemble(code))))
}


##
## Experimental Utilities
##

bcprof <- function(expr) {
    .Internal(bcprofstart())
    expr
    .Internal(bcprofstop())
    val <- structure(.Internal(bcprofcounts()),
                     names = Opcodes.names)
    hits <- sort(val[val > 0], decreasing = TRUE)
    pct <- round(100 * hits / sum(hits), 1)
    data.frame(hits = hits, pct = pct)
}

asm <- function(e, gen, env = .GlobalEnv, options = NULL) {
    cenv <- makeCenv(env)
    cntxt <- make.toplevelContext(cenv, options)
    cntxt$env <- addCenvVars(cenv, findLocals(e, cntxt))
    genCode(e, cntxt, gen = gen)
}


##
## Improved subset and subassign handling
##

cmpIndices <- function(indices, cb, cntxt) {
    n <- length(indices)
    for (i in seq_along(indices)) {
        cmp(indices[[i]], cb, cntxt, TRUE)
        if (i < n) cb$putcode(INCLNK.OP)
    }
    if (n == 2) cb$putcode(DECLNK.OP)
    else if (n > 2) cb$putcode(DECLNK_N.OP, n - 1)
}

cmpSubsetDispatch <- function(start.op, dflt.op, e, cb, cntxt) {
    if (dots.or.missing(e) || ! is.null(names(e)) || length(e) < 3)
        cntxt$stop(gettext("cannot compile this expression"), cntxt,
                   loc = cb$savecurloc())
    else {
        oe <- e[[2]]
        if (missing(oe))
            cntxt$stop(gettext("cannot compile this expression"), cntxt,
                       loc = cb$savecurloc())
        ncntxt <- make.argContext(cntxt)
        ci <- cb$putconst(e)
        label <- cb$makelabel()
        cmp(oe, cb, ncntxt)
        cb$putcode(start.op, ci, label)
        indices <- e[-c(1, 2)]
        cmpIndices(indices, cb, ncntxt)
        if (dflt.op$rank) cb$putcode(dflt.op$code, ci, length(indices))
        else cb$putcode(dflt.op$code, ci)
        cb$putlabel(label)
        if (cntxt$tailcall) cb$putcode(RETURN.OP)
        TRUE
    }
}

setInlineHandler("[", function(e, cb, cntxt) {
    if (dots.or.missing(e) || ! is.null(names(e)) || length(e) < 3)
        cmpDispatch(STARTSUBSET.OP, DFLTSUBSET.OP, e, cb, cntxt) ## punt
    else {
        nidx <- length(e) - 2;
        if (nidx == 1)
            dflt.op <- list(code = VECSUBSET.OP, rank = FALSE)
        else if (nidx == 2)
            dflt.op <- list(code = MATSUBSET.OP, rank = FALSE)
        else
            dflt.op <- list(code = SUBSET_N.OP, rank = TRUE)
        cmpSubsetDispatch(STARTSUBSET_N.OP, dflt.op, e, cb, cntxt)
    }
})

setInlineHandler("[[", function(e, cb, cntxt) {
    if (dots.or.missing(e) || ! is.null(names(e)) || length(e) < 3)
        cmpDispatch(STARTSUBSET2.OP, DFLTSUBSET2.OP, e, cb, cntxt) ## punt
    else {
        nidx <- length(e) - 2;
        if (nidx == 1)
            dflt.op <- list(code = VECSUBSET2.OP, rank = FALSE)
        else if (nidx == 2)
            dflt.op <- list(code = MATSUBSET2.OP, rank = FALSE)
        else
            dflt.op <- list(code = SUBSET2_N.OP, rank = TRUE)
        cmpSubsetDispatch(STARTSUBSET2_N.OP, dflt.op, e, cb, cntxt)
    }
})

cmpSubassignDispatch <- function(start.op, dflt.op, afun, place, call, cb,
                                 cntxt) {
    if (dots.or.missing(place) || ! is.null(names(place)) || length(place) < 3)
        cntxt$stop(gettext("cannot compile this expression"), cntxt,
                   loc = cb$savecurloc())
    else {
        ci <- cb$putconst(call)
        label <- cb$makelabel()
        cb$putcode(start.op, ci, label)
        indices <- place[-c(1, 2)]
        cmpIndices(indices, cb, cntxt)
        if (dflt.op$rank) cb$putcode(dflt.op$code, ci, length(indices))
        else cb$putcode(dflt.op$code, ci)
        cb$putlabel(label)
        TRUE
    }
}

setSetterInlineHandler("[<-", function(afun, place, origplace, call, cb, cntxt) {
    if (dots.or.missing(place) || ! is.null(names(place)) || length(place) < 3)
        cmpSetterDispatch(STARTSUBASSIGN.OP, DFLTSUBASSIGN.OP,
                          afun, place, call, cb, cntxt) ## punt
    else {
        nidx <- length(place) - 2
        if (nidx == 1)
            dflt.op <- list(code = VECSUBASSIGN.OP, rank = FALSE)
        else if (nidx == 2)
            dflt.op <- list(code = MATSUBASSIGN.OP, rank = FALSE)
        else
            dflt.op <- list(code = SUBASSIGN_N.OP, rank = TRUE)
        cmpSubassignDispatch(STARTSUBASSIGN_N.OP, dflt.op, afun, place, call,
                             cb, cntxt)
    }
})

setSetterInlineHandler("[[<-", function(afun, place, origplace, call, cb, cntxt) {
    if (dots.or.missing(place) || ! is.null(names(place)) || length(place) < 3)
        cmpSetterDispatch(STARTSUBASSIGN2.OP, DFLTSUBASSIGN2.OP,
                          afun, place, call, cb, cntxt) ## punt
    else {
        nidx <- length(place) - 2
        if (nidx == 1)
            dflt.op <- list(code = VECSUBASSIGN2.OP, rank = FALSE)
        else if (nidx == 2)
            dflt.op <- list(code = MATSUBASSIGN2.OP, rank = FALSE)
        else
            dflt.op <- list(code = SUBASSIGN2_N.OP, rank = TRUE)
        cmpSubassignDispatch(STARTSUBASSIGN2_N.OP, dflt.op, afun, place, call,
                             cb, cntxt)
    }
})

cmpSubsetGetterDispatch <- function(start.op, dflt.op, call, cb, cntxt) {
    if (dots.or.missing(call) || ! is.null(names(call)) || length(call) < 3)
        cntxt$stop(gettext("cannot compile this expression"), cntxt,
                   loc = cb$savecurloc())
    else {
        ci <- cb$putconst(call)
        end.label <- cb$makelabel()
        cb$putcode(DUP2ND.OP)
        cb$putcode(start.op, ci, end.label)
        indices <- call[-c(1, 2)]
        cmpIndices(indices, cb, cntxt)
        if (dflt.op$rank)
            cb$putcode(dflt.op$code, ci, length(indices))
        else
            cb$putcode(dflt.op$code, ci)
        cb$putlabel(end.label)
        cb$putcode(SWAP.OP)
        TRUE
    }
}

setGetterInlineHandler("[", function(call, cb, cntxt) {
    if (dots.or.missing(call) || ! is.null(names(call)) || length(call) < 3)
        cmpGetterDispatch(STARTSUBSET.OP, DFLTSUBSET.OP, call, cb, cntxt)
    else {
        nidx <- length(call) - 2;
        if (nidx == 1)
            dflt.op <- list(code = VECSUBSET.OP, rank = FALSE)
        else if (nidx == 2)
            dflt.op <- list(code = MATSUBSET.OP, rank = FALSE)
        else
            dflt.op <- list(code = SUBSET_N.OP, rank = TRUE)
        cmpSubsetGetterDispatch(STARTSUBSET_N.OP, dflt.op, call, cb, cntxt)
    }
})

setGetterInlineHandler("[[", function(call, cb, cntxt) {
    if (dots.or.missing(call) || ! is.null(names(call)) || length(call) < 3)
        cmpGetterDispatch(STARTSUBSET2.OP, DFLTSUBSET2.OP, call, cb, cntxt)
    else {
        nidx <- length(call) - 2;
        if (nidx == 1)
            dflt.op <- list(code = VECSUBSET2.OP, rank = FALSE)
        else if (nidx == 2)
            dflt.op <- list(code = MATSUBSET2.OP, rank = FALSE)
        else
            dflt.op <- list(code = SUBSET2_N.OP, rank = TRUE)
        cmpSubsetGetterDispatch(STARTSUBSET2_N.OP, dflt.op, call, cb, cntxt)
    }
})

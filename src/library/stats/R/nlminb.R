##nlminb.control <-
##    function(eval.max = NULL, iter.max = NULL, abs.tol = NULL, rel.tol = NULL,
##             x.tol = NULL, step.min = NULL, step.max = NULL, scale.init = NULL,
##             scale.upd = NULL, scale.fac = NULL, scale.tol = NULL, scale.mod = NULL,
##             sing.tol = NULL, sing.step = NULL, rel.err = NULL, diff.g = NULL)
##    list(eval.max = eval.max, iter.max = iter.max, abs.tol = abs.tol, rel.tol = 
##         rel.tol, x.tol = x.tol, step.min = step.min, step.max = step.max, 
##         scale.init = scale.init, scale.upd = scale.upd, scale.fac = scale.fac,
##         scale.tol = scale.tol, scale.mod = scale.mod, sing.tol = sing.tol,
##         sing.step = sing.step, rel.err = rel.err, diff.g = diff.g)

nlminb <-
    function(start, objective, gradient = NULL, hessian = NULL,
             scale = 1, control = list(), lower =  - Inf, upper = Inf, ...)
{
    ## Establish the working vectors and check and set options
    n <- length(par <- as.double(start))
    iv <- integer(78 + 3 * n)
    v <- double(130 + (n * (n + 27)) / 2)
    .Call("port_ivset", iv, v, PACKAGE = "stats")
    if (length(control)) {
        nms <- names(control)
        if (!is.list(control) || is.null(nms))
            stop("control argument must be a named list")
        cpos <- c(eval.max = 17, iter.max = 18, trace = 19, abs.tol = 31,
                  rel.tol = 32, x.tol = 33, step.min = 34, step.max = 35,
                  scale.init = 38, sing.tol = 37, diff.g = 42)
        pos <- pmatch(nms, names(cpos))
        if (any(nap <- is.na(pos))) {
            warn(paste("unrecognized control element(s) named `",
                       paste(nms[nap], collapse = ", "),
                       "' ignored", sep = ""))
            pos <- pos[!nap]
            control <- control[!nap]
        }
        ivpars <- pos < 4
        if (any(ivpars))
            iv[cpos[pos[ivpars]]] <- as.integer(unlist(control[ivpars]))
        if (any(!ivpars))
            v[cpos[pos[!ivpars]]] <- as.double(unlist(control[!ivpars]))
    }
    
    ## Establish the objective function and its environment
    obj <- quote(objective(.par, ...))
    rho <- new.env(parent = environment())
    assign(".par", par, envir = rho)

    ## Create values of other arguments if needed
    grad <- hess <- low <- upp <- NULL
    if (!is.null(gradient)) {
        grad <- quote(gradient(.par, ...))
        if (!is.null(hessian)) {
            if (is.logical(hessian))
                stop("Logical `hessian' argument not allowed.  See documentation.")
            hess <- quote(hessian(.par, ...))
        }
    }
    if (any(lower != -Inf) || any(upper != Inf)) { 
        low <- rep(as.double(lower), length = length(par))
        upp <- rep(as.double(upper), length = length(par))
    }
    
    ## Do the optimization
    .Call("port_nlminb", obj, grad, hess, rho, low, upp,
          d = rep(as.double(scale), length = length(par)), iv, v, PACKAGE = "stats")

    ans <- list(par = get(".par", envir = rho))
    ans$objective <- v[10]
    ans$convergence <- as.integer(if (iv[1] %in% 3:6) 0 else 1)
    ans$message <-
        switch(as.character(iv[1]),
               "3" = "X-convergence (3)",
               "4" = "relative convergence (4)",
               "5" = "both X-convergence and relative convergence (5)",
               "6" = "absolute function convergence (6)",

               "7" = "singular convergence (7)",
               "8" = "false convergence (8)",
               "9" = "function evaluation limit reached without convergence (9)",
               "10" = "iteration limit reached without convergence (9)",
               "14" = "storage has been allocated (?) (14)",

               "15" = "LIV too small (15)",
               "16" = "LV too small (16)",
               "63" = "fn cannot be computed at initial par (63)",
               "65" = "gr cannot be computed at initial par (65)")
    if (is.null(ans$message))
        ans$message <-
            paste("See PORT documentation.  Code (", iv[1], ")", sep = "")
    ans$iterations <- iv[31]
    ans$evaluations <- c("function" = iv[6], gradient = iv[30])
    ans
}

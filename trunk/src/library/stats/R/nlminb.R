#  File src/library/stats/R/nlminb.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2012 The R Core Team
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

##' used here and in nls(... algorithm = "port")
port_msg <- function(iv1) {
    switch(as.character(iv1),
	   "3" = "X-convergence (3)",
	   "4" = "relative convergence (4)",
	   "5" = "both X-convergence and relative convergence (5)",
	   "6" = "absolute function convergence (6)",

	   "7" = "singular convergence (7)",
	   "8" = "false convergence (8)",
	   "9" = "function evaluation limit reached without convergence (9)",
	   "10" = "iteration limit reached without convergence (10)",
	   "14" = "storage only has been allocated (14)",

	   "15" = "LIV too small (15)",
	   "16" = "LV too small (16)",

	   "63" = "fn cannot be computed at initial par (63)",
	   "65" = "gr cannot be computed at initial par (65)",

	   "300" = "initial par violates constraints",
	   ## otherwise:
	   sprintf("See PORT documentation.  Code (%d)", iv1))
}

## PORT  iv[] and v[] indices for setting and getting info :
port_cpos <-
    c(## iv[]:
      ## MXFCAL       MXITER         OUTLEV  (port.c)
      eval.max = 17L, iter.max = 18L, trace = 19L,
                      maxiter  = 18L,
      ##  v[]:
      ## AFCTOL      RFCTOL         XCTOL        XFTOL
      abs.tol = 31L, rel.tol = 32L, x.tol = 33L, xf.tol = 34L,
      ## LMAX0        LMAXS           SCTOL
      step.min = 35L, step.max = 36L, sing.tol = 37L,
      ## DINIT          ETA0 (for nlminb *only*)
      scale.init = 38L, diff.g = 42L)
## NB: until R 2.12.1, "step.min" was 34 instead of 35

## and for "output" v[]: see in ../src/port.c, also for NITER = 31 (below):
port_v_nms <-
    c(NREDUC = 6L, PREDUC = 7L, F = 10L, FDIF = 11L,
      FLSTGD = 12L, GTSLST = 14L,
      PLSTGD = 15L, RADFAC = 16L, DSTSAV = 18L)
port_get_named_v <- function(v) {
    setNames(v[port_v_nms], names(port_v_nms))
}


nlminb <-
    function(start, objective, gradient = NULL, hessian = NULL, ...,
             scale = 1, control = list(), lower =  - Inf, upper = Inf)
{
    ## Establish the working vectors and check and set options
    par <- setNames(as.double(start), names(start))
    n <- length(par)
    iv <- integer(78 + 3 * n)
    v <- double(130 + (n * (n + 27)) / 2)
    .Call(C_port_ivset, 2, iv, v)
    if (length(control)) {
 	nms <- names(control)
	if (!is.list(control) || is.null(nms))
	    stop("'control' argument must be a named list")
	pos <- pmatch(nms, names(port_cpos))
	if (any(nap <- is.na(pos))) {
            warning(sprintf(ngettext(length(nap),
                                     "unrecognized control element named %s ignored",
                                     "unrecognized control elements named %s ignored"),
                            paste(sQuote(nms[nap]), collapse = ", ")),
                    domain = NA)
	    pos <- pos[!nap]
	    control <- control[!nap]
	}
	ivpars <- pos <= 4 ; vpars <- !ivpars
	if (any(ivpars))
	    iv[port_cpos[pos[ivpars]]] <- as.integer(unlist(control[ivpars]))
	if (any(vpars))
	    v [port_cpos[pos[ vpars]]] <- as.double(unlist(control[vpars]))
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
                stop("logical 'hessian' argument not allowed.  See documentation.")
            hess <- quote(hessian(.par, ...))
        }
    }
    if (any(lower != -Inf) || any(upper != Inf)) {
        low <- rep_len(as.double(lower), length(par))
        upp <- rep_len(as.double(upper), length(par))
    } else low <- upp <- numeric()

    ## Do the optimization
    .Call(C_port_nlminb, obj, grad, hess, rho, low, upp,
          d = rep_len(as.double(scale), length(par)), iv, v)

    iv1 <- iv[1L]
    list(par = get(".par", envir = rho),
	 objective = v[10L],
	 convergence = (if (iv1 %in% 3L:6L) 0L else 1L),
	 iterations = iv[31L],
	 evaluations = c("function" = iv[6L], "gradient" = iv[30L]),
	 "message" = if(19 <= iv1 && iv1 <= 43) {
	     if(any(B <- iv1 == port_cpos))
		 sprintf("'control' component '%s' = %g, is out of range",
			 names(port_cpos)[B], v[iv1])
	     else
		 sprintf("V[IV[1]] = V[%d] = %g is out of range (see PORT docu.)",
			 iv1, v[iv1])
	 } else port_msg(iv1))
}

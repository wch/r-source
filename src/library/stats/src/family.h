/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2005   The R Development Core Team.
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301  USA
 */

#ifndef R_STATS_FAMILY_H
#define R_STATS_FAMILY_H

#ifdef ENABLE_NLS
#include <libintl.h>
#define _(String) dgettext ("stats", String)
#else
#define _(String) (String)
#endif

SEXP logit_link(SEXP mu);
SEXP logit_linkinv(SEXP eta);
SEXP logit_mu_eta(SEXP eta);
SEXP binomial_dev_resids(SEXP y, SEXP mu, SEXP wt);

#endif

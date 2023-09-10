/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2023 The R Core Team
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
 *  along with this program; if not, a copy is available at
 *  https://www.R-project.org/Licenses/
 */

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include <Defn.h>

#include <stdlib.h>
#include <string.h>
#ifdef HAVE_DLFCN_H
#include <dlfcn.h>
#endif


attribute_hidden SEXP R_flexiblas_info(void)
{
    void *fcb_addr = NULL;

#if defined(HAVE_DLSYM) \
    && defined(HAVE_DECL_RTLD_DEFAULT) && HAVE_DECL_RTLD_DEFAULT
    char *fcb_name = "flexiblas_current_backend";
    fcb_addr = dlsym(RTLD_DEFAULT, fcb_name);
#endif

    if (fcb_addr != NULL) {
	char *prefix = "FlexiBLAS ";
	size_t prefix_len = strlen(prefix);
	int (*fcb) (char *, size_t);
	size_t size = 64;
	char *name = NULL;

#ifdef __clang__
# pragma clang diagnostic push
# pragma clang diagnostic ignored "-Wpedantic"
#elif defined __GNUC__
# pragma GCC diagnostic push
# pragma GCC diagnostic ignored "-Wpedantic"	
#endif
	/* ISO C forbids assignment between function pointer and 'void *',
	   but this is how dlsym() returns a function pointer. */
	fcb = fcb_addr;
#ifdef __clang__
# pragma clang diagnostic pop
#elif defined __GNUC__
# pragma GCC diagnostic pop
#endif
	for(;;) {
	    if (name)
		free(name);
	    size *= 2;
	    name = (char *) malloc(prefix_len + size);
	    if (!name)
		break;
	    strcpy(name, prefix);
	    int res = fcb(name + prefix_len, size);
	    if (res < size - 1) {
		SEXP ans = mkChar(name);
		free(name);
		return ans;
	    }
	}
    }
    return R_NilValue;
}


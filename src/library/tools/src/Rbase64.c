/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2026   The R Core Team.
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
#undef _

#include "tools.h"

/* BASE64 ENCODING */

static const char b64_alphabet[] =
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";


/*
 * 1. Groups 3 bytes into a 24-bit big-endian cluster.
 * 2. Shifts by 18, 12, 6, 0 bits and masks by 0x3F (63).
 * 3. Handles 1 or 2 padding '=' characters if len % 3 != 0.

 *** Equivalent R version

 base64_encode <-
 function(x) # x is a raw vector
 {
    if(len <- length(x) == 0L) return("")
    ## group octets into 24-bit big-endian clusters with zero padding
    x <- matrix(c(x, rep(0, c(0, 2, 1)[1 + len %% 3])), 3)
    ## split into sextets
    x <- outer(
        6 * 3:0, colSums(x * 2^(8 * 2:0)),
        function(shift, cluster) (cluster %/% 2^shift) %% 2^6
    )
    ## last 1 or 2 bytes may need to be a padding byte
    if (rem <- len %% 3) x[length(x) - 0:(2-rem)] <- 64
    ## encode sextets using Base64 alphabet
    paste(c(LETTERS, letters, 0:9, '+', '/', '=')[x + 1], collapse = '')
 }
 
 */

static void raw2base64(const unsigned char *src, R_xlen_t len, char *dest) {
    const int rem = len % 3;
    const unsigned char *end = src + len - rem;

    /* Loop over [ len / 3 ] full blocks */
    for ( ; src < end; src += 3) {
        /* Pack 3 octets into a 24-bit big-endian cluster */
        const uint32_t cluster = ((uint32_t)src[0] << 16) | ((uint32_t)src[1] << 8) | src[2];
        /* Encode the cluster into sextets */
        *dest++ = b64_alphabet[(cluster >> 18) & 0x3F];
        *dest++ = b64_alphabet[(cluster >> 12) & 0x3F];
        *dest++ = b64_alphabet[(cluster >> 6)  & 0x3F];
        *dest++ = b64_alphabet[cluster & 0x3F];
    }

    /* Last partial cluster with padding */
    if (rem) {
        const uint32_t cluster = ((uint32_t)src[0] << 16) | (rem == 2 ? ((uint32_t)src[1] << 8) : 0);
        *dest++ = b64_alphabet[(cluster >> 18) & 0x3F];
        *dest++ = b64_alphabet[(cluster >> 12) & 0x3F];
        *dest++ = (rem == 2) ? b64_alphabet[(cluster >> 6)  & 0x3F] : '=';
        *dest++ = '=';
    }

    *dest = '\0';
}



SEXP Rbase64_encode(SEXP rawvec) {

    /* we assume len > 0 (len-0 caught in R) */
    R_xlen_t len = XLENGTH(rawvec);  
    const int max_len = (INT_MAX/4)*3;
    if (len > max_len)
        Rf_error("argument length (%lld) exceeds maximum supported length (%d)",
                 (long long)len, max_len);
    R_xlen_t num_blocks = (len + 2) / 3;
    R_xlen_t out_len = num_blocks * 4;

    /* Allocate buffer for encoded string + null terminator */
    char *buf = (char *) R_alloc(out_len + 1, sizeof(char));
    const unsigned char *src = RAW(rawvec);
    raw2base64(src, len, buf);
    SEXP ret = PROTECT(Rf_allocVector(STRSXP, 1));
    SET_STRING_ELT(ret, 0, Rf_mkCharLenCE(buf, (int)out_len, CE_BYTES));
    UNPROTECT(1);
    return ret;
}



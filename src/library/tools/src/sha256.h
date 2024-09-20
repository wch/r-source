/*  SHA256 implementation.
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2003-2024   The R Core Team.
 *  Based on code released into the Public Domain by
 *  Ulrich Drepper <drepper@redhat.com>.
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

#ifndef R_SHA256_H
#define R_SHA256_H 1

#include <sys/types.h>
#include <stdint.h>
#include <stdio.h> /* for FILE I/O in Rsha256_stream */

/* Structure to save state of computation between the single steps.  */
struct sha256_ctx
{
  uint32_t H[8];

  uint32_t total[2];
  uint32_t buflen;
  char buffer[128];	/* NB: always correctly aligned for uint32_t.  */
};

extern void  Rsha256_init_ctx(struct sha256_ctx *ctx);
extern void  Rsha256_process_bytes(const void *buffer, size_t len, struct sha256_ctx *ctx);
extern void* Rsha256_finish_ctx(struct sha256_ctx *ctx, void *resbuf);
extern int   Rsha256_stream(FILE *stream, void *resblock);

#endif

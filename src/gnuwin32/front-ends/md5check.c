/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2004  R Development Core Team
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
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

#include <R.h>
#include "md5.h"
#include <stdlib.h>
#include <stdio.h>
#include <strings.h>

int main (int argc, char **argv)
{
    FILE *fp, *fp2;
    int j, wrong = 0, res;
    char *p, line[500]; /* < MAX_PATH + 32+ some */
    char onfile[33], out[33];
    unsigned char resblock[16];

    if(argc != 2) {
	fprintf(stderr, "Usage: md5check /path/to/MD5/file\n");
	exit(1);
    }
    fp = fopen(argv[1], "r");
    if(!fp) {
	fprintf(stderr, "Cannot open file %s\n", argv[1]);
	exit(2);
    }
    onfile[32] = '\0';
    while(fgets(line, 500, fp)) {
	p = line + (strlen(line) - 1);
	if(*p == '\n') *p = '\0';
#ifdef DEBUG
	printf("%s: ", line+34);
#endif
	fp2 = fopen(line+34, "rb");
	if(!fp2) {
#ifdef DEBUG
	    printf("missing\n");
#endif
	    continue;
	}
	strncpy(onfile, line, 32);
	res = md5_stream(fp2, &resblock);
	if(res) {
#ifdef DEBUG
	    printf("md5 failed\n");
#endif
	    continue;
	} else {
	    for(j = 0; j < 16; j++) sprintf (out+2*j, "%02x", resblock[j]);
	    if(strcmp(onfile, out) == 0) {
#ifdef DEBUG
		printf("OK\n");
#endif
	    } else {
#ifdef DEBUG
		printf("changed\n");
		printf("  %s vs %s\n", onfile, out);
#endif
		fprintf(stderr, "%s: changed", line+34);
		wrong++;
	    }
	}
	fclose(fp2);
    }
    fclose(fp);
    if(wrong) {
	fprintf(stderr, "WARNING: %d files have been changed\n", wrong);
	exit(10);
    }
    fprintf(stderr, "No errors detected\n");
    exit(0);
}

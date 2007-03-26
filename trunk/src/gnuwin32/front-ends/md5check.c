/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2004-7  R Development Core Team
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

#define ROL_UNUSED
#include "md5.h"
#include <stdlib.h>
#include <stdio.h>
#include <strings.h>

static char nameList[3500][80];
static int nnames = 0;


void read_unist_file(char* fname)
{
    FILE *fp;
    int Version, NumRecs, EndOffset, size, res, i;
    unsigned short Typ;
    char *buf, *p, *q, name[1000], root[256];
    int haveRoot = 0;

    fp = fopen(fname, "rb");
    if(!fp) {
	fprintf(stderr, "Cannot open file %s\n", fname);
	exit(2);
    }
    fseek(fp, 320, SEEK_SET);
    fread(&Version, 4, 1, fp);
    /* 4.2.x is 15, 5.0.6 is 33 5.1.3 is 42 5.1.11 is 46 */
    if(Version > 46)
	fprintf(stderr, "Version %d of the uninst format is new and may not be supported\n", Version);
    fread(&NumRecs, 4, 1, fp);
    fread(&EndOffset, 4, 1, fp);
    buf = malloc(EndOffset); p = buf;
    fseek(fp, 4*29, SEEK_CUR);
    /* now have periodic CRCs */
    while(1) {
	fread(&size, 4, 1, fp);
	if(!size) break;
	fseek(fp, 8, SEEK_CUR); /* skip CRC? */
	res = fread(p, 1, size, fp);
	if(res < size) break;
	p += size;
    }
    p = buf;
    for(i = 0; i < NumRecs; i++) {
	unsigned char X;
	unsigned short X2;
	unsigned int X4;
	int found = 0;
	memcpy(&Typ, p, 2); p += 2;
	p += 8;
	while(++found) {
	    X = *p++;
	    if(X < 253) X4 = X;
	    else if(X == 253) {
		memcpy(&X2, p, 2); p += 2;
		X4 = X2;
	    } else if(X == 254) {
		memcpy(&X4, p, 4); p += 4;
	    } else break;
	    if(Typ == 129 && !haveRoot) {
		haveRoot = 1;
		strncpy(root, p, X4);
		*(root + X4) = '\0';
	    }
	    if(Typ == 130 && found == 1) {
		strncpy(name, p, X4);
		*(name + X4) = '\0';
		strcpy(nameList[nnames], name+strlen(root) + 1);
		for(q = nameList[nnames]; *q; q++)
		    if(*q == '\\') *q ='/';
		/* printf("%s\n", nameList[nnames]); */
		nnames++;
	    }
	    p += X4;
	}
    }
    fclose(fp);
    free(buf);
}

#include <direct.h> /* for chdir */

char *dirname(const char *fname)
{
    static char buf[500];
    int i;
    
    strcpy(buf, fname);
    for(i = strlen(buf) - 1; i >= 0; i--)
	if(buf[i] == '/' || buf[i] == '\\') { buf[i] = '\0'; return buf; }
    return(".");
}


int main (int argc, char **argv)
{
    FILE *fp, *fp2;
    int j, wrong = 0, miss = 0, res;
    char *p, fname[500], line[500]; /* < MAX_PATH + 32+ some */
    char onfile[33], out[33];
    unsigned char resblock[16];

    if(argc < 2) strcpy(fname, "../MD5"); else strcpy(fname, argv[1]);
    fp = fopen(fname, "r");
    if(!fp) {
	fprintf(stderr, "Cannot open file %s\n", fname);
	exit(2);
    }
    chdir(dirname(fname));

    if(access("unins000.dat", 4) == 0) read_unist_file("unins000.dat");

    onfile[32] = '\0';
    while(fgets(line, 500, fp)) {
	p = line + (strlen(line) - 1);
	if(*p == '\n') *p = '\0';
#ifdef DEBUG
	printf("%s: ", line+34);
#endif
	/* the next two files get altered during installation */
	if(strcmp(line+34, "etc/Rconsole") == 0) continue;
	if(strcmp(line+34, "etc/Rprofile.site") == 0) continue;	
	if(line[33] == '*')
	    fp2 = fopen(line+34, "rb");
	else
	    fp2 = fopen(line+34, "r");
	if(!fp2) {
	    int i, found = 0;
	    for(i = 0; i < nnames; i++) {
		if(strcmp(line+34, nameList[i]) == 0) {
		    found = 1;
		    break;
		}
	    }
	    
#ifdef DEBUG
	    printf("missing\n");
#endif
	    if(found) {
		fprintf(stderr, "file %s: missing\n", line+34);
		miss++;
	    }
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
	    for(j = 0; j < 16; j++) snprintf (out+2*j, 2, "%02x", resblock[j]);
	    out[32] = '\0';
	    if(strcmp(onfile, out) == 0) {
#ifdef DEBUG
		printf("OK\n");
#endif
	    } else {
#ifdef DEBUG
		printf("changed\n");
		printf("  %s vs %s\n", onfile, out);
#endif
		fprintf(stderr, "file %s: changed\n", line+34);
		wrong++;
	    }
	}
	fclose(fp2);
    }
    fclose(fp);
    if(miss) fprintf(stderr, "WARNING: %d files missing\n", miss);
    if(wrong) fprintf(stderr, "WARNING: %d files changed\n", wrong);
    if(wrong || miss) {
	fprintf(stderr, "Press ENTER to finish");
	getc(stdin);
	exit(10);
    }

    fprintf(stderr, "No errors detected\n");
    exit(0);
}

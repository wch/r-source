/*
 *  Copyright (C) Martin Maechler, 1994, 1998
 *
 *  This code is to be understood as Free Software in the sense of
 *  GNU Copyright.  You can freely use and redistribute this software
 *  for non-commercial purposes only.  You even are allowed to enhance
 *  and improve it as much as you like, as long as you distribute the
 *  source code together with the software.
 *
 *  I want you to preserve the copyright of the original author(s),
 *  and encourage you to send me any improvements by e-mail. (MM).
 *
 *  Originally from Bill Dunlap
 *  bill@stat.washington.edu
 *  Wed Feb 21, 1990
 *
 *  Much improved by Martin Maechler
 *
 *  Patched by Friedrich.Leisch@ci.tuwien.ac.at
 *  Fri Nov 22, 1996
 *
 *  Some fixes by Ross Ihaka
 *  ihaka@stat.auckland.ac.nz
 *  Sat Dec 21, 1996
 *  Integer arguments changed from "long" to "int"
 *  Bus error due to non-writable strings fixed
 *
 *	type	"real" or "integer" (R - numeric `mode').
 *
 *	width	The total field width; width < 0 means to left justify
 *		the number in this field (equivalent to flag = "-").
 *		It is possible that the result will be longer than this,
 *		but that should only happen in reasonable cases.
 *
 *	digits	The desired number of digits after the decimal point.
 *		digits < 0 uses the default for C, namely 6 digits.
 *
 *	format	"d" (for integers) or "f", "e","E", "g", "G" (for 'real')
 *		"f" gives numbers in the usual "xxx.xxx" format;
 *		"e" and "E" give n.ddde<nn> or n.dddE<nn> (scientific format);
 *		"g" and "G" puts them into scientific format if it saves
 *		space to do so.
 *
 *	flag	Format modifier as in K&R "C", 2nd ed., p.243;
 *		e.g., "0" pads leading zeros; "-" does left adjustment
 *		the other possible flags are  "+", " ", and "#".
 *	  New (Feb.98): if flag has more than characters, all are passed..
 */

#include <stdio.h>
#include <string.h>

void str_signif(char *x, int *n, char **type, int *width, int *digits,
	char **format, char **flag, char **result)
{
	int wid = *width;
	int dig = *digits;
	int i, nn = *n;
	void error(char*);
	char form[12]; /*-- Really, instead :
	  char *form;
	  form = Calloc(strlen(*flag)+strlen(*format)+ 4+1, char);
	*/

	if (wid == 0) error("Width cannot be zero\n");

	if (strcmp("d", *format) == 0) {
		if (strlen(*flag) == 0) strcpy(form, "%*d");
		else {
			strcpy(form, "%");
			strcat(form, *flag);
			strcat(form, "*d");
		}
		if (strcmp("integer", *type) == 0)
			for (i=0; i < nn; i++)
			 sprintf(result[i], form, wid, (int)((long *)x)[i]);
		else
			error("`type' must be \"integer\" for  \"d\"-format\n");
	}
	else {
		if (strlen(*flag) == 0) {
			strcpy(form, "%*.*");
		} else {
			strcpy(form, "%");
			strcat(form, *flag);
			strcat(form, "*.*");
		}
		strcat(form, *format);

		if (strcmp("real", *type) == 0)
			for (i=0; i < nn; i++)
			 sprintf(result[i], form, wid, dig, ((double *)x)[i]);
		else
			error("`type' must be \"real\" for this format\n");
	}
}

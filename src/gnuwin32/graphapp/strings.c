/*
 * GraphApp - Cross-Platform Graphics Programming Library.
 *
 * File: strings.c -- local string functions.
 * Platform: Windows  Version: 2.44  Date: 1998/09/09
 *
 * Version: 1.00  Changes: Original version by Lachlan Patrick.
 * Version: 2.00  Changes: Added del_string.
 * Version: 2.30  Changes: Now uses array(), not malloc().
 * Version: 2.44  Changes: Now uses array() instead of static char[].
 */

/* Copyright (C) 1993-1998 Lachlan Patrick

   This file is part of GraphApp, a cross-platform C graphics library.

   GraphApp is free software; you can redistribute it and/or modify it
   under the terms of the GNU Library General Public License.
   GraphApp is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY.

   See the file COPYLIB.TXT for details.
*/

#include "internal.h"

/*
 *  Create and return a newly allocated copy of a string.
 *  Null strings are converted into empty strings first.
 */
char *new_string(char *src)
{
	char *str;
	if (! src)
		src = "";
	str = array (string_length(src), char);
	if (str)
		copy_string(str, src);
	return str;
}

void del_string(char *str)
{
	discard(str);
}

/*
 *  String length function.
 *  If the string is null, the length is defined to be zero.
 */
long string_length(char *s)
{
	long len = 0;
	if (s)
		while (s[len])
			len++;
	return len;
}

/*
 *  Copy a string.
 *  Same as strcpy except it avoids doing anything to null strings.
 */
void copy_string(char *dest, char *src)
{
	int len;
	if ((dest) && (src)) {
		for (len=0; src[len]; len++)
			dest[len] = src[len];
		dest[len] = '\0';
	}
}

/*
 *  String comparison function.
 *  Null == null, null == "", "" == null, otherwise same as strcmp.
 */
int compare_strings(char *s1, char *s2)
{
	int len, diff;

	if (s1 == s2)
		return 0;
	else if (s1 == NULL) { /* s2 cannot be null since s1 != s2 */
		if (s2[0] == '\0')
			return 0;
		return -1;
	}
	else if (s2 == NULL) { /* s1 is not null */
		if (s1[0] == '\0')
			return 0;
		return +1;
	}
	else {
		for (len=0; s1[len] || s2[len]; len++) {
			diff = s1[len] - s2[len];
			if (diff)
				return diff;
		}
		return 0;
	}
}

/*
 *  Append one string to another, and return the result.
 *  This function correctly handles the situation where one or both
 *  of the parameters are temporary strings returned by an earlier
 *  call to this function.
 */
char *add_strings(char *s1, char *s2)
{
	static char *buffer = NULL;
	char *prev;
	int len1, len2;

	prev = buffer;

	if (! s1)
		return s2;
	else if (! s2)
		return s1;

	len1 = string_length(s1);
	len2 = string_length(s2);

	buffer = array (len1+len2, char);

	if (buffer != NULL) {
		copy_string(buffer, s1);
		copy_string(buffer+len1, s2);
	}

	if (prev)
		discard(prev); /* free previous string buffer */

	return buffer;
}

/*
 *  Convert a char to a string, return the result in a static buffer.
 */
char *char_to_string(char ch)
{
	static char str[2];
	str[0] = ch;
	str[1] = '\0';
	return str;
}

/*
 *  Convert an integer to a string, return the result in a static buffer.
 */
char *int_to_string(long i)
{
	static char *str = NULL;
	if (str == NULL)
		str = array(40,char);
	sprintf(str, "%ld", i);
	return str;
}

/*
 *  Convert a floating point number to a string,
 *  return the result in a static buffer.
 */
char *float_to_string(float f)
{
	static char *str = NULL;
	if (str == NULL)
		str = array(40,char);
	sprintf(str, "%g", f);
	return str;
}

/*
 *  Case-insensitive string comparison function.
 */
PROTECTED
int string_diff(char *s, char *t)
{
	int ch1, ch2;
	int diff = 0;

	while ((diff == 0) && ((*s != '\0') || (*t != '\0')))
	{
		ch1 = *s; ch2 = *t;
		if ((ch1 >= 'A') && (ch1 <= 'Z'))
			ch1 = ch1 - 'A' + 'a';
		if ((ch2 >= 'A') && (ch2 <= 'Z'))
			ch2 = ch2 - 'A' + 'a';
		diff = (ch1 - ch2);
		s++; t++;
	}

	return diff;
}

/*
 *  Produce a new string which has CR-LF instead of just '\n'
 *  to mark end of line. The returned string must later be freed.
 */
PROTECTED
char *to_dos_string(char *text)
{
	char *s;
	char *newstr;
	char prev;
	long length = 0;

	if (!text)
		return text;
	prev = '\0';
	for (s = text; *s != '\0'; s++) {
		length++;
		if ((*s == '\n') && (prev != '\r'))
			length++;
		prev = *s;
	}

	newstr = array (length, char);

	prev = '\0';
	for (s = newstr; *text != '\0'; s++, text++) {
		if ((*text == '\n') && (prev != '\r')) {
			*s = '\r';
			s++;
		}
		*s = *text;
		prev = *s;
	}
	*s = '\0';

	return newstr;
}

/*
 *  Strip carriage returns from a string. The resulting
 *  string must later be freed.
 */
PROTECTED
char *to_c_string(char *text)
{
	char *s;
	char *newstr;
	long length = 0;

	if (!text)
		return text;
	for (s = text; *s != '\0'; s++) {
		length++;
		if ((*s == '\r') && (*(s+1) == '\n'))
			length--;
	}

	newstr = array (length, char);

	for (s = newstr; *text != '\0'; s++, text++) {
		if ((*text == '\r') && (*(text+1) == '\n'))
			text++;
		*s = *text;
	}
	*s = '\0';

	return newstr;
}

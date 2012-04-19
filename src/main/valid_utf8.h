/* Private version of _pcre_valid_utf */

/*************************************************
*      Perl-Compatible Regular Expressions       *
*************************************************/

/* PCRE is a library of functions to support regular expressions whose syntax
and semantics are as close as possible to those of the Perl 5 language.

                       Written by Philip Hazel
           Copyright (c) 1997-2012 University of Cambridge

-----------------------------------------------------------------------------
Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright notice,
      this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above copyright
      notice, this list of conditions and the following disclaimer in the
      documentation and/or other materials provided with the distribution.

    * Neither the name of the University of Cambridge nor the names of its
      contributors may be used to endorse or promote products derived from
      this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
POSSIBILITY OF SUCH DAMAGE.
-----------------------------------------------------------------------------
*/

/* This module contains an internal function for validating UTF-8 character
strings. */


/*************************************************
*         Validate a UTF-8 string                *
*************************************************/

/* This function is called (optionally) at the start of compile or match, to
check that a supposed UTF-8 string is actually valid. The early check means
that subsequent code can assume it is dealing with a valid string. The check
can be turned off for maximum performance, but the consequences of supplying an
invalid string are then undefined.

Originally, this function checked according to RFC 2279, allowing for values in
the range 0 to 0x7fffffff, up to 6 bytes long, but ensuring that they were in
the canonical format. Once somebody had pointed out RFC 3629 to me (it
obsoletes 2279), additional restrictions were applied. The values are now
limited to be between 0 and 0x0010ffff, no more than 4 bytes long, and the
subrange 0xd000 to 0xdfff is excluded. However, the format of 5-byte and 6-byte
characters is still checked.

*/

static int
valid_utf8(const char *string, size_t length) // R change int->size_t
{
    const char *p;

    for (p = string; length-- > 0; p++) {
	int ab, c, d;
	c = (unsigned char)*p;
	if (c < 128) continue;                /* ASCII character */
	if (c < 0xc0) return 1;               /* Isolated 10xx xxxx byte */
	if (c >= 0xfe) return 1;             /* Invalid 0xfe or 0xff bytes */

	ab = utf8_table4[c & 0x3f];     /* Number of additional bytes */
	if (length < ab) return 1;
	length -= ab;                         /* Length remaining */

	/* Check top bits in the second byte */

	if (((d = *(++p)) & 0xc0) != 0x80) return 1;

	/* For each length, check that the remaining bytes start with
	   the 0x80 bit set and not the 0x40 bit. Then check for an
	   overlong sequence, and for the excluded range 0xd800 to
	   0xdfff. */

	switch (ab)
	{
	    /* 2-byte character. No further bytes to check for
	       0x80. Check first byte for for xx00 000x (overlong
	       sequence). */
	case 1: 
	    if ((c & 0x3e) == 0) return 1;
	    break;

	    /* 3-byte character. Check third byte for 0x80. Then check
	       first 2 bytes for 1110 0000, xx0x xxxx (overlong
	       sequence) or 1110 1101, 1010 xxxx (0xd800 - 0xdfff) */
	case 2:
	    if ((*(++p) & 0xc0) != 0x80) return 1;    /* Third byte */
	    if (c == 0xe0 && (d & 0x20) == 0) return 1;
	    if (c == 0xed && d >= 0xa0) return 1;
	    break;

	    /* 4-byte character. Check 3rd and 4th bytes for
	       0x80. Then check first 2 bytes for for 1111 0000, xx00
	       xxxx (overlong sequence), then check for a character
	       greater than 0x0010ffff (f4 8f bf bf) */
	case 3:
	    if ((*(++p) & 0xc0) != 0x80) return 1;     /* Third byte */
	    if ((*(++p) & 0xc0) != 0x80) return 1;     /* Fourth byte */
	    if (c == 0xf0 && (d & 0x30) == 0) return 1;
	    if (c > 0xf4 || (c == 0xf4 && d > 0x8f)) return 1;
	    break;

	    /* 5-byte and 6-byte characters are not allowed by RFC
	       3629, and will be rejected by the length test
	       below. However, we do the appropriate tests here so
	       that overlong sequences get diagnosed, and also in case
	       there is ever an option for handling these larger code
	       points. */

	    /* 5-byte character. Check 3rd, 4th, and 5th bytes for
	       0x80. Then check for 1111 1000, xx00 0xxx */
	case 4:
	    if ((*(++p) & 0xc0) != 0x80) return 1;     /* Third byte */
	    if ((*(++p) & 0xc0) != 0x80) return 1;     /* Fourth byte */
	    if ((*(++p) & 0xc0) != 0x80) return 1;     /* Fifth byte */
	    if (c == 0xf8 && (d & 0x38) == 0) return 1;
	    break;

	    /* 6-byte character. Check 3rd-6th bytes for 0x80. Then
	       check for 1111 1100, xx00 00xx. */
	case 5:
	    if ((*(++p) & 0xc0) != 0x80) return 1;     /* Third byte */
	    if ((*(++p) & 0xc0) != 0x80) return 1;     /* Fourth byte */
	    if ((*(++p) & 0xc0) != 0x80) return 1;     /* Fifth byte */
	    if ((*(++p) & 0xc0) != 0x80) return 1;     /* Sixth byte */
	    if (c == 0xfc && (d & 0x3c) == 0) return 1;
	    break;
	}

	/* Character is valid under RFC 2279, but 4-byte and 5-byte
	   characters are excluded by RFC 3629. The pointer p is
	   currently at the last byte of the character. */
	if (ab > 3) return 1;
    }

    return 0;
}

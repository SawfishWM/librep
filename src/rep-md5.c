/* rep-md5.c -- wrap some md5 functions

   Copyright (C) 2001 John Harper <jsh@pixelslut.com>

   $Id$

   This file is part of librep.

   librep is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   librep is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with librep; see the file COPYING.	If not, write to
   the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

#define _GNU_SOURCE

#include <config.h>
#include "repint.h"

#include "md5.h"

static repv
digest_to_repv (char digest[16])
{
    static const char hex_digits[16] = "0123456789abcdef";
    char hex_digest[32];
    int i;

    /* Currently rep has no interface to create bignums directly,
       so format to a hex-encoded string, then reparse it.

       XXX This loses if rep was compiled without GMP support.. */

    for (i = 0; i < 16; i++)
    {
	hex_digest[i*2] = hex_digits[digest[i] & 15];
	hex_digest[i*2+1] = hex_digits[digest[i] >> 4];
    }

    return rep_parse_number (hex_digest, 32, 16, 1, 0);
}

DEFUN ("md5-string", Fmd5_string, Smd5_string, (repv data), rep_Subr1) /*
::doc:rep.util.md5#md5-string::
md5-string STRING

Return the integer representing the MD5 message digest of the bytes
stored in STRING. This integer will have no more than 128 significant
bits.
::end:: */
{
    char digest[16];

    rep_DECLARE1 (data, rep_STRINGP);

    md5_buffer (rep_STR (data), rep_STRING_LEN (data), digest);

    return digest_to_repv (digest);
}

DEFUN ("md5-local-file", Fmd5_local_file,
       Smd5_local_file, (repv file), rep_Subr1) /*
::doc:rep.util.md5#md5-local-file::
md5-local-file LOCAL-FILE-NAME

Return the integer representing the MD5 message digest of the bytes
stored in the file called LOCAL-FILE-NAME (which must name a file in
the local filing system). The returned integer will have no more than
128 significant bits.
::end:: */
{
    FILE *fh;
    char digest[16];

    rep_DECLARE1 (file, rep_STRINGP);

    fh = fopen (rep_STR (file), "r");
    if (fh == 0)
	return rep_signal_file_error (file);

    md5_stream (fh, digest);
    fclose (fh);

    return digest_to_repv (digest);
}

repv
rep_dl_init (void)
{
    repv tem = rep_push_structure ("rep.util.md5");
    rep_ADD_SUBR(Smd5_string);
    rep_ADD_SUBR(Smd5_local_file);
    return rep_pop_structure (tem);
}

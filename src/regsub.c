/*
 * regsub @(#)regsub.c	1.3 of 2 April 86
 *
 * Copyright (c) 1986 by University of Toronto. Written by Henry Spencer.  Not
 * derived from licensed software.
 *
 * Permission is granted to anyone to use this software for any purpose on any
 * computer system, and to redistribute it freely, subject to the following
 * restrictions:
 *
 * 1. The author is not responsible for the consequences of use of this
 * software, no matter how awful, even if they arise from defects in it.
 *
 * 2. The origin of this software must not be misrepresented, either by explicit
 * claim or by omission.
 *
 * 3. Altered versions must be plainly marked as such, and must not be
 * misrepresented as being the original software.
 */

#ifdef JADE
#include "jade.h"
#include <lib/jade_protos.h>
#endif

/*
 * CHANGED, 14-Jan-93, by J.Harper,
 * added #ifdef __STDC__ prototype sections so I can use registerized
 * arguments
 *
 * also, I added the regsublen() function for safety & general usefulness
 * (regsub() has no checks for overstepping its dest string)
 */

#include <stdio.h>
#include "regexp.h"
#include "regmagic.h"

#ifndef CHARBITS
#define UCHARAT(p)	((int)*(unsigned char *)(p))
#else
#define UCHARAT(p)	((int)*(p)&CHARBITS)
#endif

#include <string.h>
void	regerror(char *);

/*
 * - regsub - perform substitutions after a regexp match
 *
 * data is null if the last match was a string, or the TX if the last
 * match was on a buffer.
 */
void
regsub(lasttype, matches, source, dest, data)
    int		    lasttype;
    regsubs	   *matches;
    char	   *source;
    char	   *dest;
    void	   *data;
{
    register char  *src;
    register char  *dst;
    register char   c;
    register int    no;
    register int    len;

    if (matches == NULL || source == NULL || dest == NULL) {
	regerror("NULL parm to regsub");
	return;
    }
#ifdef JADE
    if ((lasttype == reg_string && !STRINGP(VAL(data)))
	|| (lasttype == reg_tx && !BUFFERP(VAL(data))))
    {
	regerror("Bad type of data to regsub");
	return;
    }
#endif
    src = source;
    dst = dest;
    while ((c = *src++) != '\0')
    {
	if (c == '&')
	    no = 0;
	else if (c == '\\' && '0' <= *src && *src <= '9')
	    no = *src++ - '0';
	else
	    no = -1;

	if (no < 0) {		/* Ordinary character. */
	    if (c == '\\' && (*src == '\\' || *src == '&'))
		c = *src++;
	    *dst++ = c;
	} else {
	    if(lasttype == reg_string)
	    {
		if (matches->string.startp[no] != NULL
		    && matches->string.endp[no] != NULL)
		{
		    len = matches->string.endp[no]
			  - matches->string.startp[no];
		    (void) strncpy(dst, matches->string.startp[no], len);
		    dst += len;
		    if (len != 0 && *(dst - 1) == '\0')
		    {
			/* strncpy hit NUL. */
			regerror("damaged match string");
			return;
		    }
		}
	    }
#ifdef JADE
	    else if(lasttype == reg_tx)
	    {
		TX *tx = data;
		if(matches->tx.startp[no] != LISP_NULL)
		{
		    if(check_section(tx, &matches->tx.startp[no],
				     &matches->tx.endp[no]))
		    {
			long len = section_length(tx, matches->tx.startp[no],
						  matches->tx.endp[no]);
			copy_section(tx, matches->tx.startp[no],
				     matches->tx.endp[no], dst);
			dst += len;
		    }
		}
	    }
#endif
	}
    }
    *dst++ = '\0';
}

/*
 * - regsublen - dummy regsub() returning length of contructed string,
 * including terminating '\0'
 */
int
regsublen(lasttype, matches, source, data)
    int		    lasttype;
    regsubs	   *matches;
    char	   *source;
    void	   *data;
{
    register char  *src;
    register char   c;
    register int    no;
    register int    dstlen = 1;

    if (matches == NULL || source == NULL) {
	regerror("NULL parm to regsublen");
	return(0);
    }
#ifdef JADE
    if ((lasttype == reg_string && !STRINGP(VAL(data)))
	|| (lasttype == reg_tx && !BUFFERP(VAL(data))))
    {
	regerror("Bad type of data to regsublen");
	return (0);
    }
#endif
    src = source;
    while ((c = *src++) != '\0') {
	if (c == '&')
	    no = 0;
	else if (c == '\\' && '0' <= *src && *src <= '9')
	    no = *src++ - '0';
	else
	    no = -1;

	if (no < 0) {		/* Ordinary character. */
	    if (c == '\\' && (*src == '\\' || *src == '&'))
		c = *src++;
	    dstlen++;
	} else {
	    if(lasttype == reg_string)
	    {
		if (matches->string.startp[no] != NULL
		    && matches->string.endp[no] != NULL)
		{
		    dstlen += matches->string.endp[no]
			      - matches->string.startp[no];
		}
	    }
#ifdef JADE
	    else if(lasttype == reg_tx)
	    {
		TX *tx = data;
		if(matches->tx.startp[no] != LISP_NULL)
		{
		    if(check_section(tx, &matches->tx.startp[no],
				     &matches->tx.endp[no]))
		    {
			dstlen += section_length(tx, matches->tx.startp[no],
						 matches->tx.endp[no]);
		    }
		}
	    }
#endif
	}
    }
    return(dstlen);
}

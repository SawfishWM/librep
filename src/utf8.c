/* This file some code come from glib:
 * utf8.c - Operations on UTF-8 strings
 *
 * Copyright (C) 2009 Wang Diancheng.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 */


#define _GNU_SOURCE

#include <stdlib.h>
#include <string.h>

#include <config.h>
#include "repint.h"

#define UTF8_COMPUTE(Char, Mask, Len)					      \
  if (Char < 128)							      \
    {									      \
      Len = 1;								      \
      Mask = 0x7f;							      \
    }									      \
  else if ((Char & 0xe0) == 0xc0)					      \
    {									      \
      Len = 2;								      \
      Mask = 0x1f;							      \
    }									      \
  else if ((Char & 0xf0) == 0xe0)					      \
    {									      \
      Len = 3;								      \
      Mask = 0x0f;							      \
    }									      \
  else if ((Char & 0xf8) == 0xf0)					      \
    {									      \
      Len = 4;								      \
      Mask = 0x07;							      \
    }									      \
  else if ((Char & 0xfc) == 0xf8)					      \
    {									      \
      Len = 5;								      \
      Mask = 0x03;							      \
    }									      \
  else if ((Char & 0xfe) == 0xfc)					      \
    {									      \
      Len = 6;								      \
      Mask = 0x01;							      \
    }									      \
  else									      \
    Len = -1;

#define UTF8_LENGTH(Char)              \
  ((Char) < 0x80 ? 1 :                 \
   ((Char) < 0x800 ? 2 :               \
    ((Char) < 0x10000 ? 3 :            \
     ((Char) < 0x200000 ? 4 :          \
      ((Char) < 0x4000000 ? 5 : 6)))))
   

#define UTF8_GET(Result, Chars, Count, Mask, Len)			      \
  (Result) = (Chars)[0] & (Mask);					      \
  for ((Count) = 1; (Count) < (Len); ++(Count))				      \
    {									      \
      if (((Chars)[(Count)] & 0xc0) != 0x80)				      \
	{								      \
	  (Result) = -1;						      \
	  break;							      \
	}								      \
      (Result) <<= 6;							      \
      (Result) |= ((Chars)[(Count)] & 0x3f);				      \
    }

#define UNICODE_VALID(Char)                   \
    ((Char) < 0x110000 &&                     \
     (((Char) & 0xFFFFF800) != 0xD800) &&     \
     ((Char) < 0xFDD0 || (Char) > 0xFDEF) &&  \
     ((Char) & 0xFFFE) != 0xFFFE)
   
     
static const char utf8_skip_data[256] = {
  1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
  1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
  1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
  1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
  1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
  1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
  2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,
  3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,4,4,4,4,4,4,4,4,5,5,5,5,6,6,1,1
};

const char * const utf8_skip = utf8_skip_data;
#define utf8_next_char(p) (char *)((p) + utf8_skip[*(const unsigned char *)(p)])

long
utf8_strlen (const char *p,
	     int       max);
long    
utf8_pointer_to_offset (const char *str,
			const char *pos);
char *
utf8_offset_to_pointer  (const char *str,
			 long        offset);
/**
 * utf8_strlen:
 * @p: pointer to the start of a UTF-8 encoded string.
 * @max: the maximum number of bytes to examine. If @max
 *       is less than 0, then the string is assumed to be
 *       nul-terminated. If @max is 0, @p will not be examined and 
 *       may be %NULL.
 * 
 * Returns the length of the string in characters.
 *
 * Return value: the length of the string in characters
 **/
long
utf8_strlen (const char *p,
               int       max)
{
  long len = 0;
  const char *start = p;

  if(p == NULL || max == 0)
       return 0;
  
  if (max < 0)
    {
      while (*p)
        {
          p = utf8_next_char (p);
          ++len;
        }
    }
  else
    {
      if (max == 0 || !*p)
        return 0;
      
      p = utf8_next_char (p);          

      while (p - start < max && *p)
        {
          ++len;
          p = utf8_next_char (p);          
        }

      /* only do the last len increment if we got a complete
       * char (don't count partial chars)
       */
      if (p - start <= max)
        ++len;
    }

  return len;
}


/**
 * utf8_pointer_to_offset:
 * @str: a UTF-8 encoded string
 * @pos: a pointer to a position within @str
 * 
 * Converts from a pointer to position within a string to a integer
 * character offset.
 *
 * this function allows @pos to be before @str, and returns
 * a negative offset in this case.
 * 
 * Return value: the resulting character offset
 **/
long    
utf8_pointer_to_offset (const char *str,
			  const char *pos)
{
  const char *s = str;
  long offset = 0;    

  if (pos < str) 
    offset = - utf8_pointer_to_offset (pos, str);
  else
    while (s < pos)
      {
	s = utf8_next_char (s);
	offset++;
      }
  
  return offset;
}

/**
 * utf8_offset_to_pointer:
 * @str: a UTF-8 encoded string
 * @offset: a character offset within @str
 * 
 * Converts from an integer character offset to a pointer to a position
 * within the string.
 * 
 * this function allows to pass a negative @offset to
 * step backwards. It is usually worth stepping backwards from the end
 * instead of forwards if @offset is in the last fourth of the string, 
 * since moving forward is about 3 times faster than moving backward.
 * 
 * Return value: the resulting pointer
 **/
char *
utf8_offset_to_pointer  (const char *str,
			   long        offset)    
{
  const char *s = str;

  if (offset > 0) 
    while (offset--)
      s = utf8_next_char (s);
  else
    {
      const char *s1;

      /* This nice technique for fast backwards stepping 
       * through a UTF-8 string was dubbed "stutter stepping" 
       * by its inventor, Larry Ewing.
       */
      while (offset)
	{
	  s1 = s;
	  s += offset;
	  while ((*s & 0xc0) == 0x80)
	    s--;

	  offset += utf8_pointer_to_offset (s, s1);
	}
    }

  return (char *)s;
}

DEFUN("utf8-string-length", Futf8_string_length, Sutf8_string_length, (repv string), rep_Subr1) /*
::doc:rep.util.utf8#length::
length SEQUENCE

Returns the number of characters UTF-8 encoded STRING.
::end:: */
{
     rep_DECLARE1(string, rep_STRINGP);
     return rep_MAKE_INT(utf8_strlen (rep_STR(string),-1));
}

DEFUN("utf8-substring", Futf8_substring, Sutf8_substring, (repv string, repv start, repv end), rep_Subr3) /*
::doc:rep.util.utf8#utf8-substring::
utf8-substring STRING START [END]

Returns the portion of STRING(a UTF-8 encoded string) starting at
character number START and ending at the character before END (or the
end of the string is END is not given).  All indices start at zero.
::end:: */
{
    int utf8len, slen;
    char *pstart;
    char *pend;
    rep_DECLARE1(string, rep_STRINGP);
    rep_DECLARE2(start, rep_INTP);
    rep_DECLARE3_OPT(end, rep_INTP);
    utf8len = utf8_strlen(rep_STR(string), -1);
    if(rep_INT(start) > utf8len || rep_INT(start) < 0)
        return(rep_signal_arg_error(start, 2));
    pstart = utf8_offset_to_pointer(rep_STR(string), rep_INT(start));
    if(rep_INTP(end))
    {
        if((rep_INT(end) > utf8len) || (rep_INT(end) < rep_INT(start)))
            return(rep_signal_arg_error(end, 3));
	pend = utf8_offset_to_pointer(rep_STR(string),rep_INT(end));
        return(rep_string_dupn(pstart, pend - pstart));
    }
    else
    {
        slen = rep_STRING_LEN(string);
        return(rep_string_dupn(pstart, slen - (pstart-rep_STR(string))));
    }
}

repv
rep_dl_init (void)
{
    repv tem = rep_push_structure ("rep.util.utf8");
    rep_ADD_SUBR(Sutf8_substring);
    rep_ADD_SUBR(Sutf8_string_length);
    return rep_pop_structure (tem);
}

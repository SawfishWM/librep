/* readline.c -- wrap some readline functions when available
   Copyright (C) 1999 John Harper <john@dcs.warwick.ac.uk>
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
#include <rep.h>

#ifdef HAVE_LIBREADLINE
# include <readline/readline.h>
# include <readline/history.h>
#endif

DEFSYM(readline, "readline");
DEFSYM(rl_completion_generator, "rl-completion-generator");

static repv completions;

#ifdef HAVE_LIBREADLINE
static char *
completion_generator (char *word, int state)
{
    if (state == 0)
    {
	repv fun = Fsymbol_value (Qrl_completion_generator, Qt);
	if (Ffunctionp (fun) != Qnil)
	{
	    completions = rep_call_lisp1 (fun, rep_string_dup (word));
	}
	else
	{
	    repv re = Fquote_regexp (rep_string_dup (word));
	    completions = Fapropos (rep_concat2("^", rep_STR(re)), Qnil, Qnil);
	}
	if (completions == rep_NULL)
	    completions = Qnil;
    }

    if (completions != Qnil && rep_CONSP(completions)
	&& (rep_SYMBOLP(rep_CAR(completions))
	    || rep_STRINGP(rep_CAR(completions))))
    {
	repv string = rep_CAR(completions);
	if (rep_SYMBOLP(string))
	    string = rep_SYM(string)->name;
	completions = rep_CDR(completions);
	return strdup (rep_STR(string));
    }
    else
	return 0;
}
#endif

DEFUN("readline", Freadline, Sreadline, (repv prompt_), rep_Subr1)
{
    char *prompt = rep_STRINGP(prompt_) ? ((char *) rep_STR(prompt_)) : "> ";
#ifdef HAVE_LIBREADLINE
    repv ret = Qnil;
    char *input = readline (prompt);
    if (input)
    {
	int len = strlen (input);
	if (len > 0)
	    add_history (input);
	ret = rep_make_string (len + 2);
	memcpy (rep_STR(ret), input, len);
	rep_STR(ret)[len] = '\n';
	rep_STR(ret)[len+1] = 0;
	free (input);
    }
    completions = Qnil;
    return ret;
#else
    if (isatty (0))
    {
	printf (prompt);
	fflush (stdout);
    }
    return Fread_line (Fstdin_file ());
#endif
}



/* DL hooks */

rep_xsubr *rep_dl_subrs[] = { &Sreadline, 0 };
repv rep_dl_feature;

repv
rep_dl_init(void)
{
    rep_INTERN(readline);
    rep_INTERN_SPECIAL(rl_completion_generator);
    completions = Qnil;
    rep_mark_static (&completions);
    rep_dl_feature = Qreadline;
    rl_completion_entry_function = (void *) completion_generator;
    return Qt;
}

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

#ifdef HAVE_SYS_TIME_H
# include <sys/time.h>
#endif

#ifdef HAVE_UNISTD_H
# include <unistd.h>
#endif

#ifdef HAVE_LIBREADLINE
# include <readline/readline.h>
# include <readline/history.h>
#endif

DEFSYM(rl_completion_generator, "rl-completion-generator");
DEFSYM(boundp, "boundp");

static repv completion_fun;
static repv completions;

#ifdef HAVE_LIBREADLINE
static char *history_file = NULL;
static char *
completion_generator (char *word, int state)
{
    if (state == 0)
    {
	repv fun = completion_fun;
	if (fun == Qnil)
	    /* backwards compatibility, ugh */
	    fun = Fsymbol_value (Qrl_completion_generator, Qt);
	if (Ffunctionp (fun) != Qnil)
	{
	    completions = (rep_call_with_barrier
			   (Ffuncall, rep_list_2 (fun, rep_string_dup (word)),
			    rep_TRUE, 0, 0, 0));
	}
	else
	{
	    repv re = Fquote_regexp (rep_string_dup (word));
	    repv boundp = Fsymbol_value (Qboundp, Qt);
	    completions = Fapropos (rep_concat2("^", rep_STR(re)),
				    boundp, Qnil);
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

/* gratuitously stolen from guile, guile-readline/readline.c */
static int match_paren(int x, int k);
static int find_matching_paren(int k);
static void init_bouncing_parens();

static void
init_bouncing_parens()
{
  if(strncmp(rl_get_keymap_name(rl_get_keymap()), "vi", 2)) {
    rl_bind_key(')', match_paren);
    rl_bind_key(']', match_paren);
    rl_bind_key('}', match_paren);
  }
}

static int
find_matching_paren(int k)
{
  register int i;
  register char c = 0;
  int end_parens_found = 0;

  /* Choose the corresponding opening bracket.  */
  if (k == ')') c = '(';
  else if (k == ']') c = '[';
  else if (k == '}') c = '{';

  for (i=rl_point-2; i>=0; i--)
    {
      /* Is the current character part of a character literal?  */
      if (i - 2 >= 0
	  && rl_line_buffer[i - 1] == '\\'
	  && rl_line_buffer[i - 2] == '#')
	;
      else if (rl_line_buffer[i] == k)
	end_parens_found++;
      else if (rl_line_buffer[i] == '"')
	{
	  /* Skip over a string literal.  */
	  for (i--; i >= 0; i--)
	    if (rl_line_buffer[i] == '"'
		&& ! (i - 1 >= 0
		      && rl_line_buffer[i - 1] == '\\'))
	      break;
	}
      else if (rl_line_buffer[i] == c)
	{
	  if (end_parens_found==0) return i;
	  else --end_parens_found;
	}
    }
  return -1;
}

static int
match_paren(int x, int k)
{
  int tmp;
  fd_set readset;
  struct timeval timeout;
  
  rl_insert(x, k);

  /* Did we just insert a quoted paren?  If so, then don't bounce.  */
  if (rl_point - 1 >= 1
      && rl_line_buffer[rl_point - 2] == '\\')
    return 0;

  /* tmp = 200000 */
  timeout.tv_sec = 0 /* tmp / 1000000 */ ; 
  timeout.tv_usec = 200000 /* tmp % 1000000 */ ;
  FD_ZERO(&readset);
  FD_SET(fileno(rl_instream), &readset);
  
  if(rl_point > 1) {
    tmp = rl_point;
    rl_point = find_matching_paren(k);
    if(rl_point > -1) {
      rl_redisplay();
      select(1, &readset, NULL, NULL, &timeout);
    }
    rl_point = tmp;
  }

  return 0;
}

#endif

DEFUN("readline", Freadline, Sreadline,
      (repv prompt_, repv completer), rep_Subr2)
{
    char *prompt = rep_STRINGP(prompt_) ? ((char *) rep_STR(prompt_)) : "> ";
#ifdef HAVE_LIBREADLINE
    char *input;
    repv ret = Qnil, saved;
    rep_GC_root gc_saved;

    saved = completion_fun;
    completion_fun = completer;
    rep_PUSHGC (gc_saved, saved);
    input = readline (prompt);
    rep_POPGC;
    completion_fun = saved;

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
	fputs (prompt, stderr);
	fflush (stderr);
    }
    return Fread_line (Fstdin_file ());
#endif
}



/* DL hooks */

repv
rep_dl_init(void)
{
    repv tem;
    rep_INTERN(rl_completion_generator);
    rep_INTERN(boundp);
    completions = Qnil;
    completion_fun = Qnil;
    rep_mark_static (&completions);
    rep_mark_static (&completion_fun);

#ifdef HAVE_LIBREADLINE
    rl_completion_entry_function = (void *) completion_generator;
    rl_basic_quote_characters = "\"";
    if (isatty (0) && getenv("HOME"))
    {
      history_file=(char*) malloc((uint) strlen(getenv("HOME")) + (uint) strlen("/.rep_history") +2);
      if (history_file) {
       sprintf(history_file, "%s/.rep_history",getenv("HOME"));
       read_history(history_file);
      }
    }
    init_bouncing_parens();
#endif
    tem = rep_push_structure ("rep.io.readline");
    /* ::alias:readline rep.io.readline:: */
    rep_alias_structure ("readline");
    rep_ADD_SUBR(Sreadline);
    return rep_pop_structure (tem);
}

#ifdef HAVE_LIBREADLINE
void
rep_dl_kill (void)
{
  if (history_file) {
    write_history(history_file);
    free(history_file);
  }  
}
#endif

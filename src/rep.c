/* rep.c -- read-eval-print front end
   $Id$ */

#define _GNU_SOURCE

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include "rep.h"
#include <string.h>

int
main(int argc, char **argv)
{
    char *prog_name = *argv++;
    argc--;

    rep_init(prog_name, &argc, &argv, 0, 0);

    Fload(rep_string_dup("rep"), Qnil, Qnil, Qnil);

    if(rep_throw_value && rep_CAR(rep_throw_value) == Qerror)
    {
	/* If quitting due to an error, print the error cell if
	   at all possible. */
	repv stream = Fstderr_file();
	repv old_tv = rep_throw_value;
	rep_GC_root gc_old_tv;
	rep_PUSHGC(gc_old_tv, old_tv);
	rep_throw_value = rep_NULL;
	if(stream && rep_FILEP(stream))
	{
	    fputs("error--> ", stderr);
	    Fprin1(rep_CDR(old_tv), stream);
	    fputc('\n', stderr);
	}
	else
	    fputs("exiting due to error\n", stderr);
	rep_throw_value = old_tv;
	rep_POPGC;
	return 10;
    }

    if (rep_throw_value
	&& rep_CAR (rep_throw_value) == Qquit
	&& rep_INTP (rep_CDR(rep_throw_value)))
    {
	return rep_INT (rep_CDR(rep_throw_value));
    }

    return 0;
}

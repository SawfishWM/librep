/* rep.c -- read-eval-print front end
   $Id$ */

#define _GNU_SOURCE

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include "rep.h"
#include "build.h"
#include <string.h>

static void
usage (void)
{
    fputs ("\
    --version		print version details\n\
    --no-rc		don't load rc or site-init files\n\
    -f FUNCTION		call the Lisp function FUNCTION\n\
    -l FILE		load the file of Lisp forms called FILE\n\
    -q			quit\n",
	   stderr);
}

static repv
inner_main (repv arg)
{
    return rep_load_environment (rep_string_dup ("rep"));
}

int
main(int argc, char **argv)
{
    char *prog_name = *argv++;
    argc--;

    rep_init (prog_name, &argc, &argv, 0, usage);

    if (rep_get_option ("--version", 0))
    {
	printf ("rep version %s\n", REP_VERSION);
	return 0;
    }

    rep_call_with_barrier (inner_main, Qnil, rep_TRUE, 0, 0, 0);

    return rep_top_level_exit ();
}

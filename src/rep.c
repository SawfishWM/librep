/* rep.c -- read-eval-print front end
   $Id$ */

#define _GNU_SOURCE

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include "rep.h"
#include "build.h"
#include <string.h>

#ifdef HAVE_LOCALE_H
# include <locale.h>
#endif

int
main(int argc, char **argv)
{
    DEFSTRING (rep, "rep/user");

    char *prog_name = *argv++;
    argc--;

#ifdef HAVE_SETLOCALE
    setlocale (LC_ALL, "");
#endif

    rep_init (prog_name, &argc, &argv, 0, 0);

    rep_call_with_barrier (rep_load_environment, rep_VAL (&rep),
			   rep_TRUE, 0, 0, 0);

    return rep_top_level_exit ();
}

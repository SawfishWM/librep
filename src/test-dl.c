/* test-dl.c -- Test of dynamic loading */

#define _GNU_SOURCE

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include "rep.h"

DEFUN("dl-test", Fdl_test, Sdl_test, (repv arg), rep_Subr1)
{
    rep_DECLARE1(arg, rep_INTP);
    return rep_MAKE_INT(rep_INT(arg) + 42);
}

rep_xsubr *rep_dl_subrs[] = { &Sdl_test, 0 };

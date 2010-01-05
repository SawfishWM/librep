/* safemach.c -- Untrusting VM interpreter

   $Id$

   Copyright (C) 2000 John Harper <john@dcs.warwick.ac.uk>

   This file is part of librep.

   librep is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   librep is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with librep; see the file COPYING.  If not, write to
   the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

#define _GNU_SOURCE

/* AIX requires this to be the first thing in the file.  */
#include <config.h>
#ifdef __GNUC__
# define alloca __builtin_alloca
#else
# if HAVE_ALLOCA_H
#  include <alloca.h>
# else
#  ifdef _AIX
 #pragma alloca
#  else
#   ifndef alloca /* predefined by HP cc +Olibcalls */
char *alloca ();
#   endif
#  endif
# endif
#endif

#include "repint.h"


/* pull in the generic interpreter */

#define BC_APPLY_SELF safe_apply_bytecode

#define ASSERT(expr) do { if (!(expr)) goto safemach_abort; } while (0)

DEFSTRING (safemach_msg, "Illegal byte-code instruction");

#define EXTRA_VM_CODE							\
    safemach_abort:							\
	Fsignal (Qbytecode_error, rep_LIST_1 (rep_VAL (&safemach_msg)));\
	HANDLE_ERROR;

static repv safe_apply_bytecode (repv subr, int nargs, repv *args);

#define OPTIMIZE_FOR_SPACE 1
#define BE_PARANOID 1

#include "lispmach.h"


/* interface */

static repv
safe_apply_bytecode (repv subr, int nargs, repv *args)
{
    rep_DECLARE1 (subr, rep_COMPILEDP);
    return inline_apply_bytecode (subr, nargs, args);
}

DEFUN("safe-run-byte-code", Fsafe_run_byte_code, Ssafe_run_byte_code,
      (repv code, repv consts, repv stkreq), rep_Subr3)
{
    int v_stkreq, b_stkreq, s_stkreq;

    if (rep_STRUCTUREP (code))
    {
	/* install ourselves in this structure */
	rep_STRUCTURE (code)->apply_bytecode = safe_apply_bytecode;
	return Qt;
    }

    rep_DECLARE1(code, rep_STRINGP);
    rep_DECLARE2(consts, rep_VECTORP);
    rep_DECLARE3(stkreq, rep_INTP);

    v_stkreq = rep_INT (stkreq) & 0x3ff;
    b_stkreq = (rep_INT (stkreq) >> 10) & 0x3ff;
    s_stkreq = rep_INT (stkreq) >> 20;

    return vm (code, consts, 0, 0, v_stkreq, b_stkreq, s_stkreq);
}

DEFUN("safe-validate-byte-code", Fsafe_validate_byte_code,
      Ssafe_validate_byte_code, (repv bc_major, repv bc_minor), rep_Subr2)
{
    if(!rep_INTP(bc_major) || !rep_INTP(bc_minor)
       || rep_INT(bc_major) != BYTECODE_MAJOR_VERSION
       || rep_INT(bc_minor) > BYTECODE_MINOR_VERSION)
    {
	DEFSTRING (err, "File needs recompiling for current virtual machine");
	return Fsignal (Qbytecode_error,
			rep_LIST_2 (rep_VAL (&err),
				    Fsymbol_value (Qload_filename, Qt)));
    }
    else
	return Qt;
}

repv
rep_dl_init (void)
{
    repv tem = rep_push_structure ("rep.vm.safe-interpreter");
    rep_ADD_SUBR (Ssafe_run_byte_code);
    rep_ADD_SUBR (Ssafe_validate_byte_code);
    return rep_pop_structure (tem);
}

/* repgdbm.c -- rep wrapper to libgdbm
   $Id$ */

#define _GNU_SOURCE

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include "rep.h"
#include <gdbm.h>
#include <fcntl.h>

static int dbm_type;

#define rep_DBM(v)  ((rep_dbm *) rep_PTR(v))
#define rep_DBMP(v) (rep_CELL16_TYPEP(v, dbm_type) && rep_DBM(v)->dbm != 0)

typedef struct rep_dbm_struct {
    repv car;
    struct rep_dbm_struct *next;
    GDBM_FILE dbm;
    repv path;
    repv access;
    repv mode;
} rep_dbm;

static rep_dbm *dbm_chain;

DEFSYM(gdbm, "gdbm");
DEFSYM(insert, "insert");
DEFSYM(replace, "replace");

DEFUN("gdbm-open", Fgdbm_open, Sgdbm_open, (repv file, repv flags, repv mode),
      rep_Subr3) /*
::doc:gdbm-open::
gdbm-open PATH ACCESS-TYPE [MODE]
::end:: */
{
    int uflags, umode;
    rep_dbm *dbm;
    rep_GC_root gc_flags, gc_mode;

    rep_PUSHGC(gc_flags, flags);
    rep_PUSHGC(gc_mode, mode);
    file = Flocal_file_name (file);
    rep_POPGC; rep_POPGC;

    if (!file)
	return file;
    rep_DECLARE1(file, rep_STRINGP);
    rep_DECLARE2(flags, rep_SYMBOLP);

    uflags = (flags == Qwrite ? GDBM_NEWDB
	      : flags == Qappend ? GDBM_WRCREAT : GDBM_READER);
    umode = rep_INTP(mode) ? rep_INT(mode) : 0666;
    dbm = rep_ALLOC_CELL (sizeof (rep_dbm));
    if (dbm == 0)
	return rep_mem_error();
    rep_data_after_gc += sizeof (rep_dbm);
    dbm->car = dbm_type;
    dbm->path = file;
    dbm->access = flags;
    dbm->mode = rep_MAKE_INT(umode);
    dbm->dbm = gdbm_open (rep_STR(file), 0, uflags, umode, 0);
    if (dbm->dbm != 0)
    {
	dbm->next = dbm_chain;
	dbm_chain = dbm;
	return rep_VAL(dbm);
    }
    else
    {
	rep_FREE_CELL (dbm);
	return rep_signal_file_error (file);
    }
}

DEFUN("gdbm-close", Fgdbm_close, Sgdbm_close, (repv dbm), rep_Subr1) /*
::doc:gdbm-close::
gdbm-close DBM
::end:: */
{
    rep_DECLARE1 (dbm, rep_DBMP);
    gdbm_close (rep_DBM(dbm)->dbm);
    rep_DBM(dbm)->dbm = 0;
    rep_DBM(dbm)->path = Qnil;
    rep_DBM(dbm)->access = Qnil;
    rep_DBM(dbm)->mode = Qnil;
    return Qt;
}

DEFUN("gdbm-fetch", Fgdbm_fetch, Sgdbm_fetch, (repv dbm, repv key), rep_Subr2) /*
::doc:gdbm-fetch::
gdbm-fetch DBM KEY
::end:: */
{
    datum dkey, dvalue;
    rep_DECLARE1 (dbm, rep_DBMP);
    rep_DECLARE2 (key, rep_STRINGP);
    dkey.dptr = rep_STR (key);
    dkey.dsize = rep_STRING_LEN (key);
    dvalue = gdbm_fetch (rep_DBM(dbm)->dbm, dkey);
    if (dvalue.dptr == 0)
	return Qnil;
    else
	return rep_box_string (dvalue.dptr, dvalue.dsize);
}

DEFUN("gdbm-store", Fgdbm_store, Sgdbm_store, (repv dbm, repv key, repv val, repv flags), rep_Subr4) /*
::doc:gdbm-store::
gdbm-store DBM KEY VALUE [FLAGS]
::end:: */
{
    int dflags;
    datum dkey, dvalue;
    rep_DECLARE1 (dbm, rep_DBMP);
    rep_DECLARE2 (key, rep_STRINGP);
    rep_DECLARE3 (val, rep_STRINGP);
    dkey.dptr = rep_STR (key);
    dkey.dsize = rep_STRING_LEN (key);
    dvalue.dptr = rep_STR (val);
    dvalue.dsize = rep_STRING_LEN (val);
    dflags = (flags == Qinsert ? GDBM_INSERT : GDBM_REPLACE);
    return (gdbm_store (rep_DBM(dbm)->dbm, dkey, dvalue, dflags) == 0
	    ? Qt : Qnil);
}

DEFUN("gdbm-delete", Fgdbm_delete, Sgdbm_delete, (repv dbm, repv key), rep_Subr2) /*
::doc:gdbm-delete::
gdbm-delete DBM KEY
::end:: */
{
    datum dkey;
    rep_DECLARE1 (dbm, rep_DBMP);
    rep_DECLARE2 (key, rep_STRINGP);
    dkey.dptr = rep_STR (key);
    dkey.dsize = rep_STRING_LEN (key) + 1;
    return gdbm_delete (rep_DBM(dbm)->dbm, dkey) == 0 ? Qt : Qnil;
}

DEFUN("gdbm-walk", Fgdbm_walk, Sgdbm_walk, (repv fun, repv dbm), rep_Subr2) /*
::doc:gdbm-walk::
gdbm-walk DBM FUN
::end:: */
{
    rep_GC_root gc_dbm, gc_fun;
    repv ret = Qnil;
    datum dkey;
    rep_DECLARE1 (dbm, rep_DBMP);
    rep_PUSHGC (gc_dbm, dbm);
    rep_PUSHGC (gc_fun, fun);
    dkey = gdbm_firstkey (rep_DBM(dbm)->dbm);
    while (dkey.dptr)
    {
	if (!rep_call_lisp1 (fun, rep_string_dupn (dkey.dptr, dkey.dsize)))
	{
	    ret = rep_NULL;
	    free (dkey.dptr);
	    break;
	}
	dkey = gdbm_nextkey (rep_DBM(dbm)->dbm, dkey);
    }
    rep_POPGC; rep_POPGC;
    return ret;
}

DEFUN("gdbmp", Fgdbmp, Sgdbmp, (repv arg), rep_Subr1) /*
::doc:gdbmp::
gdbmp ARG

Returns t if ARG is an gdbm object (created by `gdbm-open').
::end:: */
{
    return rep_DBMP(arg) ? Qt : Qnil;
}



static void
dbm_mark (repv val)
{
    rep_MARKVAL (rep_DBM(val)->path);
    rep_MARKVAL (rep_DBM(val)->access);
    rep_MARKVAL (rep_DBM(val)->mode);
}

static void
dbm_sweep (void)
{
    rep_dbm *x = dbm_chain;
    dbm_chain = 0;
    while (x != 0)
    {
	rep_dbm *next = x->next;
	if (!rep_GC_CELL_MARKEDP (rep_VAL(x)))
	{
	    if (x->dbm != 0)
		gdbm_close (x->dbm);
	    rep_FREE_CELL (x);
	}
	else
	{
	    rep_GC_CLR_CELL (rep_VAL(x));
	    x->next = dbm_chain;
	    dbm_chain = x;
	}
	x = next;
    }
}

static void
dbm_print (repv stream, repv dbm)
{
    rep_stream_puts (stream, "#<dbm ", -1, rep_FALSE);
    if (rep_STRINGP(rep_DBM(dbm)->path))
	rep_stream_puts (stream, rep_PTR(rep_DBM(dbm)->path), -1, rep_TRUE);
    else
	rep_stream_puts (stream, "nil", -1, rep_FALSE);
    rep_stream_putc (stream, '>');
}

static int
dbm_compare (repv v1, repv v2)
{
    return (v1 == v2) ? 0 : 1;
}

repv
rep_dl_init (void)
{
    dbm_type = rep_register_new_type ("gdbm", dbm_compare,
				      dbm_print, dbm_print,
				      dbm_sweep, dbm_mark,
				      0, 0, 0, 0, 0, 0, 0);
    rep_ADD_SUBR(Sgdbm_open);
    rep_ADD_SUBR(Sgdbm_close);
    rep_ADD_SUBR(Sgdbm_fetch);
    rep_ADD_SUBR(Sgdbm_store);
    rep_ADD_SUBR(Sgdbm_delete);
    rep_ADD_SUBR(Sgdbm_walk);
    rep_ADD_SUBR(Sgdbmp);

    rep_INTERN (gdbm);
    rep_INTERN (insert);
    rep_INTERN (replace);
    return Qgdbm;
}

/* repsdbm.c -- rep wrapper to libsdbm
   $Id$ */

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include "rep.h"
#include "sdbm.h"
#include <fcntl.h>

static int dbm_type;

#define rep_DBM(v)  ((rep_dbm *) rep_PTR(v))
#define rep_DBMP(v) (rep_CELL16_TYPEP(v, dbm_type) && rep_DBM(v)->dbm != 0)

typedef struct rep_dbm_struct {
    repv car;
    struct rep_dbm_struct *next;
    DBM *dbm;
    repv path;
    repv access;
    repv mode;
} rep_dbm;

static rep_dbm *dbm_chain;

DEFSYM(sdbm, "sdbm");
DEFSYM(insert, "insert");
DEFSYM(replace, "replace");

DEFUN("dbm-open", Fdbm_open, Sdbm_open, (repv file, repv flags, repv mode),
      rep_Subr3) /*
::doc:Sdbm-open::
dbm-open PATH ACCESS-TYPE [MODE]
::end:: */
{
    int uflags, umode;
    rep_dbm *dbm;
    rep_DECLARE1(file, rep_STRINGP);
    rep_DECLARE2(flags, rep_SYMBOLP);
    uflags = (flags == Qwrite ? O_RDWR | O_CREAT | O_TRUNC
	      : (flags == Qappend ? O_RDWR | O_CREAT : O_RDONLY));
    umode = rep_INTP(mode) ? rep_INT(mode) : 0666;
    dbm = rep_ALLOC_CELL (sizeof (rep_dbm));
    if (dbm == 0)
	return rep_mem_error();
    dbm->car = dbm_type;
    dbm->path = file;
    dbm->access = flags;
    dbm->mode = rep_MAKE_INT(umode);
    dbm->dbm = dbm_open (rep_STR(file), uflags, umode);
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

DEFUN("dbm-close", Fdbm_close, Sdbm_close, (repv dbm), rep_Subr1) /*
::doc:Sdbm-close::
dbm-close DBM
::end:: */
{
    rep_DECLARE1 (dbm, rep_DBMP);
    dbm_close (rep_DBM(dbm)->dbm);
    rep_DBM(dbm)->dbm = 0;
    rep_DBM(dbm)->path = Qnil;
    rep_DBM(dbm)->access = Qnil;
    rep_DBM(dbm)->mode = Qnil;
    return Qt;
}

DEFUN("dbm-fetch", Fdbm_fetch, Sdbm_fetch, (repv dbm, repv key), rep_Subr2) /*
::doc:Sdbm-fetch::
dbm-fetch DBM KEY
::end:: */
{
    datum dkey, dvalue;
    rep_DECLARE1 (dbm, rep_DBMP);
    rep_DECLARE2 (key, rep_STRINGP);
    dkey.dptr = rep_STR (key);
    dkey.dsize = rep_STRING_LEN (key);
    dvalue = dbm_fetch (rep_DBM(dbm)->dbm, dkey);
    if (dvalue.dptr == 0)
	return Qnil;
    else
	return rep_string_dupn (dvalue.dptr, dvalue.dsize);
}

DEFUN("dbm-store", Fdbm_store, Sdbm_store, (repv dbm, repv key, repv val, repv flags), rep_Subr4) /*
::doc:Sdbm-store::
dbm-store DBM KEY VALUE [FLAGS]
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
    dflags = (flags == Qinsert ? DBM_INSERT : DBM_REPLACE);
    return (dbm_store (rep_DBM(dbm)->dbm, dkey, dvalue, dflags) == 0
	    ? Qt : Qnil);
}

DEFUN("dbm-delete", Fdbm_delete, Sdbm_delete, (repv dbm, repv key), rep_Subr2) /*
::doc:Sdbm-delete::
dbm-delete DBM KEY
::end:: */
{
    datum dkey;
    rep_DECLARE1 (dbm, rep_DBMP);
    rep_DECLARE2 (key, rep_STRINGP);
    dkey.dptr = rep_STR (key);
    dkey.dsize = rep_STRING_LEN (key);
    return dbm_delete (rep_DBM(dbm)->dbm, dkey) == 0 ? Qt : Qnil;
}

DEFUN("dbm-firstkey", Fdbm_firstkey, Sdbm_firstkey, (repv dbm), rep_Subr1) /*
::doc:Sdbm-firstkey::
dbm-firstkey DBM
::end:: */
{
    datum dkey;
    rep_DECLARE1 (dbm, rep_DBMP);
    dkey = dbm_firstkey (rep_DBM(dbm)->dbm);
    if (dkey.dptr == 0)
	return Qnil;
    else
	return rep_string_dupn (dkey.dptr, dkey.dsize);
}

DEFUN("dbm-nextkey", Fdbm_nextkey, Sdbm_nextkey, (repv dbm), rep_Subr1) /*
::doc:Sdbm-nextkey::
dbm-nextkey DBM
::end:: */
{
    datum dkey;
    rep_DECLARE1 (dbm, rep_DBMP);
    dkey = dbm_nextkey (rep_DBM(dbm)->dbm);
    if (dkey.dptr == 0)
	return Qnil;
    else
	return rep_string_dupn (dkey.dptr, dkey.dsize);
}

DEFUN("dbm-rdonly", Fdbm_rdonly, Sdbm_rdonly, (repv dbm), rep_Subr1) /*
::doc:Sdbm-rdonly::
dbm-rdonly DBM
::end:: */
{
    rep_DECLARE1 (dbm, rep_DBMP);
    return dbm_rdonly (rep_DBM(dbm)->dbm) ? Qt : Qnil;
}

DEFUN("dbm-error", Fdbm_error, Sdbm_error, (repv dbm), rep_Subr1) /*
::doc:Sdbm-error::
dbm-error DBM
::end:: */
{
    rep_DECLARE1 (dbm, rep_DBMP);
    return dbm_error (rep_DBM(dbm)->dbm) ? Qt : Qnil;
}

DEFUN("dbmp", Fdbmp, Sdbmp, (repv arg), rep_Subr1) /*
::doc:Sdbmp::
dbmp ARG

Returns t if ARG is a dbm object (created by `dbm-open').
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
		dbm_close (x->dbm);
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

rep_xsubr *rep_dl_subrs[] = { &Sdbm_open, &Sdbm_close, &Sdbm_fetch,
			      &Sdbm_store, &Sdbm_delete, &Sdbm_firstkey,
			      &Sdbm_nextkey, &Sdbm_rdonly, &Sdbm_error,
			      &Sdbmp, 0 };

void
rep_dl_init (void)
{
    dbm_type = rep_register_new_type ("dbm", dbm_compare,
				      dbm_print, dbm_print,
				      dbm_sweep, dbm_mark,
				      0, 0, 0, 0, 0, 0, 0);
    rep_INTERN (sdbm);
    rep_INTERN (insert);
    rep_INTERN (replace);
    Fprovide (Qsdbm);
}
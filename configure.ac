dnl  Process this file with autoconf to produce a configure script.
dnl  Copyright (C) 1998 John Harper <john@dcs.warwick.ac.uk>
dnl  $Id: configure.in,v 1.146 2006/02/01 05:47:41 jsh Exp $
dnl
dnl  This file is part of librep.
dnl
dnl  librep is free software; you can redistribute it and/or modify it
dnl  under the terms of the GNU General Public License as published by
dnl  the Free Software Foundation; either version 2, or (at your option)
dnl  any later version.
dnl
dnl  librep is distributed in the hope that it will be useful, but
dnl  WITHOUT ANY WARRANTY; without even the implied warranty of
dnl  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
dnl  GNU General Public License for more details.
dnl
dnl  You should have received a copy of the GNU General Public License
dnl  along with librep; see the file COPYING.  If not, write to
dnl  the Free Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.

AC_REVISION($Revision: 1.146 $)
AC_INIT(src/rep_subrs.h)

AC_CONFIG_HEADER(config.h src/rep_config.h)
AC_CONFIG_MACRO_DIR([m4])

AM_PROG_LIBTOOL

dnl Release versioning info
version="0.92.7"

dnl libtool versioning info: `CURRENT:REVISION:AGE'. CURRENT is the
dnl current interface id, REVISION is the version number of this
dnl implementation, AGE defines the first interface id also supported
dnl (i.e. all interfaces between CURRENT-AGE and CURRENT are supported)
libcurrent=16
librevision=0
libage=0
libversion="$libcurrent:$librevision:$libage"

makefile_template="Makefile.in:Makedefs.in"
output_files="src/Makefile:Makedefs.in:src/Makefile.in\
 lisp/Makefile:Makedefs.in:lisp/Makefile.in\
 Makefile:Makedefs.in:Makefile.in\
 man/Makefile:Makedefs.in:man/Makefile.in\
 intl/Makefile librep.spec librep.pc librep.ebuild rules.mk"

dnl Find the system type
AC_CANONICAL_HOST

dnl Remove trailing slash in $prefix if necessary
case "${prefix}" in
  */)
    prefix=`echo ${prefix} | sed -e 's/^\(.*\)\/$/\1/'`
    ;;
esac

repdir='${datadir}/rep'
replispdir='${repdir}/lisp'
repexecdir='${libdir}/rep'
repcommonexecdir='${libdir}/rep'
repdocfile='${repexecdir}/doc-strings'

emacssitelispdir='${datadir}/emacs/site-lisp'

dnl make sure we can find libraries and headers under $prefix
case "$prefix" in
  /|/usr|/usr/local|NONE)
    ;;
  *)
    LDFLAGS="${LDFLAGS} -L$prefix/lib"
    CPPFLAGS="${CPPFLAGS} -I$prefix/include"
    ;;
esac

EXTRA_LIBOBJS=""
AC_SUBST(EXTRA_LIBOBJS)

AC_SUBST(LDFLAGS)
AC_SUBST(CPPFLAGS)
AC_SUBST(CFLAGS)

dnl Checks for programs.
AC_PROG_CC
AC_ISC_POSIX
AC_PROG_CPP
AC_PROG_INSTALL
AC_PROG_LN_S
AC_PROG_MAKE_SET
AC_PROG_GCC_TRADITIONAL

AC_DISABLE_STATIC
AC_PROG_LIBTOOL

dnl Abort if shared libraries aren't enabled
if test "${enable_shared}" != "yes"; then
  AC_MSG_ERROR([Need shared libraries enabled])
fi

dnl Checks for libraries.
AC_CHECK_FUNC(gethostent, , AC_CHECK_LIB(nsl, gethostent))
AC_CHECK_FUNC(setsockopt, , AC_CHECK_LIB(socket, setsockopt))
AC_CHECK_FUNC(dlopen, , AC_CHECK_LIB(dl, dlopen))

dnl Checks for header files.
AC_HEADER_DIRENT
AC_HEADER_STDC
AC_HEADER_SYS_WAIT
AC_HEADER_TIME
AC_CHECK_HEADERS(fcntl.h sys/ioctl.h sys/time.h sys/utsname.h unistd.h siginfo.h memory.h stropts.h termios.h string.h limits.h argz.h locale.h nl_types.h malloc.h sys/param.h)

dnl Check for GNU MP library and header files
AC_ARG_WITH(gmp,
 [  --without-gmp		  Do not use GMP for bignum/rational numbers],
 [], [with_gmp=maybe])

GMP_LIBS="-lm"
if test "$with_gmp" != "no"; then
  AC_ARG_WITH(gmp-prefix, [  --with-gmp-prefix=DIR   path to GMP],
   [if test "$withval" != "no"; then
      CPPFLAGS="${CPPFLAGS} -I$withval/include"
      LDFLAGS="${LDFLAGS} -L$withval/lib"
    fi],
   dnl debian brain-damage
   [if test -d /usr/include/gmp2; then
      CPPFLAGS="${CPPFLAGS} -I/usr/include/gmp2"
    fi])

  found_gmp=no
  AC_CHECK_HEADER(gmp.h,
    [AC_CHECK_LIB(gmp, mpz_init,
       [GMP_LIBS="-lgmp -lm"; found_gmp=yes],
       [AC_CHECK_LIB(gmp, __gmpz_init,
          [GMP_LIBS="-lgmp -lm"; found_gmp=yes])])])

  if test "$found_gmp" = "yes"; then
    AC_DEFINE(HAVE_GMP, 1, [Have libgmp])
    _libs="$LIBS"
    LIBS="$LIBS $GMP_LIBS"
    AC_CHECK_FUNC(__gmp_randinit, AC_DEFINE(HAVE_GMP_RANDINIT, 1, [Have randinit in libgmp]))
    LIBS="$_libs"
  elif test "$with_gmp" != "no"; then
    AC_MSG_ERROR([cannot find GMP (--without-gmp for cut-down non-GMP build)])
  fi
fi
AC_SUBST(GMP_LIBS)

dnl Check for GNU DBM library and header files
AC_ARG_WITH(gdbm-prefix,
[  --with-gdbm-prefix=DIR  path to GDBM],[
  if test "$withval" != "no"; then
    CPPFLAGS="${CPPFLAGS} -I$withval/include"
    LDFLAGS="${LDFLAGS} -L$withval/lib"
  fi
])
AC_CHECK_HEADER(gdbm.h,
    AC_CHECK_LIB(gdbm, gdbm_open, GDBM_LIBS="-lgdbm",
	AC_MSG_ERROR(Cannot find GDBM library)),
    AC_MSG_ERROR(Cannot find GDBM header))
AC_SUBST(GDBM_LIBS)

dnl Check for readline
AC_ARG_WITH(readline,
 [  --with-readline         support fancy command input editing
  --without-readline      Do not use readline], [], [with_readline=maybe])

if test "$with_readline" != "no"; then
  dnl Save in case test with directory specified fails
  _cppflags=${CPPFLAGS}
  _ldflags=${LDFLAGS}

  AC_ARG_WITH(readline-prefix,
  [  --with-readline-prefix=DIR path to readline],
  [ if test "$withval" != "no" -a "$withval" != "yes"; then
    CPPFLAGS="${CPPFLAGS} -I$withval/include"
    LDFLAGS="${LDFLAGS} -L$withval/lib"
  fi ])

  dnl check for terminal library
  dnl this is a very cool solution from octave's configure.in
  unset tcap
  for termlib in ncurses curses termcap terminfo termlib; do
    AC_CHECK_LIB(${termlib}, tputs, [tcap="$tcap -l$termlib"])
    case "$tcap" in
      *-l${termlib}*)
        break
      ;;
    esac
  done

  AC_CHECK_HEADER(readline/readline.h,
    AC_CHECK_LIB(readline, readline,[
      READLINE_LIBS=" -L/lib${libsuff} -lreadline $tcap"
      AC_DEFINE(HAVE_LIBREADLINE, 1, [Have libreadline])] found_readline=yes, , $tcap))

  if test -z "$READLINE_LIBS"; then
    if test "$with_readline_prefix" = "yes"; then
      AC_MSG_ERROR([cannot find readline libraries])
    else
      CPPFLAGS=${_cppflags}
      LDFLAGS=${_ldflags}
    fi
  fi
fi
AC_SUBST(READLINE_LIBS)

dnl Check for ffi
FFI_MIN_VER=3.0

AC_ARG_WITH(ffi,
 [  --with-ffi              Support for ffi
  --without-ffi           Do not use ffi], [], [with_ffi=yes])

if test "$with_ffi" != "no"; then
  PKG_CHECK_MODULES(LIBFFI, libffi >= $FFI_MIN_VER
  ,[found_ffi=yes]
  ,[
     AC_CHECK_HEADER([ffi.h], AC_DEFINE(HAVE_FFI_H, 1, [Have ffi.h]), [LIBFFI_FOUND=no])
     if test "x${LIBFFI_FOUND}" = xno; then
       AC_CHECK_HEADER([ffi/ffi.h], AC_DEFINE(HAVE_FFI_FFI_H, 1, [Have ffi/ffi.h]) ,[LIBFFI_FOUND=no])
     fi
     AC_SEARCH_LIBS([ffi_call], [ffi],[LIBFFI_LIBS="-lffi"],[LIBFFI_FOUND=no])
     if test "x${LIBFFI_FOUND}" = xno; then
       AC_MSG_ERROR([cannot locate libffi])
     fi
   ])
fi

dnl Check for dynamic loading
AC_CHECK_HEADER(dlfcn.h, [AC_DEFINE(HAVE_DLFCN_H, 1, [Have dlfcn header])])
AC_CHECK_HEADER(dl.h, [AC_DEFINE(HAVE_DL_H, 1, [Have dl header])])
AC_CHECK_HEADER(sys/dl.h, [AC_DEFINE(HAVE_SYS_DL_H, 1, [Have sysdl header])])
dl_ok=no
AC_CHECK_FUNC(dlopen, [AC_DEFINE(HAVE_DLOPEN, 1, [Have dlopen]) dl_ok=dl],
    AC_CHECK_LIB(dl, dlopen, [AC_DEFINE(HAVE_DLOPEN, 1, [Have dlopen]) dl_ok=dl]))
AC_CHECK_FUNC(shl_load, [AC_DEFINE(HAVE_SHL_LOAD, 1, [Have shlload]) dl_ok=shl],
    AC_CHECK_LIB(dld, shl_load, [AC_DEFINE(HAVE_DLOPEN, 1, [Have dlopen]) dl_ok=shl]))

if test $dl_ok = dl; then
  AC_MSG_CHECKING([for preceding underscore in symbols])
  dnl copied from glib configure.in
  AC_TRY_RUN([#ifdef HAVE_DLFCN_H
	      # include <dlfcn.h>
	      #endif
              int glib_underscore_test (void) { return 42; }
              int main() {
                void *f1 = (void*)0, *f2 = (void*)0, *handle;
                handle = dlopen ((void*)0, 0);
                if (handle) {
                  f1 = dlsym (handle, "glib_underscore_test");
                  f2 = dlsym (handle, "_glib_underscore_test");
                } return (!f2 || f1);
              }],
              [AC_DEFINE(DLSYM_NEED_USCORE, 1, [Need dlsym uscore])
	       AC_MSG_RESULT(yes)],
	      [AC_MSG_RESULT(no)])

  dnl RTLD_GLOBAL is broken for Tru64 UNIX V5.0 and V5.0A, such that
  dnl   xh = dlopen("libx.so", RTLD_GLOBAL|RTLD_LAZY); /* provides func */
  dnl   yh = dlopen("liby.so", RTLD_GLOBAL|RTLD_LAZY); /* provides func */
  dnl   yf = dlsym(yh, "func"); /* Error: returns func from libx.so */
  dnl It's hard to test for this apart from the version string, so
  dnl that's what we use.
  case ${host} in
    *-dec-osf5*)
      AC_MSG_CHECKING([for broken RTLD_GLOBAL on Tru64])
      if /usr/sbin/sizer -v | grep -E -q -e 'UNIX [[TVX]]5\.0A?(-[[[:digit:]]]+)? '; then
	  AC_DEFINE(BROKEN_RTLD_GLOBAL, 1, [Broken rtld global])
	  AC_MSG_RESULT(yes)
      else
	  AC_MSG_RESULT(no)
      fi
      ;;
  esac
fi

if test $dl_ok != no; then
  AC_DEFINE(HAVE_DYNAMIC_LOADING, 1, [Have dynamic Loading])
else
  AC_MSG_ERROR([cannot figure out how to do dynamic loading])
fi

dnl Checks for typedefs, structures, and compiler characteristics.
AC_C_CONST
AC_C_INLINE
AC_TYPE_OFF_T
AC_TYPE_PID_T
AC_TYPE_SIZE_T
AC_TYPE_SIGNAL

dnl Checks for library functions.
AC_FUNC_ALLOCA
AC_FUNC_MEMCMP
AC_FUNC_MMAP
AC_FUNC_VPRINTF
AC_CHECK_FUNCS(getcwd gethostname select socket strcspn strerror strstr stpcpy strtol psignal strsignal snprintf grantpt lrand48 getpagesize setitimer dladdr dlerror munmap putenv setenv setlocale strchr strcasecmp strncasecmp strdup __argz_count __argz_stringify __argz_next siginterrupt gettimeofday strtoll strtoq)
AC_REPLACE_FUNCS(realpath)

dnl check for crypt () function
AC_CHECK_FUNC(crypt, [AC_DEFINE(HAVE_CRYPT)],
    AC_CHECK_LIB(crypt, crypt, [AC_DEFINE(HAVE_CRYPT, 1, [Have crypt])
			        LIBS="$LIBS -lcrypt"]))

dnl Custom tests

dnl How do we get dependency lines in the Makefile?
if test "x${GCC}" = "xyes"; then
  MAKEDEP='$(CC) -MM'
else
  case ${host} in
    *-dec-osf*)
      dnl works on Tru64
      MAKEDEP='$(CC) -M'
      ;;
    *-sun-solaris*)
      dnl works on Solaris
      MAKEDEP='/usr/ccs/lib/cpp -M'
      ;;
    *)
      dnl disable dependences?
      MAKEDEP='true'
      ;;
  esac
fi
AC_SUBST(MAKEDEP)

dnl If using GCC and it doesn't look as though the cflags have been
dnl set explicitly, add some warning options.
if test "x${GCC}" = "xyes" -a "x$CFLAGS" = "x-g -O2"; then
  CFLAGS="${CFLAGS} -Wall -Wpointer-arith -Wmissing-prototypes"
  case ${host} in
    *-sun-solaris*)
      dnl Turn off implicit-int warnings since the X11 includes
      dnl on Solaris generate a lot of these
      CFLAGS="${CFLAGS} -Wno-implicit-int"
      ;;
    *-apple-darwin)
      dnl use -no-cpp-precomp with apple cc
      CFLAGS="${CFLAGS} -no-cpp-precomp"
      ;;
  esac
fi

AC_ARG_WITH(extra-cflags,
  [  --with-extra-cflags=FLAGS Extra flags to pass to C compiler],
  CFLAGS="${CFLAGS} $with_extra_cflags")

dnl Does <unistd.h> declare char **environ?
AC_CACHE_CHECK([whether unistd.h declares environ], jade_cv_decl_environ,
 AC_TRY_COMPILE([#include <unistd.h>], [char **foo = environ;],
  [jade_cv_decl_environ=yes],
  [jade_cv_decl_environ=no]))
if test ${jade_cv_decl_environ} = no; then
  AC_DEFINE(ENVIRON_UNDECLARED, 1, [Env undeclared])
fi

dnl Are ptys available, and if so, how?
AC_MSG_CHECKING([whether /dev/ptmx exists])
if test -r /dev/ptmx; then
  AC_MSG_RESULT([yes])
  AC_DEFINE(HAVE_PTYS, 1, [Have ptys])
  AC_DEFINE(HAVE_DEV_PTMX, 1, [Have dev/ptmx])
else
  AC_MSG_RESULT([no])
fi

dnl Look for old-style /dev/ptyXN devices
AC_CACHE_CHECK([whether /dev/ptyXN devices exist], jade_cv_sys_dev_pty,
 jade_cv_sys_dev_pty=no
 for c in p q r s t u v w x y z a b c d e f g h i j k l m n o; do
   if test -r "/dev/pty${c}0"; then
     jade_cv_sys_dev_pty=${c}
     break
   fi
 done)
if test ${jade_cv_sys_dev_pty} != no; then
  AC_DEFINE(HAVE_PTYS, 1, [Have ptys])
  AC_DEFINE_UNQUOTED(FIRST_PTY_LETTER, '${jade_cv_sys_dev_pty}', [The first pty letter])
fi

dnl Try to find the aclocal directory for installation
aclocaldir=none
AC_ARG_WITH(aclocaldir,
  [  --with-aclocaldir=DIR   Directory in which to install autoconf macros],
  aclocaldir=$withval)
AC_MSG_CHECKING([for aclocal directory])
if test "$aclocaldir" = none; then
  aclocaldir="`aclocal --print-ac-dir 2>/dev/null`"
  if test "x${aclocaldir}" = "x"; then
    aclocaldir='${datadir}/aclocal'
  fi
fi
AC_MSG_RESULT([${aclocaldir}])
AC_SUBST(aclocaldir)

AC_ARG_ENABLE(continuations,
 [  --disable-continuations Do not include support for continuations
			   or multi-threading],
 [if test "$enableval" != "no"; then AC_DEFINE(WITH_CONTINUATIONS, 1, [Have continuations]) fi],
 [AC_DEFINE(WITH_CONTINUATIONS, 1, [Have continuations])])

AC_ARG_ENABLE(dballoc,
 [  --enable-dballoc	  Trace all memory allocations],
 [if test "$enableval" != "no"; then AC_DEFINE(DEBUG_SYS_ALLOC, 1, [Debug sys alloc]) fi])

AC_ARG_ENABLE(dbsyms,
 [  --disable-dbsyms	  When writing debug output, do not translate
			   addresses to symbol names],
 [if test "$enableval" != "no"; then AC_DEFINE(DB_RESOLVE_SYMBOLS, 1, [DB resolve symbols]) fi],
 [AC_DEFINE(DB_RESOLVE_SYMBOLS, 1, [DB resolve symbols])])

AC_ARG_ENABLE(gprof,
 [  --enable-gprof	  Build for gprof (needs --enable-static)],
 [CFLAGS="${CFLAGS} -pg"; LDFLAGS="${LDFLAGS} -pg"])

AC_ARG_ENABLE(full-name-terminator,
 [  --enable-full-name-terminator=C Full name in gecos field of passwd
			   file is terminated by first C character],
 [if test "$enableval" != "no"; then
   enableval="'$enableval'"
   AC_DEFINE_UNQUOTED(FULL_NAME_TERMINATOR, $enableval, [Have Fullname Terminator])
  fi])

dnl Assumption for now
HAVE_UNIX=1
AC_DEFINE(rep_HAVE_UNIX, 1, [Having Unix])

dnl We need to find an integer type that's big enough to store any pointer
AC_MSG_CHECKING([for data type to store Lisp values])
AC_ARG_WITH(value-type,
 [  --with-value-type=TYPE  Implicitly signed integer type, at least as wide
			   as a pointer. [TYPE=<probed>] (see README)], [],
 [with_value_type="undef"])
if test "${with_value_type}" = "undef"; then
  AC_TRY_RUN([#include <stdlib.h>
             int main () { exit (!(sizeof (int) >= sizeof (void *)));}],
	     [with_value_type=int])
fi
if test "${with_value_type}" = "undef"; then
  AC_TRY_RUN([#include <stdlib.h>
             int main () { exit (!(sizeof (long int) >= sizeof (void *)));}],
	     [with_value_type="long int"])
fi
if test "${with_value_type}" = "undef"; then
  AC_TRY_RUN([#include <stdlib.h>
             int main () { exit (!(sizeof (long long int) >= sizeof (void *)));}],
	     [with_value_type="long long int"])
fi
if test "${with_value_type}" = "undef"; then
  AC_MSG_ERROR([cannot find Lisp value type; set --with-value-type (see README)])
fi
AC_MSG_RESULT([${with_value_type}])
AC_DEFINE_UNQUOTED(rep_PTR_SIZED_INT, ${with_value_type}, [Have ptr sized int])

dnl Find the size (in bits) of the chosen value type
AC_MSG_CHECKING([for size of Lisp value type])
AC_ARG_WITH(value-sizeof,
 [  --with-value-sizeof=N   Size (in bytes) of value type. [N=<probed>]], [],
 [with_value_sizeof="undef"])
if test "${with_value_sizeof}" = "undef"; then
  dnl the following fragment is inspired by AC_CHECK_SIZEOF
  AC_TRY_RUN([#include <stdio.h>
		#include <stdlib.h>
		int main () {
		FILE *f = fopen ("conftestval", "w");
		if (!f) exit (1);
		fprintf (f, "%d\n", sizeof (${with_value_type}));
		exit (0);
	      }],
	      [ with_value_sizeof=`cat conftestval`; rm -f conftestval])
fi
AC_MSG_RESULT([${with_value_sizeof} bytes])
AC_DEFINE_UNQUOTED(rep_PTR_SIZED_INT_SIZEOF, ${with_value_sizeof}, [Have ptr sized int sizeof])

dnl What's the suffix to get an integer constant of the above size?
AC_MSG_CHECKING([for value type constant suffix])
case "${with_value_type}" in
  "long long int"|"long long")
    rep_value_suffix=LL
    ;;
  *)
    dnl We should be okay to default to longs..
    rep_value_suffix=L
    ;;
esac
AC_MSG_RESULT([${rep_value_suffix}])
AC_DEFINE_UNQUOTED(rep_PTR_SIZED_INT_SUFFIX, ${rep_value_suffix}, [Have ptr sized suffix])

dnl What's the printf integer conversion for this type?
AC_MSG_CHECKING([for value type printf conversion])
case "${with_value_type}" in
  "long long int"|"long long")
    dnl other options would be L or q!?
    rep_value_conv=ll
    ;;
  long|"long int")
    rep_value_conv=l
    ;;
  *)
    rep_value_conv=
    ;;
esac
AC_MSG_RESULT([${rep_value_conv}])
AC_DEFINE_UNQUOTED(rep_PTR_SIZED_INT_CONV, "${rep_value_conv}", [Have ptr sized int conv])

dnl Check for size of some types
AC_CHECK_SIZEOF(long)
AC_CHECK_SIZEOF(long long)
AC_CHECK_SIZEOF(void *)

if test "x$ac_cv_sizeof_long_long" != "x0"; then
  AC_DEFINE(rep_HAVE_LONG_LONG, 1, [Have long long])
fi

dnl This is the malloc granularity. It's unlikely that anything has
dnl less alignment than the default value of the number of bytes in
dnl the value type
AC_ARG_WITH(malloc-alignment,
 [  --with-malloc-alignment=BYTES Must be less than or equal to the alignment
			   returned by the malloc function. [BYTES=N]], [],
 [with_malloc_alignment=${with_value_sizeof}])
AC_DEFINE_UNQUOTED(rep_MALLOC_ALIGNMENT, ${with_malloc_alignment}, [Have malloc alignment])

dnl Check for gcc crashes on alpha (seems to be fixed in new snapshots, but
dnl just to make it compile on all machines)
if test "x${GCC}" = "xyes"; then
  AC_MSG_CHECKING([for broken alpha gcc])
  AC_TRY_COMPILE([],[__builtin_return_address(1);],
		 [AC_MSG_RESULT(no)],
		 [AC_DEFINE(BROKEN_ALPHA_GCC, 1, [Have broken alpha gcc])
		  AC_MSG_RESULT(yes)
		  AC_MSG_WARN([Backtrace support will not be compiled])])
fi

AC_MSG_CHECKING([for stack growth direction])
AC_ARG_WITH(stack-direction,
 [  --with-stack-direction=DIR Stack growth direction. -1 for downwards,
			   +1 for upwards.],
 [AC_MSG_RESULT(${with_stack_direction})],
 [with_stack_direction=unknown])
if test "${with_stack_direction}" = unknown; then
  case ${host_cpu} in
    sparc*|i?86*|powerpc*|x86_64*|arm*|aarch64*|s390*|mips*|alpha*|ia64*|loongarch64*)
      AC_MSG_RESULT([assuming downwards])
      with_stack_direction="-1"
      ;;
    hppa*|metag*)
      AC_MSG_RESULT([assuming upwards])
      with_stack_direction="1"
      ;;
    dnl any other known stack directions..?
  esac
fi
if test "${with_stack_direction}" = unknown; then
  AC_TRY_RUN([#include <stdlib.h>
		 int level = 1;
		 void inner (char *foo) { char bar;
		 if (level) { --level; inner (foo); }
		 exit (!(foo >= &bar)); }
	       int main () { char foo; inner (&foo); } ],
   [AC_MSG_RESULT([downwards])
    with_stack_direction=-1],
   [AC_MSG_RESULT([upwards])
    with_stack_direction=+1])
fi
if test "${with_stack_direction}" != unknown; then
  AC_DEFINE_UNQUOTED(STACK_DIRECTION, ${with_stack_direction}, [Have stack direction])
fi

AC_ARG_ENABLE(paranoia,          [  --enable-paranoia       use wall, wno-error, ansi, pedantic compiler flags], [paranoia=$enableval],  [paranoia="no"])

if test $paranoia = "yes"; then
        CFLAGS+=" -Wall -ansi -pedantic"
fi

dnl Nonstandard exported symbols
AC_SUBST(version)
AC_SUBST(libversion)
AC_SUBST(repdir)
AC_SUBST(repexecdir)
AC_SUBST(repcommonexecdir)
AC_SUBST(replispdir)
AC_SUBST(repdocfile)
AC_SUBST(emacssitelispdir)
AC_SUBST(HAVE_UNIX)
AC_SUBST(HAVE_X11)

AC_SUBST(datarootdir)

AC_DEFINE_UNQUOTED(rep_VERSION, "${version}", [Rep version])
AC_DEFINE_UNQUOTED(rep_INTERFACE, ${libcurrent}, [Rep interface])

dnl Hacks for libintl
gt_LC_MESSAGES
VERSION="$version"
PACKAGE="librep"
DATADIRNAME="share"
MKINSTALLDIRS='${top_srcdir}/mkinstalldirs'
INTLOBJS="\$(GETTOBJS)"
GT_YES='#'
GT_NO=''
l=l
AC_SUBST(VERSION)
AC_SUBST(PACKAGE)
AC_SUBST(DATADIRNAME)
AC_SUBST(MKINSTALLDIRS)
AC_SUBST(INTLOBJS)
AC_SUBST(GT_YES)
AC_SUBST(GT_NO)
AC_SUBST(l)
AC_SUBST(LIBFFI_LIBS)
AC_SUBST(FFI_MIN_VER)

dnl If it looks like GNU gettext is in libc, don't compile a local copy
USE_INCLUDED_LIBINTL=yes
AC_SUBST(USE_INCLUDED_LIBINTL)
AC_CHECK_FUNC(_nl_msg_cat_cntr,
 [AC_DEFINE(LIBC_GETTEXT, 1, [Have libc gettext])
  AC_CHECK_HEADERS(libintl.h)
  USE_INCLUDED_LIBINTL=no])

AC_OUTPUT(${output_files})

mv librep.ebuild librep-$version.ebuild

if test "$with_gmp" != "no" && test "$found_gmp" = "yes" ; then
	with_gmp=yes
else	with_gmp=no
fi

if test "$with_readline" != "no" && test "$found_readline" = "yes"; then
	with_readline=yes
else	with_readline=no
fi

if test "$with_libffi" != "no" && test "$found_ffi" = "yes"; then
	with_ffi=yes
else	with_ffi=no
fi

echo "
  == == == == == == == == == == == == ==

  librep:	$version

  == == == == == == == == == == == == ==

  prefix:       $prefix
  exec_prefix:  $exec_prefix
  libdir:       $libdir

  == == == == == == == == == == == == ==

  compiler:	$CC
  cflags:	$CFLAGS $DEVELOPMENT_CFLAGS
  preprocessor: $CPP
  cppflags:     $CPPFLAGS
  ldflags:      $LDFLAGS
  makeflags:    $MAKEFLAGS

  == == == == == == == == == == == == ==

  libgmp:	$with_gmp
  libgdbm:	yes
  readline:	$with_readline
  libffi:	$with_ffi

  == == == == == == == == == == == == ==
  "

dnl If it doesn't look like GNU Make is being used, give a friendly warning
tem=`make --version -f /dev/null 2>&1 | grep GNU`
if test "x$tem" = "x"; then
  AC_MSG_WARN([You need to use GNU Make when compiling])
fi

dnl Local variables:
dnl major-mode: sh-mode
dnl End:

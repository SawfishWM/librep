# rules.mk.sh -- Build dynamically-loadable objects for librep
# $Id$

repdir=$1
repexecdir=$2
repdocfile=$3

cat <<EOF
# rules.mk

repdir=$repdir
repexecdir=$repexecdir
repdocfile=$repdocfile

rep_LIBTOOL=\$(repexecdir)/libtool

# use this like:
# foo.la : foo.lo bar.lo
#	\$(rep_DL_LD) link-opts...

rep_DL_LD=\$(rep_LIBTOOL) \$(CC) -avoid-version -module -rpath \$(repexecdir)
rep_DL_INSTALL=\$(rep_LIBTOOL) \$(INSTALL)
rep_DL_UNINSTALL=\$(rep_LIBTOOL) rm

# Rule for libtool controlled C objects
%.lo : %.c
	\$(rep_LIBTOOL) \$(CC) -c \$(CPPFLAGS) \$(CFLAGS) \$<

EOF

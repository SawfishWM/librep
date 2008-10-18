# rules.mk.sh -- Build dynamically-loadable objects for librep
# $Id$

repdir=$1
repcommonexecdir=$2
repdocfile=$3

cat <<EOF
# rules.mk

repdir=$repdir
repcommonexecdir=$repcommonexecdir
rpath_repcommonexecdir=$repcommonexecdir

rep_LIBTOOL=\$(repcommonexecdir)/libtool --tag CC
rep_INSTALL_ALIASES=\$(repcommonexecdir)/install-aliases

# use this like:
# foo.la : foo.lo bar.lo
#	\$(rep_DL_LD) link-opts...

rep_DL_LD=\$(rep_LIBTOOL) --mode=link --tag=CC \$(CC) -avoid-version -module \
	  -rpath \$(rpath_repcommonexecdir)

rep_DL_INSTALL=\$(rep_LIBTOOL) --mode=install \$(INSTALL)
rep_DL_UNINSTALL=\$(rep_LIBTOOL) --mode=uninstall rm

# Rule for libtool controlled C objects
%.lo : %.c
	\$(rep_LIBTOOL) --mode=compile --tag=CC \$(CC) -c \$(CPPFLAGS) \$(CFLAGS) \$<

EOF

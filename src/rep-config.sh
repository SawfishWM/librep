#!/bin/sh

# load libtool configuration
ltconf=/tmp/libtool.conf.$$
../libtool --config >$ltconf
. $ltconf
rm -f $ltconf

prefix=$1
libdir=$2
version=$3
LIBS=$4
repexecdir=$5

libpath="-L${libdir}"

# So that we keep -R options where required
if test -n "$hardcode_libdir_flag_spec"; then
  eval flag=\"$hardcode_libdir_flag_spec\"
  libpath="$libpath $flag"
fi

cat <<EOF
#!/bin/sh

usage="usage: rep-config [--version] [--libs] [--cflags] [--execdir]"

if test \$# -eq 0; then
      echo "\${usage}" 1>&2
      exit 1
fi

while test \$# -gt 0; do
  case \$1 in
    --version)
      echo "${version}"
      ;;
    --cflags)
      echo "-I${prefix}/include -I${repexecdir}"
      ;;
    --libs)
      echo "${libpath} -lrep ${LIBS}"
      ;;
    --execdir)
      echo "${repexecdir}"
      ;;
    *)
      echo "\${usage}" 1>&2
      exit 1
      ;;
  esac
  shift
done
EOF

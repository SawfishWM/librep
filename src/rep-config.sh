#!/bin/sh

prefix=$1
exec_prefix=$2
version=$3
LIBS=$4
repexecdir=$5

libpath="-L${exec_prefix}/lib"

# Try to figure out which systems will require the -R option, libtool
# seems to contain a line like the following (from solaris):
#	hardcode_libdir_flag_spec="-R\$libdir"

hardcode=`grep '^hardcode_libdir_flag_spec' ../libtool`

if test "x${hardcode}" != "x"; then
  libdir="${exec_prefix}/lib"
  # Eval twice to remove the backslash
  eval eval $hardcode
  libpath="$libpath $hardcode_libdir_flag_spec"
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
      echo ${version}
      ;;
    --cflags)
      if test ${prefix}/include != /usr/include ; then
        includes=-I${prefix}/include
      fi
      echo \$includes
      ;;
    --libs)
      echo ${libpath} -lrep ${LIBS}
      ;;
    --execdir)
      echo ${repexecdir}
      ;;
    *)
      echo "\${usage}" 1>&2
      exit 1
      ;;
  esac
  shift
done
EOF

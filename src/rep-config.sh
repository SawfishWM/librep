#!/bin/sh

prefix=$1
exec_prefix=$2
version=$3
LIBS=$4

libpath="-L${exec_prefix}"
if echo "$LIBS" | fgrep -s -- -R; then
  # assume that system needs -R for shared libraries
  libpath="$libpath -R${exec_prefix}/lib"
fi

cat <<EOF
#!/bin/sh

usage="usage: rep-config [--version] [--libs] [--cflags]"

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
    *)
      echo "\${usage}" 1>&2
      exit 1
      ;;
  esac
  shift
done
EOF

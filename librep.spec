
%define nam	librep
%define ver	0.6
%define rel	1

Summary: embeddable Lisp environment
Name: %{nam}
Version: %{ver}
Release: %{rel}
Copyright: GPL
Group: Development/Languages
Source: ftp.dcs.warwick.ac.uk:/people/John.Harper/librep/librep-%{ver}.tar.gz
URL: http://www.dcs.warwick.ac.uk/~john/sw/librep.html
Packager: John Harper <john@dcs.warwick.ac.uk>

%description
This is an Emacs Lisp-like runtime library for UNIX. It contains a LISP
interpreter, byte-code compiler and virtual machine. Applications may
use the LISP interpreter as an extension language, or it may be used
for standalone scripts.

%package devel
Summary: librep include files and link libraries
Group: Development/Languages
Requires: %{nam}

%description devel
Link libraries and C header files for librep development.

%prep
%setup

%build
./configure --prefix %{_prefix} %{_host}
make CFLAGS="$RPM_OPT_FLAGS"

%install
rm -f %{_prefix}/info/librep*
make install
gzip -9nf %{_prefix}/info/librep*

%files
%doc NEWS README etc/TODO
%{_prefix}/bin/rep
%{_prefix}/bin/rep-remote
%{_prefix}/lib/librep.so.*
%{_prefix}/share/rep/%{ver}/lisp/*.jl
%{_prefix}/share/rep/%{ver}/lisp/*.jlc
%{_prefix}/share/rep/%{ver}/DOC.*
%{_prefix}/libexec/rep/%{ver}/%{_host}/lib*.so*
%{_prefix}/libexec/rep/%{ver}/%{_host}/lib*.la
%{_prefix}/info/librep*

%files devel
%{_prefix}/bin/rep-config
%{_prefix}/bin/repdoc
%{_prefix}/lib/librep.a
%{_prefix}/lib/librep.so
%{_prefix}/include/rep.h
%{_prefix}/include/rep_*.h
%{_prefix}/libexec/rep/%{_host}/libtool
%{_prefix}/libexec/rep/%{_host}/rules.mk
%{_prefix}/share/aclocal/rep.m4

%post
ldconfig

%postun
ldconfig

Summary: Streams orchestrator - message exchange
Name: Streams
Version: 0.1
Release: 1
Source: %{name}-%{version}.tar.gz
License: BSD
Group: Development/Libraries
BuildRequires: erlang 
Prereq: erlang
Buildroot: %{_tmppath}/%{name}-%{version} 

%description
The Streams orchestrator manages gateways and transformations

%define _streams_libdir %{_libdir}/streams

%prep
%setup -q
#sed -i 's|LIB_DIR|%{_sbindir}|' orchestrator.sh
mkdir -p %{buildroot}

%build

%install
make create-var-dirs build/opt/erlang-rfc4627 build/opt/ibrowse build/opt/rabbitmq build/opt/rabbitmq-erlang-client build/opt/couchdb-0.9.0 all
make -f Makefile.install LIB_TARGET_DIR=%{buildroot}%{_streams_libdir} SBIN_TARGET_DIR=%{buildroot}%{_sbindir} install 
echo '******%{_streams_libdir}'

%clean

%files
%defattr(-,root,root)
/usr/lib/streams
%{_streams_libdir}
%{_sbindir}


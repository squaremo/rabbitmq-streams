Summary: RabbitMQ Streams orchestrator - message exchange
Name: rabbitmq-streams
Version: 0.1
Release: 1
Source: %{name}-%{version}.tar.gz
License: BSD
Group: Development/Libraries
BuildRequires: erlang
Requires: erlang
Requires: rabbitmq-server
Requires: couchdb
Requires: python-simplejson
Requires: java
Prereq: erlang
Buildroot: %{_tmppath}/%{name}-%{version}

%description
The Streams orchestrator manages gateways and transformations

%define _streams_libdir %{_libdir}/rabbitmq-streams
%define _plugin_dir %{_streams_libdir}/plugins

%prep
%setup -q
mkdir -p %{buildroot}

%build

%install
make install-local-stuff all
make -f Makefile.install LIB_TARGET_DIR=%{buildroot}%{_streams_libdir} SBIN_TARGET_DIR=%{buildroot}%{_sbindir} PLUGIN_TARGET_DIR=%{buildroot}%{_plugin_dir} install
sed -i -e "s:../harness/python/lib:../python:" %{buildroot}%{_streams_libdir}/scripts/*.py
%clean

%files
%defattr(-,root,root)
%{_streams_libdir}
%{_plugin_dir}
%{_sbindir}

%post
/etc/init.d/rabbitmq-server start
/etc/init.d/couchdb start
/usr/sbin/rabbitmqctl delete_user guest
/usr/sbin/rabbitmqctl delete_user feedshub_admin
/usr/sbin/rabbitmqctl add_user feedshub_admin feedshub_admin
/usr/sbin/rabbitmqctl set_permissions feedshub_admin '.*' '.*' '.*'

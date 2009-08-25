Summary: RabbitMQ Streams orchestrator - message exchange
Name: rabbitmq-streams
Version: 0.1
Release: 1
Source: %{name}-%{version}.tar.gz
License: BSD
Group: Development/Libraries
BuildRequires: erlang
Requires: erlang >= 5.6.5
Requires: rabbitmq-server >= 1.6
Requires: couchdb >= 0.9
Requires: python-simplejson
Requires: java >= 1.6
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
# horrible hack
ln -fs %{buildroot}%{_streams_libdir}/erlang/orchestrator/scripts/* %{buildroot}%{_sbindir}

/etc/init.d/rabbitmq-server start
/usr/sbin/rabbitmqctl delete_user guest
/usr/sbin/rabbitmqctl delete_user feedshub_admin
/usr/sbin/rabbitmqctl add_user feedshub_admin feedshub_admin
/usr/sbin/rabbitmqctl set_permissions feedshub_admin '.*' '.*' '.*'
/etc/init.d/couchdb start
python /usr/lib/rabbitmq-streams/scripts/import_config.py /usr/lib/rabbitmq-streams/examples/basic_config/

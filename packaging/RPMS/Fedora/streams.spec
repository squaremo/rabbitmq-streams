Summary: RabbitMQ Streams orchestrator - message exchange
Name: rabbitmq-streams
Version: 0.1
Release: 2
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
make create-var-dirs build/opt/erlang-rfc4627 build/opt/ibrowse build/opt/rabbitmq build/opt/rabbitmq-erlang-client build/opt/couchdb-0.9.0 all
make -f Makefile.install LIB_TARGET_DIR=%{buildroot}%{_streams_libdir} SBIN_TARGET_DIR=%{buildroot}%{_sbindir} PLUGIN_TARGET_DIR=%{buildroot}%{_plugin_dir} install 
sed -i -e "s:../harness/python/lib:../python:" %{buildroot}%{_streams_libdir}/scripts/*.py
sed -i -e "s:../build/opt/erlang-rfc4627:../rfc4627:" -e "s:../build/opt/rabbitmq-erlang-client:../amqp:" -e "s:../build/opt/rabbitmq:../rabbit:" -e "s:../build/opt/ibrowse:../ibrowse:" %{buildroot}%{_streams_libdir}/erlang/orchestrator/streamsctl

%clean

%files
%defattr(-,root,root)
%{_streams_libdir}
%{_plugin_dir}
%{_sbindir}

%post
/usr/sbin/rabbitmq-server &
sleep 3
/usr/sbin/rabbitmqctl delete_user guest
/usr/sbin/rabbitmqctl delete_user feedshub_admin
/usr/sbin/rabbitmqctl add_user feedshub_admin feedshub_admin
/usr/sbin/rabbitmqctl set_permissions feedshub_admin '.*' '.*' '.*'

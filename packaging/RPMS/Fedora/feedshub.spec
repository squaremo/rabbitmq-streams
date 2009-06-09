Summary: FeedsHub orchestrator - message exchange
Name: FeedsHub
Version: 0.1
Release: 1
Source: %{name}-%{version}.tar.gz
License: MPLv1.1
Group: Development/Libraries
BuildRequires: erlang 
Prereq: erlang
Buildroot: %{_tmppath}/%{name}-%{version}-root 

%description
The FeedsHub orchestrator blah blah blah

%define _feedshub_libdir %{_libdir}/feedshub

%prep
%setup -q
sed -i 's|LIB_DIR|%{_sbindir}|' orchestrator.sh

%build

%install
make LIB_TARGET_DIR=%{buildroot}%{_feedshub_libdir} SBIN_TARGET_DIR=%{buildroot}%{_sbindir} install 

%clean
rm -rf $RPM_BUILD_ROOT

%files
%defattr(-,root,root)
%{_feedshub_libdir}/erlang-rfc4627
%{_feedshub_libdir}/ibrowse
%{_feedshub_libdir}/rabbitmq
%{_feedshub_libdir}/rabbitmq-erlang-client
%{_feedshub_libdir}/orchestrator
%{_sbindir}/orchestrator.sh


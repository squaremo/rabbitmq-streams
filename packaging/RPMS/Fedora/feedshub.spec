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

%build

%install
make TARGET_DIR=%{buildroot}%{_feedshub_libdir} install 

%clean
rm -rf $RPM_BUILD_ROOT

%files
%defattr(-,root,root)
%{_feedshub_libdir}/erlang-rfc4627
%{_feedshub_libdir}/ibrowse
%{_feedshub_libdir}/rabbitmq
%{_feedshub_libdir}/rabbitmq-erlang-client


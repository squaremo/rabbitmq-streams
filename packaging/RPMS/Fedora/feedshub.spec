Summary: FeedsHub orchestrator - message exchange
Name: FeedsHub
Version: 0.1
Release: 1
Source0: %{name}-%{version}.tar.gz
License: MPLv1.1
Group: Development/Libraries
#BuildRequires: erlang 
#Prereq: erlang

%description
The FeedsHub orchestrator blah blah blah

%define _feedshub_libdir %{_libdir}/feedshub
%prep
%setup -q

%build
make

%install
rm -rf $RPM_BUILD_ROOT
make install

%clean
rm -rf $RPM_BUILD_ROOT

%files
%defattr(-,root,root)
file.txt

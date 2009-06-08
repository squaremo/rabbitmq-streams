Summary: FeedsHub orchestrator - message exchange
Name: FeedsHub
Version: 0.1
Release: 1
Source0: %{name}-%{version}.tar.gz
License: GPL
Group: Development/Tools
BuildRequires: erlang 
Prereq: erlang
%description
The FeedHub orchestrator blah blah blah
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

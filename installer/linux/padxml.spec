Name:           padxml
Version:        %{?version}
Release:        2%{?dist}
Summary:        PadXml
License:        MIT
BuildArch:      x86_64
Requires:       gtk2

%define _build_name_fmt %%{name}-%%{version}.rpm

%description
A simple tool for creating and editing PAD XML files used for software distribution and listings. 

%prep

%build

%install
rm -rf %{buildroot}
mkdir -p %{buildroot}
cp -a "%{staging_dir}/." "%{buildroot}/"

%files -f %{_sourcedir}/padxml.files

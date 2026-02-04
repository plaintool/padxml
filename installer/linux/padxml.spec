Name:           padxml
Version:        %{?version}
Release:        2%{?dist}
Summary:        PadXML XML editor
License:        MIT
BuildArch:      x86_64
Requires:       gtk2

%description
PadXML is a lightweight XML editor.

%prep

%build

%install
rm -rf %{buildroot}
mkdir -p %{buildroot}
cp -a "%{staging_dir}/." "%{buildroot}/"

%files -f %{_sourcedir}/padxml.files

%changelog
* Thu Jan 01 2025 PadXML Team <support@example.com> - %{version}-2
- Automated build

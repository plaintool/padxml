Buildroot: ./rpmbuild/BUILDROOT
Name: padxml
Version: 1.0.0
Release: 2
Summary: A simple application for creating and organizing task lists
License: see /usr/share/doc/padxml/copyright
Distribution: Fedora
Group: Converted/base
Requires: gtk2

%define _build_name_fmt %%{name}-%%{version}.rpm
%define _unpackaged_files_terminate_build 0

%description
PadXml

%install
rm -rf %{buildroot}
mkdir -p %{buildroot}/usr/bin
mkdir -p %{buildroot}/usr/share/applications
mkdir -p %{buildroot}/usr/share/pixmaps
mkdir -p %{buildroot}/usr/share/mime/packages
mkdir -p %{buildroot}/usr/share/icons/hicolor/64x64/mimetypes
mkdir -p %{buildroot}/usr/share/icons/hicolor/64x64/apps
mkdir -p %{buildroot}/usr/share/icons/hicolor/128x128/mimetypes
mkdir -p %{buildroot}/usr/share/icons/hicolor/128x128/apps

cp -p ~/DATA/usr/bin/padxml %{buildroot}/usr/bin/
cp -p ~/DATA/usr/share/applications/x-padxml.desktop %{buildroot}/usr/share/applications/
cp -p ~/DATA/usr/share/pixmaps/padxml.png %{buildroot}/usr/share/pixmaps/
cp -p ~/DATA/usr/share/mime/packages/x-padxml.xml %{buildroot}/usr/share/mime/packages/
cp -p ~/DATA/usr/share/icons/hicolor/16x16/mimetypes/paddoc.png %{buildroot}/usr/share/icons/hicolor/16x16/mimetypes/
cp -p ~/DATA/usr/share/icons/hicolor/16x16/apps/x-padxml.png %{buildroot}/usr/share/icons/hicolor/16x16/apps/
cp -p ~/DATA/usr/share/icons/hicolor/24x24/mimetypes/paddoc.png %{buildroot}/usr/share/icons/hicolor/24x24/mimetypes/
cp -p ~/DATA/usr/share/icons/hicolor/24x24/apps/x-padxml.png %{buildroot}/usr/share/icons/hicolor/24x24/apps/
cp -p ~/DATA/usr/share/icons/hicolor/32x32/mimetypes/paddoc.png %{buildroot}/usr/share/icons/hicolor/32x32/mimetypes/
cp -p ~/DATA/usr/share/icons/hicolor/32x32/apps/x-padxml.png %{buildroot}/usr/share/icons/hicolor/32x32/apps/
cp -p ~/DATA/usr/share/icons/hicolor/48x48/mimetypes/paddoc.png %{buildroot}/usr/share/icons/hicolor/48x48/mimetypes/
cp -p ~/DATA/usr/share/icons/hicolor/48x48/apps/x-padxml.png %{buildroot}/usr/share/icons/hicolor/48x48/apps/
cp -p ~/DATA/usr/share/icons/hicolor/64x64/mimetypes/paddoc.png %{buildroot}/usr/share/icons/hicolor/64x64/mimetypes/
cp -p ~/DATA/usr/share/icons/hicolor/64x64/apps/x-padxml.png %{buildroot}/usr/share/icons/hicolor/64x64/apps/
cp -p ~/DATA/usr/share/icons/hicolor/128x128/mimetypes/paddoc.png %{buildroot}/usr/share/icons/hicolor/128x128/mimetypes/
cp -p ~/DATA/usr/share/icons/hicolor/128x128/apps/x-padxml.png %{buildroot}/usr/share/icons/hicolor/128x128/apps/
cp -p ~/DATA/usr/share/icons/hicolor/256x256/mimetypes/paddoc.png %{buildroot}/usr/share/icons/hicolor/256x256/mimetypes/
cp -p ~/DATA/usr/share/icons/hicolor/256x256/apps/x-padxml.png %{buildroot}/usr/share/icons/hicolor/256x256/apps/

chmod +x %{buildroot}/usr/bin/padxml

%files
%dir /usr/share/applications
%dir /usr/share/icons/hicolor/16x16/mimetypes
%dir /usr/share/icons/hicolor/16x16/apps
%dir /usr/share/icons/hicolor/24x24/mimetypes
%dir /usr/share/icons/hicolor/24x24/apps
%dir /usr/share/icons/hicolor/32x32/mimetypes
%dir /usr/share/icons/hicolor/32x32/apps
%dir /usr/share/icons/hicolor/48x48/mimetypes
%dir /usr/share/icons/hicolor/48x48/apps
%dir /usr/share/icons/hicolor/64x64/mimetypes
%dir /usr/share/icons/hicolor/64x64/apps
%dir /usr/share/icons/hicolor/128x128/mimetypes
%dir /usr/share/icons/hicolor/128x128/apps
%dir /usr/share/icons/hicolor/256x256/mimetypes
%dir /usr/share/icons/hicolor/256x256/apps
%dir /usr/share/mime/packages
%dir /usr/share/pixmaps

/usr/bin/padxml
/usr/share/applications/x-padxml.desktop
/usr/share/pixmaps/padxml.png
/usr/share/mime/packages/x-padxml.xml
/usr/share/icons/hicolor/16x16/mimetypes/paddoc.png
/usr/share/icons/hicolor/16x16/apps/x-padxml.png
/usr/share/icons/hicolor/24x24/mimetypes/paddoc.png
/usr/share/icons/hicolor/24x24/apps/x-padxml.png
/usr/share/icons/hicolor/32x32/mimetypes/paddoc.png
/usr/share/icons/hicolor/32x32/apps/x-padxml.png
/usr/share/icons/hicolor/48x48/mimetypes/paddoc.png
/usr/share/icons/hicolor/48x48/apps/x-padxml.png
/usr/share/icons/hicolor/64x64/mimetypes/paddoc.png
/usr/share/icons/hicolor/64x64/apps/x-padxml.png
/usr/share/icons/hicolor/128x128/mimetypes/paddoc.png
/usr/share/icons/hicolor/128x128/apps/x-padxml.png
/usr/share/icons/hicolor/256x256/mimetypes/paddoc.png
/usr/share/icons/hicolor/256x256/apps/x-padxml.png

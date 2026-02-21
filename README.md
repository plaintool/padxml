# PadXml

A lightweight application designed to create, edit, and maintain PAD (Portable Application Description) XML files. PAD files are used to describe software products in a standardized format for software directories, download sites, and application catalogs, helping automate software listing and distribution.

[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](https://opensource.org/licenses/MIT)
[![Build with: Lazarus](https://img.shields.io/badge/Build_with-Lazarus-blueviolet)](https://www.lazarus-ide.org/)
[![Platform: Windows Linux](https://img.shields.io/badge/Platform-Windows_Linux-yellow)](#)
[![Latest Release](https://img.shields.io/github/v/release/plaintool/padxml?label=Release)](https://github.com/plaintool/padxml/releases/latest)

![sample1](samples/sample1.png)

## Installation

[![latest version](https://img.shields.io/github/v/release/plaintool/padxml?color=blue&label=Latest%20release&style=for-the-badge)](https://github.com/plaintool/padxml/releases/latest)

### Windows

Several installer options are available on the releases page:

| Description | Files |
|-------------|-------|
| **User installer (MSI)** — installs the application **for the current user** | `padxml‑x64.msi`<br>`padxml‑x86.msi` |
| **System installer (MSI)** — installs the application **for all users on the system** | `padxml‑x64‑allusers.msi`<br>`padxml‑x86‑allusers.msi` |
| **Portable version** — saves its settings to `form_settings.json` if it is near the executable; otherwise, in the user directory | `padxml‑x86‑x64‑portable.zip` |

Download the installer from the [releases page](https://github.com/plaintool/padxml/releases), run it, and follow the on-screen instructions. After installation, you can launch PadXml from the Start menu or from the desktop shortcut.

---

### Linux
### *Debian-like systems (Debian, Ubuntu, etc.)*

Download the appropriate `.deb` package for your system from the [releases page](https://github.com/plaintool/padxml/releases), or use `wget` directly. An example of using `wget` is provided on the release page.

If you downloaded the package manually, you can also install it via:
```bash
sudo dpkg -i /path/to/padxml.deb
```
If there are missing dependencies, fix them by running:
```bash
sudo apt-get install -f
```

### *RPM-based systems (Fedora, RHEL, CentOS, openSUSE, etc.)*

Download the appropriate `.rpm` package for your system, or use `wget` directly. An example of using `wget` is provided on the release page.

If you downloaded the package manually, you can also install it via:
```bash
sudo dnf install /path/to/padxml.rpm
```
If there are missing dependencies, fix them by running:
```bash
sudo dnf install -y gtk2
```

## Donate 💖

If you like this application, you can read how to support it [here](https://plaintool.github.io/padxml/donate.htm).

## Licensing

PadXml is licensed under the MIT License. See the LICENSE file for details.
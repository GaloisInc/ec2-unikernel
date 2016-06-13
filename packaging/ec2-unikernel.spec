Name:          ec2-unikernel
Version:       %{_version}
Release:       1%{?dist}
Summary:       A tool for uploading unikernels into EC2
Group:         System Environment/Base
License:       BSD3
URL:           http://github.com/GaloisInc/ec2-unikernel
Source0:       ec2-unikernel-%{_version}.tar.gz

BuildRequires: cabal-install, ghc-compiler, coreutils
Requires:      libguestfs-tools-c, glibc, libffi, gmp, zlib

%define debug_package %{nil}

%description
A tool for uploading and registering unikernels into the EC2 ecosystem,
including upload, import, and AMI registration of the unikernel binary.
Supports any paravirtualized unikernel that builds ELF binaries for
Xen, and can include an arbitrary number of modules (initrds, etc.) as
required.

%prep
%setup
cabal sandbox init
cabal update

%build
cabal install --disable-executable-dynamic

%install
rm -rf %{buildroot}
mkdir -p %{buildroot}%{_bindir}
install .cabal-sandbox/bin/ec2-unikernel %{buildroot}%{_bindir}/ec2-unikernel

%files
%{_bindir}/ec2-unikernel

%changelog


#!/bin/sh
rpm_package() {
  mkdir -p rpmbuild/{SOURCES,SPECS}
  mkdir -p results
  cabal sdist
  cp dist/ec2-unikernel*.tar.gz rpmbuild/SOURCES/
  cp packaging/ec2-unikernel.spec rpmbuild/SPECS/
  rpmbuild -ba --define "_version ${MY_VERSION}"     \
               --define "_topdir ${TOPDIR}/rpmbuild" \
               rpmbuild/SPECS/ec2-unikernel.spec
  cp rpmbuild/RPMS/*/ec2-unikernel-${MY_VERSION}-1.fc*.rpm results/
  cp rpmbuild/SRPMS/ec2-unikernel-${MY_VERSION}-1.*.src.rpm results/
}

source /etc/os-release

case "${ID}" in
  "ubuntu") TYPE="deb" ;;
  "debian") TYPE="deb" ;;
  "fedora") TYPE="rpm" ;;
  "centos") TYPE="rpm" ;;
  *)        TYPE="unknowne" ;;
esac

MY_VERSION=`grep "^version: " ec2-unikernel.cabal | sed "s/version: *//g"`
TOPDIR=`pwd`
echo "Building a ${TYPE} package for version ${MY_VERSION}"

case "${TYPE}" in
  "deb") echo "No debian builder yet." ; exit 1 ;;
  "rpm") rpm_package ;;
  *)     echo "Unknown package type."; exit 2 ;;
esac

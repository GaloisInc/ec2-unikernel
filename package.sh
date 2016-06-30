#!/bin/sh

rpm_package() {
  mkdir -p rpmbuild/{SOURCES,SPECS}
  mkdir -p packages
  cabal sdist
  cp dist/ec2-unikernel*.tar.gz rpmbuild/SOURCES/
  cp packaging/ec2-unikernel.spec rpmbuild/SPECS/
  rpmbuild -ba --define "_version ${MY_VERSION}"     \
               --define "_topdir ${TOPDIR}/rpmbuild" \
               rpmbuild/SPECS/ec2-unikernel.spec
  cp rpmbuild/RPMS/*/ec2-unikernel-${MY_VERSION}-1.fc*.rpm packages/
  cp rpmbuild/SRPMS/ec2-unikernel-${MY_VERSION}-1.*.src.rpm packages/
}

deb_package() {
  # get the base tarball
  SRC_TARBALL=ec2-unikernel_${MY_VERSION}.orig.tar.gz
  cabal sdist
  cp dist/ec2-unikernel*.tar.gz ./${SRC_TARBALL}

  # get the weird debian configuration tarball
  CONF_TARBALL=ec2-unikernel_${MY_VERSION}-1.debian.tar.gz
  NOW=`date -R`
  rm -rf tmp
  mkdir tmp
  cp -r packaging/debian tmp/debian
  sed -i -e "s!VERSION!${MY_VERSION}!g" -e "s!NOW!${NOW}!g" tmp/debian/changelog
  tar cz -C tmp -f ${CONF_TARBALL} debian/
  rm -rf tmp

  # build the desc file
  DESC_FILE=ec2-unikernel_${MY_VERSION}-1.dsc
  ORIG_SHA1=`openssl sha -sha1 ${SRC_TARBALL} | sed 's/.*= //g'`
  ORIG_SHA256=`openssl sha -sha256 ${SRC_TARBALL} | sed 's/.*= //g'`
  ORIG_SIZE=`stat -c "%s" ${SRC_TARBALL}`
  CONF_SHA1=`openssl sha -sha1 ${CONF_TARBALL} | sed 's/.*= //g'`
  CONF_SHA256=`openssl sha -sha256 ${CONF_TARBALL} | sed 's/.*= //g'`
  CONF_SIZE=`stat -c "%s" ${CONF_TARBALL}`
  sed -e "s!ORIG_SHA1!${ORIG_SHA1}!g"     \
      -e "s!CONF_SHA1!${CONF_SHA1}!g"     \
      -e "s!ORIG_SHA256!${ORIG_SHA256}!g" \
      -e "s!CONF_SHA256!${CONF_SHA256}!g" \
      -e "s!ORIG_SIZE!${ORIG_SIZE}!g"     \
      -e "s!CONF_SIZE!${CONF_SIZE}!g"     \
      -e "s!VERSION!${MY_VERSION}!g"      \
      packaging/ec2-unikernel.dsc > ${DESC_FILE}

  # now actually build the thing
  tar zxf ${SRC_TARBALL}
  tar zxf ${CONF_TARBALL} -C ec2-unikernel-${MY_VERSION}
  (cd ec2-unikernel-${MY_VERSION} && dpkg-buildpackage -rfakeroot -uc -us)

  # save the packages
  mkdir -p packages
  cp *.deb packages/
  cp *.dsc packages/
  cp *.changes packages/
}

. /etc/os-release

which ghc > /dev/null
if [ $? != 0 ]; then
  echo "GHC is required for package generation!"
  exit 1
fi

which cabal > /dev/null
if [ $? != 0 ]; then
  echo "Cabal-install is required for package generation!"
  exit 1
fi

case "${ID}" in
  "ubuntu") TYPE="deb" ;;
  "debian") TYPE="deb" ;;
  "fedora") TYPE="rpm" ;;
  "centos") TYPE="rpm" ;;
  *)        TYPE="unknown" ;;
esac

MY_VERSION=`grep "^version: " ec2-unikernel.cabal | sed "s/version: *//g"`
TOPDIR=`pwd`
echo "Building a ${TYPE} package for version ${MY_VERSION}"

case "${TYPE}" in
  "deb") deb_package ;;
  "rpm") rpm_package ;;
  *)     echo "Unknown package type."; exit 2 ;;
esac

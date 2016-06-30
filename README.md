# ec2-unikernel

This tool is designed to provide a single-step mechanism for uploading
your unikernels to run on EC2. At its core, it takes an ELF binary that
is the unikernel, along with any auxiliary modules, uploads them to S3,
and them bundles the whole collection as an AMI that you can then launch
at will.

THIS SOFTWARE IS ALPHA QUALITY. BE WARNED.

## Where to Get Binaries

### Fedora Binaries

The easiest way to get binary installations for Fedora 22, 23, and 24
is through the HaLVM repositories. Using this method will also get you
automatically updated when successive versions come out. To use the
HaLVM repositories, run `dnf install` with one of the following links,
depending on your version and architecture:

  * Fedora 22 (32-bit):
    (http://repos.halvm.org/fedora-22/i686/halvm-yum-repo-22-3.fc22.noarch.rpm)
  * Fedora 23 (32-bit):
    (http://repos.halvm.org/fedora-23/i686/halvm-yum-repo-23-3.fc23.noarch.rpm)
  * Fedora 24 (32-bit):
    (http://repos.halvm.org/fedora-24/i686/halvm-yum-repo-24-3.fc24.noarch.rpm)
  * Fedora 22 (64-bit):
    (http://repos.halvm.org/fedora-22/x86_64/halvm-yum-repo-22-3.fc22.noarch.rpm)
  * Fedora 23 (64-bit):
    (http://repos.halvm.org/fedora-23/x86_64/halvm-yum-repo-23-3.fc23.noarch.rpm)
  * Fedora 24 (64-bit):
    (http://repos.halvm.org/fedora-24/x86_64/halvm-yum-repo-24-3.fc24.noarch.rpm)

Then run `dnf update` to get all the information you need on the
packages in this repository, and `dnf install ec2-unikernel` to install
the tool

### Ubuntu Binaries

Ubuntu binaries are also available on `repos.halvm.org`, although not
in a nice friendly repository structure. (As an aside, if someone wants
to tell me how I could make such a thing, please send me an email.) So
you'll just need to download these manually:

  * Ubuntu 16.04 (32-bit):
    (http://repos.halvm.org/ubuntu-16.04/i686/ec2-unikernel_0.9-1_i386.deb)
  * Ubuntu 16.04 (64-bit):
    (http://repos.halvm.org/ubuntu-16.04/x86_64/ec2-unikernel_0.9-1_amd64.deb)

Both of these packages should be signed with the HaLVM Maintainer key (fetch
[here](http://repos.halvm.org/RPM-GPG-KEY-HaLVM), fingerprint 6240d595) using
the `dpkg-sig` tool, if you want to verify the release.

## Installation

First, we always suggest using a binary from the previous section, as
they will usually tell you about any software prerequisites you are
missing. (See the section on "Prerequisites" for non-software requirements.)

If you're prefer to build from source, you can either pull the latest
version from Hackage by doing:

```
cabal install ec2-unikernel
```

Or you can get the bleeding edge by pulling this repository and running
`cabal install` directly. If you do the latter, let me suggest that a
sandbox (or the forthcoming new-configure/new-build/new-install chain)
might be your friend, as `ec2-unikernel` has one hell of a dependency
chain.

## Limitations

At the moment, `ec2-unikernel` only works with paravirtualized, 64-bit
binaries. Extending the latter to support 32-bit binaries would be a
lovely introductory project for someone who wants to join the project.
Support for HVM domains might be a bit more work.

In addition, `ec2-unikernel` only works on Linux systems with the `guestfish`
program installed.

## Prerequisites

This program has three prerequisites:

  * You must have an AWS account, account key, and secret key, with all
    the relevant permissions to create S3 buckets and objects and register
    EC2 snapshots and APIs.

  * As part of this, you must create a `vmimport` role and use it. (Another
    feature for someone to add: allow people to use a different name for
    this role.) See [this page from
Amazon](https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/VMImportPrerequisites.html#vmimport-service-role).
You can find the policy files they mention in the `policies/` subdirectory.

  * You must have installed the `guestfish` program.


# ec2-unikernel

This tool is designed to provide a single-step mechanism for uploading
your unikernels to run on EC2. At its core, it takes an ELF binary that
is the unikernel, along with any auxiliary modules, uploads them to S3,
and them bundles the whole collection as an AMI that you can then launch
at will.

THIS SOFTWARE IS ALPHA QUALITY.

## Where to Get Binaries

TBD

## Installation

First, we always suggest using a binary from the previous section, as
they will usually tell you about any software prerequisites you are
missing. (See the section on "Prerequisites" for non-software requirements.)

If you're prefer to build from source, you can either pull the latest
version from Hackage by doing:

   cabal install ec2-unikernel

Or you can get the bleeding edge by pulling this repository and doing
a `cabal install` directly. If you do the latter, let me suggest that a
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


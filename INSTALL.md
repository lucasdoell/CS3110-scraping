# Installation Guide

## Dependencies
The following dependencies are required to build and run the application:
- mechaml
- ounit2
- batteries
- ANSITerminal
- python3
- python3-bs4
- python-is-python3

There are two ways to install the dependencies.

### Option 1: Using `make`
If you are on Ubuntu or using the CS 3110 VM, you can use the `make` command to install the dependencies.
```sh
make install
```
It will ask you for your sudo password, then install the necessary dependencies.

### Option 2: Manual Installation
To install the dependencies, run the following commands:
```sh
$ sudo apt-get upgrade
# or the equivalent depending on your operating system

# OPAM Packages
$ opam install mechaml
$ opam install ounit2
$ opam install batteries
$ opam install ANSITerminal

# Python Packages
$ sudo apt-get install python3
$ sudo apt-get install python3-bs4
$ sudo apt-get install python-is-python3
```

## Build
To build the application, run the following command:
```sh
$ make build
```

## Run
To run the application, run the following command:
```sh
$ make test
```
If all of the dependencies are functioning properly, the test output will look like this:
```
$ make test
OCAMLRUNPARAM=b dune exec test/main.exe
Testing HTML parsing:               

➾ Testing Basketball:

Player: Lebron James
Scoring: 25.8
Rebounding: 8.6
Shooting: 52.5
Assists: 6.3
Defense: 0.7
Misc: -47
Advanced: 32.1

* truncated for brevity *
```

## Troubleshooting
If the following error occurs during the installation of Mechaml:
```
<><> Error report <><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>
┌─ The following actions failed
│ λ build conf-gmp        4
│ λ build conf-pkg-config 2
└─ 
╶─ No changes have been performed

The packages you requested declare the following system dependencies. Please make sure they are installed before retrying:
    libgmp-dev pkg-config
```

The solution is to run the following command:
```sh
$ sudo apt-get install libgmp-dev pkg-config
```
Then, it will be possible to complete the installation of Mechaml by running:
```sh
$ opam install mechaml
```

If the following error occurs during the installation of Python:
```
E: Failed to fetch http://security.ubunty.com/ubuntu/pool/main/e/expat/libexpat1-dev_2.2.9-1ubuntu0.4_amd64.deb  404  Not Found
E: Failed to fetch http://security.ubunty.com/ubuntu/pool/main/z/zlib/zlib1g-dev_1.2.11.dfsg-2ubuntu1.3_amd64.deb  404  Not Found
E: Unable to fetch some archives, maybe run apt-get update or try with --fix-missing?
```

You can solve this problem by running the following command:
```sh
$ sudo apt-get update
```

This applies to any missing packages. Please try to run `sudo apt-get update` before installing any packages to make installation easier.
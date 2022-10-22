# Installation Guide

## Dependencies
The following dependencies are required to build and run the application:
- mechaml
- ounit2

To install the dependencies, run the following commands:
```bash
$ opam install mechaml
$ opam install ounit2
```

## Build
To build the application, run the following command:
```bash
$ make build
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

The solution to this problem is to run the following command:
```bash
$ sudo apt-get install libgmp-dev pkg-config
```
Then, it will be possible to complete the installation of Mechaml by running:
```bash
$ opam install mechaml
```
# Installation Guide

## Dependencies
The following dependencies are required to build and run the application:
- mechaml
- ounit2
- batteries

To install the dependencies, run the following commands:
```bash
$ opam install mechaml
$ opam install ounit2
$ opam install batteries
```

## Build
To build the application, run the following command:
```bash
$ make build
```

## Run
To run the application, run the following command:
```bash
$ make test
```
If all of the dependencies are functioning properly, the test output will look like this:
```
Testing HTML parsing:               
Testing [data/query.html]

Player: Lebron James
Scoring: 'PPG'; '25.5';
Rebounding: 'RPG'; '12.5';
Shooting: 'EFG%'; '50.0';
Assists: 'APG'; '7.0';
Defense: 'BPG'; '1.0';
Misc: '+/-'; '-11';
Advanced: 'USG%'; '29.9'
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
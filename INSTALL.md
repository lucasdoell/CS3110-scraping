## Troubleshooting
The following error occurred during the installation of Mechaml:
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

The solution to this problem was to run:
```bash
sudo apt-get install libgmp-dev pkg-config
```
Then, it was possible to complete the installation of Mechaml by running:
```bash
opam install mechaml
```
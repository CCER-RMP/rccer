
# ccer R Library

## How to install

From within an R Studio session or script:

```
# only need to run this once
install.packages("devtools")
# re-run this when you want to get the latest code from the repo
devtools::install_github("CCER-RMP/rccer")
```

## How to use

In R:

```
# load the package
library("rccer")
# install commonly used packages
installRequiredPackages()
# load commonly used packages
rmpInit()
```

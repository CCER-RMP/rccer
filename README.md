
# ccer R Library

Commonly used code for doing data validation, ETL, analysis, etc. This
is a work in progress.

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
# load them
loadRequiredPackages()

# ready to use the functions in ccer.R

df <- readCSV("C:/Users/username/somefile.csv")

transformed <- df %>%
    mutate(field2 = field1 + 2)

writeToTsv("transformed.txt", transformed)
```

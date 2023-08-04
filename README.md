This is a private R package with no license due to assuming no one else will use this package.

Install the package.
```
devtools::install_github('lijiaqi-github/R.package.atelier')
```

The following is the code added to Rprofile.site when R version is updated.<br>
```
file.edit(file.path(R.home("etc"), "Rprofile.site"))
################################################################################
    options(scipen = 8,digits = 8)
    grDevices::windowsFonts(Times = grDevices::windowsFont("Times New Roman"))

     suppressPackageStartupMessages(library(tableone))
     suppressPackageStartupMessages(library(rms))
     suppressPackageStartupMessages(library(epiDisplay))
     suppressPackageStartupMessages(library(DescTools))
     suppressPackageStartupMessages(library(tidyverse))
     suppressPackageStartupMessages(library(data.table))
     library(atelier)

     cat("Package loading accomplished")

     Sys.setenv(`_R_S3_METHOD_REGISTRATION_NOTE_OVERWRITES_` = "false")
################################################################################
```

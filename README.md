A private R package with no license due to assuming no one else will use this package.

Install the package.
```
devtools::install_github('lijiaqi-github/R.package')
```
The following is the code added to Rprofile.site when R version is updated.<br>
```
################################################################################
    options(scipen = 8,digits = 8)
    grDevices::windowsFonts(Times = grDevices::windowsFont("Times New Roman"))
    options(max.print = 1000)


     suppressPackageStartupMessages(library(tableone))
     suppressPackageStartupMessages(library(rms))
     suppressPackageStartupMessages(library(epiDisplay))
     suppressPackageStartupMessages(library(DescTools))
     suppressPackageStartupMessages(library(tidyverse))
     suppressPackageStartupMessages(library(data.table))
     library(MCV)

     cat("Package loading accomplished")

     Sys.setenv(`_R_S3_METHOD_REGISTRATION_NOTE_OVERWRITES_` = "false")
################################################################################
```

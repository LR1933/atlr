This is a private R package with no license due to assuming no one else will use this package.

Install the package.
```
devtools::install_github('lijiaqi-github/R.atelier')
```

The following is the code added to Rprofile.site when R version is updated.<br>
```
################################################################################
options(scipen = 8,digits = 8)
grDevices::windowsFonts(Times = grDevices::windowsFont("Times New Roman"))
options(max.print = 10000)

cat("\033[36mType the following code to edit Rprofile.site.\033[0m\n")
cat("  file.edit(file.path(Sys.getenv('R_HOME'), 'etc', 'Rprofile.site'))\n")
cat("\033[36mType the following code to install MCV.\033[0m\n")
cat("  devtools::install_github('lijiaqi-github/R.atelier')\n")

Sys.setenv(`_R_S3_METHOD_REGISTRATION_NOTE_OVERWRITES_` = "false")
################################################################################
```

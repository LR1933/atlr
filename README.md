## Edit Rprofile.site:

```
file.edit(file.path(Sys.getenv('R_HOME'), 'etc', 'Rprofile.site'))
```
## Install atlr:
```
devtools::install_github('lijiaqi-github/atlr')
```

## Update packages:
```
Update.packages(ask = FALSE, dependencies = TRUE)
```

## Check packages:
```
installed.packages()[, c("Package", "Version", "Built")]
```
## Check packages Version:
```
packageVersion("")
```

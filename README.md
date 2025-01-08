## Type the following code to edit Rprofile.site:

```
file.edit(file.path(Sys.getenv('R_HOME'), 'etc', 'Rprofile.site'))
```
## Type the following code to install atlr:
```
devtools::install_github('lijiaqi-github/atlr')
```

## Update packages:
```
pdate.packages(ask = FALSE, dependencies = TRUE)
```

## Check packages:
```
installed.packages()[, c("Package", "Version", "Built")]
```
## Check packages Version:
```
packageVersion("rms")
```

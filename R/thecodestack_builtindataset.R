#' @docType data
#' @name testdt
#'
#'
NULL
testdt <- data.table::fread("C:/Users/lijiaqi/OneDrive/3. Academic field/1. R stack/4. packages/thecodestack/data/testdataset.csv",
                      na.strings = c(NULL,''))

# save(testdt, file = "thecodestack_builtindataset.Rdata")

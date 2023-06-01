## the package for R functions and packages ###################################3
## 2023/04/05 version ##########################################################
## shift + ctrl + alt + R

##【Data Cleansing】############################################################
## rounding ####################################################################
#' Title
#'
#' @param data
#' @param n
#'
#' @return
#' @export
#'
#' @examples
round_function <- function(data, n){
    data_sign  <- sign(data)
    data       <- abs(data) * 10 ^ n
    data       <- data + 0.5
    data       <- trunc(data)
    return(data_sign * data / 10 ^ n)
}

## geometric mean ##############################################################
#' geometric mean
#'
#' @param geo_mean.values  the $variable
#'
#' @return geometric mean
#' @export
#'
#' @examples
# expmean(log(x))) # geometric mean
# exp(sum(log(x)/length(x)) # geometric mean
# exp(mean(log(x)))
geo_mean <- function(geo_mean.values){exp(mean(log(geo_mean.values)))}

## frequency and type ##########################################################
#' Title
#'
#' @param fvar.variable
#' @param fvar.bar
#'
#' @return
#' @export
#'
#' @examples
fvar <- function(fvar.variable, fvar.bar = TRUE) {
    fvar.variable.label <-
        sub(".*\\$", "", deparse(substitute(fvar.variable)))
    if (fvar.bar) {
        if (nlevels(as.factor(fvar.variable)) <= 10) {
            print(
                epiDisplay::tab1(
                    fvar.variable,
                    missing = TRUE,
                    bar.values = "frequency",
                    cex = 1,
                    cex.names = 1,
                    main = paste(
                        "The frequency distribution plot of",
                        fvar.variable.label
                    ),
                    xlab = paste(fvar.variable.label, "values"),
                    ylab = "Frequency",
                    col = "black"
                )
            )
        }
        else{
            print(
                epiDisplay::tab1(
                    fvar.variable,
                    missing = TRUE,
                    bar.values = 0,
                    cex = 1,
                    cex.names = 1,
                    main = paste(
                        "The frequency distribution plot of",
                        fvar.variable.label
                    ),
                    xlab = paste(fvar.variable.label, "values"),
                    ylab = "Frequency",
                    col = "black"
                )
            )
        }
    }
    else {
        print(
            epiDisplay::tab1(
                fvar.variable,
                missing = TRUE,
                bar.values = "frequency",
                cex = 1,
                cex.names = 1,
                main = paste(
                    "The frequency distribution plot of",
                    fvar.variable.label
                ),
                xlab = paste(fvar.variable.label, "values"),
                ylab = "Frequency",
                col = "black"
            )
        )
    }
    cat(
        "\n",
        paste(
            "The type of variable is",
            typeof(fvar.variable),
            "/",
            class(fvar.variable)
        ),
        "\n",
        paste("The number of zero variables is",
              sum(fvar.variable == 0)
        ),
        "\n",
        paste("The number of missing variables is",
              sum(is.na(fvar.variable))
        ),
        "\n",
        paste("The number of null variables is",
              sum(is.null(fvar.variable))
        ),
        "\n",
        paste("The number of variables containing space is",
              sum(grepl(" ", fvar.variable))
        ),
        sep = ""
    )
}

## summary #####################################################################
#' Title
#'
#' @param fs.varibale
#' @param fs.group
#'
#' @return
#' @export
#'
#' @examples
fs <- function(fs.varibale,fs.group = NA){
    fsd <- data.table(var  = fs.varibale,
                      cat  = fs.group)
    fs  <- fsd[, .(Nrow    = NROW(var),
                 　Sum     = round_function(sum(var, na.rm = TRUE), 8),
                　 Mean    = round_function(mean(var, na.rm = TRUE), 8),
                   stdDev  = round_function(sd(var, na.rm = TRUE),8),
                   Min     = round_function(min(var, na.rm = TRUE), 8),
                   Q25     = round_function(quantile(var, probs = 0.25,
                                                     na.rm = TRUE), 8),
                   Median  = round_function(median(var, na.rm = TRUE), 8),
                   Q75     = round_function(quantile(var, probs = 0.75,
                                                     na.rm = TRUE), 8),
                   Max     = round_function(max(var, na.rm = TRUE), 8),
                   Missing = sum(is.na(var))
    ),
    by = cat]
    fs <- as.data.table(fs)
    setnames(fs, 1, "Groups")
    return(fs)
}

## dummy variable ##############################################################
#' Title
#'
#' @param dv.variable
#' @param dv.n
#'
#' @return
#' @export
#'
#' @examples
dv <- function(dv.variable,dv.n){
    dv.dummy <- cut(dv.variable,
                    quantile(dv.variable,
                             probs = seq(0, 1, 1/dv.n),
                             na.rm = TRUE
                    ),
                    labels = c(0:(dv.n - 1)),
                    include.lowest = TRUE
    )
    return(dv.dummy)
}

## update Japanese .calender ###################################################
#' Title
#'
#' @param J.calendar
#'
#' @return
#' @export
#'
#' @examples
update.J.cal <- function(J.calendar) {
    cat("This function supports a conversion range between 大正1年(1912) and 令和50年(2068).")
    for (i in 1:15) {
        old_val <- paste0("大", i, "\\.")
        new_val <- paste0(1911 + i, "\\.")
        J.calendar <- gsub(old_val, new_val, J.calendar)
    }
    for (i in 1:64) {
        old_val <- paste0("昭", i, "\\.")
        new_val <- paste0(1925 + i, "\\.")
        J.calendar <- gsub(old_val, new_val, J.calendar)
    }
    for (i in 1:31) {
        old_val <- paste0("平", i, "\\.")
        new_val <- paste0(1988 + i, "\\.")
        J.calendar <- gsub(old_val, new_val, J.calendar)
    }
    for (i in 1:50) {
        old_val <- paste0("令", i, "\\.")
        new_val <- paste0(2018 + i, "\\.")
        J.calendar <- gsub(old_val, new_val, J.calendar)
    }
    return(J.calendar)
}

##【Data analysis】#############################################################
## Cochran Armitage Trend Test #################################################
# dose <- matrix(c(10,9,10,7, 0,1,0,3),
#                byrow=TRUE,
#                nrow=2,
#                dimnames=list(resp=0:1, dose=0:3)
#                )
# prop.trend.test(dose)
#' Title
#'
#' @param Realtive_risk_number.exposure
#' @param Realtive_risk_number.group
#'
#' @return
#' @export
#'
#' @examples
Cochran_Armitage_Trend_Test <- function(Realtive_risk_number.exposure,
                                        Realtive_risk_number.group){
    Realtive_risk_number.crosstable <- gmodels::CrossTable(
        Realtive_risk_number.exposure,
        Realtive_risk_number.group,
        prop.t = FALSE,
        prop.r = FALSE,
        prop.c = FALSE
    )
    Realtive_risk_number.occurtable <- data.frame(
        No.of_non_cases = Realtive_risk_number.crosstable$t[1,],
        No.of_cases     = Realtive_risk_number.crosstable$t[2,]
    )
    ##!! dont use data.table because data.table has no row title
    Realtive_risk_number.horizontal_occurtable <- as.matrix(
        t(Realtive_risk_number.occurtable)
    )
    print(Realtive_risk_number.horizontal_occurtable)
    DescTools::CochranArmitageTest(Realtive_risk_number.horizontal_occurtable)
}




## Table 1 #####################################################################
#' Title
#'
#' @param Table_one.analysis_data
#' @param Table_one.all_varibales
#' @param Table_one.categorical_variables
#' @param Table_one.group
#' @param Table_one.nonnormal_variables
#' @param Table_one.contDigits
#'
#' @return
#' @export
#'
#' @examples
Table_one <- function(Table_one.analysis_data,
                      Table_one.all_varibales,
                      Table_one.categorical_variables,
                      Table_one.group = NA,
                      Table_one.nonnormal_variables = NULL,
                      Table_one.contDigits = 2
                      ) {
    if (is.na(Table_one.group)) {
        Table_one.print <- print(
            tableone::CreateTableOne(
                data          = Table_one.analysis_data,
                vars          = Table_one.all_varibales,
                factorVars    = Table_one.categorical_variables
            ),
            contDigits    = Table_one.contDigits,
            format        = c("p"),
            #format = c("fp", "f", "p", "pf")
            nonnormal     = Table_one.nonnormal_variables,
            showAllLevels = FALSE,
            formatOptions = list(big.mark = ","),
            noSpaces      = TRUE
        )}
    else {
        Table_one.print <- print(
            tableone::CreateTableOne(
                data          = Table_one.analysis_data,
                vars          = Table_one.all_varibales,
                strata        = Table_one.group,
                factorVars    = Table_one.categorical_variables
            ),
            contDigits    = Table_one.contDigits,
            format        = c("p"),
            #format = c("fp", "f", "p", "pf")
            nonnormal     = Table_one.nonnormal_variables,
            showAllLevels = FALSE,
            formatOptions = list(big.mark = ","),
            noSpaces      = TRUE
        )
    }
    write.table(Table_one.print,
                paste0("clipboard-",
                       formatC(100*100,
                               format = "f",
                               digits = 0)
                ),
                sep       = "\t",
                row.names = TRUE,
                col.names = FALSE,
                dec       = "."
    )
}

## number of total and event ###################################################
#' Title
#'
#' @param fpn.exposure
#' @param fpn.event
#'
#' @return
#' @export
#'
#' @examples
fpn <- function(fpn.exposure,
                fpn.event,
                fpn.test = FALSE){
    if (length(unique(fpn.event)) == 2) {
        if (fpn.test) {
            fpn.crosstable <- gmodels::CrossTable(
                fpn.event,
                fpn.exposure,
                fisher = TRUE,
                prop.t = FALSE,
                prop.r = FALSE,
                prop.c = FALSE
            )
        } else {
            fpn.crosstable <- gmodels::CrossTable(
                fpn.event,
                fpn.exposure,
                fisher = FALSE,
                prop.t = FALSE,
                prop.r = FALSE,
                prop.c = FALSE
            )
        }
        fpn.occurtable <- data.frame(No.of_non_cases = fpn.crosstable$t[1, ],
                                     No.of_cases     = fpn.crosstable$t[2, ])
        ##!! not use data.table because data.table has no row title !!##
        fpn.occurtable$No.of_participants <- rowSums(fpn.occurtable)
        fpn.occurtable <- fpn.occurtable[, -1]
        fpn.horizontal_occurtable <- as.data.frame(t(fpn.occurtable))
        fpn.print_horizontal_occurtable <- as.data.frame(t(fpn.occurtable))
        fpn.print_horizontal_occurtable$total =
            rowSums(fpn.print_horizontal_occurtable)
        print(fpn.print_horizontal_occurtable[c("No.of_participants",
                                                "No.of_cases"), ])
        write.table(
            fpn.horizontal_occurtable[c("No.of_participants",
                                        "No.of_cases"), ],
            paste0("clipboard-",
                   formatC(
                       100 * 100,
                       format = "f",
                       digits = 0
                   )),
            sep       = "\t",
            row.names = FALSE,
            col.names = FALSE,
            dec       = "."
        )
    } else {
    if (fpn.test) {
        fpn.crosstable <- gmodels::CrossTable(
            fpn.event,
            fpn.exposure,
            fisher = TRUE,
            prop.t = FALSE,
            prop.r = FALSE,
            prop.c = FALSE
        )
    } else {
        fpn.crosstable <- gmodels::CrossTable(
            fpn.event,
            fpn.exposure,
            fisher = FALSE,
            prop.t = FALSE,
            prop.r = FALSE,
            prop.c = FALSE
        )
    }
}
}

## number of person-years ######################################################
#' Title
#'
#' @param fpy.exposure
#' @param fpy.event
#'
#' @return
#' @export
#'
#' @examples
fpy <- function(fpy.exposure, fpy.event) {
    fpy <- as.data.table(fs(fpy.exposure, fpy.event))
    setnames(fpy,"Sum","Peason-years")
    setorder(fpy,"Groups")
    fpy.table <- as.data.frame(t(fpy[,c(1,3)]))
    colnames(fpy.table) <- NULL
    fpy.table$total <- as.numeric(as.data.table(fs(fpy.exposure))[,3])
    fpy.table[1,]$total <- ""
    print(fpy.table)
    write.table(
        round_function(t(fpy[, 3]), 0),
        paste0("clipboard-",
               formatC(
                   100 * 100,
                   format = "f",
                   digits = 0
               )),
        sep       = "\t",
        row.names = FALSE,
        col.names = FALSE,
        dec       = "."
    )
}

## logistic model ##############################################################
#' Title
#'
#' @param ORs.analysis_data
#' @param ORs.model
#' @param ORs.n
#'
#' @return
#' @export
#'
#' @examples
ORs <- function(ORs.analysis_data,ORs.model,ORs.n){
    logistic_model <- glm(ORs.model,
                          data   = ORs.analysis_data,
                          family = binomial(link = 'logit'))
    ORs.observation <- paste("Number", "of", "observation", "is",
                             length(residuals(logistic_model)))
    ORs.model_results <- exp(cbind(OR = coef(logistic_model),
                                     confint(logistic_model)))
    ORs.round_model_results <- round_function(ORs.model_results,2)
    ORs.table <- data.table(Exposures = row.names(ORs.round_model_results),
                            ORs       = paste(ORs.round_model_results[,1],
                                              " (",
                                              ORs.round_model_results[,2],
                                              "-",
                                              ORs.round_model_results[,3],
                                              ")",
                                              sep = ""),
                            P_value   = round_function(
                                as.numeric(
                                    summary(logistic_model)$coefficients[,c(4)]
                                ), 4
                            )
    )
    ORs.horizontal_table <- as.data.frame(t(ORs.table))
    ORs.horizontal_table_print <- ORs.horizontal_table[
        row.names(ORs.horizontal_table) == "ORs",
        (ncol(ORs.horizontal_table) - ORs.n + 1):ncol(ORs.horizontal_table)
    ]
    print(summary(logistic_model))
    print(ORs.table)
    print(ORs.horizontal_table_print)
    print(noquote(ORs.observation))
    write.table(ORs.horizontal_table_print,
                paste0("clipboard-",
                       formatC(100*100,
                               format = "f",
                               digits = 0)
                ),
                sep       = "\t",
                row.names = FALSE,
                col.names = FALSE,
                dec       = "."
    )
}

## optimism adjusted AUC #######################################################
# http://cainarchaeology.weebly.com/r-function-for-optimism-adjusted-auc.html
# https://academic.oup.com/aje/article/180/3/318/2739260
# !!note: the Dependent Variable must be stored in the first column to the left
#' Title
#'
#' @param data
#' @param fit
#' @param B
#'
#' @return
#' @export
#'
#' @examples
optimism_adjusted_AUC <- function(data, fit, B){
    library(kimisc)
    library(pROC)
    fit.model <- fit
    data$pred.prob <- fitted(fit.model)
    auc.app <- roc(as.numeric(unlist(data[,1])),
                   data$pred.prob,
                   data = data)$auc # require 'pROC'
    auc.boot <- vector(mode = "numeric", length = B)
    auc.orig <- vector(mode = "numeric", length = B)
    o <- vector(mode = "numeric", length = B)
    auc.adj.individual <- vector(mode = "numeric", length = B)
    for (i in 1:B) {
        boot.sample <- sample.rows(data,
                                   nrow(data),
                                   replace = TRUE) # require 'kimisc'
        fit.boot <- glm(formula(fit.model),
                        data   = boot.sample,
                        family = "binomial")
        boot.sample$pred.prob <- fitted(fit.boot)
        auc.boot[i] <- roc(as.numeric(unlist(boot.sample[,1])),
                           boot.sample$pred.prob,
                           data = boot.sample)$auc
        data$pred.prob.back <- predict.glm(fit.boot,
                                           newdata = data,
                                           type    = "response")
        auc.orig[i] <- roc(as.numeric(unlist(data[,1])),
                           data$pred.prob.back,
                           data = data)$auc
        o[i] <- auc.boot[i] - auc.orig[i]
        auc.adj.individual[i] = auc.app - o[i]
    }
    auc.adj <- auc.app - (sum(o)/B)
    boxplot(auc.boot, auc.orig, names = c("auc.boot", "auc.orig"))
    title(main    = paste("Optimism-adjusted AUC",
                          "\nn of bootstrap resamples:",
                          B),
    sub     = paste("auc.app (blue line)=",
                    round(auc.app, digits = 4),
                    "\nadj.auc (red line)=",
                    round(auc.adj, digits = 4)),
    cex.sub = 0.8)
    abline(h   = auc.app,
           col = "blue",
           lty = 2)
    abline(h   = auc.adj,
           col = "red",
           lty = 3)
    The95CI  <- data.table(
        optimism_adjusted_AUC = auc.adj,
        under_95_CI           = quantile(auc.adj.individual, probs = 0.025),
        upper_95_CI           = quantile(auc.adj.individual, probs = 0.925))
    rownames(The95CI) <- ""
    return(The95CI)
}

## cox model ###################################################################
# The proportional hazards assumption is checked using statistical tests and
# -graphical diagnostics based on the scaled Schoenfeld residuals.
# In principle, the Schoenfeld residuals are independent of time. Plot with a
# -non-random pattern against time is evidence of violation of the assumption.
#' Title
#'
#' @param phdata
#' @param phmodel
#'
#' @return
#' @export
#'
#' @examples
PH_assumption <- function(phdata,phmodel){
    phdata <<- phdata
    zph <- survival::cox.zph(survival::coxph(phmodel,
                                             data = phdata))
    print(zph)
    survminer::ggcoxzph(zph)
    rm(phdata, envir = .GlobalEnv)
}

#' Title
#'
#' @param HRs.analysis_data
#' @param HRs.model
#' @param HRs.n
#'
#' @return
#' @export
#'
#' @examples
HRs <- function(HRs.analysis_data,HRs.model,HRs.n){
    cox_model <- survival::coxph(HRs.model,
                                 data = HRs.analysis_data,)
    HRs.observation <- paste("Number", "of", "observation", "is",
                             length(residuals(cox_model)))
    HRs.model_results <- exp(cbind(HR = coef(cox_model),
                                     confint(cox_model)))
    HRs.round_model_results <- round_function(HRs.model_results,2)
    HRs.table <- data.frame(Exposures = row.names(HRs.round_model_results),
                            HRs       = paste(HRs.round_model_results[,1],
                                              " (",
                                              HRs.round_model_results[,2],
                                              "-",
                                              HRs.round_model_results[,3],
                                              ")",
                                              sep = ""),
                            P_value   = round_function(
                                as.numeric(
                                    summary(cox_model)$coefficients[,c(5)]
                                ), 4
                                )
                            )
    HRs.horizontal_table <- as.data.frame(t(HRs.table))
    summary(cox_model)
    print(HRs.table)
    HRs.horizontal_table_print <- HRs.horizontal_table[
        row.names(HRs.horizontal_table) == "HRs",
        (ncol(HRs.horizontal_table) - HRs.n + 1):ncol(HRs.horizontal_table)]
    print(summary(cox_model))
    print(HRs.table)
    print(HRs.horizontal_table_print)
    print(noquote(HRs.observation))
    write.table(HRs.horizontal_table_print,
                paste0("clipboard-",
                       formatC(100*100,
                               format = "f",
                               digits = 0)),
                sep       = "\t",
                row.names = FALSE,
                col.names = FALSE,
                dec       = "."
    )
}

#' Title
#'
#' @param KMplot.analysis_data
#' @param KMplot.model
#'
#' @return
#' @export
#'
#' @examples
KMplot <- function(KMplot.analysis_data, KMplot.model){
    KMplot.KM <- survminer::surv_fit(KMplot.model,
                                     data = KMplot.analysis_data)
    # summary(KMplot, times=seq(0, 1000, 250))
    survminer::ggsurvplot(KMplot.KM,
                          data               = KMplot.analysis_data,
                          conf.int           = TRUE,
                          pval               = TRUE,
                          pval.method        = FALSE,
                          pval.coord         = c(0,1.0),
                          # test.for.trend     = TRUE, # > 2 groups
                          risk.table         = TRUE,
                          # surv.median.line   = c("hv"),
                          # legend.labs        = c("Group1", "Group2"),
                          legend.title       = "Group",
                          # palette            = c("gray25", "gray50"),
                          risk.table.height  = .25)
}


#' Title
#'
#' @param FG.time
#' @param FG.status
#' @param FG.model
#' @param FG.n
#'
#' @return
#' @export
#'
#' @examples
Fine.Gray.HRs <- function(FG.time,FG.status,FG.model,FG.n){
    FG.cox_model <- cmprsk::crr(time = FG.time,
                                status = FG.status,
                                FG.model,
                                failcode = 1,
                                cencode = 0
    )
    FG.observation <- paste("Number","of","observation", "is",
                            FG.cox_model$n
    )
    FG.model_results <- summary(FG.cox_model)
    FG.round_model_results <- round_function(FG.model_results$conf.int[,-2],2)
    FG.table <- data.frame(Exposures = row.names(FG.round_model_results),
                           FG.HRs       = paste(FG.round_model_results[,1],
                                                " (",
                                                FG.round_model_results[,2],
                                                "-",
                                                FG.round_model_results[,3],
                                                ")",
                                                sep = ""),
                           P_value   = round_function(
                               FG.model_results$coef[,5]
                               , 4
                           )
    )
    FG.horizontal_table <- as.data.frame(t(FG.table))
    summary(FG.cox_model)
    print(FG.table)
    FG.horizontal_table_print <- FG.horizontal_table[
        row.names(FG.horizontal_table) == "FG.HRs",
        (ncol(FG.horizontal_table) - FG.n + 1):ncol(FG.horizontal_table)
    ]
    print(summary(FG.cox_model))
    print(FG.table)
    print(FG.horizontal_table_print)
    print(noquote(FG.observation))
    write.table(FG.horizontal_table_print,
                paste0("clipboard-",
                       formatC(100*100,
                               format = "f",
                               digits = 0)
                ),
                sep       = "\t",
                row.names = FALSE,
                col.names = FALSE,
                dec       = "."
    )
}

## modified_poisson_model ######################################################
#' Title
#'
#' @param PRs.analysis_data
#' @param PRs.model
#' @param PRs.n
#'
#' @return
#' @export
#'
#' @examples
PRs <- function(PRs.analysis_data,PRs.model,PRs.n){
    modified_poisson_model <- glm(PRs.model,
                                  data   = PRs.analysis_data,
                                  family = poisson(link = "log")
    )
    PRs.observation <- paste("Number","of","observation", "is",
                             length(residuals(modified_poisson_model)))
    PRs.model_results <- coeftest(modified_poisson_model,
                                  vcov = sandwic
    )
    PRs.round_model_results <- round_function(
        exp(
            cbind(PR  = PRs.model_results[,1],
                  LCI = PRs.model_results[,1] + qnorm(0.05/2)*PRs.model_results[,2],
                  UCI = PRs.model_results[,1] - qnorm(0.05/2)*PRs.model_results[,2])
        ),2
    )
    PRs.table <- data.table(Exposures = row.names(PRs.round_model_results),
                            PRs       = paste(PRs.round_model_results[,1],
                                              " (",
                                              PRs.round_model_results[,2],
                                              "-",
                                              PRs.round_model_results[,3],
                                              ")",
                                              sep = ""))
    PRs.horizontal_table <- as.data.table(t(PRs.table))
    PRs.horizontal_table_print <- PRs.horizontal_table[
        row.names(PRs.horizontal_table) == "PRs",
        (ncol(PRs.horizontal_table) - PRs.n + 1):ncol(PRs.horizontal_table)
    ]
    print(summary(modified_poisson_model))
    print(PRs.table)
    print(PRs.horizontal_table[,
                               (ncol(PRs.horizontal_table) - PRs.n + 1):ncol(PRs.horizontal_table)
    ]
    )
    write.table(PRs.horizontal_table_print,
                paste0("clipboard-",
                       formatC(100*100,
                               format = "f",
                               digits = 0)
                ),
                sep       = "\t",
                row.names = FALSE,
                col.names = FALSE,
                dec       = "."
    )
    print(noquote(PRs.observation))
}

## generalized estimating equation model ######################################
#' Title
#'
#' @param GEE.analysis_data
#' @param GEE.model
#' @param GEE.order_variable
#' @param GEE.n
#'
#' @return
#' @export
#'
#' @examples
GEE <- function(GEE.analysis_data,
                GEE.model,GEE.order_variable,
                GEE.n){
    GEE.analysis_data$ID_GEE <- GEE.order_variable
    GEE.analysis_data <- arrange(GEE.analysis_data,ID_GEE)
    GEE_model <- GEEglm(GEE.model,
                        id     = ID_GEE,
                        data   = GEE.analysis_data,
                        corstr = 'exchangeable',
                        family = 'gaussian')
    GEE.observation <- paste("Number","of","observation","is",
                             length(residuals(GEE_model)))
    GEE.model_results <- coef(summary(GEE_model))
    GEE.coefficient <- with(
        as.data.table(GEE.model_results),
        cbind(coefficient = as.numeric(Estimate),
              lwr = as.numeric(Estimate - qnorm((1 + 0.95)/2)*Std.err),
              upr = as.numeric(Estimate + qnorm((1 + 0.95)/2)*Std.err))
    )
    GEE.round_coefficient <- round_function(GEE.coefficient,2)
    GEE.table <- data.table(Exposures   = row.names(GEE.model_results),
                            coefficient = as.numeric(GEE.round_coefficient[, 1]),
                            The_95_CI   = paste(" ",GEE.round_coefficient[, 2],
                                                "-",GEE.round_coefficient[, 3],
                                                sep = ""),
                            P_value     = round_function(GEE.model_results[, 4],
                                                         3)
    )
    GEE.table$P_value[GEE.table$P_value <= 0.001] <- "<0.001"
    GEE.table$P_value[
        GEE.table$P_value >  0.001 & GEE.table$P_value <= 0.01
    ] <- "<0.01"
    GEE.print_table <- GEE.table[
        as.numeric(nrow(GEE.table) - GEE.n + 1):nrow(GEE.table),
    ]
    print(GEE_model)
    summary(GEE_model)
    print(GEE.model_results)
    print(GEE.coefficient)
    print(GEE.print_table)
    write.table(GEE.print_table[,2:4],
                paste0("clipboard-",
                       formatC(100*100,
                               format = "f",
                               digits = 0)
                ),
                sep       = "\t",
                row.names = FALSE,
                col.names = FALSE,
                dec       = "."
    )
    print(noquote(GEE.observation))
}

##【Build a theme】#############################################################
#' @importFrom rstudioapi create_theme add_theme
#' @export
mytheme <- function() {
    theme_file <-
        system.file("themes/dark.rstheme", package = "MCV")
    add_theme(theme_file)
    create_theme("Dark")
}


##【Build a package】###########################################################
# devtools::load_all() # loading the latest package for testing
# ctrl+alt+shift+R
devtools::build()

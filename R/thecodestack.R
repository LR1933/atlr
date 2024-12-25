## 2024/04/01 version ##########################################################

##【Data cleansing】############################################################
## rounding ####################################################################
#' Title rounding
#'
#' @param data
#' @param n
#'
#' @return
#' @export
#'
#' @examples round_function(iris$Sepal.Length,0)
round_function <- function(data, n){
    data_sign  <- sign(data)
    data       <- abs(data) * 10 ^ n
    data       <- data + 0.5
    data       <- trunc(data)
    return(data_sign * data / 10 ^ n)
}

## copy paste ##################################################################
#' Title copy paste
#'
#' @param fcopy.var
#'
#' @return
#' @export
#'
#' @examples fcopy(iris$Sepal.Length)
fcopy <- function(fcopy.var) {
write.table(fcopy.var,
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

## geometric mean ##############################################################
#' geometric mean
#'
#' @param geo_mean.values  the $variable
#'
#' @return geometric mean
#' @export
#'
#' @examples geo_mean(iris$Sepal.Length)
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
#' @examples fvar(iris$Sepal.Length)
fvar <- function(fvar.variable, fvar.bar = TRUE) {
    fvar.variable.label <-
        sub(".*\\$", "", deparse(substitute(fvar.variable)))
        if (nlevels(as.factor(fvar.variable)) <= 10) {
            print(
                epiDisplay::tab1(
                    fvar.variable,
                    graph = fvar.bar,
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
                    graph = fvar.bar,
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
    cat(
        "\n",
        paste(
            "The type/class of variable: ",
            typeof(fvar.variable),
            "/",
            class(fvar.variable)
        ),
        "\n",
        paste(
            "The number of zero/missing/null/space-inclusive variables: ",
            sum(fvar.variable %in% c(0,"0"), na.rm = TRUE),
            "/",
            sum(is.na(fvar.variable)),
            "/",
            sum(is.null(fvar.variable)),
            "/",
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
#' @examples  fs(iris$Sepal.Length, iris$Species)
fs <- function(fs.varibale,fs.group = NA){
    fsd <- data.table(var  = fs.varibale,
                      cat  = fs.group)
    fs  <- fsd[, .(N       = NROW(var),
                   Sum     = round_function(sum(var,      na.rm = TRUE), 3),
                   Mean    = round_function(mean(var,     na.rm = TRUE), 3),
                   stdDev  = round_function(sd(var,       na.rm = TRUE), 3),
                   Min     = round_function(min(var,      na.rm = TRUE), 3),
                   Q25     = round_function(quantile(var, probs = 0.25,
                                                     na.rm = TRUE), 3),
                   Median  = round_function(median(var,   na.rm = TRUE), 3),
                   Q75     = round_function(quantile(var, probs = 0.75,
                                                     na.rm = TRUE), 3),
                   Max     = round_function(max(var,      na.rm = TRUE), 3),
                   length  = length(unique(var)),
                   Missing = sum(is.na(var))
    ),
    keyby = cat]
    fs <- as.data.table(fs)
    setnames(fs, 1, "Groups")
    return(fs)
}

## linear check ################################################################
#' Title
#'
#' @param fsp.exposure
#' @param fsp.outcome
#'
#' @return
#' @export
#'
#' @examples fsp(iris$Sepal.Length, iris$Petal.Width)
fsp <- function(fsp.exposure, fsp.outcome) {
    if (!is.numeric(fsp.exposure) || !is.numeric(fsp.outcome)) {
        stop("Inputed values must be numeric.")
    }
    fsp.xmissing <- sum(is.na(fsp.exposure))
    fsp.ymissing <- sum(is.na(fsp.outcome))
    fsp.data <- data.frame(eval(fsp.exposure), eval(fsp.outcome))
    fsp.data <- fsp.data[complete.cases(fsp.data),]
    colnames(fsp.data) <- c("fsp.data.exposure",
                            "fsp.data.outcome")
    fsp.xname <- gsub(".*\\$", "", deparse(substitute(fsp.exposure)))
    fsp.yname <- gsub(".*\\$", "", deparse(substitute(fsp.outcome)))
    if (length(unique(fsp.data$fsp.data.outcome)) > 2) {
        fsp.fitlinear <-
            ols(fsp.data$fsp.data.outcome ~ fsp.data$fsp.data.exposure)
        fsp.fit3 <-
            ols(fsp.data$fsp.data.outcome ~ rcs(fsp.data$fsp.data.exposure, 3))
        fsp.fit4 <-
            ols(fsp.data$fsp.data.outcome ~ rcs(fsp.data$fsp.data.exposure, 4))
        fsp.fit5 <-
            ols(fsp.data$fsp.data.outcome ~ rcs(fsp.data$fsp.data.exposure, 5))
        fsp.data$linear <- predict(fsp.fitlinear)
        fsp.data$rcs3   <- predict(fsp.fit3)
        fsp.data$rcs4   <- predict(fsp.fit4)
        fsp.data$rcs5   <- predict(fsp.fit5)
    } else {
        fsp.fitlinear <-
            lrm(fsp.data$fsp.data.outcome ~ fsp.data$fsp.data.exposure)
        fsp.fit3 <-
            lrm(fsp.data$fsp.data.outcome ~ rcs(fsp.data$fsp.data.exposure, 3))
        fsp.fit4 <-
            lrm(fsp.data$fsp.data.outcome ~ rcs(fsp.data$fsp.data.exposure, 4))
        fsp.fit5 <-
            lrm(fsp.data$fsp.data.outcome ~ rcs(fsp.data$fsp.data.exposure, 5))
        fsp.data$linear <- predict(fsp.fitlinear, type = "lp")
        fsp.data$rcs3   <- predict(fsp.fit3, type = "lp")
        fsp.data$rcs4   <- predict(fsp.fit4, type = "lp")
        fsp.data$rcs5   <- predict(fsp.fit5, type = "lp")
    }
    fsp.gather <- gather(fsp.data,
                         key   = "model",
                         value = "value",
                         c("linear", "rcs3", "rcs4", "rcs5"))
    fsp.plot <- ggplot(fsp.gather,
                       aes_string(x = "fsp.data.exposure",
                                  y = "fsp.data.outcome")) +
        geom_point() +
        geom_line(aes(
            x = fsp.data.exposure,
            y = value,
            linetype = model,
            colour = model
        ),
        linewidth = 1.0) +
        scale_color_manual(values = c(
            "linear" = "black",
            "rcs3" = "green",
            "rcs4" = "red",
            "rcs5" = "blue"
        )) +
        scale_linetype_manual(values = c(
            "linear" = "solid",
            "rcs3" = "dashed",
            "rcs4" = "dashed",
            "rcs5" = "dashed"
        )) +
        labs(
            x = as.character(substitute(fsp.xname)),
            y = as.character(substitute(fsp.yname)),
            title = "Scatter plot"
        ) +
        theme_minimal() +
        theme(
            plot.title       = element_text(
                family = "Times",
                size = 11,
                face = "bold"
            ),
            plot.subtitle    = element_text(vjust     = 1),
            plot.caption     = element_text(vjust     = 1),
            axis.line        = element_line(linewidth = 0.5,
                                            linetype = "solid"),
            panel.grid.major = element_line(colour    = "white"),
            axis.title       = element_text(
                family    = "Times",
                colour    = "Black",
                size      = 9
            ),
            axis.text        = element_text(
                family    = "Times",
                colour    = "Black",
                size      = 9
            ),
            panel.background = element_rect(
                fill     = "gray99",
                colour   = "white",
                linetype = "twodash"
            ),
            plot.background  = element_rect(fill     = "white")
        )
    print(fsp.plot)

    if (length(unique(fsp.data$fsp.data.outcome)) > 2) {
        cat(
            "\n",
            paste("Number of missing values in", fsp.xname, ": ", fsp.xmissing),
            "\n",
            paste("Number of missing values in", fsp.yname, ": ", fsp.ymissing),
            "\n",
            "\n",
            paste0("y = ",
                   round(coef(fsp.fitlinear)[1], 2),
                   " + ",
                   round(coef(fsp.fitlinear)[2], 2),
                   "x"),
            "\n",
            paste(
                "R square for linear model: ",
                round_function(fsp.fitlinear$stats["R2"], 3),
                sep = ""
            ),
            "\n",
            paste("Mean absolute error: ",
                  round_function(mean(
                      abs(fsp.data$fsp.data.outcome -
                              fsp.data$linear)
                  ), 3),
                  sep = ""),
            "\n",
            paste(
                "Root mean square wrror: ",
                round_function(sqrt(mean((fsp.data$fsp.data.outcome -
                                              fsp.data$linear) ^ 2
                )), 3),
                sep = ""
            ),
            "\n",
            "\n",
            paste(
                "R square for rcs model with 3 konts: ",
                round_function(fsp.fit3$stats["R2"], 3),
                sep = ""
            ),
            "\n",
            paste("Mean absolute error: ",
                  round_function(mean(
                      abs(fsp.data$fsp.data.outcome -
                              fsp.data$rcs3)
                  ), 3),
                  sep = ""),
            "\n",
            paste(
                "Root mean square wrror: ",
                round_function(sqrt(mean((fsp.data$fsp.data.outcome -
                                              fsp.data$rcs3) ^ 2
                )), 3),
                sep = ""
            ),

            "\n",
            "\n",
            paste(
                "R square for rcs model with 4 konts: ",
                round_function(fsp.fit4$stats["R2"], 3),
                sep = ""
            ),
            "\n",
            paste("Mean absolute error: ",
                  round_function(mean(
                      abs(fsp.data$fsp.data.outcome -
                              fsp.data$rcs4)
                  ), 3),
                  sep = ""),
            "\n",
            paste(
                "Root mean square wrror: ",
                round_function(sqrt(mean((fsp.data$fsp.data.outcome -
                                              fsp.data$rcs4) ^ 2
                )), 3),
                sep = ""
            ),
            "\n",
            "\n",
            paste(
                "R square for rcs model with 5 konts: ",
                round_function(fsp.fit5$stats["R2"], 3),
                sep = ""
            ),
            "\n",
            paste("Mean absolute error: ",
                  round_function(mean(
                      abs(fsp.data$fsp.data.outcome -
                              fsp.data$rcs5)
                  ), 3),
                  sep = ""),
            "\n",
            paste(
                "Root mean square wrror: ",
                round_function(sqrt(mean((fsp.data$fsp.data.outcome -
                                              fsp.data$rcs5) ^ 2
                )), 3),
                sep = ""
            )
        )
    } else {
        cat(
            "\n",
            paste("Number of missing values in", fsp.xname, ": ", fsp.xmissing),
            "\n",
            paste("Number of missing values in", fsp.yname, ": ", fsp.ymissing),
            "\n",
            "\n",
            paste(
                "Pseudo R square for linear model: ",
                round_function(fsp.fitlinear$stats["R2"], 3),
                sep = ""
            ),
            "\n",
            paste(
                "Pseudo R square for rcs model with 3 konts: ",
                round_function(fsp.fit3$stats["R2"], 3),
                sep = ""
            ),
            "\n",
            paste(
                "Pseudo R square for rcs model with 4 konts: ",
                round_function(fsp.fit4$stats["R2"], 3),
                sep = ""
            ),
            "\n",
            paste(
                "Pseudo R square for rcs model with 5 konts: ",
                round_function(fsp.fit5$stats["R2"], 3),
                sep = ""
            )
        )
    }
}

## Categorical check using box-and-whisker plot ################################
#' Title
#'
#' @param fbp.exposure
#' @param fbp.outcome
#'
#' @return
#' @export
#'
#' @examples fbp(round(iris$Petal.Length,0), iris$Sepal.Length)
fbp <- function(fbp.exposure, fbp.outcome) {
    fsp.data <- data.frame(eval(fbp.exposure), eval(fbp.outcome))
    fbp.xname <- gsub(".*\\$", "", deparse(substitute(fbp.exposure)))
    fbp.yname <- gsub(".*\\$", "", deparse(substitute(fbp.outcome)))
    if (any(is.na(fbp.outcome))) {
        stop(fbp.yname, " cannot contain missing values")
    }
    if (any(is.na(fbp.exposure))) {
        stop(fbp.xname, " cannot contain missing values")
    }
    if (!is.numeric(fbp.outcome)) {
        stop(fbp.yname, " must be a numeric variable")
    }
    if ((!is.numeric(fbp.exposure) | !is.character(fbp.exposure)) &&
        length(unique(fbp.exposure)) > 10) {
        stop(fbp.xname, " must beless than 10 unique values")
    }
    fbp.plot <-
        ggplot(fsp.data, aes(x = factor(fbp.exposure), y = fbp.outcome)) +
        coord_cartesian() +
        geom_boxplot(
            outlier.colour = "red",
            outlier.shape  = 11,
            outlier.size   = 0.5,
            width          = 0.5,
            lwd            = 0.8
        ) +
        #       stat_boxplot(geom  = 'errorbar',
        #                     width = 0.2,
        #                   cex   = 0.2) +
        #      geom_dotplot(
        #           binaxis    = 'y',
        #           stackdir   = 'center',
        #           position   = position_jitter(
        #              seed   = 161893,
        #             width  = 0.3,
        #              height = 0.3
        #          ),
    #          binwidth   = 1,
    #          dotsize = 0.5,
    #          colour = "black",
    #           fill = "grey70"
    #     ) +
    labs(
        title = "Boxplot",
        x = as.character(substitute(fbp.xname)),
        y = as.character(substitute(fbp.yname))
    ) +
        theme_classic() +
        theme(
            axis.text.x  = element_text(size = 10, colour = "black"),
            axis.title.x = element_blank(),
            axis.title.y = element_text(size = 10, colour = "black"),
            axis.text.y  = element_text(size = 10, colour = "black"),
        )
    print(fs(fbp.outcome, fbp.exposure))
    print(fbp.plot)
}

## dummy variable ##############################################################
#' Title
#'
#' @param fdv.variable
#' @param fdv.n
#'
#' @return
#' @export
#'
#' @examples fdv(iris$Sepal.Length,3)
fdv <- function(fdv.variable,fdv.n){
    fdv.dummy <- cut(fdv.variable,
                     quantile(fdv.variable,
                              probs = seq(0, 1, 1/fdv.n),
                              na.rm = TRUE
                     ),
                     labels = c(0:(fdv.n - 1)),
                     include.lowest = TRUE
    )
    return(fdv.dummy)
}

## update Japanese calender ####################################################
#' Title
#'
#' @param J.calendar
#'
#' @return
#' @export
#'
#' @examples update.J.cal(c("平 5", "令 1.12.12"))
update.J.cal <- function(J.calendar) {
    cat("Supports a conversion range between 大正1年(1912) and 令和50年(2068)",
        "\n")
    J.calendar <- gsub(" ", "", J.calendar)
    for (i in 1:15) {
        old_val <- paste0("大", i)
        new_val <- paste0(1911 + i)
        J.calendar <- gsub(old_val, new_val, J.calendar)
    }
    for (i in 1:64) {
        old_val <- paste0("昭", i)
        new_val <- paste0(1925 + i)
        J.calendar <- gsub(old_val, new_val, J.calendar)
    }
    for (i in 1:31) {
        old_val <- paste0("平", i)
        new_val <- paste0(1988 + i)
        J.calendar <- gsub(old_val, new_val, J.calendar)
    }
    for (i in 1:50) {
        old_val <- paste0("令", i)
        new_val <- paste0(2018 + i)
        J.calendar <- gsub(old_val, new_val, J.calendar)
    }
    return(J.calendar)
}

##【Data analysis】#############################################################
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
Table.one <- function(Table_one.analysis_data,
                      Table_one.all_varibales,
                      Table_one.categorical_variables,
                      Table_one.nonnormal_variables = NULL,
                      Table_one.group = NA,
                      Table_one.contDigits = 2,
                      Table_one.test = FALSE
) {
    if (is.na(Table_one.group)) {
        Table_one.print <- print(
            tableone::CreateTableOne(
                data          = Table_one.analysis_data,
                vars          = Table_one.all_varibales,
                test          = Table_one.test,
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
                test          = Table_one.test,
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
#' @param fpn.event
#' @param fpn.exposure
#'
#' @return
#' @export
#'
#' @examples
function(fpn.event, fpn.exposure, fpn.test = FALSE){
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
  }
    fpn.occurtable <- data.table("No.of participants" = fpn.crosstable$t[1, ],
                                 "No.of cases"        = fpn.crosstable$t[2, ])
    fpn.occurtable$"No.of participants" <- rowSums(fpn.occurtable)
    fpn.horizontal_occurtable <- transpose( fpn.occurtable, 
                                            keep.names = "RowNames")
    fpn.horizontal_occurtable$total =
      rowSums(fpn.horizontal_occurtable[, -1, with = FALSE])
    print(fpn.horizontal_occurtable[RowNames %in% c("No.of participants", 
                                                    "No.of cases")])
    write.table(
      fpn.horizontal_occurtable[RowNames %in% c("No.of participants",
                                                "No.of cases"), ],
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

## number of person-years ######################################################
#' Title
#'
#' @param fpy.pyear
#' @param fpy.exposure
#'
#' @return
#' @export
#'
#' @examples
fpy <- function(fpy.pyear, fpy.exposure) {
  fpy <- as.data.table(fs(fpy.pyear, fpy.exposure))
  setnames(fpy,"Sum","Peason-years")
  setorder(fpy,"Groups")
  fpy.table <- as.data.frame(t(fpy[,c(1,3)]))
  colnames(fpy.table) <- NULL
  fpy.table$total <- as.numeric(as.data.table(fs(fpy.pyear))[,3])
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

## linear model ################################################################
#' Title
#'
#' @param fglm.data
#' @param fglm.model
#' @param fglm.n
#' @param fglm.t
#'
#' @return
#' @export
#'
#' @examples
fglm <- function(fglm.data,
                 fglm.model,
                 fglm.n = 1,
                 fglm.t = FALSE) {
    fglm.fit <- ols(formula = fglm.model,
                    data = environment(fglm.data))
    fglm.fit$coefficients
    summary(fglm.fit)
    fglm.table <- data.table(Vars = rownames(summary(fglm.fit)),
                             Beta = fglm.fit$coefficients[-1])
    fglm.clip <-
        fglm.table[(nrow(fglm.table) - fglm.n + 1):nrow(fglm.table)]
    if (fglm.t) {
        fglm.clip.output <- format(round(t(fglm.clip[, 2]), 3), nsmall = 3)
    } else{
        fglm.clip.output <- format(round(fglm.clip[, 2], 3),  nsmall = 3)
    }
    print(fglm.fit)
    cat("\n")
    cat("\n")
    print(fglm.clip)
    write.table(
        fglm.clip.output,
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

#' logistic model
#'
#' @param flrm.data
#' @param flrm.model
#' @param flrm.n
#' @param flrm.t
#'
#' @return
#' @export
#'
#' @examples
flrm <- function(flrm.data, flrm.model, flrm.n = 1, flrm.t = FALSE){
    flrm.fit <- lrm(formula = flrm.model,
                    data    = environment(flrm.data))
    flrm.table <- as.data.frame(summary(flrm.fit))
    flrm.summary <- format(round(exp(flrm.table[seq(1,
                                                    nrow(flrm.table),
                                                    by = 2),
                                                c(4, 6:7)]), # ORs locations
                                 2),
                           nsmall = 2)
    flrm.table <- data.table(
        Vars  = rownames(flrm.summary),
        ORs   = paste0(flrm.summary[,1],
                       "(",
                       flrm.summary[,2],
                       "-",
                       flrm.summary[,3],
                       ")")
    )
    flrm.clip <- flrm.table[(nrow(flrm.table) - flrm.n + 1):nrow(flrm.table)]
    if (flrm.t) {
        flrm.clip.output <- t(flrm.clip[,2])
    } else{
        flrm.clip.output <- flrm.clip[,2]
    }
    print(flrm.fit, coefs = 0)
    cat("\n")
    cat("\n")
    print(summary(flrm.fit))
    print(flrm.clip)
    write.table(flrm.clip.output,
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

## cox model ###################################################################
# The proportional hazards assumption is checked using statistical tests and
# -graphical diagnostics based on the scaled Schoenfeld residuals.
# In principle, the Schoenfeld residuals are independent of time. Plot with a
# -non-random pattern against time is evidence of violation of the assumption.
#' Proportional hazards assumption
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

#' Cox model
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

#' Cox model with rcs
#'
#' @param fcph.data
#' @param fcph.model
#' @param fcph.n
#'
#' @return
#' @export
#'
#' @examples
fcph <- function(fcph.data, fcph.model, fcph.n){
cox_model <- cph(fcph.model,
    ,
    x = TRUE,
    y = TRUE,
    data = fcph.data
)
summary(cox_model)
cox.model_results <- exp(cbind(HRs = coef(cox_model),
                               confint(cox_model)))
cox.round_results <- round_function(cox.model_results,2)
cox.table <- data.table(
    Exposures = row.names(cox.round_results),
    HRs       = paste(
        sprintf("%.2f", cox.round_results[, 1]),
        " (",
        sprintf("%.2f", cox.round_results[, 2]),
        "-",
        sprintf("%.2f", cox.round_results[, 3]),
        ")",
        sep = ""
    )
)
print(cox_model)
print(cox.table)
cox.print <- cox.table[(nrow(cox.table) - fcph.n + 1):nrow(cox.table), c("HRs")]
write.table(cox.print,
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


#' Kaplan-Meier plot
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


#'Fine.Gray model for competing risk
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
    FG.cox_model <- cmprsk::crr(ftime = FG.time,
                                fstatus = FG.status,
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

## modified summary ############################################################
#' Titlemodified summary
#'
#' @param frr.fit
#' @param frr.var 
#' @param n
#'
#' @return
#' @export
#'
#' @examples 
frr <- function(frr.fit, frr.var = NULL, n = 2) {

  if ("rms" %in% class(frr.fit) && any((grepl("\\'", names(coef(frr.fit)))))) {
    stop("Error: Including nonlinear varibale. Using frr(summary()).")
  }
    
  if (is.null(frr.fit)) {stop("Error: Fit cannot be NULL.")}

  if (is.symbol(substitute(frr.var))) {
    frr.var <- as.character(substitute(frr.var))
  }
    
  if ("summary.rms" %in% class(frr.fit) && "matrix" %in% class(frr.fit)) {
    
    frr.table1 <- as.data.frame(frr.fit[seq(2, nrow(frr.fit), by = 2), -c(8)])
    
    frr.table1$"Risk Ratio (95% CI)" <- paste0(
      format(round(frr.table1$Effect, n), nsmall = n),
      " (",
      format(round(frr.table1$"Lower 0.95", n), nsmall = n),
      " - ",
      format(round(frr.table1$"Upper 0.95", n), nsmall = n),
      ")"
    )
    frr.table1 <- frr.table1[, -c(4:7)]
    
    frr.table2 <- as.data.frame(frr.fit[seq(1, nrow(frr.fit), by = 2), -c(8)])
    frr.table2$Rowname <- rownames(frr.table2)
    
    frr.table12 <- merge(frr.table2,
                         frr.table1,
                         all.x = T,
                         by = c("Low", "High", "Diff."))
    frr.table12 <- frr.table12[match(rownames(frr.table2), frr.table12$Rowname), ]
    frr.table12 <- frr.table12[, c(
      "Rowname",
      "Low",
      "High",
      "Diff.",
      "Effect",
      "S.E.",
      "Lower 0.95",
      "Upper 0.95",
      "Risk Ratio (95% CI)"
    )]
    
    rownames(frr.table12) <- NULL
    
    if (is.character(frr.var)) {
      if (grepl(" ", frr.var)) {
        stop("Error: Invalid varibale input. Including space.")
      } else {
        frr.coef <- grep(paste0("^", frr.var), frr.table12$Rowname, value = TRUE) # "^" is for same initial wordss
        
        if (length(frr.coef) > 1) {
          stop("Error: Multiple matches found: ",
               paste(frr.coef, collapse = ", "))
        } else if (length(frr.coef) == 0) {
          stop("Error: Invalid varibale input. Non-existent variable.")
        }
      }
      
      frr.table <- frr.table12[frr.table12$Rowname == frr.coef, ]
      
      print(frr.table)
      fcopy(frr.table$`Risk Ratio (95% CI)`)
    }
    
    if (is.numeric(frr.var)) {
      if (max(frr.var) > length(frr.table12$Rowname)) {
        stop("Error: Invalid varibale input. Exceeding upper limit.")
      }
      if (min(frr.var) <= 0) {
        stop("Error: Invalid varibale input. Exceeding lower limit.")
      }
      
      frr.indices <- seq(min(frr.var), max(frr.var))
      frr.table <- frr.table12[frr.indices, ]
      
      print(frr.table)
      fcopy(frr.table$`Risk Ratio (95% CI)`)
    }
    
    if (is.null(frr.var)) {
      frr.table <- frr.table12
      print(frr.table)
      fcopy(frr.table$`Risk Ratio (95% CI)`)
    }
  } else {
    if (is.character(frr.var)) {
      if (grepl(" ", frr.var)) {
        stop("Error: Invalid varibale input. Including space.")
      } else {
        frr.coefficients <- coef(frr.fit)
        frr.coef <- grep(paste0("^", frr.var), names(frr.coefficients), value = TRUE) # "^" is for same initial wordss
        
        if (length(frr.coef) > 1) {
          stop("Error: Multiple matches found: ",
               paste(frr.coef, collapse = ", "))
        } else if (length(frr.coef) == 0) {
          stop("Error: Invalid varibale input. Non-existent variable.")
        }
      }
      
      frr.Standard_error <- sqrt(diag(vcov(frr.fit)))
      frr.se <- grep(paste0("^", frr.var), names(frr.Standard_error), value = TRUE)
      
      frr.ncoef <- as.numeric(frr.coefficients[frr.coef])
      frr.nse <- as.numeric(frr.Standard_error[frr.se])
      
      frr.rr <- paste0(round(exp(frr.ncoef), n),
                       " (",
                       round(exp(frr.ncoef - frr.nse * qnorm(0.975)), n),
                       " - ",
                       round(exp(frr.ncoef + frr.nse * qnorm(0.975)), n),
                       ")")
      
      frr.table <- data.frame(
        Rowname                = names(frr.coefficients[frr.coef]),
        Effect                 = frr.ncoef,
        S.E.                   = frr.nse,
        "Risk Ratio (95% CI)" = frr.rr
      )
      names(frr.table) <- c("Rowname", "Effect", "S.E.", "Risk Ratio (95% CI)")
      print(frr.table)
      fcopy(frr.rr)
    }
    
    if (is.numeric(frr.var)) {
      if (max(frr.var) > length(names(coef(frr.fit)))) {
        stop("Error: Invalid varibale input. Exceeding upper limit.")
      }
      if (min(frr.var) <= 0) {
        stop("Error: Invalid varibale input. Exceeding lower limit.")
      }
      
      frr.ncoef <- as.numeric(coef(frr.fit))
      frr.nse <- as.numeric(sqrt(diag(vcov(frr.fit))))
      
      frr.rr <- paste0(round(exp(frr.ncoef), n),
                       " (",
                       round(exp(frr.ncoef - frr.nse * qnorm(0.975)), n),
                       " - ",
                       round(exp(frr.ncoef + frr.nse * qnorm(0.975)), n),
                       ")")
      
      frr.table <- data.frame(
        Rowname                = names(coef(frr.fit)),
        Effect                 = frr.ncoef,
        S.E.                   = frr.nse,
        "Risk Ratio (95% CI)" = frr.rr
      )
      names(frr.table) <- c("Rowname", "Effect", "S.E.", "Risk Ratio (95% CI)")
      
      frr.indices <- seq(min(frr.var), max(frr.var))
      
      print(frr.table[frr.indices, ])
      fcopy(frr.table[frr.indices, ][, -c(1:3)])
    }
    if (is.null(frr.var)) {
      frr.ncoef <- as.numeric(coef(frr.fit))
      frr.nse <- as.numeric(sqrt(diag(vcov(frr.fit))))
      
      frr.rr <- paste0(round(exp(frr.ncoef), n),
                       " (",
                       round(exp(frr.ncoef - frr.nse * qnorm(0.975)), n),
                       " - ",
                       round(exp(frr.ncoef + frr.nse * qnorm(0.975)), n),
                       ")")
      
      frr.table <- data.frame(
        Rowname                = names(coef(frr.fit)),
        Effect                 = frr.ncoef,
        S.E.                   = frr.nse,
        "Risk Ratio (95% CI)" = frr.rr
      )
      names(frr.table) <- c("Rowname", "Effect", "S.E.", "Risk Ratio (95% CI)")
      print(frr.table)
      fcopy(frr.table)
    }
  }
}


##【Build a package】###########################################################
# devtools::load_all() # loading the latest package for testing
# ctrl+alt+shift+R
devtools::build()

##【Output messages】###########################################################
fmessgaes <- paste0(
  "\033[36mType the following code to edit Rprofile.site:\033[0m\n",
  "  file.edit(file.path(Sys.getenv('R_HOME'), 'etc', 'Rprofile.site'))\n\n",
  "\033[36mType the following code to install atelier:\033[0m\n",
  "  devtools::install_github('lijiaqi-github/R.atelier')\n\n",
  "\033[36mUpdate packages:\033[0m\n",
  "  update.packages(ask = FALSE, dependencies = TRUE)\n"
)




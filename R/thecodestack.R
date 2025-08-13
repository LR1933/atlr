##【Build a package】###########################################################
# devtools::load_all()
# devtools::build()
# devtools::document()
# devtools::check()
require(data.table)
require(ggplot2)
require(dplyr)
require(rms)

## rounding ####################################################################
#' Title rounding
#' @param d
#' @param n
#' @return
#' @export
#' @examples fround(4.5, 0);round(4.5, 0)
fround <- function(d, n) {
    s <- sign(d)
    d <- abs(d) * 10^n
    d <- d + 0.5
    d <- trunc(d)
    return(s * d / 10^n)
}


## copy ########################################################################
#' Title copy
#' @param c
#' @return
#' @export
#' @examples fcopy(iris$Sepal.Length)
fcopy <- function(c) {
    write.table(
        c,
        paste0(
            "clipboard-",
            formatC(
                100 * 100,
                format = "f",
                digits = 0
            )
        ),
        sep = "\t",
        row.names = FALSE,
        col.names = FALSE,
        dec = "."
    )
}


## html ########################################################################
#' Title html view
#' @param c
#' @param title
#' @param backgroundcolor
#' @param textcolor
#' @return
#' @export
#' @examples fhtml(summary(iris$Sepal.Length))
fhtml <- function(
    h,
    title = "Output",
    backgroundcolor = "black",
    textcolor = "white"
) {
    if (!(inherits(h, "data.frame") || is.matrix(h))) {
        h <- tibble::enframe(h, name = "name", value = "value")
    }
    css <- sprintf(
        "
      body {
        background-color: %s !important;
        color: %s !important;
      }
      table.dataTable {
        background-color: %s !important;
        color: %s !important;
      }
      table.dataTable td,
      table.dataTable th {
        background-color: %s !important;
        color: %s !important;
      }
    ",
        backgroundcolor,
        textcolor,
        backgroundcolor,
        textcolor,
        backgroundcolor,
        textcolor
    )

    htmltools::browsable(
        htmltools::tagList(
            htmltools::tags$title(title),
            htmltools::tags$style(htmltools::HTML(css)),
            DT::datatable(
                h,
                options = list(
                    paging = FALSE,
                    searching = FALSE,
                    info = FALSE
                ),
                class = "display compact"
            )
        )
    )
}


## judge discrete variables #####################################################
#' judge discrete variables
#' @param d
#' @param t
#' @return judge discrete variables
#' @export
#' @examples fdiscrete_var(iris$Sepal.Length)
#' @examples fdiscrete_var(cars$dist)
fdiscrete_var <- function(d, t = 10) {
    if (is.factor(d) || is.character(d)) {
        return(TRUE)
    }
    if (is.numeric(d)) {
        u <- unique(d)
        if (length(u) <= t) {
            return(TRUE)
        }
        if (all(abs(u - round(u)) < .Machine$double.eps^0.5)) {
            return(TRUE)
        }
    }
    return(FALSE)
}


## update Japanese calender ####################################################
#' Title update Japanese calender
#' @param J
#' @return
#' @export
#'           J <- "令 1.12.12"
#' @examples Jcal(c("平 5", "令 1.12.12", "昭 64.1"))
#' @examples fhtml(Jcal(c("平 5", "令 1.12.12", "昭 64.1")))
Jcal <- function(J) {
    rule <- list(
        list(p = "大", s = 1911, count = 15),
        list(p = "昭", s = 1925, count = 64),
        list(p = "平", s = 1988, count = 31),
        list(p = "令", s = 2018, count = 50)
    )
    result <- sapply(J, function(x) {
        o <- gsub(" ", "", x)
        for (y in rule) {
            pattern <- paste0("^", y$p, "(\\d+).*")
            if (grepl(pattern, o)) {
                a <- sub(pattern, "\\1", o)
                c <- as.integer(a)
                if (c > 0 && c <= y$count) {
                    n <- y$s + c
                    t <- paste0(y$p, a)
                    f <- sub(t, n, o)
                    return(f)
                }
            }
        }
        stop(
            "Input error."
        )
    })
    return(unname(result))
}


## geometric mean ##############################################################
#' geometric mean
#' @param g
#' @return geometric mean
#' @export
#' @examples fgmean(iris$Sepal.Length)
#' @examples fhtml(fgmean(iris$Sepal.Length))
# expmean(log(x))) # geometric mean
# exp(sum(log(x)/length(x)) # geometric mean
# exp(mean(log(x)))
fgmean <- function(g) {
    exp(mean(log(g)))
}


## variable status #############################################################
#' Title variable status
#' @param fv.variable
#' @return
#' @export
#' @examples fv(iris$Sepal.Length)
#' @examples fhtml(fv(iris$Sepal.Length))
fv <- function(fv.variable) {
    name <- sub(".*\\$", "", deparse(substitute(fv.variable)))
    class <- paste(class(fv.variable), collapse = ", ")
    type <- typeof(fv.variable)
    n <- length(fv.variable)
    unique <- length(unique(fv.variable))
    char <- as.character(fv.variable)
    na <- sum(is.na(fv.variable))
    zero <- sum(char %in% c("0"), na.rm = TRUE)
    empty <- sum(char == "", na.rm = TRUE)
    whitespace <- sum(grepl("^\\s+$", char), na.rm = TRUE)
    containsspace <- sum(grepl(" ", char), na.rm = TRUE)
    i <- data.frame(
        Metric = c(
            "N",
            "type",
            "Class",
            "zero values",
            "Empty strings",
            "Unique values",
            "Missing values",
            "Whitespace-only",
            "Containing spaces"
        ),
        Value = c(
            n,
            type,
            class,
            zero,
            empty,
            unique,
            na,
            whitespace,
            containsspace
        ),
        stringsAsFactors = FALSE
    )
    names(i)[1] <- name
    i$Percent <- ""
    r <- c(4:9)
    v <- as.numeric(i$Value[r])
    if (n > 0) {
        p <- paste0(round(v / n * 100, 2), "%")
    } else {
        p <- "0.00%"
    }
    i$Percent[r] <- p
    print(knitr::kable(i, align = 'l'))
    invisible(i)
}


## summary #####################################################################
#' Title
#' @param fs.varibale
#' @param fs.group
#' @return
#' @export
#'            fs.varibale <- iris$Sepal.Length
#'            fs.group <- iris$Species
#' @examples  fs(iris$Sepal.Length, iris$Species)
#' @examples  fs(iris$Sepal.Length)
#' @examples  fhtml(fs(iris$Sepal.Length, iris$Species))
fs <- function(fs.varibale, fs.group = NA) {
    label <- sub(".*\\$", "", deparse(substitute(fs.varibale)))

    fsd <- data.table(var = fs.varibale, cat = fs.group)
    fs.table <- fsd[,
        .(
            N = NROW(var),
            Sum = round(sum(var, na.rm = TRUE), 3),
            Mean = round(mean(var, na.rm = TRUE), 3),
            stdDev = round(sd(var, na.rm = TRUE), 3),
            Var = round(var(var, na.rm = TRUE), 3),
            Min = round(min(var, na.rm = TRUE), 3),
            Q25 = round(quantile(var, probs = 0.25, na.rm = TRUE), 3),
            Q50 = round(median(var, na.rm = TRUE), 3),
            Q75 = round(quantile(var, probs = 0.75, na.rm = TRUE), 3),
            Max = round(max(var, na.rm = TRUE), 3),
            length = length(unique(var)),
            Missing = sum(is.na(var))
        ),
        keyby = cat
    ]
    fs.table <- as.data.table(fs.table)
    if (any(is.na(fs.group))) {
        fs.table <- fs.table[, -1, with = FALSE]
    }
    if (any(!is.na(fs.group))) {
        setnames(fs.table, 1, label)
    }
    return(fs.table)
}


## frequency and type ##########################################################
#' Title frequency
#' @param fd.variable
#' @return
#' @export
#' @examples fd(iris$Sepal.Length)
#' @examples fd(iris$Species)
#' @examples fhtml(fd(iris$Sepal.Length))
fd <- function(fd.variable, plot = TRUE) {
    label <- sub(".*\\$", "", deparse(substitute(fd.variable)))
    f <- factor(fd.variable, exclude = NULL)
    df <- as.data.frame(table(f, useNA = "ifany"))
    colnames(df) <- c("Values", "Frequency")
    df$Percent <- round(df$Frequency / sum(df$Frequency) * 100, 2)
    df$CumFrequency <- cumsum(df$Frequency)
    df$CumPercent <- round(cumsum(df$Percent), 2)
    p <- ggplot(df, aes(x = Values, y = Frequency)) +
        geom_bar(stat = "identity", fill = "black") +
        labs(
            title = label,
            x = "",
            y = ""
        ) +
        theme_grey() +
        theme(
            axis.title = element_text(size = 10),
            axis.text = element_text(size = 10)
        ) +
        scale_x_discrete(
            breaks = df$Values[seq(
                1,
                length(df$Values),
                by = max(1, floor(length(df$Values) / 10))
            )]
        )
    if (nlevels(f) <= 10) {
        p <- p + geom_text(aes(label = Frequency), vjust = -0.5, size = 5)
    }
    if (plot) {
        print(p)
    }
    print(df)
}


## frequency cross table #######################################################
#' Title cross table
#' @param fdc.variable1
#' @param fdc.variable2
#' @return
#' @export
#' @examples fdc(iris$Species, iris$Sepal.Length)
#' @examples fhtml(fdc(iris$Species, iris$Sepal.Length))
fdc <- function(fdc.variable1, fdc.variable2) {
    p <- ggplot(
        data.frame(v1 = fdc.variable1, v2 = fdc.variable2) %>%
            count(v1, v2),
        aes(x = v1, y = v2, fill = n)
    ) +
        geom_tile(color = "white") +
        geom_text(aes(label = n), color = "black", size = 4) +
        scale_fill_gradient(low = "lightblue", high = "lightcoral") +
        labs(
            x = sub(".*\\$", "", deparse(substitute(fdc.variable1))),
            y = sub(".*\\$", "", deparse(substitute(fdc.variable2))),
            fill = "Frenquency"
        ) +
        theme_gray()

    t <- janitor::tabyl(
        data.frame(v1 = fdc.variable1, v2 = fdc.variable2),
        v1,
        v2
    ) %>%
        janitor::adorn_totals(c("row", "col")) %>%
        janitor::adorn_percentages("row") %>%
        janitor::adorn_pct_formatting(digits = 2) %>%
        janitor::adorn_ns(position = "front")

    colnames(t)[1] <- paste0(
        sub(".*\\$", "", deparse(substitute(fdc.variable1))),
        "/",
        sub(".*\\$", "", deparse(substitute(fdc.variable2)))
    )
    print(p)
    print(t)
}


## frequency plot ##############################################################
#' Title
#' @param fdp.variable
#' @param fdp.binwidth
#' @return
#' @export
#'           fdp.variable <- iris$Sepal.Width
#' @examples fdp(iris$Sepal.Width)
#' @examples fhtml(fdp(iris$Sepal.Width))
#' @examples fdp(cars$dist)
#' @examples fhtml(fdp(cars$dist))
fdp <- function(fdp.variable, fdp.binwidth = 1) {
    if (!is.numeric(fdp.variable) || length(fdp.variable) <= 10) {
        stop(
            "Input error: Data must be a numeric vector with multiple numbers ( > 10 )."
        )
    }
    label <- gsub(".*\\$", "", deparse(substitute(fdp.variable)))
    if (fdiscrete_var(fdp.variable)) {
        fdp.plot <- ggplot() +
            geom_histogram(
                aes(x = fdp.variable),
                color = "white",
                bins = 30
            ) +
            labs(x = label) +
            theme_grey()
    } else {
        fdp.plot <- ggplot() +
            geom_histogram(
                aes(x = fdp.variable, y = after_stat(density)),
                color = "white",
                bins = 30
            ) +
            geom_density(aes(x = fdp.variable), color = "grey", size = 1) +
            scale_y_continuous(
                name = "Density",
                sec.axis = sec_axis(
                    trans = ~ . * length(fdp.variable),
                    name = "Frequency"
                )
            ) +
            labs(x = label) +
            theme_grey()
    }
    print(fdp.plot)
    return(fd(fdp.variable, plot = FALSE))
}


## linear check ################################################################
#' Title
#' @param fsp.x
#' @param fsp.y
#' @return
#' @export
#' @examples fsp(iris$Sepal.Length, iris$Petal.Width)
#' @examples fhtml(fsp(iris$Sepal.Length, iris$Petal.Width))
fsp <- function(fsp.exposure, fsp.y) {
    if (!is.numeric(fsp.exposure) || !is.numeric(fsp.y)) {
        stop("Input error: Inputed values must be numeric.")
    }
    fsp.xmissing <- sum(is.na(fsp.exposure))
    fsp.ymissing <- sum(is.na(fsp.y))
    fsp.data <- data.table(eval(fsp.exposure), eval(fsp.y))
    fsp.data <- fsp.data[complete.cases(fsp.data), ]
    colnames(fsp.data) <- c("fsp.data.exposure", "fsp.data.y")
    fsp.xname <- gsub(".*\\$", "", deparse(substitute(fsp.exposure)))
    fsp.yname <- gsub(".*\\$", "", deparse(substitute(fsp.y)))
    if (length(unique(fsp.data$fsp.data.y)) > 10) {
        fsp.fitlinear <-
            ols(fsp.data$fsp.data.y ~ fsp.data$fsp.data.exposure)
        fsp.fit3 <-
            ols(fsp.data$fsp.data.y ~ rcs(fsp.data$fsp.data.exposure, 3))
        fsp.fit4 <-
            ols(fsp.data$fsp.data.y ~ rcs(fsp.data$fsp.data.exposure, 4))
        fsp.fit5 <-
            ols(fsp.data$fsp.data.y ~ rcs(fsp.data$fsp.data.exposure, 5))
        fsp.data$linear <- predict(fsp.fitlinear)
        fsp.data$rcs3 <- predict(fsp.fit3)
        fsp.data$rcs4 <- predict(fsp.fit4)
        fsp.data$rcs5 <- predict(fsp.fit5)
    } else {
        stop("Input error: Y must be multivalued (>10).")
    }
    g <- tidyr::gather(
        fsp.data,
        key = "model",
        value = "value",
        c("linear", "rcs3", "rcs4", "rcs5")
    )
    p <- ggplot(
        g,
        aes(x = fsp.data.exposure, y = fsp.data.y)
    ) +
        geom_point(size = 0.5) +
        geom_line(
            aes(
                x = fsp.data.exposure,
                y = value,
                linetype = model,
                colour = model
            ),
            linewidth = 1.0
        ) +
        scale_color_manual(
            values = c(
                "linear" = "black",
                "rcs3" = "green",
                "rcs4" = "red",
                "rcs5" = "blue"
            )
        ) +
        scale_linetype_manual(
            values = c(
                "linear" = "solid",
                "rcs3" = "solid",
                "rcs4" = "solid",
                "rcs5" = "solid"
            )
        ) +
        labs(
            title = paste0(
                "y = ",
                round(coef(fsp.fitlinear)[1], 2),
                " + ",
                round(coef(fsp.fitlinear)[2], 2),
                "x"
            ),
            x = as.character(substitute(fsp.xname)),
            y = as.character(substitute(fsp.yname))
        ) +
        theme_grey() +
        theme(
            plot.title = element_text(size = 20),
            axis.title = element_text(size = 10),
            axis.text = element_text(size = 10)
        )
    t <- data.table(
        Model = c(
            "linear",
            "rcs with 3 konts",
            "rcs with 4 konts",
            "rcs with 5 konts"
        ),
        R2 = c(
            round(fsp.fitlinear$stats["R2"], 3),
            round(fsp.fit3$stats["R2"], 3),
            round(fsp.fit4$stats["R2"], 3),
            round(fsp.fit5$stats["R2"], 3)
        ),
        MAE = c(
            round(mean(abs(fsp.data$fsp.data.y - fsp.data$linear)), 3),
            round(mean(abs(fsp.data$fsp.data.y - fsp.data$rcs3)), 3),
            round(mean(abs(fsp.data$fsp.data.y - fsp.data$rcs4)), 3),
            round(mean(abs(fsp.data$fsp.data.y - fsp.data$rcs5)), 3)
        ),
        RMSE = c(
            round(
                sqrt(mean((fsp.data$fsp.data.y - fsp.data$linear)^2)),
                3
            ),
            round(sqrt(mean((fsp.data$fsp.data.y - fsp.data$rcs3)^2)), 3),
            round(sqrt(mean((fsp.data$fsp.data.y - fsp.data$rcs4)^2)), 3),
            round(sqrt(mean((fsp.data$fsp.data.y - fsp.data$rcs5)^2)), 3)
        ),
        check.names = FALSE
    )
    print(p)
    print(knitr::kable(t))
    t <- as.data.frame(t)
    t[, 2:4] <- lapply(t[, 2:4], function(column) {
        sprintf("%.3f", column)
    })
    invisible(t)
}


## Categorical check using box-and-whisker plot ################################
#' Title
#' @param fbp.x
#' @param fbp.y
#' @return
#' @export
#' @examples fbp(mtcars$gear, mtcars$wt)
#' @examples fhtml(fbp(mtcars$gear, mtcars$wt))
fbp <- function(fbp.x, fbp.y) {
    fsp.data <- data.table(eval(fbp.x), eval(fbp.y))
    fbp.xlabel <- gsub(".*\\$", "", deparse(substitute(fbp.x)))
    fbp.ylabel <- gsub(".*\\$", "", deparse(substitute(fbp.y)))
    if (any(is.na(fbp.y))) {
        stop(fbp.ylabel, " cannot contain missing values")
    }
    if (any(is.na(fbp.x))) {
        stop(fbp.xlabel, " cannot contain missing values")
    }
    if (!is.numeric(fbp.y)) {
        stop(fbp.ylabel, " must be a numeric variable")
    }
    if (
        (!is.numeric(fbp.x) | !is.character(fbp.x)) &&
            length(unique(fbp.x)) > 10
    ) {
        stop(fbp.xlabel, " must beless than 10 unique values")
    }
    p <-
        ggplot(fsp.data, aes(x = factor(fbp.x), y = fbp.y)) +
        coord_cartesian() +
        geom_boxplot(
            outlier.colour = "red",
            outlier.shape = 10,
            outlier.size = 2,
            width = 0.5,
            lwd = 0.8
        ) +
        labs(
            x = as.character(substitute(fbp.xlabel)),
            y = as.character(substitute(fbp.ylabel))
        ) +
        theme_grey() +
        theme(
            axis.text.x = element_text(size = 15, colour = "black"),
            axis.title.x = element_text(size = 15, colour = "black"),
            axis.title.y = element_text(size = 15, colour = "black"),
            axis.text.y = element_text(size = 15, colour = "black"),
        )
    print(p)
    t <- data.frame(fs(fbp.y, fbp.x))
    names(t)[1] <- paste0(fbp.ylabel, "/", fbp.xlabel)
    print(t)
    invisible(t)
}


##【Data analysis】#############################################################
## Table 1 #####################################################################
# https://cran.r-project.org/web/packages/tableone/vignettes/introduction.html
#' Title
#' @param ft.data
#' @param ft.vars
#' @param ft.cats
#' @param ft.nvars
#' @param ft.group
#' @param ft.cdig
#' @param ft.test
#' @return
#' @export
#' @examples ft(iris, colnames(iris),"Species","Petal.Length","Species")
#' @examples fhtml(ft(iris, colnames(iris),"Species","Petal.Length","Species"))
ft <- function(
    ft.data,
    ft.vars,
    ft.cats,
    ft.nvars = NULL,
    ft.group = NULL,
    ft.cdig = 1,
    ft.test = FALSE
) {
    if (is.null(ft.group)) {
        ft.print <- print(
            tableone::CreateTableOne(
                data = ft.data,
                vars = ft.vars,
                test = ft.test,
                factorVars = ft.cats
            ),
            smd = TRUE,
            catDigits = 1,
            contDigits = ft.cdig,
            format = c("fp"),
            # format          = c("fp", "f", "p", "pf")
            nonnormal = ft.nvars,
            showAllLevels = FALSE,
            formatOptions = list(big.mark = ","),
            noSpaces = TRUE
        )
    } else {
        ft.print <- print(
            tableone::CreateTableOne(
                data = ft.data,
                vars = ft.vars,
                strata = ft.group,
                test = ft.test,
                factorVars = ft.cats
            ),
            smd = TRUE,
            catDigits = 1,
            contDigits = ft.cdig,
            format = c("fp"),
            # format          = c("fp", "f", "p", "pf")
            nonnormal = ft.nvars,
            showAllLevels = FALSE,
            formatOptions = list(big.mark = ","),
            noSpaces = TRUE
        )
    }
    fcopy(ft.print)
    invisible(ft.print)
}

## number of total and event ###################################################
#' Title
#' @param fpn.event
#' @param fpn.exposure
#' @return
#' @export
#'           fpn.event <- mtcars$am
#'           fpn.exposure <- mtcars$gear
#' @examples fpn(mtcars$am , mtcars$gear)
#' @examples fpn(mtcars$am , mtcars$gear, outcome = "0")
#' @examples fpn(mtcars$am , mtcars$gear, T, T)
#' @examples fhtml(fpn(mtcars$am , mtcars$gear))
fpn <- function(
    fpn.event,
    fpn.exposure,
    fpn.prop = FALSE,
    fpn.test = FALSE,
    outcome = "1"
) {
    Event <- fpn.event
    Exposure <- fpn.exposure
    if (length(unique(fpn.event)) != 2) {
        fpn.crosstable <- gmodels::CrossTable(
            Event,
            Exposure,
            fisher = fpn.test,
            prop.chisq = fpn.prop,
            prop.t = fpn.prop,
            prop.r = fpn.prop,
            prop.c = fpn.prop
        )
        print(fpn.table)
    } else {
        fpn.crosstable <- gmodels::CrossTable(
            Event,
            Exposure,
            fisher = fpn.test,
            prop.chisq = fpn.prop,
            prop.t = fpn.prop,
            prop.r = fpn.prop,
            prop.c = fpn.prop
        )
        t <- data.frame(
            rbind(
                "No. of events" = as.vector(fpn.crosstable$t[outcome, ]),
                "No. of participants" = as.vector(colSums(fpn.crosstable$t))
            )
        )
        t$Total <- rowSums(t)
        colnames(t) <- c(colnames(fpn.crosstable$t), "Total")
        print(t)
        fcopy(t)
        invisible(t)
    }
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
#'           fpy.pyear <- iris$Sepal.Length
#'           fpy.exposure <- iris$Species
#' @examples fpy(iris$Sepal.Length, iris$Species)
#' @examples fhtml(fpy(iris$Sepal.Length, iris$Species))
fpy <- function(fpy.pyear, fpy.exposure) {
    t <- fs(fpy.pyear, fpy.exposure)
    t <- data.frame(`Person years` = t$Sum, check.names = FALSE)
    rownames(t) <- as.character(fs(fpy.pyear, fpy.exposure)[[1]])
    t <- as.data.frame(t(t))
    t$Total <- rowSums(t)
    fcopy(t)
    print(t)
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
# fglm <- function(fglm.data, fglm.model, fglm.n = 1, fglm.t = FALSE) {
#     fglm.fit <- ols(formula = fglm.model, data = environment(fglm.data))
#     fglm.fit$coefficients
#     summary(fglm.fit)
#     fglm.table <- data.table(
#         Vars = rownames(summary(fglm.fit)),
#         Beta = fglm.fit$coefficients[-1]
#     )
#     fglm.clip <-
#         fglm.table[(nrow(fglm.table) - fglm.n + 1):nrow(fglm.table)]
#     if (fglm.t) {
#         fglm.clip.output <- format(round(t(fglm.clip[, 2]), 3), nsmall = 3)
#     } else {
#         fglm.clip.output <- format(round(fglm.clip[, 2], 3), nsmall = 3)
#     }
#     print(fglm.fit)
#     cat("\n")
#     cat("\n")
#     print(fglm.clip)
#     write.table(
#         fglm.clip.output,
#         paste0(
#             "clipboard-",
#             formatC(
#                 100 * 100,
#                 format = "f",
#                 digits = 0
#             )
#         ),
#         sep = "\t",
#         row.names = FALSE,
#         col.names = FALSE,
#         dec = "."
#     )
# }

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
# ORs <- function(ORs.analysis_data, ORs.model, ORs.n) {
#     logistic_model <- glm(
#         ORs.model,
#         data = ORs.analysis_data,
#         family = binomial(link = 'logit')
#     )
#     ORs.observation <- paste(
#         "Number",
#         "of",
#         "observation",
#         "is",
#         length(residuals(logistic_model))
#     )
#     ORs.model_results <- exp(cbind(
#         OR = coef(logistic_model),
#         confint(logistic_model)
#     ))
#     ORs.round_model_results <- round(ORs.model_results, 2)
#     ORs.table <- data.table(
#         Exposures = row.names(ORs.round_model_results),
#         ORs = paste(
#             ORs.round_model_results[, 1],
#             " (",
#             ORs.round_model_results[, 2],
#             "-",
#             ORs.round_model_results[, 3],
#             ")",
#             sep = ""
#         ),
#         P_value = round(
#             as.numeric(
#                 summary(logistic_model)$coefficients[, c(4)]
#             ),
#             4
#         )
#     )
#     ORs.horizontal_table <- as.data.frame(t(ORs.table))
#     ORs.horizontal_table_print <- ORs.horizontal_table[
#         row.names(ORs.horizontal_table) == "ORs",
#         (ncol(ORs.horizontal_table) - ORs.n + 1):ncol(ORs.horizontal_table)
#     ]
#     print(summary(logistic_model))
#     print(ORs.table)
#     print(ORs.horizontal_table_print)
#     print(noquote(ORs.observation))
#     write.table(
#         ORs.horizontal_table_print,
#         paste0("clipboard-", formatC(100 * 100, format = "f", digits = 0)),
#         sep = "\t",
#         row.names = FALSE,
#         col.names = FALSE,
#         dec = "."
#     )
# }

## cox model ###################################################################
# The proportional hazards assumption is checked using statistical tests and
# -graphical diagnostics based on the scaled Schoenfeld residuals.
# In principle, the Schoenfeld residuals are independent of time. Plot with a
# -non-random pattern against time is evidence of violation of the assumption.
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
# HRs <- function(HRs.analysis_data, HRs.model, HRs.n) {
#     cox_model <- survival::coxph(HRs.model, data = HRs.analysis_data, )
#     HRs.observation <- paste(
#         "Number",
#         "of",
#         "observation",
#         "is",
#         length(residuals(cox_model))
#     )
#     HRs.model_results <- exp(cbind(HR = coef(cox_model), confint(cox_model)))
#     HRs.round_model_results <- round(HRs.model_results, 2)
#     HRs.table <- data.frame(
#         Exposures = row.names(HRs.round_model_results),
#         HRs = paste(
#             HRs.round_model_results[, 1],
#             " (",
#             HRs.round_model_results[, 2],
#             "-",
#             HRs.round_model_results[, 3],
#             ")",
#             sep = ""
#         ),
#         P_value = round(
#             as.numeric(
#                 summary(cox_model)$coefficients[, c(5)]
#             ),
#             4
#         )
#     )
#     HRs.horizontal_table <- as.data.frame(t(HRs.table))
#     summary(cox_model)
#     print(HRs.table)
#     HRs.horizontal_table_print <- HRs.horizontal_table[
#         row.names(HRs.horizontal_table) == "HRs",
#         (ncol(HRs.horizontal_table) - HRs.n + 1):ncol(HRs.horizontal_table)
#     ]
#     print(summary(cox_model))
#     print(HRs.table)
#     print(HRs.horizontal_table_print)
#     print(noquote(HRs.observation))
#     write.table(
#         HRs.horizontal_table_print,
#         paste0("clipboard-", formatC(100 * 100, format = "f", digits = 0)),
#         sep = "\t",
#         row.names = FALSE,
#         col.names = FALSE,
#         dec = "."
#     )
# }

#' Kaplan-Meier plot
#'
#' @param KMplot.analysis_data
#' @param KMplot.model
#'
#' @return
#' @export
#'
#' @examples
# KMplot <- function(KMplot.analysis_data, KMplot.model) {
#     KMplot.KM <- survminer::surv_fit(KMplot.model, data = KMplot.analysis_data)
#     # summary(KMplot, times=seq(0, 1000, 250))
#     survminer::ggsurvplot(
#         KMplot.KM,
#         data = KMplot.analysis_data,
#         conf.int = TRUE,
#         pval = TRUE,
#         pval.method = FALSE,
#         pval.coord = c(0, 1.0),
#         # test.for.trend     = TRUE, # > 2 groups
#         risk.table = TRUE,
#         # surv.median.line   = c("hv"),
#         # legend.labs        = c("Group1", "Group2"),
#         legend.title = "Group",
#         # palette            = c("gray25", "gray50"),
#         risk.table.height = .25
#     )
# }

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
# Fine.Gray.HRs <- function(FG.time, FG.status, FG.model, FG.n) {
#     FG.cox_model <- cmprsk::crr(
#         ftime = FG.time,
#         fstatus = FG.status,
#         FG.model,
#         failcode = 1,
#         cencode = 0
#     )
#     FG.observation <- paste("Number", "of", "observation", "is", FG.cox_model$n)
#     FG.model_results <- summary(FG.cox_model)
#     FG.round_model_results <- round(FG.model_results$conf.int[, -2], 2)
#     FG.table <- data.frame(
#         Exposures = row.names(FG.round_model_results),
#         FG.HRs = paste(
#             FG.round_model_results[, 1],
#             " (",
#             FG.round_model_results[, 2],
#             "-",
#             FG.round_model_results[, 3],
#             ")",
#             sep = ""
#         ),
#         P_value = round(
#             FG.model_results$coef[, 5],
#             4
#         )
#     )
#     FG.horizontal_table <- as.data.frame(t(FG.table))
#     summary(FG.cox_model)
#     print(FG.table)
#     FG.horizontal_table_print <- FG.horizontal_table[
#         row.names(FG.horizontal_table) == "FG.HRs",
#         (ncol(FG.horizontal_table) - FG.n + 1):ncol(FG.horizontal_table)
#     ]
#     print(summary(FG.cox_model))
#     print(FG.table)
#     print(FG.horizontal_table_print)
#     print(noquote(FG.observation))
#     write.table(
#         FG.horizontal_table_print,
#         paste0("clipboard-", formatC(100 * 100, format = "f", digits = 0)),
#         sep = "\t",
#         row.names = FALSE,
#         col.names = FALSE,
#         dec = "."
#     )
# }

## modified summary.rms ########################################################
#' Titlemodified summary
#'
#' @param fe.fit
#' @param fe.var
#' @param n
#'
#' @return
#' @export
#'
#' @examples options(datadist =  datadist(iris)); iris$Species <-  sample(0:1, 150, replace = TRUE)
#'
#' @examples fe(lm(Sepal.Length ~ ., iris), 2:3)
#' @examples fe(lm(Sepal.Length ~ ., iris), c("Petal.Length", "Sepal.Width"))
#'
#' @examples fe(ols(Sepal.Length ~ .,  subset(iris, select = -Species)), 2:3)
#' @examples fe(ols(Sepal.Length ~ .,  subset(iris, select = -Species)), c("Petal.Length", "Sepal.Width"))
#'
#' @examples fe(summary(ols(Sepal.Length ~ .,  subset(iris, select = -Species)), Petal.Length = c(1,2)), 1:2)
#' @examples fe(summary(ols(Sepal.Length ~ .,  subset(iris, select = -Species)), Petal.Length = c(1,2)), c("Petal.Length", "Sepal.Width"))
#'
#' @examples fe(lrm(Species ~.,  iris), 2:3)
#' @examples fe(lrm(Species ~ .,  iris),c("Petal.Length", "Sepal.Width"))
#'
#' @examples fe(summary(lrm(Species ~ .,  iris), Sepal.Length = c(1,2),  Sepal.Width = c(1,2)), 2:3)
#' @examples fe(summary(lrm(Species ~ .,  iris), Sepal.Length = c(1,2),  Sepal.Width = c(1,2)), c("Petal.Length", "Sepal.Width"))
fe <- function(fe.fit, fe.var = NULL, n = 2) {
    Risk_ratio <- paste0(sub("^ ", "", rownames(fe.fit)[2]), " (95% CI)")
    if (!(grepl("ratio", Risk_ratio, ignore.case = TRUE))) {
        Risk_ratio <- "Effect (95% CI)"
    }
    if ("rms" %in% class(fe.fit) && any((grepl("\\'", names(coef(fe.fit)))))) {
        stop("Error: including nonlinear varibale. use fe(summary()).")
    }
    if (is.null(fe.fit)) {
        stop("Error: fit is NULL.")
    }
    if (is.symbol(substitute(fe.var))) {
        fe.var <- as.character(substitute(fe.var))
    }
    if ("summary.rms" %in% class(fe.fit) && "matrix" %in% class(fe.fit)) {
        if (grepl("ratio", Risk_ratio, ignore.case = TRUE)) {
            fe.table1 <- as.data.frame(fe.fit[
                seq(1, nrow(fe.fit), by = 2),
                -c(8)
            ])
            fe.table1$"E (95% CI)" <- paste0(
                format(
                    round(
                        as.numeric(fe.fit[seq(2, nrow(fe.fit), by = 2), 4]),
                        n
                    ),
                    nsmall = n
                ),
                " (",
                format(
                    round(
                        as.numeric(fe.fit[seq(2, nrow(fe.fit), by = 2), 6]),
                        n
                    ),
                    nsmall = n
                ),
                " - ",
                format(
                    round(
                        as.numeric(fe.fit[seq(2, nrow(fe.fit), by = 2), 7]),
                        n
                    ),
                    nsmall = n
                ),
                ")"
            )
            fe.table1$Factor <- rownames(fe.table1)
        } else {
            fe.table1 <- as.data.frame(fe.fit[, -c(8)])
            fe.table1$"E (95% CI)" <- paste0(
                format(round(fe.table1$Effect, n), nsmall = n),
                " (",
                format(round(fe.table1$"Lower 0.95", n), nsmall = n),
                " - ",
                format(round(fe.table1$"Upper 0.95", n), nsmall = n),
                ")"
            )
            fe.table1$Factor <- rownames(fe.table1)
        }
        fe.table1 <- fe.table1[, c(
            "Factor",
            "Low",
            "High",
            "Diff.",
            "E (95% CI)"
        )]
        fe.table2 <- as.data.table(fe.table1)
        colnames(fe.table2)[colnames(fe.table2) == "E (95% CI)"] <- Risk_ratio
        # fe.var is character type
        if (is.character(fe.var)) {
            if (any(grepl(" ", fe.var))) {
                stop("Error: invalid varibale input. including space.")
            } else {
                pattern <- paste0("^(", paste(fe.var, collapse = "|"), ")") # "^" is for same initial wordss
                fe.coef <- grep(pattern, fe.table2$Factor, value = TRUE)
                if (length(fe.coef) == 0) {
                    stop(
                        "Error: invalid varibale input. non-existent variable."
                    )
                }
                fe.table <- fe.table2[
                    grepl(paste(fe.var, collapse = "|"), fe.table2$Factor),
                ]
            }
        } else if (is.numeric(fe.var)) {
            # fe.var is numeric type
            if (max(fe.var) > length(fe.table2$Factor)) {
                stop("Error: invalid varibale input. exceeding upper limit.")
            } else if (min(fe.var) <= 0) {
                stop("Error: invalid varibale input. exceeding lower limit.")
            } else {
                fe.indices <- seq(min(fe.var), max(fe.var))
                fe.table <- fe.table2[fe.indices, ]
            }
        } else if (is.null(fe.var)) {
            # fe.var is null
            fe.table <- fe.table2
        } else {
            stop("Error: check function related to input of summary.rms.")
        }
        print(fe.table, row.names = FALSE)
        fcopy(fe.table[, ..Risk_ratio])
    } else if ("list" %in% typeof(fe.fit) && "matrix" %nin% class(fe.fit)) {
        #
        # fe.fit is the fit rather than the summary
        model_classes <- as.list(setNames(
            c(
                rep("Effect (95% CI)", 5),
                rep("Odds ratio (95% CI)", 2),
                rep("Hazard ratio (95% CI)", 2)
            ),
            c(
                "lm",
                "glm",
                "ols",
                "Glm",
                "Gls", # rep("Effect (95% CI)", 5)
                "lrm",
                "orm", # rep("Odds ratio (95% CI)", 2)
                "cph",
                "coxph"
            ) # rep("Hazard ratio (95% CI)", 2))
        ))
        Risk_ratio <- model_classes[[class(fe.fit)[1]]]
        if (is.null(Risk_ratio)) {
            stop("Error: check `model_classes` in function.")
        }
        if (
            "glm" %in%
                class(fe.fit) &&
                "rms" %nin% class(fe.fit) &&
                (length(fe.fit$family$link) > 0 &&
                    grepl("logit", fe.fit$family$link))
        ) {
            Risk_ratio <- "Odds ratio (95% CI)"
        }
        if (grepl("ratio", Risk_ratio, ignore.case = TRUE)) {
            if ("rms" %in% class(fe.fit)) {
                fe.rr <- paste0(
                    format(
                        round(
                            as.numeric(summary(fe.fit)[
                                seq(2, nrow(summary(fe.fit)), by = 2),
                                4
                            ]),
                            n
                        ),
                        nsmall = n
                    ),
                    " (",
                    format(
                        round(
                            as.numeric(summary(fe.fit)[
                                seq(2, nrow(summary(fe.fit)), by = 2),
                                6
                            ]),
                            n
                        ),
                        nsmall = n
                    ),
                    " - ",
                    format(
                        round(
                            as.numeric(summary(fe.fit)[
                                seq(2, nrow(summary(fe.fit)), by = 2),
                                7
                            ]),
                            n
                        ),
                        nsmall = n
                    ),
                    ")"
                )
                fe.rr <- c("", fe.rr)
            } else {
                fe.rr <- paste0(
                    format(round(exp(coef(fe.fit)), n), nsmall = n),
                    " (",
                    format(
                        round(exp(as.numeric(confint(fe.fit)[, 1])), n),
                        nsmall = n
                    ),
                    " - ",
                    format(
                        round(exp(as.numeric(confint(fe.fit)[, 2])), n),
                        nsmall = n
                    ),
                    ")"
                )
            }
        } else {
            fe.rr <- paste0(
                format(round(coef(fe.fit), n), nsmall = n),
                " (",
                format(round(as.numeric(confint(fe.fit)[, 1]), n), nsmall = n),
                " - ",
                format(round(as.numeric(confint(fe.fit)[, 2]), n), nsmall = n),
                ")"
            )
        }
        fe.table1 <- data.frame(
            Factor = names(coef(fe.fit)),
            Effect = coef(fe.fit),
            S.E. = sqrt(diag(vcov(fe.fit))),
            "Risk Ratio (95% CI)" = fe.rr
        )
        names(fe.table1) <- c("Factor", "Effect", "S.E.", "Risk Ratio (95% CI)")
        colnames(fe.table1)[
            colnames(fe.table1) == "Risk Ratio (95% CI)"
        ] <- Risk_ratio
        fe.table1 <- as.data.table(fe.table1)
        fe.table1 <- fe.table1[fe.table1[[4]] != "", ]
        # fe.var is character type
        if (is.character(fe.var)) {
            if (any(grepl(" ", fe.var))) {
                stop("Error: invalid varibale input. including space.")
            } else {
                pattern <- paste0("^(", paste(fe.var, collapse = "|"), ")") # "^" is for same initial words
                fe.coef <- grep(pattern, fe.table1$Factor, value = TRUE)
                if (length(fe.coef) == 0) {
                    stop(
                        "Error: invalid varibale input. non-existent variable."
                    )
                }
            }
            fe.table <- fe.table1[
                grepl(paste(fe.var, collapse = "|"), fe.table1$Factor),
            ]
        } else if (is.numeric(fe.var)) {
            # fe.var is numeric type
            if (max(fe.var) > length(names(coef(fe.fit)))) {
                stop("Error: invalid varibale input. exceeding upper limit.")
            }
            if (min(fe.var) <= 0) {
                stop("Error: invalid varibale input. exceeding lower limit.")
            }
            fe.indices <- seq(min(fe.var), max(fe.var))
            fe.table <- fe.table1[fe.indices, ]
        } else if (is.null(fe.var)) {
            # fe.var is null
            fe.table <- fe.table1
        } else {
            stop("Error: check function related to input of fit.")
        }
        print(fe.table, row.names = FALSE)
        fcopy(fe.table[, ..Risk_ratio])
    } else {
        stop("Error: input except fit or summary.rms; check function.")
    }
}

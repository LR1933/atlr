## the code paradigm ###########################################################
## 20250201 version ############################################################
## topic: Health care intervention and cardiovascular events
## start date: 2025-02-21
## dataset: jharp
## preprocessing ###############################################################
rm(list = ls())
options(scipen = 20, digits =  4)
require(atlr) 
# cat(fmessgaes)
# Sys.setenv(LANG = "en_US.UTF-8")
# options(prType = 'html') 
# install.packages("")
# devtools::install_github('lijiaqi-github/atlr')
# installed.packages()["", c("Version")] 
# installed.packages()["", c("Depends")] 
# installed.packages()["", c("Priority")] 
# detach("package:atlr", unload = TRUE) 
# remove.packages("") 
# update.packages(lib.loc = .libPaths()[1], ask = FALSE)

#### data preparation ##########################################################
## read data ###################################################################
dt  <- file.path("C:",
                 "Database", 
                 "",
                 "",
                 "")
dt  <- fread(dt , na.strings = c(NULL,''))
# dt <- fread("", select = c("column1", "column2"))

fdt <- file.path("C:",
                 "Users",
                 "hange",
                 "Documents",
                 "Database", 
                 "",
                 "",
                 "")
fdt <- fread(fdt, na.strings = c(NULL,''))

dt <- bind_rows(dt, dt1) # bind data
dt <- merge(dt, fdt, all.x = TRUE, by = c("id")) # merge data
dt[!duplicated(dt$id), "id"] # output unique line

nrow <- nrow(dt)

# dt <- readxl::read_excel(dt, )
# dt <- haven::read_sas("")
# dt <- read.csv(dt,fileEncoding = "cp932", na = "") # fileEncoding = "UTF-8-BOM"
# fwrite(dt, "C://dt.csv")
# write.csv(dt, "C://dt.csv")

## overview of data ############################################################
SmartEDA::ExpData(dt, type = 1)
describe(dt[, .(age)], 'Baseline Variables')
# head(dt)
# names(dt)
# view(dt)
# dim(dt)
# str(dt)

# all(dt$sex.y == dt$sex.y) # check consistency
# diffdf::diffdf(dt[,c("")], 
#                dt[,c("")],
#                suppress_warnings = TRUE) # check difference
# setdiff(dt$x, dt$y) # output dt$x not in dt$y

# lapply(dt[,15], function(x)all(is.na(x))) # check null value
# dt <- dt[, which(unlist(lapply(dt, function(x)!all(is.na(x))))), with = FALSE]

## check duplication ###########################################################
## check row
# any(duplicated(dt$))
# table(duplicated(dt$)) # No. of false (no duplicates): ; No. of true: 0.
# dt[, duplicatedID := rowid(id)] # numbering duplicated rows
# fvar(dt$duplicatedID)
# dt <- dt[!duplicated(dt, by =c(), fromLast = FALSE)] # the initial duplicates columns
# dt <- dt[!duplicated(dt, by =c(), fromLast = TRUE)]  # the last duplicates columns
# dt[duplicated(id) | duplicated(id,fromLast = TRUE), ] # all duplicates columns

## check odd and even rows
# d <- dt[duplicated(dt$id) | duplicated(dt$id, fromLast = TRUE)]
# setorder(d,id)
# odd_rows  <- dt$id[seq(1, nrow(t), by = 2)]
# even_rows <- dt$id[seq(2, nrow(t), by = 2)]
# all.equal(odd_rows, even_rows) # having / no duplicated data 
# identical(odd_rows, even_rows)

## check column
# any(grepl("\\.x", names(dt))) # false means no duplicates
# colnames(dt) <- make.unique(names(dt)) # rename duplicate columns

## data cleaning and checking ##################################################
## exclusion ###################################################################
# included.cols <- c("",
#           "",
#           "",
#           "sex",
#           "age")
# t(dt[, lapply(.SD, function(x) sum(is.na(x))), .SDcols = included.cols])

# dt <- dt[complete.cases(dt[, included.cols, with = FALSE])]
# t(dt[, lapply(.SD, function(x) sum(is.na(x))), .SDcols = included.cols])

# VIM::aggr(dt[,included.cols],
#           col      = c('white','grey'),
#           numbers  = TRUE,
#           sortVars = FALSE,
#           combined = TRUE,
#           prop     = TRUE,
#           labels   = names(dt[, included.cols]),
#           cex.axis = 1,
#           gap      = 3)

## variables pre-processing and visualization ##################################
# names(dt)
var.cols <- c(# exposures
  "",
  "",
  "",
  "",
  # outcomes
  "",
  "",
  "",
  "",
  # covariates
  "",
  "",
  "",
  "",
  "sex",
  "age"
)

dt[] <- lapply(dt, as.numeric)
# dt[, c(var.cols) := lapply(.SD, as.numeric), .SDcols = var.cols]
# SmartEDA::ExpData(dt[,..var.cols], type = 2)
# t(dt[, lapply(.SD, function(x) sum(is.na(x))), .SDcols = var.cols])
# t(dt[, lapply(.SD, typeof), .SDcols = var.cols])

# dt[, var.cols, with = FALSE] %>% gather() %>% ggplot(aes(x = value)) +
#                                              geom_histogram() + 
#                                               theme_minimal() + 
#                                               facet_wrap( ~ key, 
#                                                           scales = "free")

#### data processing procedures ################################################
## outcomes ####################################################################
dt$py <- as.duration(dt$date1 %--% dt$date2) / dyears(1)
fvar()
fs()
fsp()
fbp()

## excluding individuals with medical history ##################################
# Individuals with history of : 
dt <- dt[] 
p1.nrow <- nrow- nrow(dt)
# Individuals with history of : 
dt <- dt[] 
p2.nrow <- nrow - nrow(dt) - p1.nrow
# Individuals with history of : 
dt <- dt[] 
p3.nrow <- nrow - nrow(dt) - p1.nrow - p2.nrow
# Available individuals: 
paste0("Individuals in input data : ", nrow)
paste0("Individuals with history A: ", p1.nrow)
paste0("Individuals with history B: ", p2.nrow)
paste0("Individuals with history C: ", p3.nrow)
paste0("Available individuals     : ", nrow(dt))

## exposures ###################################################################
fvar()
fs()
fsp()
fbp()

## age, sex and area ###########################################################
# setnames(dt,"年齢","age")
fvar(dt$age)
fs(dt$age)
dt <- dt[!is.na(age) & age >= 65, ]

dt$z <- ave(dt$, dt$group, dt$group2, FUN = scale) # Z score
dt$age.z <- ave(dt$age, FUN = scale)
fs(dt$age.z)

fvar(dt$sex)
dt <- dt[!is.na(sex), ]

## covariates ##################################################################
fvar(dt$wt)
fvar(dt$ht)
dt[!is.na(wt) & !is.na(ht), bmi := wt / ht / ht * 10000]
fvar(dt$bmi)
fs(dt$bmi)

dt[bmi > 0 & bmi < 30         , obesity := 0]
dt[bmi >= 30                  , obesity := 1]
dt[bmi >  0    & bmi < 18.5, bmi.groups := 1]
dt[bmi >= 18.5 & bmi < 25.0, bmi.groups := 2]
dt[bmi >= 25.0 & bmi < 30.0, bmi.groups := 3]
dt[bmi >= 30.0             , bmi.groups := 4]
dt[bmi <= 0 | is.na(bmi)   , bmi.groups := NA]

dt[, bmi.groups := factor(bmi.groups)]
dt[, bmi.groups := relevel(bmi.groups, ref = 2)]
fvar(dt$bmi.groups)

## labels and units ############################################################
label(dt$age) <- 'Age'
label(dt$sex) <- 'Sex'
label(dt$sexf) <- 'Female'
label(dt$area) <- 'Community'
label(dt$bmi) <- 'Body mass index'
label(dt$smoker) <- 'Smoking status'
label(dt$alcohDT) <- 'Alcohol intake'
label(dt$sbp) <- 'Systolic blood pressure'
label(dt$hbpmed) <- 'Antihypertensive medications use'
label(dt$ldmed) <- 'Lipid medications use'
label(dt$chol) <- 'Total cholesterol levels'
label(dt$hdl) <- 'HDL cholesterol levels'
label(dt$nonHDL) <- 'nonHDL cholesterol levels'
label(dt$glu) <- 'Serum glucose levels'
label(dt$IDBFA2) <- 'Family history of diabetes'
label(dt$tg) <- 'Serum triglyceride levels'
label(dt$TIMEm) <- 'Postprandial time'

units(dt$pyear) <- "Years"

## data dist ###################################################################
var.list <- c(# exposures
  "",
  "",
  "",
  "",
  # outcomes
  "",
  "",
  "",
  "",
  # covariates
  "",
  "",
  "",
  "",
  "sex",
  "age"
)

options(datadist=NULL)
dt.ddist <- datadist(dt[, ..var.list])
options(datadist = dt.ddist)
## data processing procedures have competed <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

#### anaylsis procedures #######################################################
#### characteristics table; Table 1 ############################################
names(dt)

Tableone.all_varibales <- c("",
                            "",
                            "",
                            "",
                            "sex",
                            "age"
)
Tableone.categorical_variables <- c("",
                                    "",
                                    "",
                                    "",
                                    "",
                                    "sex"
)
Tableone.nonnormal_variables <- c("",
                                  "",
                                  "",
                                  "",
                                  ""
)
Tableone.group <- ""
T1(
  dt,
  Tableone.all_varibales,
  Tableone.categorical_variables,
  Tableone.nonnormal_variables,
  Tableone.group,
)

#### Visualization #############################################################
## 3D plot
plotly::plot_ly(
  x = ~ iris$Sepal.Length,
  y = ~ iris$Sepal.Width,
  z = ~ iris$Species,
  type   = "scatter3d",
  mode   = "markers",
  marker = list(size = 2)
) 

## linear plot
p1 <- ggplot(dt, mapping = aes(x =  , y = )) +
  geom_smooth(method           = "lm", 
              colour           = "black"
              ) +
  geom_point(position          = position_jitter(seed = 123), 
             size              = 1
             ) +
  scale_x_continuous(limits    =   c(0, 100), # scale_x_log10
                     breaks    = seq(0, 100, by = 10),
                     labels    = seq(0, 100, by = 10),
                     expression(paste("",))
  ) +
  # scale_y_continuous(limits = c(1e0, 1e4),
  #                    labels = trans_format("log10", math_format(10^.x))) +
  scale_y_continuous(limits    =   c(0, 10),
                     breaks    = seq(0, 10, by = 2),
                     labels    = seq(0, 10, by = 2),
                     ""
  ) +
  coord_cartesian(xlim         = c(),
                  ylim         = c(),
                  expand       = TRUE) +
  labs(caption = NULL, 
       title   = "",
       x       = "", 
       y       = "") +
  theme(plot.title             = element_text(size      = 10,
                                              face      = "bold"),
        plot.subtitle          = element_text(vjust     = 1),
        plot.caption           = element_text(vjust     = 1),
        axis.line              = element_line(linewidth = 0.5, 
                                              linetype  = "solid"),
        axis.title             = element_text(colour    = "black",
                                              size      = 9),
        axis.text              = element_text(colour    = "black",
                                              size      = 9),
        axis.ticks.length      = unit(0.5, "cm"),
        panel.grid.major       = element_line(colour    = "white"),
        panel.background       = element_rect(fill      = "gray99",
                                              colour    = "white",
                                              linetype  = "twodash"
        ),
        legend.key.width       = unit(10, "lines"),
        legend.position        = c(0.8, 0.8),
        legend.title           = element_text(size      = 10),
        legend.text            = element_text(size      = 10),
        legend.box.background  = element_rect(color     = "black",
                                              size      = 1, 
                                              fill      = "transparent")
        # plot.margin            = margin(0.03, 0.02, 0.02, 0.01, "npc") #上右下左 
  )

## box plot
stat_box_data <- function(y, upper_limit = min() ) {
  return(
    data.table(
      y = upper_limit + 10,
      label = paste('count  =', length(y),          '\n',
                    'mean   =', round(mean(y), 2),  '\n',
                    'median =', round(median(y),2), '\n'
      )
    )
  )
}

p1 <- ggplot(dt, mapping = aes(x =   , y =   )) +
  geom_boxplot(outlier.colour  = "black", 
               outlier.size    = 1, 
               width           = 1
               ) +
  stat_boxplot(geom            = 'errorbar', 
               width           = 1, 
               cex             = 1
               ) +
  geom_dotplot(binaxis         = 'y', 
               stackdir        = 'center', 
               dotsize         = 1, 
               binwidth        = 1
               ) +
  # geom_signif(comparisons      = list(c(" ", " ")), # ggsignif package
  #             test             = "wilcox.test",
  #             map_signif_level = c("P < 0.001" = 0.001,
  #                                  "P < 0.01"  = 0.01,
  #                                  "P < 0.05"  = 0.05), # function(p) sprintf("p = %5.3f", p)
  #             y_position       = 1,
  #             vjust            = 1,
  #             tip_length       = 1,
  #             textsize         = 1,
  #             colour           = "black") +
  stat_summary(fun.data        = stat_box_data,
               geom            = "text",
               linetype        = "",
               size            = 1,
               width           = 1,
               hjust           = 1,
               vjust           = 1
  ) +
  scale_color_manual(values    = c("black", "gray25", "gray50")) +
  scale_x_continuous(limits    =   c(0, 100),
                     breaks    = seq(0, 100, by = 10),
                     labels    = seq(0, 100, by = 10),
                     expression(paste("",))
  ) +
  scale_y_continuous(limits    =   c(0, 10),
                     breaks    = seq(0, 10, by = 2),
                     labels    = seq(0, 10, by = 2),
                     ""
  ) +
  coord_cartesian(xlim         = c(),
                  ylim         = c(),
                  expand       = TRUE) +
  labs(caption = NULL, 
       title   = "",
       x       = "", 
       y       = "") +
  theme(plot.title             = element_text(size      = 10,
                                              face      = "bold"),
        plot.subtitle          = element_text(vjust     = 1),
        plot.caption           = element_text(vjust     = 1),
        axis.line              = element_line(linewidth = 0.5, 
                                              linetype  = "solid"),
        axis.title             = element_text(colour    = "black",
                                              size      = 9),
        axis.text              = element_text(colour    = "black",
                                              size      = 9),
        axis.ticks.length      = unit(0.5, "cm"),
        panel.grid.major       = element_line(colour    = "white"),
        panel.background       = element_rect(fill      = "gray99",
                                              colour    = "white",
                                              linetype  = "twodash"
        ),
        legend.key.width       = unit(10, "lines"),
        legend.position        = c(0.8, 0.8),
        legend.title           = element_text(size      = 10),
        legend.text            = element_text(size      = 10),
        legend.box.background  = element_rect(color     = "black",
                                              size      = 1, 
                                              fill      = "transparent")
        # plot.margin            = margin(0.03, 0.02, 0.02, 0.01, "npc") #上右下左 
  )


# par(mfrow=c(2, 2)) # drawing of plural graphs 2*2

all_plot <- gridExtra::grid.arrange(p1, 
                                    p2, 
                                    p3, 
                                    p4,
                                    nrow = 2,ncol = 2)

ggsave("plot.tiff", 
       plot, 
       height = 12, 
       width  = 8, 
       dpi    = 300, 
       device = "tiff",
       top    = "", 
       bottom = "",
       left   = "",
       right  = grid::textGrob("",
                               gp    = grid::gpar(fontsize = 9),
                               hjust = 1,
                               x     = 1
                               )
       )

#### figure 1 ##################################################################


#### multiple imputation #######################################################
## using Fully Conditional Specification (FCS) implemented by the MICE algorithm
midt <- dt[, 
           c("", 
             "", 
             "", 
             "", 
             "", 
             "", 
             "")]

mice::md.pattern(midt, 
                 plot = TRUE
                 )

midt.imp <- mice::mice(midt,
                       m     = 10,
                       maxit = 10,
                       pred  = mice::quickpred(midt,
                                               mincor=0.3,
                                               method = "pearson"
                       ),
                       # pred, method = 'pearson''kendall''spearman'
                       seed  = 161893)
# midt.imp$loggedEvents
# method for continuous variable     : pmm     -- predictive mean matching
# method for binary variable         : logreg  -- logistic regression
# method for categorical variable    : polyreg -- bayesian polytomous regression
# method for grade variable (ordered): polyr   -- proportional odds model
midt.imp$method  # rerun mice::mice for renew the imputation method if needs
midt.imp$pred    # rerun mice::mice for renew the predictor matrix if needs
mice::stripplot(midt.imp)
mice::complete(dt.imp, 1) # show the multiplied data
# produces a data frame of imputed datasets. include = T means orginal dataset.
# midt.comp <- complete(midt.imp, "long", include = TRUE)

##  multiplied data analysis
imp.fit <- with(midt.imp, cph(Surv(py, ) ~ age + sex  ))

summary(imp.fit)
summary(imp.fit$analyses[[1]]) # for specific multiplied data
mice::pool(imp.fit)
summary(mice::pool(imp.fit),
        conf.int     = TRUE,
        exponentiate = TRUE) # for risk ratio

## statificated analysis
cox_results <- list()
for (i in 1:10) {
  midt.spec <- as.data.table(mice::complete(midt.imp, i))
  midt.cox_model <- cph(
    Surv(py, y) ~ age + sex ,
    data = midt.spec
  )
  midt.cox_results[[i]] <- midt.cox_model
}

summary(mice::pool(midt.cox_results),
        conf.int     = TRUE,
        exponentiate = TRUE)


#### table 2 ###################################################################
#### pyear table
fpn(dt$event, dt$exposure)
fpy(dt$pyear, dt$exposure) 

## linear regression
fit.ols <- ols(y ~ age + sex , data = dt,x = TRUE)

frr(summary(fit.ols))
summary(fit.ols)
ggplot(fit.ols)
autoplot(fit.ols)
anova(fit.ols)

mean(fit.ols$residuals^2) # Mean Square Error
sqrt(mean(fit.ols$residuals^2)) # Root Mean Square Error


## estimated marginal means
emmeans::emm_options(opt.digits = FALSE)
geo_mean(dt$)
liner_model_results <- lm(log() ~ age + sex  , 
                          data = dt)
summary(liner_model_results)
emmeans <- emmeans::emmeans(liner_model_results,
                            specs = trt.vs.ctrl ~ age,
                            type  = "response"
)
confint(emmeans)
summary(emmeans)

#### post hoc analysis


#### logistic regression
# k <- with(dt.ddist, quantile(, c(0.10,  0.50, 0.90)))
logistic.model.0 <- ~ age + sex + 
logistic.model.1 <- ~
logistic.model.2 <- ~


fit.lrm <- lrm(logistic.model.0, data = dt,x = TRUE, y = TRUE)
summary(fit.lrm)
frr(summary(fit.lrm))
ggplot(fit.lrm)
anova(fit.lrm)
predict(fit.lrm)

#### ordinal logistic regression
# https://stats.oarc.ucla.edu/r/dae/ordinal-logistic-regression/
# https://stats.oarc.ucla.edu/r/faq/ologit-coefficients/
ordinal.model.0 <- ~ age + sex + 
ordinal.model.1 <- ~
ordinal.model.2 <- ~
  
fit.orm <- orm(ordinal.model.0, data = dt)
summary(orm.fit)

orm.f <- function(y) {
  c('Y>=1' = qlogis(mean(y >= 1)),
    'Y>=2' = qlogis(mean(y >= 2)),
    'Y>=3' = qlogis(mean(y >= 3)))
}

orm.logit.table <- summary( ~ p, fun = orm.f, data = dt)
plot(
  orm.logit.table,
  which = 1:3,
  pch = 1:3,
  xlab = 'logit',
  vnames = ''
)

#### cox regression
# k <- with(dt.ddist, quantile(, c(0.10,  0.50, 0.90)))
cox.model.0 <- Surv(py, y) ~ age + sex + 
cox.model.1 <- Surv(py, y) ~  
cox.model.2 <- Surv(py, y) ~
  
fit.cph <- cph(cox.model.0, x = TRUE, y = TRUE)
survival::cox.zph(fit.cph, "rank")
survminer::ggcoxzph(survival::cox.zph(fit.cph))
plot(survival::cox.zph(fit.cph))

summary(fit.cph)
frr(summary(fit.cph))
plot(Predict(fit.cph))

anova(fit.cph)
AIC(fit.cph)
predict(fit.cph)

cal.cph <- calibrate(fit.cph, u=10, B =10)
plot(cal.cph)


#### restricted cubic spline
dt.ddist$limits["Adjust to", exposure] <- 0
rcs_model <- update()
rcs_fit <- Predict(rcs_model, 
                   exposure, 
                   ref.zero = TRUE, 
                   fun = exp)
# rcs_fit <- setDT(rcs_fit) # if necessary
rcs_plot <- ggplot(rcs_fit, 
                   aes(exposure, yhat) #  "lower", "upper"
                   ) +
  geom_line(colour             = "Black",
            aes(linetype       = factor()),
            linewidth          = 0.8,
            ) +
  # scale_linetype_manual(name   = "", # for linetype legend
  #                       values = c("solid",  # if rcs_fit is a dataset
  #                                  "longdash",
  #                                  "dashed",
  #                                  "twodash",
  #                                  "dotdash",
  #                                  "dotted")
  #                    ) +
  # geom_ribbon(aes(ymin         = lower, # for shadow if rcs_fit is a dataset
  #                 ymax         = upper,
  #                 group        = factor()),
  #                 alpha        = 0.1
  #             ) +
  scale_x_continuous(limits    =   c(0, 100),
                     breaks    = seq(0, 100, by = 10),
                     labels    = seq(0, 100, by = 10),
                     expression(paste("",))
                     ) +
  scale_y_continuous(limits    = c(0, 10),
                     breaks    = seq(0, 10, by = 2),
                     labels    = seq(0, 10, by = 2),
                     "Hazard ratios and 95% confidence intervals"
                     ) +
  coord_cartesian(xlim         = c(),
                  ylim         = c(),
                  expand       = TRUE) +
  labs(caption = NULL, 
       title   = "",
       x       = ", mg/dL", 
       y       = "Predicted hazard ratios and 95% confidence intervals") +
  theme(plot.title             = element_text(size      = 10,
                                              face      = "bold"),
        plot.subtitle          = element_text(vjust     = 1),
        plot.caption           = element_text(vjust     = 1),
        axis.line              = element_line(linewidth = 0.5, 
                                              linetype  = "solid"),
        axis.title             = element_text(colour    = "black",
                                              size      = 9),
        axis.text              = element_text(colour    = "black",
                                              size      = 9),
        axis.ticks.length      = unit(0.5, "cm"),
        panel.grid.major       = element_line(colour    = "white"),
        panel.background       = element_rect(fill      = "gray99",
                                              colour    = "white",
                                              linetype  = "twodash"
                                              ),
        legend.key.width       = unit(10, "lines"),
        legend.position        = c(0.8, 0.8),
        legend.title           = element_text(size      = 10),
        legend.text            = element_text(size      = 10),
        legend.box.background  = element_rect(color     = "black",
                                              size      = 1, 
                                              fill      = "transparent")
        # plot.margin            = margin(0.03, 0.02, 0.02, 0.01, "npc") #上右下左 
        ) +
  geom_hline(yintercept        = 1, 
             linetype          = "dashed") +
  annotate("",
           x                   = 0,
           y                   = Inf,
           vjust               = 2,
           hjust               = "left",
           label               = ("P value for non-linearity = ")
           ) 
  # geom_histogram(data          = dt, # for double plot if necessary
  #                aes(x         = TG, 
  #                    y         = ..count.. ),
  #                binwidth      = 1,
  #                fill          = "gray50",
  #                alpha         = 0.5
  #                ) 
    
    
#### Kaplan–Meier estimator
KMplot.model <- Surv(py, y) ~ 1
KMplot.data  <- dt
  survminer::ggsurvplot(
    survminer::surv_fit(KMplot.model,
                        data = KMplot.data),
    data               = KMplot.data,
    conf.int           = TRUE,
    pval               = TRUE,
    pval.method        = TRUE,
    # pval.coord         = c(0,1.0),
    # test.for.trend     = TRUE, # > 2 groups
    risk.table         = TRUE,
    # surv.median.line   = c("hv"),
    # legend.labs        = c("Group1", "Group2"),
    legend.title       = "Group",
    # palette            = c("gray25", "gray50"),
    risk.table.height  = .25
  )
ggsurvfit::survfit(KMplot.model,
                   data = KMplot.data)
summary(survival::survfit(KMplot.model,
                          data = KMplot.data),
        times = 1)


#### Fine-Gray sub distribution model ##########################################
# https://www.annualreviews.org/doi/10.1146/annurev.publhealth.20.1.145
# https://onlinelibrary.wiley.com/doi/full/10.1002/sim.8399
competing.risk.model.0 <- model.matrix(
  ~  age +
     sex 
    ,
  data = dt
)[, -1]
competing.risk.model.1 <- model.matrix(
  ~  age +
     sex 
    ,
  data = dt
)[, -1]



#### model diagnostics #########################################################
model <- fit.

## multicollinearity
car::vif(model)

## residuals
residuals <- residuals(model)
# pearson residuals
residuals <- residuals(model, type = "pearson")
# Martingale residuals for cox model
residuals <- residuals(model, type = "martingale")
# deviance residuals for logistic and cox model
residuals <- residuals(model, type = "deviance")

# residuals plot
plot(fitted(model), res);abline(0,0)
hist(residuals) # histogram of residuals
plot(model, which = 1)  # residuals-fit plots
plot(model, which = 2)  # standardized residual-fit plot
car::residualPlots(model, col.quad = "black")

## Q-Q plot for residuals
# qqnorm(res);qqline(res)
car::qqp(
  model,
  id = F,
  simulate = F,
  col.lines = "black"
)

## density plot.
plot(density(residuals))

## E-values 
# https://louisahsmith.github.io/evalue/index.html


#### table X ###################################################################



#### sensitivity analysis; table X #############################################



#### stratified analysis; table X ##############################################



#### decision curve analysis ###################################################
# https://www.danieldsjoberg.com/dca-tutorial/dca-tutorial-r.html
dca1 <- cph(Surv(py, y) ~ age, 
            data = dt,
            x    = TRUE,
            y    = TRUE,
            surv = TRUE
            )
dca2 <- cph(Surv(py, y) ~ age,
            data = dt,
            x    = TRUE,
            y    = TRUE,
            surv = TRUE
            )

dcadt <- dcadt %>% mutate(
  mn1 = 1 - survest(dca1, newdata = dcadt, times   = 10)$surv,
  mn2 = 1 - survest(dca2, newdata = dcadt, times   = 10)$surv
)

dplot <- dcurves::dca(Surv(py, y) ~ mn1 + mn2,
    data       = dcadt,
    time       = 10,
    thresholds = seq(0, 0.1, 0.001), # x axis scale
    label      = list(mn1 = "",
                      mn2 = "")
)

as_tibble(dplot) %>%
  mutate(label = as.character(label)) %>%
  mutate(label = ifelse(label == "Treat All", 
                                 "Intervene All", 
                        label)) %>%
  mutate(label = ifelse(label == "Treat None", 
                                 "Intervene None", 
                        label)) %>%
  mutate(label = factor(label,
                        levels = c("Intervene All",
                                   "Intervene None",
                                   "-based prediction",
                                   "-based prediction + "
                                   )
                        )) %>%
  dplyr::filter(!is.na(net_benefit)) %>%
  ggplot(aes(x        = threshold,
             y        = net_benefit,
             roup     = label,
             color    = label,
             linetype = label
             )) +
  stat_smooth(method = "loess",
              se = FALSE,
              formula = "y ~ x",
              span = 0.5
              ) +
  scale_color_manual(values = c('gray70', 
                                'black', 
                                'black', 
                                'black')
                     ) +
  scale_linetype_manual(values = c("solid", 
                                   "solid", 
                                   "dotted", 
                                   "dotdash")
                        ) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  coord_cartesian(ylim      = c(, )) +
  labs(caption  = NULL,
       x        = "Threshold Probability",
       y        = "Net Benefit",
       color    = "Prediction Strategy",
       linetype = "Prediction Strategy"
  ) +
  theme(plot.title             = element_text(size      = 10,
                                              face      = "bold"),
        plot.subtitle          = element_text(vjust     = 1),
        plot.caption           = element_text(vjust     = 1),
        axis.line              = element_line(linewidth = 0.5, 
                                              linetype  = "solid"),
        axis.title             = element_text(colour    = "black",
                                              size      = 9),
        axis.text              = element_text(colour    = "black",
                                              size      = 9),
        axis.ticks.length      = unit(0.5, "cm"),
        panel.grid.major       = element_line(colour    = "white"),
        panel.background       = element_rect(fill      = "gray99",
                                              colour    = "white",
                                              linetype  = "twodash"
        ),
        legend.key.width       = unit(10, "lines"),
        legend.position        = c(0.8, 0.8),
        legend.title           = element_text(size      = 10),
        legend.text            = element_text(size      = 10),
        legend.box.background  = element_rect(color     = "black",
                                              size      = 1, 
                                              fill      = "transparent")
        # plot.margin            = margin(0.03, 0.02, 0.02, 0.01, "npc") #上右下左 
        ) +
  annotate("",
           x                   = 0,
           y                   = Inf,
           vjust               = 2,
           hjust               = "left",
           label               = ("P value for non-linearity = ")
  ) 

## the code stack ##############################################################
## 2024/08/01 version ##########################################################

## The global options ##########################################################
# reset
rm(list = ls())

# modify number of decimal places displayed
options(scipen = 10,digits = 10)

# show conflicts among packages
conflicts(where = search(), TRUE)

# list objects and their structure
ls.str()

# reset all opinions
default_opts <- callr::r(function(){options()})
options(default_opts)

## Read data ###################################################################
# obtain all file names in a folder 
folder_path <- "C:///"
files <- list.files(path = folder_path, pattern = "\\.csv$", full.names = TRUE)

## Data processing #############################################################
# drop quotes
noquote("noqueto")

# choose a subgroup
v <- c("Sepal.Length", "Sepal.Width")
as.data.table(iris)[, ..v]

# order rows by
x <- setorder(iris, Sepal.Length, na.last = FALSE)

# new variable for max/min among a and b
iris[, Sepal.max := pmax(Sepal.Length, Sepal.Width, na.rm = TRUE)]

# change column order .before/.after
iris <- iris %>% relocate(Sepal.Length, .after =  Sepal.Width)

# only 0 or 1, not sum
iris$n <- as.numeric(grepl("i", iris$Species))

# create new columns
iris[, paste0("Q",1:15) := 0]

# creat a date
iris$date1 <- as.Date(with(iris, paste("2000", "12", "12",sep="-")), "%Y-%m-%d")
# creat a date
iris$date2 <- as.Date(as.character("2006-1-1"),"%Y-%m-%d")
# date duration
iris$d <- as.duration(iris$date1 %--% iris$date2) / dyears(1)
# obtain year
year(ymd("2006-1-1"))

# fixed decimal number
format(round(1.000, 2), nsmall =2)

# C-style String
# https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/sprintf
# %m.nf: m represents minimum width; n represents number of decimal places
sprintf("%f"        , pi)
sprintf("%5.2f"     , pi)
sprintf("%05.2f"    , pi)
sprintf("%+f"       , -pi) # + means positive(+) or negative(-) signs
sprintf("% f"       , -pi) # space means negative(-) signs if negative number
sprintf("%-10.1f"   , pi) # left justified
sprintf("%10.1f"    , pi) # right justified
sprintf("%10.5d"    , 3)
sprintf("%10.5s"    , "a")
sprintf("%10.2f%%"  , 0.05)

# Regular expressions
# see more:
# https://r4ds.had.co.nz/strings.html
# https://cran.r-project.org/web/packages/stringr/vignettes/regular-expressions.html
expression(x^2) # ^2 in plot
# str_c() is a advanced function like paste()； paste0() is s simple version.

# change part of var to others
iris$Species <- gsub("i", "\\.", as.character(iris$Species))

# apply series
# see more:
# https://www.statology.org/a-guide-to-apply-lapply-sapply-and-tapply-in-r/
# apply: a function to the rows or columns
# (X, MARGIN((row = 1, column = 2)), FUN)
# lapply: a function to each element or column as a list result
# sapply(X, FUN)
# sapply: a function to each element or column as a vector result
# tapply(X, INDEX, FUN)
# tapply: a function to apply in subsets
# like tapply(iris$Sepal.Length, iris$Species, max)
iris <- iris[, lapply(.SD, sum), .SDcols = c("Sepal.Length"), by = c("Species")]
iris[, .SDcols := lapply(.SD, as.character), .SDcols = c("Sepal.Length")]

# .N: Returns the frequency.
iris[, .N, by = "Species"]
# .I: Returns the row number.
iris[, .I[1], by = "Species"]
# .SD: Returns the data subset of the current group.
iris[, .SD[1:2], by = "Species"]
# .BY: Returns the key value of the current group.
iris[, .(mean_length = mean(Sepal.Length), species_name = .BY$Species), by = "Species"]
# .GRP: Returns the group number.
iris[, .(group_number = .GRP), by = "Species"]
# .NGRP: Returns the total number of groups.
iris[, .NGRP, by = "Species"]

# return A*B cross row numbers, and can be overlayed
iris[, .N, by = c("Species","Sepal.Length")][, .N, by = "Species"][N > 1]

# Stratified random sampling
resample <- dt %>%
  group_by(age.group, sex) %>%
  slice_sample(prop = 0.3, replace = FALSE)


## correlation coefficient #####################################################
dt.corr <- dt[,c("", "")]
psych::corr.test(dt.corr,
                 method = "pearson", 
                 adjust = "holm"
                 )
# method = "pearson" "kendall" "spearman"
# adjust = "holm" “holm” “bonferroni” “fdr” etc.
dt.corr %>% GGally::ggpairs()

## Compare sample differences ##################################################
## assumptions for parametric tests
## normality test
# Shapiro-Wilk normality test
shapiro.test(dt) # ＞0.05 is normal distribution; N <50
# Kolmogorov-Smirnov normality test
ks.test(dt, "pnorm") # ＞0.05 is normal distribution; N > 50

## equality of variances test
car::scatterplot(response ~ classes, data = dt, col = "black") #boxplot
# Levene’s Test
car::leveneTest(response ~ groups, data = dt) #＞0.05 is equality of variances

## Parametric tests ############################################################
# T test
car::leveneTest(dt$, dt$)
t.test(dt$~dt, var.equal = TRUE, paired = TRUE)

# Chi-squared test
chisq.test(dt$, dt$)
chiq.result <- chisq.test(xtabs( ~ , data = dt))
fisher.test(matrix(c(0, 0, 0, 0), nrow = 2))

# Cochran Armitage trend test
CochranArmitageTest.table <- table(dt$a, dt$b)
DescTools::CochranArmitageTest(CochranArmitageTest.table)

# ANOVA
# one-way anova
anova.fit <- aov( ~ + age + sex, data=dt)
summary(anova.fit)
car::Anova(anova.fit, type = 3)

## Nonparametric tests #########################################################
# Wilcoxon test
# Wilcoxon signed-rank test for paired data
# Mann–Whitney U test (Wilcoxon rank-sum test) for unpaired data
coin::wilcox_test(   ~ factor(   ),
                  data = dt,
                  conf.int = TRUE,
                  paired = FALSE
                  )

# Kruskal–Wallis test
kruskal.test( ~ , data = dt)
# DunnTest performs the post hoc pairwise multiple comparisons procedure-
# appropriate to follow the rejection of a Kruskal-Wallis test.
dunnTest( ~ , data = dt, method="holm")

# Jonckheere's trend test
# when sample size > 100 or same number exists, there is a Warning message.
JonckheereTerpstraTest( ~ , dt)

# adjust P-values for multiple comparisons
p.adjust(p, method = "bonferroni")
# c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY",
#   "fdr", "none")


#### mixed linear model ########################################################
# http://mfviz.com/hierarchical-models/ # visualization site
# https://m-clark.github.io/mixed-models-with-R/introduction.html
mixed.linear.model.initial <-
  ~  +
  (1 | )

mixed.linear.model.1 <-
  ~  +
  (1 | )

mixed.linear.model.2 <-
  ~  +
  (1 + | )
dt %>%
  ggplot(aes(
    ,
    ,
    group = ,
    color =
  )) +
  geom_point() +
  geom_smooth(method = "lm", se = F)

mixed.fit <-
  lme4::lmer(
    mixed.linear.model.2
  )
print(summary(mixed.fit),corr = FALSE)
AIC(mixed.fit)
BIC(mixed.fit)
lattice::dotplot(ranef(mixed.fit))
broom.mixed::tidy(mixed.fit)

Estimate  <- summary(mixed.fit)$coefficients[,1]
Std.Error <- summary(mixed.fit)$coefficients[,2]

mixed.slope.table <- data.table(
  Exposures = row.names(as.data.frame(Estimate)),
  slope     = paste0(round_function(Estimate,2),
                     " (",
                     round_function(Estimate - 1.96 * Std.Error,2),
                     "-",
                     round_function(Estimate + 1.96 * Std.Error,2),
                     ")"
  )
)
mixed.slope.table

#### mixed logistic model ######################################################
mixed.or.model.initial <-
  ~  +
  (1 | )

mixed.or.model.1 <-
  ~  +
  (1 | )

mixed.or.model.2 <-
  ~  +
  (1 + | )

mixed.fit <-
  lme4::glmer(
    mixed.or.model.1,
    data    = dt,
    family  = binomial(link = "logit"),
    control = lme4::glmerControl(optimizer = "bobyqa",
                                 optCtrl   = list(maxfun = 1e6))
  )
print(summary(mixed.fit),corr = FALSE)
AIC(mixed.fit)
BIC(mixed.fit)
lattice::dotplot(ranef(mixed.fit))
broom.mixed::tidy(mixed.fit)

Estimate  <- summary(mixed.fit)$coefficients[,1]
Std.Error <- summary(mixed.fit)$coefficients[,2]

mixed.ORs.table <- data.table(
  Exposures = row.names(as.data.frame(Estimate)),
  ORs       = paste0(round_function(exp(Estimate),2),
                     " (",
                     round_function(exp(Estimate - 1.96 * Std.Error),2),
                     "-",
                     round_function(exp(Estimate + 1.96 * Std.Error),2),
                     ")"
  )
)
mixed.ORs.table

#### G-formula #################################################################
# https://www.hsph.harvard.edu/miguel-hernan/causal-inference-book/
# https://www.sciencedirect.com/science/article/pii/S2666389920300088?via%3Dihub

ymodel <- /outcome/ ~ /exposure/ + L1 + L2 + L3 + lag1_/exposure/ + lag1_L1 + lag1_L2 + t0
histories <- c(lagged, lagavg)
histvars <- list(c('/exposure/', 'L1', 'L2'), c('L1', 'L2'))
covparams <-
  list(
    covmodels = c(
      L1 ~ lag1_/exposure/ + lag_cumavg1_L1 + lag_cumavg1_L2 +
        L3 + /time interval/,
      L2 ~ lag1_/exposure/ + L1 + lag_cumavg1_L1 +
        lag_cumavg1_L2 + L3 + /time interval/,
      /exposure/  ~ lag1_/exposure/ + L1 + L2 + lag_cumavg1_L1 +
        lag_cumavg1_L2 + L3 + /time interval/
    )
  )

gform_basic <- gfoRmula::gformula(
  obs_data = basicdata,
  ymodel = ymodel,
  outcome_type = 'survival',
  id = 'id',
  time_points = /time interval number/,
  time_name = '/time interval/',
  covnames = c('L1', 'L2', '/exposure/'), # time-varying variables
  covtypes = c('binary', 'bounded normal', 'binary'),
  basecovs = c("L3"), # time-independent variables
  outcome_name = 'Y',
  compevent_name = 'D',
  intvars = list('/exposure/', '/exposure/'),
  interventions = list(list(c(gfoRmula::static, rep(0, time_points))),
                       list(c(gfoRmula::static, rep(1, time_points)))),
  int_descript = c('Never treat', 'Always treat'),
  covparams = covparams,
  histories = histories,
  histvars = histvars,
  nsimul = 10000,
  seed = 1234
)
gform_basic
plot(gform_basic)

## Latent class mixed model ####################################################
# https://cran.r-project.org/web/packages/lcmm/vignettes/latent_class_model_with_hlme.html
# https://cran.r-project.org/web/packages/lcmm/vignettes/introduction.html
# https://www.jstatsoft.org/article/view/v078i02
lattice::xyplot(
  /outcome/~ age,
  dt,
  groups = id,
  col = dt$id,
  lwd = 1,
  type = "l"
)

dt[, age.40 := (age - 40) / 10]
latent.j.fit <- lcmm::Jointlcmm(
  /outcome/ ~ poly(age.40, degree = 2, raw = TRUE) + sexc
  random = ~ poly(age.40, degree = 2, raw = TRUE),
  survival = Surv(py, end) ~ sexc, # Survival analysis for each class from outcome
  hazard = "Weibull",
  hazardtype = "PH",
  subject = "REGI",
  data = dt,
  ng = 1
)
summary(latent.j.nonfasting.fit)
lcmm::summarytable(latent.j.nonfasting.fit)

latent.j.fit./n/ <- lcmm::Jointlcmm(
  /outcome/ ~ poly(age.40, degree = 2, raw = TRUE) + sexc,
  mixture =　~ poly(age.40, degree = 2, raw = TRUE) + sexc,
  random  = ~ poly(age.40, degree = 2, raw = TRUE) ,
  survival = Surv(py, end) ~ sexc,
  hazard = "Weibull",
  hazardtype = "PH",
  subject = "REGI",
  data = dt,
  ng = /n/,
  B = latent.j.fit
)
summary(latent.j.fit./n/)
lcmm::summarytable(latent.j.fit./n/)
lcmm::postprob(latent.j.fit./n/)

fs(dt$/outcome/)
fs(dt$age.40)
rdt <- data.table(age.40 = seq(0, 10, length = 100),
                  sexc = 0)
pred <- predictY(latent.j.fit./outcome/,
                 newdata = rdt,
                 var.time = "age.40")
plot(
  pred,
  bty = "l", # axis style
  ylim = c(0, 10),
  legend.loc = "bottomleft",
  ylab = "/outcome/",
  xlab = "age in decades from 40 years",
  lwd = 2
)

## ROC analysis#################################################################
# https://www.ucr.uu.se/en/services/biostatistics/downloads
# http://cainarchaeology.weebly.com/r-function-for-optimism-adjusted-auc.html

# graph a ROC curve with AUC and 95% CI and cutpoint
logistic_model_origin <-
  glm(
    *outcome ~ *exposure,
    family = binomial(logit),
    data = dt,
    x = TRUE
  )
summary(logistic_model_origin)
ROC_origin <- pROC::roc(
  response                = *dt$outcome,
  predictor               = logistic_model_origin$fitted,
  plot                    = TRUE,
  ci                      = T,
  print.auc               = TRUE,
  print.thres             = "best",
  print.thres.best.method = "youden",
  legacy.axes             = T
)

# bootstrap 95% CI of AUC
set.seed(161893)
ci.auc(
  ROC_origin,
  conf.level = 0.95,
  method = c("bootstrap"),
  boot.n = 500
)

# optimism adjusted  AUC and 95% CI
dt <- dt %>% relocate(*outcome)
set.seed(161893)
optimism_adjusted_AUC(dt, logistic_model_origin, 500)

# graph a plot calibration
plotCalibration(data     = dt,
                cOutcome = 1,
                predRisk = logistic_model_origin$fitted,
                groups   = 10)

# create a ROC table
ROC.table <- data.table(
  Sensitivities       = ROC_origin$sensitivities,
  Specificities       = ROC_origin$specificities,
  The_1_Specificities = 1 - ROC_origin$specificities,
  Thresholds          = ROC_origin$thresholds,
  youden              = ROC_origin$specificities + ROC_origin$sensitivities - 1
)
ROC.table

# calculate the real value of cutpoint
ROC.cutoff <- coords(ROC_origin,
                     x           = "best",
                     ret         = c("threshold", "sensitivity", "specificity"),
                     best.method = "youden"
)
ROC.cutoff
ROC.result <- data.table(
  The_cutoff_value = as.numeric((
    log(ROC.cutoff[1]/(1 - ROC.cutoff[1])) -
      coef(logistic_model_origin)[1])/coef(logistic_model_origin)[2]
  ),
  The_AUC = round_function(ROC_origin$auc, 3),
  The_95_CI_delong_method = paste(
    round_function(
      as.numeric(
        as.list(
          ci.auc(ROC_origin,
                 conf.level = 0.95
          )
        )[1]
      ),
      3
    ),
    "-",
    round_function(
      as.numeric(
        as.list(
          ci.auc(
            ROC_origin,
            conf.level = 0.95)
        )[3]
      ),
      3
    )
  )
)
ROC.result

## calculate sensitivity and specificity
dt$exposure_cutoff <- 0
dt[ >= ,]$exposure_cutoff <- 1
fvar(dt$exposure_cutoff)
logistic_model_cut <- glm( ~ , family = binomial(logit), data = dt,x = TRUE)
summary(logistic_model_cut)
ROC_Cut <- roc(response    = ,
               predictor   = logistic_model_cut$fitted,
               plot        = TRUE,
               ci          = T,
               print.auc   = TRUE,
               print.thres = "best",
               print.thres.best.method = "youden",
               legacy.axes = TRUE
)
ROC.table <- data.table(
  Sensitivities       = ROC_Cut$sensitivities,
  Specificities       = ROC_Cut$specificities,
  The_1_Specificities = 1 - ROC_Cut$specificities,
  Thresholds          = ROC_Cut$thresholds,
  youden             = ROC_Cut$specificities + ROC_Cut$sensitivities - 1
)
ROC.table


#### AUC for k-fold cross-validation ###########################################
# logistic regression based on 10-fold cross-validation
set.seed(161893)
fit <- train(form            =  ~ ,
             data            = dt,
             trControl       = trainControl(method = "repeatedcv",
                                            number          = 10,
                                            repeats         = 10,
                                            classProbs      = TRUE,
                                            summaryFunction = twoClassSummary
             ),
             method = "glm",
             family = "binomial",
             metric = "ROC"
)
fit$resample
print(fit)

# AUC ROC for test data
# https://topepo.github.io/
# -caret/measuring-performance.html#measures-for-class-probabilities

predictTest <- data.table(
  obs = GermanCreditTest$Class,      ## observed class labels
  predict(fit,
          newdata = GermanCreditTest,
          type    = "prob"
  ),                         ## predicted class probabilities
  pred = predict(fit,
                 newdata = GermanCreditTest,
                 type    = "raw"
  )                   ## predicted class labels
)

twoClassSummary(data = predictTest, lev = levels(predictTest$obs))

## Machine learning ############################################################
# https://topepo.github.io/caret//index.html
# https://rstudio-pubs-static.s3.amazonaws.com/444182_d2db0ff6047d41c1ac435ea893e564ca.html
# http://topepo.github.io/caret/train-models-by-tag.html#Neural_Network

set.seed(161893)
Partition_symbol <- caret::createDataPartition(dt$outcome,
                                               p = 0.75, list = FALSE)
validation <- dt[-Partition_symbol,]
train      <- dt[ Partition_symbol,]

set.seed(161893)
train_fit <- caret::train(~  + ,
                          data   = train,
                          method = "rf",
                          metric = "Accuracy",
                          trControl = trainControl(method = "cv", number = 10)
)
train_fit$finalModel

predictions <- caret::predict(train_fit, newdata = validation)
caret::confusionMatrix(predictions, dt$outcome, positive = 1)

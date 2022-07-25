## ----setup, include=FALSE-----------------------------------------------------
knitr::knit_hooks$set(purl = knitr::hook_purl)

## ----load packages, message=FALSE, warning=FALSE------------------------------
# Use install.packages("packagename") if the packages are not installed in R

# Basic packages
library(gridExtra) # Plot and arrange multiple graphs in a single pane
library(tidyverse) # For ggplot and dplyr packages
library(readxl) # Fore reading Excel files
library(stargazer) # For neat tables and outputs, where possibles

# For machine learning, logistic regression, plots and tests
library(car) 
library(caret) # For building ML models
library(corrplot) # For correlation plot
library(mlbench) # For data sets
library(lmtest) # For statistical tests
library(glmnet) # For LASSO logistic regression
library(ROCR) # For determining cutoff-probability using ROC


# For dealing with imbalanced data sets
library(ROSE) # For under- and over-sampling methods
library(smotefamily) # For SMOTE methods

## ----load dataset from wd, message=FALSE--------------------------------------
# This step loads the file from the working directory. If unsure of wd, use 
# getwd() to check the location of the wd and setwd() to set the wd where the file is saved.

default.data <- data.frame(read_excel("credit card default.xls"))

## ----head tail----------------------------------------------------------------
# Show only the first 10 columns as there are too many variables

stargazer(rbind(head(default.data[,1:10]), tail(default.data[,1:10])),
          type = "text",
          title = "First and Last 6 Observations in Default Data",
          summary = F)

colnames(default.data)

## ----adjust column names------------------------------------------------------
colnames(default.data) <- default.data[1,]

default.data <- default.data[-1,]

## ----adjust row names---------------------------------------------------------
# Remove the first column since ID matches the auto-assigned row numbers.
default.data <- default.data[,-1]

rownames(default.data) <- NULL # Reset the row numbers

stargazer(head(default.data[,1:10]),
          type = "text",
          title = "First 6 Observations in Default Data, Adjusted",
          summary = F)

## ----renaming columns---------------------------------------------------------
newnames <- c("PAY_SEP","PAY_AUG","PAY_JUL", "PAY_JUN", "PAY_MAY", "PAY_APR",
              "BILLAMT_SEP","BILLAMT_AUG","BILLAMT_JUL", "BILLAMT_JUN", "BILLAMT_MAY", "BILLAMT_APR",
              "PAYAMT_SEP","PAYAMT_AUG","PAYAMT_JUL", "PAYAMT_JUN", "PAYAMT_MAY", "PAYAMT_APR")

oldnames <-names(default.data %>%
                   dplyr::select(grep("PAY" , names(default.data)), 
                                 grep("BILL", names(default.data)))) 

colnames(default.data)[colnames(default.data) %in% oldnames] <- newnames

default.data <- default.data %>%
  rename(default = `default payment next month`)

colnames(default.data)

## ----structure----------------------------------------------------------------
str(default.data)

## ----changing value types-----------------------------------------------------
# Changing variables PAYAMT, BILLAMT, AGE and LIMIT_BAL to numeric
default.data <- default.data %>% 
  mutate(
    across(
      c(grep("PAYAMT", names(default.data)), 
        grep("BILLAMT", names(default.data)), 
        "AGE", "LIMIT_BAL"), 
      as.numeric))

# Changing variables SEX, EDUCATION, MARRIAGE and default to factor
default.data <- default.data %>%
  mutate(
    across(
      c("SEX", "EDUCATION", "MARRIAGE", "default",
        grep("PAY_", names(default.data))),
      as.factor))

## ----summary------------------------------------------------------------------
summary(default.data)

## ----replace unknown class with others----------------------------------------
# For EDUCATION
levels(default.data$EDUCATION)[levels(default.data$EDUCATION) %in% c("0", "5", "6")] <- "4"

# For MARRIAGE
levels(default.data$MARRIAGE)[levels(default.data$MARRIAGE) == "0"] <- "3"

default.data %>% select(where(is.factor)) %>% str()

## ----dist of default, fig.align="center"--------------------------------------
ggplot(data = default.data, aes(x = default, fill = default)) +
  geom_bar()

## ----dist of sex_educ_marr_age, message=FALSE, fig.align="center"-------------
# For categorical and discrete variables
sex <- ggplot(data = default.data) +
  geom_bar(aes(x = SEX, fill = default)) +
  theme(legend.title = element_text(size = 10),
        legend.key.size = unit(0.5, "cm"),
        axis.title.x = element_text(size = 10))

education <- ggplot(data = default.data) + 
  geom_bar(aes(x = EDUCATION, fill = default)) +
  theme(legend.title = element_text(size = 10),
        legend.key.size = unit(0.5, "cm"),
        axis.title.x = element_text(size = 10))

marriage <- ggplot(data = default.data) +
  geom_bar(aes(x = MARRIAGE, fill = default)) +
  theme(legend.title = element_text(size = 10),
        legend.key.size = unit(0.5, "cm"),
        axis.title.x = element_text(size = 10))

age <- ggplot(data = default.data) +
  geom_histogram(aes(x = AGE, fill = default)) +
  theme(legend.title = element_text(size = 10),
        legend.key.size = unit(0.5, "cm"),
        axis.title.x = element_text(size = 10))

gridExtra::grid.arrange(sex, education, marriage, age, ncol = 2, nrow = 2)

## ----contingency table--------------------------------------------------------
colno <- grep("PAY_", colnames(default.data))

cont_tab <- list()

for (i in colno) {
  temp_tab <- table(default.data$default, default.data[,i])
  
  name <- colnames(default.data)[i]
  
  cont_tab[[name]] <- temp_tab
}

cont_tab

## ----dist billamt and payamt, fig.align="center"------------------------------
# For continuous variables
billamt <- default.data %>%
  select(grep("BILLAMT", names(default.data)))

payamt <- default.data %>%
  select(grep("PAYAMT", names(default.data)))

par(mfrow = c(1, 6), mar = c(1, 3, 3, 2))
for (i in 1:6) {
  boxplot(billamt[,i], main = names(billamt)[i])
}

par(mfrow = c(1, 6), mar = c(1, 3, 3, 2))
for (i in 1:6) {
  boxplot(payamt[,i], main = names(payamt)[i])
}

## ----corrplot, fig.align="center"---------------------------------------------
# Calculate the correlation between continuous variables
correl <- default.data %>% 
  select(where(is.numeric)) %>%
  cor(method = "spearman")

# Plot the correlation in a heatmap
corrplot(corr = correl,
         method = "color",
         type = "lower",
         main = "Correlation Matrix of Continuous Variables in Default Data",
         mar = c(0, 0, 1.5, 0))

## ----avg and max util---------------------------------------------------------
# Calculate average bill using rowMeans function
avg_util <- round((rowMeans(billamt)/ default.data$LIMIT_BAL)*100, 2)

## ----bind variables-----------------------------------------------------------
new_data <- data.frame(default.data[,1:11],
                       default.data[, grep("PAYAMT", names(default.data))],
                       avg_util,
                       default = default.data$default)

stargazer(rbind(head(new_data), tail(new_data)), 
          type = "text", 
          title = "First and Last 6 Observations in New Data", 
          summary = F,
          digits = 2)

## ----changing classes---------------------------------------------------------
# Set level 0 for those who were originally given levels -2, -1 and 0

for (i in colno) {
  levels(new_data[,i])[levels(new_data[,i]) %in% c("-2", "-1", "0")] <- "0"
  
  levels(new_data[,i])[levels(new_data[,i]) %in% c("1", "2", "3")] <- "1"
  
  levels(new_data[,i])[!levels(new_data[,i]) %in% c("-2", "-1", "0", "1", "2", "3")] <- "2"
}

levels(new_data$PAY_SEP) # To check as an example

## ----scaling LIMIT_BAL avg_pmt and pmt_bill_diff------------------------------
new_data$LIMIT_BAL <- new_data$LIMIT_BAL / 1000

new_data[, grep("PAYAMT", names(new_data))] <- new_data[, grep("PAYAMT", names(new_data))] / 1000

summary(new_data)

## ----correl new var, fig.align="center"---------------------------------------
correl <- new_data %>% 
  select(where(is.numeric)) %>%
  cor(method = "spearman")

corrplot(corr = correl,
         method = "color",
         type = "lower",
         addCoef.col = "black",
         main = "Correlation Matrix of Continuous Variables in New Data",
         mar = c(0, 0, 1.5, 0))

## ----split data---------------------------------------------------------------
# Split data into 80% training and 20% testing sets
set.seed(1)
validation <- createDataPartition(new_data$default, p = 0.80, list = F)

traindata <- new_data[validation,]

testdata <- new_data[-validation,]

## ----full model---------------------------------------------------------------
full.model <- glm(default == "1" ~ .,    # 1 refers to those who default
                  data = traindata,
                  family = binomial(link = "logit"))

stargazer(full.model, 
          type = "text", 
          title = "Regression Output of Full Model")

## ----vif2---------------------------------------------------------------------
car::vif(full.model)

## ----rest model wo marriage---------------------------------------------------
# Remove marriage variables
rest.model <- glm(default == "1" ~ .-MARRIAGE -PAYAMT_APR -PAYAMT_MAY -PAYAMT_JUN -PAYAMT_JUL,
                   data = traindata,
                   family = binomial(link = "logit"))

stargazer(rest.model, 
          type = "text", 
          title = "Regression Output of Restricted Model")

## ----lrtest2------------------------------------------------------------------
lmtest::lrtest(full.model, rest.model) # Result: Use full model

## ----cooksd-------------------------------------------------------------------
# Influence using Cook's Distance
plot(full.model, which = 4)

head(sort(cooks.distance(full.model), decreasing = T))

## ----standardized resid-------------------------------------------------------
# Outliers with standardized/studentized residuals
std.resid <- rstandard(full.model)

val <- abs(std.resid) > 3

table(val)

val[val == "TRUE"] # Can be used to further investigate the observations

## ----studentized resid2-------------------------------------------------------
car::outlierTest(full.model) # No outliers detected by studentized residuals

## ----looking at influential outlier-------------------------------------------
# Use this command to get data on the influential outlier(s)
traindata[val == "TRUE",]

## ----fitvalues and logit------------------------------------------------------
prob.fullmod <- fitted.values(full.model)

logit = log(prob.fullmod/(1-prob.fullmod))

## ----plot, fig.align="center", message=FALSE, warning=FALSE-------------------
p1 <- ggplot(data = traindata, aes(x = logit, y = LIMIT_BAL)) +
  geom_point(alpha = 0.5) + 
  geom_smooth(method = "loess")

p2 <- ggplot(data = traindata, aes(x = logit, y = AGE)) +
  geom_point(alpha = 0.5) + 
  geom_smooth(method = "loess")

p3 <- ggplot(data = traindata, aes(x = logit, y = avg_util)) +
  geom_point(alpha = 0.5) + 
  geom_smooth(method = "loess")

p4 <- ggplot(data = traindata, aes(x = logit, y = PAYAMT_SEP)) +
  geom_point(alpha = 0.5) + 
  geom_smooth(method = "loess")

p5 <- ggplot(data = traindata, aes(x = logit, y = PAYAMT_AUG)) +
  geom_point(alpha = 0.5) + 
  geom_smooth(method = "loess")

p6 <- ggplot(data = traindata, aes(x = logit, y = PAYAMT_JUL)) +
  geom_point(alpha = 0.5) + 
  geom_smooth(method = "loess")

gridExtra::grid.arrange(p1, p2, p3, p4, p5, p6, ncol = 2, nrow = 3)

## ----null model---------------------------------------------------------------
# Create a null model that predicts using grand mean
null.model <- glm(default == "1" ~ 1,
                  data = traindata,
                  family = binomial(link = "logit"))

stargazer(null.model,
          type = "text",
          title = "Regression Output of Null Model")

## ----bwd model----------------------------------------------------------------
bwd.model <- step(full.model,
                  scope = list(lower = null.model, upper = full.model),
                  direction = "backward", 
                  k = 2,
                  trace = T)
# Indicate scope argument to define range of models to examine
# Indicate direction = "backward" for backward elimination, "forward" for forward selection
# and "both" for stepwise regression
# Indicate k = 2 for AIC, k = log(n) for BIC
# Indicate trace = F if want to suppress the running of the step by step regression

stargazer(bwd.model,
          type = "text",
          title = "Regression Output of Backward Elimination Model")

## ----model.matrix-------------------------------------------------------------
x_var <- model.matrix(default ~ ., traindata)[,-1] # Remove intercept column

colnames(x_var) # Does not include the y variable

y_var <- ifelse(traindata$default == "1", 1, 0)

## ----lambda-------------------------------------------------------------------
set.seed(2)
lasso.lambda <- glmnet::cv.glmnet(x = x_var, 
                                  y = y_var, 
                                  alpha = 1,
                                  family = "binomial",
                                  type.measure = "mse")

# alpha = 1 for lasso, family = "binomial" for logistic regression
# type.measure = "mse" to find lambda that optimize mean squared error, 
# "class" can also be used instead to optimize mis-classification error

## ----optimal lambda-----------------------------------------------------------
# optimal value of lambda
opt.lambda <- lasso.lambda$lambda.1se

opt.lambda

## ----lasso model--------------------------------------------------------------
lasso.model <- glmnet(x_var,
                      y_var,
                      alpha = 1,
                      family = "binomial",
                      lambda = opt.lambda)

coef(lasso.model) # intercept and coefficients

## ----prob using full model----------------------------------------------------
# Returns the probability of default
isres_fullmodel <- predict(full.model, traindata, type = "response")

## ----ROC and plot-------------------------------------------------------------
# First argument: predicted probability, Second argument: observed response
ROCRpred <- ROCR::prediction(predictions = isres_fullmodel, 
                             labels = traindata$default,
                             label.ordering = c("0", "1"))

# Performance based on true positive rate and false positive rate
ROCperf <- ROCR::performance(prediction.obj = ROCRpred,
                             measure = "tpr",
                             x.measure = "fpr")

# Plot ROC and indicate threshold values, with a vertical line at 0.2
plot(ROCperf, 
     colorize = T, 
     print.cutoffs.at = seq(0, by = 0.05),
     text.adj = c(-0.2,1))

abline(v = 0.2)

## ----prec_recall plot---------------------------------------------------------
# Performance based on precision and recall
prec_rec <- ROCR::performance(prediction.obj = ROCRpred, 
                              measure = "prec", 
                              x.measure = "rec")

# Plot Precision-Recall and indicate threshold values, with a vertical line at 0.6
plot(prec_rec, 
     colorize = T, 
     print.cutoffs.at = seq(0, by = 0.05),
     text.adj = c(-0.2,-0.5))

abline(v = 0.6)

## ----conMat fullmodel is------------------------------------------------------
# Use 0.2 as threshold
ispred.fullmodel <- as.factor(ifelse(isres_fullmodel > 0.2, "1", "0"))

caret::confusionMatrix(data = ispred.fullmodel, 
                       reference = traindata$default, 
                       dnn = c("Predicted", "Actual"),
                       positive = "1",
                       mode = "everything")

## ----ROC stepmodel is---------------------------------------------------------
isres_stepmodel <- predict(bwd.model, traindata, type = "response")

# First argument: predicted probability, Second argument: observed response
ROCRpred <- ROCR::prediction(predictions = isres_stepmodel, 
                             labels = traindata$default, 
                             label.ordering = c("0", "1"))

# Performance based on true positive rate and false positive rate
ROCperf <- ROCR::performance(prediction.obj = ROCRpred,
                             measure = "tpr",
                             x.measure = "fpr")

# Plot ROC and indicate threshold values, with a vertical line at 0.2
plot(ROCperf, 
     colorize = T, 
     print.cutoffs.at = seq(0, by = 0.05),
     text.adj = c(-0.2,1))

abline(v = 0.2)

## ----prec_rec stepmodel is----------------------------------------------------
# Performance based on precision and recall
prec_rec <- ROCR::performance(prediction.obj = ROCRpred, 
                              measure = "prec", 
                              x.measure = "rec")

# Plot Precision-Recall and indicate threshold values, with a vertical line at 0.6
plot(prec_rec, 
     colorize = T, 
     print.cutoffs.at = seq(0, by = 0.05),
     text.adj = c(-0.2,-0.5))

abline(v = 0.6)

## ----conMat stepmodel is------------------------------------------------------
# Use 0.2 as threshold
ispred.stepmodel <- as.factor(ifelse(isres_stepmodel > 0.2, "1", "0"))

caret::confusionMatrix(data = ispred.stepmodel, 
                       reference = traindata$default, 
                       dnn = c("Predicted", "Actual"),
                       positive = "1",
                       mode = "everything")

## ----ROC lasso is-------------------------------------------------------------
isres_lassomodel <- predict(lasso.model, x_var, type = "response")

ROCRpred <- ROCR::prediction(predictions = isres_lassomodel, 
                             labels = factor(y_var), 
                             label.ordering = c("0", "1"))

# Performance based on true positive rate and false positive rate
ROCperf <- ROCR::performance(prediction.obj = ROCRpred,
                             measure = "tpr",
                             x.measure = "fpr")

# Plot ROC and indicate threshold values, with a vertical line at 0.2
plot(ROCperf, 
     colorize = T, 
     print.cutoffs.at = seq(0, by = 0.05),
     text.adj = c(-0.2,1))

abline(v = 0.2)

## ----prec_rec lasso is--------------------------------------------------------
# Performance based on precision and recall
prec_rec <- ROCR::performance(prediction.obj = ROCRpred, 
                              measure = "prec", 
                              x.measure = "rec")

# Plot Precision-Recall and indicate threshold values, with a vertical line at 0.6
plot(prec_rec, 
     colorize = T, 
     print.cutoffs.at = seq(0, by = 0.05),
     text.adj = c(-0.2,-0.5))

abline(v = 0.6)

## ----conMat lasso is----------------------------------------------------------
## Use 0.2 as threshold
ispred.lasso <- as.factor(ifelse(isres_lassomodel > 0.2, "1", "0"))

caret::confusionMatrix(data = ispred.lasso, 
                       reference = factor(y_var),
                       dnn = c("Predicted", "Actual"),
                       positive = "1",
                       mode = "everything")

## ----conMat fullmodel oos-----------------------------------------------------
# Change to testdata instead for out-of-sample evaluation
osres_fullmodel <- predict(full.model, testdata, type = "response")

# Use the same threshold as in-sample evaluation
ospred.fullmodel <- as.factor(ifelse(osres_fullmodel > 0.2, "1", "0"))

caret::confusionMatrix(data = ospred.fullmodel, 
                       reference = testdata$default,
                       dnn = c("Predicted", "Actual"),
                       positive = "1",
                       mode = "everything")

## ----conMat stepmodel oos-----------------------------------------------------
osres_stepmodel <- predict(bwd.model, testdata, type = "response")

ospred.stepmodel <- as.factor(ifelse(osres_stepmodel > 0.2, "1", "0"))

caret::confusionMatrix(data = ospred.stepmodel, 
                       reference = testdata$default,
                       dnn = c("Predicted", "Actual"),
                       positive = "1",
                       mode = "everything")

## ----convert testdata to matrix-----------------------------------------------
xtest <- model.matrix(default ~ ., testdata)[,-1] # Remove intercept column
ytest <- ifelse(testdata$default == "1", 1, 0)

## ----conMat lasso oos---------------------------------------------------------
osres_lassomodel <- predict(lasso.model, xtest, type = "response")

ospred.lasso <- as.factor(ifelse(osres_lassomodel > 0.2, "1", "0"))

caret::confusionMatrix(data = ospred.lasso, 
                       reference = factor(ytest),
                       dnn = c("Predicted", "Actual"),
                       positive = "1",
                       mode = "everything")

## ----undersample--------------------------------------------------------------
# Set final sample size N to be twice that of the minority class, meaning it will have
# equal numbers of default and non-default observations
set.seed(123)

undersample <- ROSE::ovun.sample(formula = default ~ ., 
                                 data = traindata, 
                                 method = "under",
                                 N = nrow(traindata[traindata$default == "1",])*2)

# Check number of default and non-default observations
summary(undersample$data)

## ----undersample model--------------------------------------------------------
un_samp.model <- step(glm(formula = default == "1" ~ .,
                          data = undersample$data, 
                          family = binomial(link = "logit")),
                      direction = "backward", 
                      k = 2,
                      trace = F)

## ----oversample---------------------------------------------------------------
# Set final sample size N to be twice that of the majority class, meaning it will have
# equal numbers of default and non-default observations
set.seed(124)

oversample <- ROSE::ovun.sample(formula = default ~ ., 
                                data = traindata, 
                                method = "over",
                                N = nrow(traindata[traindata$default == "0",])*2)

# Check number of default and non-default observations
summary(oversample$data)

## ----oversample model---------------------------------------------------------
ov_samp.model <- step(glm(formula = default == "1" ~ .,
                          data = oversample$data, 
                          family = binomial(link = "logit")),
                      direction = "backward", 
                      k = 2,
                      trace = F)

## ----ROC undersample is-------------------------------------------------------
isres_unsamp <- predict(un_samp.model, undersample$data, type = "response")

# First argument: predicted probability, Second argument: observed response
ROCRpred <- ROCR::prediction(predictions = isres_unsamp, 
                             labels = undersample$data$default, 
                             label.ordering = c("0", "1"))

# Performance based on true positive rate and false positive rate
ROCperf <- ROCR::performance(prediction.obj = ROCRpred,
                             measure = "tpr",
                             x.measure = "fpr")

# Plot ROC and indicate threshold values, with a vertical line at 0.2
plot(ROCperf, 
     colorize = T, 
     print.cutoffs.at = seq(0, by = 0.05),
     text.adj = c(-0.2,1))

abline(v = 0.2)

## ----prec_rec undersample is--------------------------------------------------
# Performance based on precision and recall
prec_rec <- ROCR::performance(prediction.obj = ROCRpred, 
                              measure = "prec", 
                              x.measure = "rec")

# Plot Precision-Recall and indicate threshold values, with a vertical line at 0.6
plot(prec_rec, 
     colorize = T, 
     print.cutoffs.at = seq(0, by = 0.05),
     text.adj = c(-0.2,-0.5))

abline(v = 0.6)

## ----conMat undersample is----------------------------------------------------
# Confusion Matrix of In-Sample Evaluation
# Use 0.45 as threshold
ispred.unsamp <- as.factor(ifelse(isres_unsamp > 0.45, "1", "0"))

caret::confusionMatrix(data = ispred.unsamp, 
                       reference = undersample$data$default, 
                       dnn = c("Predicted", "Actual"),
                       positive = "1",
                       mode = "everything")

## ----conMat undersample oos---------------------------------------------------
# Confusion Matrix for Out-of-Sample Evaluation
osres_unsamp <- predict(un_samp.model, testdata, type = "response")

ospred.unsamp <- as.factor(ifelse(osres_unsamp > 0.45, "1", "0"))

caret::confusionMatrix(data = ospred.unsamp, 
                       reference = testdata$default,
                       dnn = c("Predicted", "Actual"),
                       positive = "1",
                       mode = "everything")

## ----ROC oversample is--------------------------------------------------------
isres_ovsamp <- predict(ov_samp.model, oversample$data, type = "response")

# First argument: predicted probability, Second argument: observed response
ROCRpred <- ROCR::prediction(predictions = isres_ovsamp, 
                             labels = oversample$data$default, 
                             label.ordering = c("0", "1"))

# Performance based on true positive rate and false positive rate
ROCperf <- ROCR::performance(prediction.obj = ROCRpred,
                             measure = "tpr",
                             x.measure = "fpr")

# Plot ROC and indicate threshold values, with a vertical line at 0.2
plot(ROCperf, 
     colorize = T, 
     print.cutoffs.at = seq(0, by = 0.05),
     text.adj = c(-0.2,1))

abline(v = 0.2)

## ----prec_rec oversample is---------------------------------------------------
# Performance based on precision and recall
prec_rec <- ROCR::performance(prediction.obj = ROCRpred, 
                              measure = "prec", 
                              x.measure = "rec")

# Plot Precision-Recall and indicate threshold values, with a vertical line at 0.6
plot(prec_rec, 
     colorize = T, 
     print.cutoffs.at = seq(0, by = 0.05),
     text.adj = c(-0.2,-0.5))

abline(v = 0.6)

## ----conMat oversample is-----------------------------------------------------
# Confusion Matrix for In-Sample Evaluation
# Use 0.45 as threshold
ispred.ovsamp <- as.factor(ifelse(isres_ovsamp > 0.45, "1", "0"))

caret::confusionMatrix(data = ispred.ovsamp, 
                       reference = oversample$data$default, 
                       dnn = c("Predicted", "Actual"),
                       positive = "1",
                       mode = "everything")

## ----conMat oversample oos----------------------------------------------------
# Confusion Matrix for Out-of-Sample Evaluation
osres_ovsamp <- predict(ov_samp.model, testdata, type = "response")

ospred.ovsamp <- as.factor(ifelse(osres_ovsamp > 0.45, "1", "0"))

caret::confusionMatrix(data = ospred.ovsamp, 
                       reference = testdata$default,
                       dnn = c("Predicted", "Actual"),
                       positive = "1",
                       mode = "everything")


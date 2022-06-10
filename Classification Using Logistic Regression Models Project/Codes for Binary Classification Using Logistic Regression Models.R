
# Load packages ----
library(car) 
library(caret) # For building ML models
library(corrplot) # For correlation plot
library(tidyverse) # For ggplot and dplyr packages
library(mlbench) # For data sets
library(lmtest) # For statistical tests
library(glmnet) # For LASSO logistic regression
library(ROCR) # Calculate threshold using ROC

# Exploratory Data Analysis ----
## Load data and summarize
data("PimaIndiansDiabetes")
str(PimaIndiansDiabetes)
summary(PimaIndiansDiabetes)
head(PimaIndiansDiabetes, n = 4)
tail(PimaIndiansDiabetes, n = 4)

## Filtering of data
data <- PimaIndiansDiabetes %>%
  dplyr::filter(glucose != 0 & pressure != 0 & triceps != 0 & mass != 0)

summary(data)

## Split input and output variables
response <- subset(data, select = c(diabetes))
explvar <- subset(data, select = -c(diabetes))
summary(response)
summary(explvar)

## Distribution of diabetes variable
ggplot(data = response) + 
  geom_bar(aes(x = diabetes, fill = diabetes)) + 
  ggtitle("Distribution of Outcomes from Diabetes Test") +
  xlab("Diabetes") + ylab("Frequency") +
  theme(plot.title = element_text(hjust = 0.5))

## Distribution of explanatory variables
par(mfrow = c(1, 4), mar = c(1, 3, 2, 1))
for (i in 1:4) {
  boxplot(explvar[,i], main = names(explvar)[i])
}
for (i in 5:8) {
  boxplot(explvar[,i], main = names(explvar)[i])
}

par(mfrow = c(1, 4), mar = c(2, 3, 2, 1))
for (i in 1:4) {
  hist(explvar[,i], main = names(explvar)[i])
}
for (i in 5:8) {
  hist(explvar[,i], main = names(explvar)[i])
}

## Test if explanatory variables are normally distributed
store_test <- list()

for (i in 1:8) {
  store_test <- cbind(store_test, 
                      shapiro.test(x = explvar[,i]))
}

store_test["data.name",] <- names(explvar)

store_test

store_test["p.value",] > 0.05

## Correlation matrix heatmap
corrX <- cor(x = explvar, method = "spearman")

corrplot(corr = corrX, 
         method = "color", 
         addCoef.col = "black",
         title = "Correlation Between Explanatory Variables",
         mar = c(0, 0, 1, 0))

# Building the logistic regression model ----
## Full model
model1 <- glm(diabetes == "pos" ~ ., 
              data = data, 
              family = binomial(link = "logit"))

summary(model1)

## Restricted Model
model2 <- glm(diabetes == "pos" ~ pregnant + glucose + mass + pedigree,
              data = data,
              family = binomial(link = "logit"))

summary(model2)

## Likelihood Ratio test
lmtest::lrtest(model2, model1)

# VIF to check for multicollinearity
car::vif(model2)

# Predict probabilities using model2
prob.model2 <- fitted.values(model2)

## Calculate logit and bind into a new data frame with variables for plotting
pred <- names(coef(model2)[-1])

plotdata <- subset(data, select = pred)

plotdata <- plotdata %>%
  mutate(logit = log(prob.model2/(1-prob.model2))) %>%
  gather(key = "pred", value = "pred.value", -logit)

ggplot(data = plotdata, aes(x = logit, y = pred.value)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "loess") +
  facet_wrap(. ~ pred, scales = "free")

## Transforming pedigree to make it linear to the log-odds
model3 <- glm(diabetes == "pos" ~ pregnant + glucose + mass + log(pedigree),
              data = data,
              family = binomial(link = "logit"))

summary(model3)

prob.model3 <- fitted.values(model3)
logit = log(prob.model3/(1-prob.model3))

plotdata2 <- subset(data, select = pred)

plotdata2 <- plotdata2 %>%
  mutate(l.pedigree = log(pedigree))

ggplot(data = plotdata2, aes(x = logit, y = l.pedigree)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "loess")

# Checking for Influential Outliers ----

## Find outliers using standardized residual errors
std.resid <- rstandard(model3)
val <- abs(std.resid) > 3
table(val)["TRUE"]

outlierTest(model3)

## Find influential values in model 3 by visualizing Cook's Distance
plot(model3, which = 4)
sort(cooks.distance(model3), decreasing = T)

# Evaluating the Models -----

## Storing fitted values
prob.model1 <- fitted.values(model1) 

## Confusion Matrix
prediction.model1 <- as.factor(ifelse(prob.model1 > 0.5, "pos", "neg"))
confusionMatrix(data$diabetes,
                prediction.model1,
                dnn = c("Actual", "Predicted"))

prediction.model2 <- as.factor(ifelse(prob.model2 > 0.5, "pos", "neg"))
confusionMatrix(data$diabetes,
                prediction.model2,
                dnn = c("Actual", "Predicted"))

prediction.model3 <- as.factor(ifelse(prob.model3 > 0.5, "pos", "neg"))
confusionMatrix(data$diabetes, 
                prediction.model3,
                dnn = c("Actual", "Predicted"))

# Using other sources of data ----

## Load package
library(readxl)

## Load data from wd
default.data <- data.frame(read_excel("credit card default.xls"))

## Head and tail of data
head(default.data)

tail(default.data)

colnames(default.data)

## Adjust column names
colnames(default.data) <- default.data[1,]
default.data <- default.data[-1,]
head(default.data)

## Adjust row names
default.data <- default.data[,-1]

rownames(default.data) <- NULL

head(default.data)

### Renaming columns
newnames <- c("PAY_SEP","PAY_AUG","PAY_JUL", "PAY_JUN", "PAY_MAY", "PAY_APR",
              "BILLAMT_SEP","BILLAMT_AUG","BILLAMT_JUL", "BILLAMT_JUN", "BILLAMT_MAY", "BILLAMT_APR",
              "PAYAMT_SEP","PAYAMT_AUG","PAYAMT_JUL", "PAYAMT_JUN", "PAYAMT_MAY", "PAYAMT_APR")

oldnames <-colnames(default.data %>%
                      dplyr::select(grep("PAY" , names(default.data)), 
                                    grep("BILL", names(default.data)))) 

colnames(default.data)[colnames(default.data) %in% oldnames] <- newnames

default.data <- default.data %>%
  rename(default = `default payment next month`)

colnames(default.data)

# EDA of Credit Card Default Data ----

str(default.data)

## Changing to appropriate value type
default.data <- default.data %>% 
  mutate(
    across(
      c(grep("PAYAMT", names(default.data)), 
        grep("BILLAMT", names(default.data)), 
        "AGE", "LIMIT_BAL"), 
      as.numeric))

str(default.data)

default.data <- default.data %>%
  mutate(
    across(
      c("SEX", "EDUCATION", "MARRIAGE", "default", 
             grep("PAY_", names(default.data))), 
      as.factor))

str(default.data)

## View summary of data
summary(default.data)

### Replace unknown class with Others
# For EDUCATION
levels(default.data$EDUCATION)[levels(default.data$EDUCATION) %in% c("0", "5", "6")] <- "4"
# For MARRIAGE
levels(default.data$MARRIAGE)[levels(default.data$MARRIAGE) == "0"] <- "3"

## Univariate Plots ====
library(gridExtra)

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

# For continuous variables
billamt <- default.data %>%
  select(grep("BILLAMT", names(default.data)))

payamt <- default.data %>%
  select(grep("PAYAMT", names(default.data)))

par(mfrow = c(1, 6), mar = c(1, 3, 3, 2))
for (i in 1:6) {
  boxplot(billamt[,i], main = names(billamt)[i])
}

for (i in 1:6) {
  boxplot(payamt[,i], main = names(payamt)[i])
}

## Multivariate Plots ====
correl <- default.data %>% 
  select(where(is.numeric)) %>%
  cor(method = "spearman")

corrplot(corr = correl,
         method = "color",
         type = "lower")

## Creating new variables ====
# Variable that is the median bill amount from Apr to Sep divided by limit balance
avg_util <- round((rowMeans(billamt) / default.data$LIMIT_BAL)*100, 4)

# Variable that shows the mean payment value over the period Apr to Sep
avg_pmt <- round(rowMeans(payamt), 4)

# First let us bind the data that we want into a new data frame
new_data <- data.frame(default.data[,1:11],
                       avg_util,
                       avg_pmt,
                       default = default.data$default)
head(new_data, n = 4)

# Convert the pay history status variable
cont_tab <- list(sep = table(new_data$default, new_data$PAY_SEP),
                 aug = table(new_data$default, new_data$PAY_AUG),
                 jul = table(new_data$default, new_data$PAY_JUL),
                 jun = table(new_data$default, new_data$PAY_JUN),
                 may = table(new_data$default, new_data$PAY_MAY),
                 apr = table(new_data$default, new_data$PAY_APR))

cont_tab

# I would simplify the number of dummies into 2 groups
# 0 - those who paid duly/early or delayed payment for one month
# 1 - those who delayed payment for two month or more

# Change the categorical classes
for (i in 6:11) {
  levels(new_data[,i])[levels(new_data[,i]) %in% c("-2", "-1", "0", "1")] <- "0"
  
  levels(new_data[,i])[!levels(new_data[,i]) %in% c("-2", "-1", "0", "1")] <- "1"
}

summary(new_data)
str(new_data)

# Check for incomplete observations
nrow(new_data[!complete.cases(new_data),])

# Test to see if numerical variables still have high correlation
correl <- new_data %>% 
  select(where(is.numeric)) %>%
  cor(method = "spearman")

corrplot(corr = correl,
         method = "color",
         type = "lower",
         addCoef.col = "black")

# Modeling ----

# Split data into 80% training and 20% testing sets
set.seed(1)
validation <- createDataPartition(new_data$default, p = 0.80, list = F)

traindata <- new_data[validation,]

testdata <- new_data[-validation,]

## Full Model ====
# Add all explanatory variables
full.model <- glm(default == "1" ~ .,
                  data = traindata,
                  family = binomial(link = "logit"))
summary(full.model)

# VIF to test for multicollinearity
car::vif(full.model)

## Restricted Model ====
# Remove those with high p-values in full model
# Remove marriage variables
rest.model <- glm(default == "1" ~ .-MARRIAGE,
                   data = traindata,
                   family = binomial(link = "logit"))
summary(rest.model)

### Likelihood Ratio Test ====
lmtest::lrtest(full.model, rest.model) # Use full model

### Checking Influential Outliers of Full Model ====
# Influence using Cook's Distance
plot(full.model, which = 4)
head(sort(cooks.distance(full.model), decreasing = T))

# Outliers with standardized/studentized residuals
std.resid <- rstandard(full.model)
val <- abs(std.resid) > 3
table(val)
val[val == "TRUE"]

car::outlierTest(full.model)
# There seems to be 1 influential outlier as per the standardized residuals.

### Checking Linearity Assumption of Full Model ====
# Find fitted values of full model
prob.fullmod <- fitted.values(full.model)

# Calculate logit and bind into data
logit = log(prob.fullmod/(1-prob.fullmod))

p1 <- ggplot(data = traindata, aes(x = logit, y = LIMIT_BAL)) +
  geom_point(alpha = 0.5) + 
  geom_smooth(method = "loess")

p2 <- ggplot(data = traindata, aes(x = logit, y = AGE)) +
  geom_point(alpha = 0.5) + 
  geom_smooth(method = "loess")

p3 <- ggplot(data = traindata, aes(x = logit, y = avg_util)) +
  geom_point(alpha = 0.5) + 
  geom_smooth(method = "loess")

p4 <- ggplot(data = traindata, aes(x = logit, y = avg_pmt)) +
  geom_point(alpha = 0.5) + 
  geom_smooth(method = "loess")

gridExtra::grid.arrange(p1, p2, p3, p4, ncol = 2, nrow = 2)
# Has rather non-linear relationship with LIMIT_BAL

### In-sample Evaluation of Full Model ====
# Determine threshold value
res_fullmodel <- predict(full.model, traindata, type = "response")

# First argument: predicted probability, Second argument: observed response
ROCpred <- ROCR::prediction(res_fullmodel, traindata$default)

# Performance based on true positive rate and false positive rate
ROCperf <- ROCR::performance(ROCpred, "tpr", "fpr")

# Plot ROC and indicate threshold values
plot(ROCperf, 
     colorize = T, 
     print.cutoffs.at = seq(0, by = 0.01),
     text.adj = c(-0.2,1))

# Use 0.17 as threshold
pred.fullmodel <- as.factor(ifelse(res_fullmodel > 0.17, "1", "0"))

caret::confusionMatrix(traindata$default, 
                       pred.fullmodel, 
                       dnn = c("Actual", "Predicted"))

### Out-of-Sample of Full Model ====
# Change to testdata instead for out-of-sample evaluation
res_fullmodel <- predict(full.model, testdata, type = "response")

# Use the same threshold as in-sample evaluation
pred.fullmodel <- as.factor(ifelse(res_fullmodel > 0.17, "1", "0"))

caret::confusionMatrix(testdata$default,
                       pred.fullmodel,
                       dnn = c("Actual", "Predicted"))

## Stepwise Model Using Backward Elimination ====
# Create a null model that predicts using grand mean
null.model <- glm(default == "1" ~ 1,
                  data = traindata,
                  family = binomial(link = "logit"))
summary(null.model)

bwd.model <- step(full.model,
                  scope = list(lower = null.model, upper = full.model),
                  direction = "backward", 
                  k = 2,
                  trace = T)
# Indicate scope argument to define range of models to examine
# Indicate direction = "backward" for backward elimination
# Indicate k = 2 for AIC, k = log(n) for BIC
# Indicate trace = F if want to suppress the running of the step by step regression

summary(bwd.model)

### In-Sample of Stepwise Model ====
# Determine threshold value
res_stepmodel <- predict(bwd.model, traindata, type = "response")

ROCpred <- ROCR::prediction(res_stepmodel, traindata$default)

ROCperf <- ROCR::performance(ROCpred, "tpr", "fpr")

plot(ROCperf, 
     colorize = T, 
     print.cutoffs.at = seq(0, by = 0.02),
     text.adj = c(-0.2,1))

# Use 0.17 as threshold
pred.stepmodel <- as.factor(ifelse(res_stepmodel > 0.17, "1", "0"))

caret::confusionMatrix(traindata$default,
                       pred.stepmodel,
                       dnn = c("Actual", "Predicted"))

### Out-of-Sample of Stepwise Model ====
res_stepmodel <- predict(bwd.model, testdata, type = "response")

pred.stepmodel <- as.factor(ifelse(res_stepmodel > 0.17, "1", "0"))

caret::confusionMatrix(testdata$default,
                       pred.stepmodel,
                       dnn = c("Actual", "Predicted"))

## LASSO Logistic Regression ----

### Convert data frame to matrix ====
x_var <- model.matrix(default ~ ., traindata)[,-1] # Remove intercept column
y_var <- ifelse(traindata$default == "1", 1, 0)

### Finding lambda value ====
lasso.lambda <- glmnet::cv.glmnet(x_var, 
                                  y_var, 
                                  alpha = 1,
                                  family = "binomial",
                                  type.measure = "mse")
# alpha = 1 for lasso, family = "binomial" for logistic regression

# optimal value of lambda
opt.lambda <- lasso.lambda$lambda.1se

### Fitting the LASSO logistic model ====
lasso.model <- glmnet(x_var,
                      y_var,
                      alpha = 1,
                      family = "binomial",
                      lambda = opt.lambda)

lasso.model$a0; lasso.model$beta # intercept and coefficients

### In-Sample of LASSO Logistic Regression ====
res_lassomodel <- predict(lasso.model, x_var, type = "response")

ROCpred <- ROCR::prediction(res_lassomodel, factor(y_var))

ROCperf <- ROCR::performance(ROCpred, "tpr", "fpr")

plot(ROCperf, 
     colorize = T, 
     print.cutoffs.at = seq(0, by = 0.01),
     text.adj = c(-0.2,1))

## Use 0.17 as threshold
pred.lasso <- as.factor(ifelse(res_lassomodel > 0.17, "1", "0"))

caret::confusionMatrix(factor(y_var),
                       pred.lasso,
                       dnn = c("Actual", "Predicted"))

### Out-of-Sample LASSO Logistic Regression ====
xtest <- model.matrix(default ~ ., testdata)[,-1] # Remove intercept column
ytest <- ifelse(testdata$default == "1", 1, 0)

res_lassomodel <- predict(lasso.model, xtest, type = "response")

pred.lasso <- as.factor(ifelse(res_lassomodel > 0.17, "1", "0"))

caret::confusionMatrix(factor(ytest),
                       pred.lasso,
                       dnn = c("Actual", "Predicted"))










# Load packages ----
library(car) 
library(caret) # For building ML models
library(corrplot) # For correlation plot
library(tidyverse) # For ggplot and dplyr packages
library(mlbench) # For data sets
library(lmtest) # For statistical tests
library(glmnet) # For LASSO logistic regression

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
  store_test <- cbind(store_test, shapiro.test(x = explvar[,i]))
}

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
prediction.model1 <- as.factor(ifelse(prob.model1 > 0.5, "pos", "neg"))
confusionMatrix(data$diabetes, prediction.model1)

prediction.model2 <- as.factor(ifelse(prob.model2 > 0.5, "pos", "neg"))
confusionMatrix(data$diabetes, prediction.model2)

prediction.model3 <- as.factor(ifelse(prob.model3 > 0.5, "pos", "neg"))
confusionMatrix(data$diabetes, prediction.model3)

# Using other sources of data ----

## Load package
library(readxl)

## Load data from wd
default.data <- read_excel("credit card default.xls")

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
head(default.data)

temp.df <- default.data

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
temp.df <- default.data # Save to backup

# To make it simple we can change all to numeric first, then to factors
default.data <- default.data %>% 
  mutate(across(where(is.character), as.numeric))

str(default.data)

default.data <- default.data %>%
  mutate(
    across(
      c("SEX", "EDUCATION", "MARRIAGE", "default", 
             grep("PAY_", names(default.data))), 
      as.factor))

str(default.data)

temp.df <- default.data # Save progress of dataset

## View summary of data
summary(default.data)

### Replace unknown class with Others





## Univariate Plots
ggplot(data = default.data) +
  geom_bar(aes(x = SEX, fill = default))

# 
ggplot(data = default.data) +
  geom_bar(aes(x = EDUCATION, fill = default))








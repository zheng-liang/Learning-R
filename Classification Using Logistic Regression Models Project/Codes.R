
# Load packages
library(car) 
library(caret) # For building ML models
library(corrplot) # For correlation plot
library(tidyverse) # For ggplot and dplyr packages
library(mlbench) # For data sets

# Exploratory Data Analysis
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

# Building the logistic regression model
## Full model
model1 <- glm(diabetes ~ ., 
              data = data, 
              family = binomial(link = "logit"))

summary(model1)

## Restricted Model
model2 <- glm(diabetes ~ pregnant + glucose + mass + pedigree,
              data = data,
              family = binomial(link = "logit"))

summary(model2)

# Likelihood Ratio test
lmtest::lrtest(model2, model1)

# VIF to check for multicollinearity
car::vif(model2)

# Predict probabilities using model2
prob <- fitted.values(model2)

# Calculate logit and bind into a new data frame with variables for plotting
pred <- names(coef(model2)[-1])

plotdata <- subset(data, select = pred)

plotdata <- plotdata %>%
  mutate(logit = log(prob/(1-prob))) %>%
  gather(key = "pred", value = "pred.value", -logit)

ggplot(data = plotdata, aes(x = logit, y = pred.value)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "loess") +
  facet_wrap(. ~ pred, scales = "free")

# Transforming pedigree to make it linear to the log-odds
model3 <- glm(diabetes ~ pregnant + glucose + mass + log(pedigree),
              data = data,
              family = binomial(link = "logit"))

summary(model3)

prob2 <- fitted.values(model3)
logit = log(prob2/(1-prob2))

plotdata2 <- subset(data, select = pred)

plotdata2 <- plotdata2 %>%
  mutate(l.pedigree = log(pedigree), pedigree = NULL)

ggplot(data = plotdata2, aes(x = logit, y = l.pedigree)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "loess")

# Find influential values in model 3 by visualizing Cook's Distance
plot(model3, which = 4)

# Find outliers using standardized residual errors
model3data <- augment(model3) %>%
  mutate(index = 1:n())













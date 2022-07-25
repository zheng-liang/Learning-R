## ----setup, include=FALSE-----------------------------------------------------
knitr::knit_hooks$set(purl = knitr::hook_purl)

## ----load packages, message=FALSE, class.source = "fold-show"-----------------
# Use install.packages("packagename") if the packages are not installed in R
library(car) 
library(caret) # For building ML models
library(corrplot) # For correlation plot
library(smotefamily) # For SMOTE when there are imbalanced datasets
library(lmtest) # For statistical tests
library(mlbench) # For data sets
library(ROCR) # For determining cutoff-probability using ROC
library(ROSE) # For re-sampling techniques when there are imbalanced datasets
library(stargazer) # For neat tables and outputs, where possibles
library(tidyverse) # For ggplot and dplyr packages

## ----load PimaIndiansDiabetes dataset-----------------------------------------
data("PimaIndiansDiabetes")

## ----structure and summary----------------------------------------------------
str(PimaIndiansDiabetes)

summary(PimaIndiansDiabetes)

## ----head and tail of df------------------------------------------------------
stargazer::stargazer(rbind(head(PimaIndiansDiabetes), tail(PimaIndiansDiabetes)), 
                     type = "text", 
                     title = "First and Last 6 Observations in PimaIndiansDiabetes Dataset",
                     summary = F)

## ----filter out obs-----------------------------------------------------------
data <- PimaIndiansDiabetes %>%
  dplyr::filter(glucose != 0 & pressure != 0 & triceps != 0 & mass != 0)

summary(data)

## ----split response and explvar-----------------------------------------------
response <- subset(data, select = c(diabetes))

explvar <- subset(data, select = -c(diabetes))

summary(response)
summary(explvar)

## ----dist of diabetic test, fig.width=6, fig.height=4.5, fig.align='center'----
ggplot(data = response) + 
  geom_bar(aes(x = diabetes, fill = diabetes)) + 
  ggtitle("Distribution of Outcomes from Diabetes Test") +
  xlab("Diabetes") + ylab("Frequency") +
  theme(plot.title = element_text(hjust = 0.5))

## ----boxplot of dependent variables, fig.width=6, fig.height=4.5, fig.align='center'----
par(mfrow = c(1, 4), mar = c(1, 3, 2, 1))

for (i in 1:4) {
  boxplot(explvar[,i], main = names(explvar)[i])
}

for (i in 5:8) {
  boxplot(explvar[,i], main = names(explvar)[i])
}

## ----histogram of dependent variables, fig.width=6, fig.height=4.5, fig.align='center'----
par(mfrow = c(1, 4), mar = c(2, 3, 2, 1))

for (i in 1:4) {
  hist(explvar[,i], main = names(explvar)[i])
}

for (i in 5:8) {
  hist(explvar[,i], main = names(explvar)[i])
}

## ----shapirotest--------------------------------------------------------------
store_test <- list()

for (i in 1:8) {
  store_test <- cbind(store_test, 
                      shapiro.test(x = explvar[,i]))
}

store_test["data.name",] <- names(explvar)

store_test

store_test["p.value",] > 0.05

## ----correlation matrix heatmap, fig.align='center'---------------------------
corrX <- cor(x = explvar, method = "spearman")

corrplot(corr = corrX, 
         method = "color", 
         addCoef.col = "black",
         title = "Correlation Between Explanatory Variables",
         mar = c(0, 0, 1, 0))

## ----create logit model1------------------------------------------------------
# Full model
model1 <- glm(diabetes == "pos" ~ ., #indicate "pos" so that it is 1 and "neg" is 0
              data = data, 
              family = binomial(link = "logit"))

stargazer::stargazer(model1, 
                     type = "text", 
                     title = "Logit Regression Output for Full Model")

## ----create logit model2------------------------------------------------------
# Restricted model
model2 <- glm(diabetes == "pos" ~ pregnant + glucose + mass + pedigree, 
              data = data,
              family = binomial(link = "logit"))

stargazer::stargazer(model2, 
                     type = "text", 
                     title = "Logit Regression Output for Restricted Model")

## ----lrtest1------------------------------------------------------------------
lmtest::lrtest(model2, model1)

## ----vif1---------------------------------------------------------------------
car::vif(model2)

## ----fitted values from model2------------------------------------------------
prob.model2 <- fitted.values(model2)

head(prob.model2)

## ----calculate logit and bind into data---------------------------------------
pred <- names(coef(model2)[-1]) # Exclude intercept

plotdata <- subset(data, select = pred) # Only keep the variables used in the model

plotdata <- plotdata %>%
  mutate(logit = log(prob.model2/(1-prob.model2))) %>% # Bind logit to plotdata
  gather(key = "pred", value = "pred.value", -logit) 

## ----plot variables against logit, message=FALSE, fig.align='center'----------
ggplot(data = plotdata, aes(x = logit, y = pred.value)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "loess") +
  facet_wrap(. ~ pred, scales = "free")

## ----create logit model3------------------------------------------------------
model3 <- glm(diabetes == "pos" ~ pregnant + glucose + mass + log(pedigree),
              data = data,
              family = binomial(link = "logit"))

stargazer::stargazer(model3, 
                     type = "text", 
                     title = "Logit Regression Output for Model With Transformed Variable")

## ----store fitted values from model3 and calculate logit----------------------
prob.model3 <- fitted.values(model3)

logit = log(prob.model3/(1-prob.model3))

## ----transfrom pedigree with log----------------------------------------------
plotdata2 <- subset(data, select = pred)

plotdata2 <- plotdata2 %>%
  mutate(l.pedigree = log(pedigree))

## ----plot logped against logit, fig.width=6, fig.height=4.5, fig.align='center'----
ggplot(data = plotdata2, aes(x = logit, y = l.pedigree)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "loess")

## ----cooks distance-----------------------------------------------------------
plot(model3, which = 4)

head(sort(cooks.distance(model3), decreasing = T))

## ----std resid----------------------------------------------------------------
std.resid <- rstandard(model3)

val <- abs(std.resid) > 3

table(val)["TRUE"] # Returned NA because there are no TRUE logical values

## ----studentized resid1-------------------------------------------------------
car::outlierTest(model3)

## ----prob model1--------------------------------------------------------------
prob.model1 <- fitted.values(model1) #can also use predict(model1, type="response")

## ----prediction and conMat model1---------------------------------------------
prediction.model1 <- as.factor(ifelse(prob.model1 > 0.5, "pos", "neg"))

confusionMatrix(data = prediction.model1,
                reference = data$diabetes,
                dnn = c("Predicted", "Actual"),
                positive = "pos", mode = "everything")

## ----prediction and conMat model2---------------------------------------------
prediction.model2 <- as.factor(ifelse(prob.model2 > 0.5, "pos", "neg"))

confusionMatrix(data = prediction.model2,
                reference = data$diabetes,
                dnn = c("Predicted", "Actual"),
                positive = "pos", mode = "everything")

## ----prediction and conMat model3---------------------------------------------
prediction.model3 <- as.factor(ifelse(prob.model3 > 0.5, "pos", "neg"))

confusionMatrix(data = prediction.model3,
                reference = data$diabetes,
                dnn = c("Predicted", "Actual"),
                positive = "pos", mode = "everything")

## ----ROC and plot-------------------------------------------------------------
# First argument is the probabilities extracted using fitted() or predict()
# Second argument is the classification labels with same dimensions as first argument
# Third argument is the order, with first label being negative, second being positive class
ROCRpred <- ROCR::prediction(predictions = prob.model2, labels = data$diabetes, label.ordering = c("neg" , "pos"))

# Performance based on true positive rate and false positive rate (ROC curve)
ROCperf <- ROCR::performance(prediction.obj = ROCRpred, measure = "tpr", x.measure = "fpr")

# Plot ROC Curve and indicate threshold value
plot(ROCperf, 
     colorize = T, 
     print.cutoffs.at = seq(0, by = 0.05), 
     text.adj = c(-0.2, 1),
     main = "ROC Curve for Pima Indians Diabetes Dataset")

## ----prec_rec plot------------------------------------------------------------
# Performance based on precision and recall
Precperf <- ROCR::performance(prediction.obj = ROCRpred, measure = "prec", x.measure = "rec")

# Plot Precision-Recall Curve with threshold values
plot(Precperf, 
     colorize = T, 
     print.cutoffs.at = seq(0, by = 0.05), 
     text.adj = c(-0.2, 1),
     main = "Precision-Recall Curve for Pima Indians Diabetes Dataset")

## ----conMat 0.3---------------------------------------------------------------
new_pred.model2 <- as.factor(ifelse(prob.model2 > 0.35, "pos", "neg"))

confusionMatrix(data = new_pred.model2,
                reference = data$diabetes,
                dnn = c("Predicted", "Actual"),
                positive = "pos", mode = "everything")


## ----undersample data---------------------------------------------------------
# Argument 1 indicates the class vs all other variables in the dataset
# Argument 2 indicates where to find the data
# Argument 3 indicates whether to over- or under-sample
# Argument 4 indicates target number of observations, in this case I put 2 times of the minority class

undersample <- ROSE::ovun.sample(diabetes ~ ., 
                                 data = data, 
                                 method = "under", 
                                 N = 177*2)

summary(undersample)

## ----oversample data----------------------------------------------------------
# For over-sampling, I indicate 2 times of majority class for target number of observations

oversample <- ROSE::ovun.sample(diabetes ~ .,
                                data = data,
                                method = "over",
                                N = 355*2)

summary(oversample)

## ----smote data---------------------------------------------------------------
# X refers to data frame or matrix containing numeric-attributed explanatory variables
# target refers to vector of the class corresponding to X
# K = 5 is the default number of nearest neighbors, SMOTE uses KNN to generate artificial data
# dup_size refers to the multiples of minority observations compared to majority observations
# In this case, 1 means to generate 1 minority for 1 majority observation

smote <- smotefamily::SMOTE(X = explvar,
                            target = response, 
                            K = 5, 
                            dup_size = 1)

# Extract the newly generated data set that contains both the synthetic and original observations
smote_data <- smote$data

summary(smote_data)

# diabetes variable has been replaced by class and is in character class, need to change it to factor
smote_data$class <- factor(smote_data$class)

summary(smote_data)

## ----model4 undersamp restricted----------------------------------------------
model4 <- glm(diabetes == "pos" ~ pregnant + glucose + mass + pedigree, 
              data = undersample$data, 
              family = binomial(link = "logit"))

stargazer(model4, 
          type = "text", 
          title = "Logit Regression Output for Under-Sampling Method")

## ----prob model4 and conMat---------------------------------------------------
prob.model4 <- predict(model4, type = "response")

prediction.model4 <- as.factor(ifelse(prob.model4 > 0.5, "pos", "neg"))

confusionMatrix(data = prediction.model4,
                reference = undersample$data$diabetes,
                dnn = c("Predicted", "Actual"),
                positive = "pos", mode = "everything")

## ----model5 oversamp restricted-----------------------------------------------
model5 <- glm(diabetes == "pos" ~ pregnant + glucose + mass + pedigree,
              data = oversample$data,
              family = binomial(link = "logit"))

stargazer(model5, 
          type = "text", 
          title = "Logit Regression Output for Over-Sampling Method")

## ----prob model5 and conMat---------------------------------------------------
prob.model5 <- predict(model5, type = "response")

prediction.model5 <- as.factor(ifelse(prob.model5 > 0.5, "pos", "neg"))

confusionMatrix(data = prediction.model5,
                reference = oversample$data$diabetes,
                dnn = c("Predicted", "Actual"),
                positive = "pos", mode = "everything")

## ----model6 smote restricted--------------------------------------------------
model6 <- glm(class == "pos" ~ pregnant + glucose + mass + pedigree,
              data = smote_data,
              family = binomial(link = "logit"))

stargazer(model6, 
          type = "text", 
          title = "Logit Regression Output for SMOTE Method")

## ----prob model6 and conMat---------------------------------------------------
prob.model6 <- predict(model6, type = "response")

prediction.model6 <- as.factor(ifelse(prob.model6 > 0.5, "pos", "neg"))

confusionMatrix(data = prediction.model6,
                reference = smote_data$class,
                dnn = c("Predicted", "Actual"),
                positive = "pos", mode = "everything")


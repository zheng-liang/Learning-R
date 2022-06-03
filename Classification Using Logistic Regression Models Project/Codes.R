
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

# Split input and output variables
input <- data["diabetes"]
output <- subset(data, select = -c(diabetes))

# Distribution of diabetes variable
plot(input, main = "Distribution of Diabetic Test",
     ylab = "Frequency", col = c("red", "blue"))
















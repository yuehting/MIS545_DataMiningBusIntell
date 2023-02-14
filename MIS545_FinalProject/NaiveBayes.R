# Peter Yeu-Shyang Yeh
# MIS 545 Section 02
# GroupProject.R

# Preliminary Setup -------------------------------------------------------
# Sets the working directory
setwd("C:/Users/peter/Desktop/Rcodes/UofA/MIS545/GroupProject")

# Installs and loads tidyverse library
# install.packages("tidyverse")
library(tidyverse)

# Installs and loads tidyverse library
# install.packages("lubridate")
library(lubridate)

# Installs and loads MASS library
# install.packages("MASS")
library(MASS)

# Installs and loads corrplot library
# install.packages("corrplot")
library(corrplot)

# Installs and loads e1071 library
# install.packages("e1071")
library(e1071)

# Read in CSV -------------------------------------------------------------
# Reads the csv into R with date column
boxOffice <- read.csv(file = "weekendsv2.csv",
                      colClasses = "character",
                      na.strings="?")

# Converts data from character into data
boxOffice$Date <- as.Date(boxOffice$Date, format="%m/%d/%Y")

# Converts data from character into numeric values
boxOffice$Year <- as.numeric(boxOffice$Year)
boxOffice$Weekend <- as.numeric(boxOffice$Weekend)
boxOffice$Gross <- as.numeric(boxOffice$Gross)

# Converts to a tibble
as_tibble(boxOffice)

# Displays the tibble
print(boxOffice)
str(boxOffice)
summary(boxOffice)

# Explanatory Data Analysis -----------------------------------------------
# Recodes Date into Months
boxOffice <- boxOffice %>%
  mutate(Month = month(ymd(boxOffice$Date)))

boxOffice$Month <- as.numeric(boxOffice$Month)

# Recodes Months into Season
boxOffice <- boxOffice %>%
  mutate(WinterSeason = 0)

boxOffice <- boxOffice %>%
  mutate(SpringSeason = 0)

boxOffice <- boxOffice %>%
  mutate(SummerSeason = 0)

boxOffice <- boxOffice %>%
  mutate(FallSeason = 0)

for (i in 1:length(boxOffice$Month)) {
  if(boxOffice$Month[i] > 11) {
    boxOffice$WinterSeason[i] <- 1
  } else if(boxOffice$Month[i] > 8) {
    boxOffice$FallSeason[i] <- 1
  } else if(boxOffice$Month[i] > 5) {
    boxOffice$SummerSeason[i] <- 1
  } else if(boxOffice$Month[i] > 2) {
    boxOffice$SpringSeason[i] <- 1
  } else {
    boxOffice$WinterSeason[i] <- 1
  }
}

# Factorizes Winter Season
boxOffice$WinterSeason <- as.factor(boxOffice$WinterSeason)

# Factorizes Spring Season
boxOffice$SpringSeason <- as.factor(boxOffice$SpringSeason)

# Factorizes Season
boxOffice$SummerSeason <- as.factor(boxOffice$SummerSeason)

# Factorizes Season
boxOffice$FallSeason <- as.factor(boxOffice$FallSeason)

# Season factorization check
summary(boxOffice)

# Selects movies from 2000-2021
boxOffice2K <- filter(.data = boxOffice, Year > 1999)

# Coding in column of continuous week number since start of 2000
MilleniumWeek <- c(1:length(boxOffice2K$Gross))
boxOffice2K <- cbind(boxOffice2K, MilleniumWeek)

# Week numeric check
summary(boxOffice2K)

# HolidayWeekend Data for 2000-2020
holidayWeekend <- read.csv(file = "HolidayWeek.csv",
                           colClasses = "character",
                           na.strings="?")

# Converts data from character into data
holidayWeekend$HolidayWeekend <- as.Date(holidayWeekend$HolidayWeekend,
                                         format="%m/%d/%Y")
# Summary indicates a NA output
summary(holidayWeekend)

# NA filled with the appropriate date
holidayWeekend$HolidayWeekend[82] <- as.Date("5/11/2008",
                                             format="%m/%d/%Y")

# Summary indicates a NA output
summary(holidayWeekend)

# Generate holiday weekend column with 0 default
boxOffice2K <- boxOffice2K %>%
  mutate(HolidayWeekend = 0)

# Generate a nested for loop code holiday weekend with 1 for matching dates 
for (i in 1:length(boxOffice2K$Date)) {
  for (j in 1:length(holidayWeekend$HolidayWeekend)) {
    if(boxOffice2K$Date[i] == holidayWeekend$HolidayWeekend[j]) {
      boxOffice2K$HolidayWeekend[i] <- 1
    }
  }
}

# Turns the HolidayWeekend into a logical feature
boxOffice2K$HolidayWeekend <- as.factor(boxOffice2K$HolidayWeekend)

# j does not match up with the number of holiday weekends
summary(boxOffice2K$HolidayWeekend)

# Data Subsetting By Year -------------------------------------------------
# Streaming Availablility
boxOffice2K <- boxOffice2K %>%
  mutate(Streaming = ifelse(Year > 2007, 1, 0))
boxOffice2K$Streaming <- as.factor(boxOffice2K$Streaming)

# Original Streaming Availablility
boxOffice2K <- boxOffice2K %>%
  mutate(OriginalStreaming = ifelse(Year > 2012, 1, 0))
boxOffice2K$OriginalStreaming <- as.factor(boxOffice2K$OriginalStreaming)

# International Streaming Availablility
boxOffice2K <- boxOffice2K %>%
  mutate(InternationalStreaming = ifelse(Year > 2017, 1, 0))
boxOffice2K$InternationalStreaming <- as.factor(
  boxOffice2K$InternationalStreaming)

# Pandemic
boxOffice2K <- boxOffice2K %>%
  mutate(Pandemic= ifelse(Year > 2019, 1, 0))
boxOffice2K$Pandemic <- as.factor(boxOffice2K$Pandemic)

# Streaming factor check
summary(boxOffice2K)

# Visualizations ----------------------------------------------------------
# Plot, noticeable dip at year 2020.
plot(boxOffice2K$Gross ~ boxOffice2K$Date)

# Filter out the outlier data
boxOfficePrePandemic <- filter(.data = boxOffice2K, Pandemic == 0)

# Plot data of interest
plot(boxOfficePrePandemic$Gross ~ boxOfficePrePandemic$MilleniumWeek)

# Histogram
hist(boxOfficePrePandemic$Gross,
     xlab = "Gross")

# Correlation check, create dataset of only numeric columns
boxOfficePrePandemicCorrelation <- c(boxOfficePrePandemic$Gross,
                                     boxOfficePrePandemic$Year,
                                     boxOfficePrePandemic$Weekend,
                                     boxOfficePrePandemic$MilleniumWeek)

# Gives dimensions to the matrix
dim(boxOfficePrePandemicCorrelation) <- c(1043,4)

# Gives labels to the matrix
colnames(boxOfficePrePandemicCorrelation) <- c("Gross",
                                               "Year",
                                               "Weekend",
                                               "MilleniumWeek")

# Visualizes the correlations
corrplot(cor(boxOfficePrePandemicCorrelation),
         method = "number",
         type = "lower")

# Correlation of Year and Gross
cor(boxOfficePrePandemicCorrelation[,1], boxOfficePrePandemicCorrelation[,2])

# Correlation of MilleniumWeek and Gross
cor(boxOfficePrePandemicCorrelation[,1], boxOfficePrePandemicCorrelation[,4])

# Gross is skewed. Need to generate box cox to determine transformation
boxOfficePrePandemicLinear <- lm(data = boxOfficePrePandemic, Gross ~
                                   MilleniumWeek +
                                   
                                   # FallSeason is the baseline
                                   WinterSeason +
                                   SpringSeason +
                                   SummerSeason +
                                   HolidayWeekend +
                                   
                                   # No Streaming is the baseline
                                   Streaming +
                                   OriginalStreaming +
                                   InternationalStreaming)

# Generates box cox transformations, -0.2 is the best transformation
grossBoxCox <- boxcox(boxOfficePrePandemicLinear,
                      lambda = seq(-.5,.3,0.1),
                      interp = F)

# Generates Normalized Gross using boxcox transformation
boxOfficePrePandemic <- boxOfficePrePandemic %>%
  mutate(NormalizedGross = Gross^(-0.2))
summary(boxOfficePrePandemic)

# Visualize normalized gross
hist(boxOfficePrePandemic$NormalizedGross)

# Correlation check, create dataset of only numeric columns
boxOfficePrePandemicCorrelation <- c(boxOfficePrePandemic$NormalizedGross,
                                     boxOfficePrePandemic$Year,
                                     boxOfficePrePandemic$Weekend,
                                     boxOfficePrePandemic$MilleniumWeek)

# Gives dimensions to the matrix
dim(boxOfficePrePandemicCorrelation) <- c(1043,4)

# Gives labels to the matrix
colnames(boxOfficePrePandemicCorrelation) <- c("NormalizedGross",
                                               "Year",
                                               "Weekend",
                                               "MilleniumWeek")

# Visualizes the correlations
corrplot(cor(boxOfficePrePandemicCorrelation),
         method = "number",
         type = "lower")

# Correlation of Year and NormalizedGross
cor(boxOfficePrePandemicCorrelation[,1], boxOfficePrePandemicCorrelation[,2])

# Correlation of MilleniumWeek and Gross
cor(boxOfficePrePandemicCorrelation[,1], boxOfficePrePandemicCorrelation[,4])

# Generate a scattterplot
plot(boxOfficePrePandemic$NormalizedGross ~ boxOfficePrePandemic$MilleniumWeek,
     xlab = "Millenium Week",
     ylab = "Normalized Gross")

# Generate linear model using normalized gross
boxOfficeNormalLM <- lm(data = boxOfficePrePandemic, NormalizedGross ~
                          MilleniumWeek +
                          
                          # FallSeason is the baseline
                          WinterSeason +
                          SpringSeason +
                          SummerSeason +
                          HolidayWeekend +
                          
                          # No streaming is the baseline
                          Streaming +
                          OriginalStreaming +
                          InternationalStreaming)

# Generates red line of linear model
abline(boxOfficeNormalLM,
       col = "red")

# Summary of linear model
summary(boxOfficeNormalLM)

# Generate a success criteria using the fitted values of the model
boxOfficePrePandemic <- boxOfficePrePandemic %>%
  mutate(SuccessCriteria = fitted(boxOfficeNormalLM))

# Generates a success binary column with default 0  
boxOfficePrePandemic <- boxOfficePrePandemic %>%
  mutate(Success = 0)

# For loop to code for success and fail
for (i in 1:length(boxOfficePrePandemic$NormalizedGross)) {
  
  # Compares Normalized Gross with Success Criteria
  if(boxOfficePrePandemic$NormalizedGross[i] <
     boxOfficePrePandemic$SuccessCriteria[i]) {
    
    # Success is for Normalized Gross less than the criteria
    boxOfficePrePandemic$Success[i] <- 1
  } else{
    boxOfficePrePandemic$Success[i] <- 0
  }
}

# Factorizes the Success
boxOfficePrePandemic$Success <- as.factor(boxOfficePrePandemic$Success)

# Summary of the final data set
summary(boxOfficePrePandemic)

# Naive Bayes -------------------------------------------------------------
# Establishes seed for random sampling
set.seed(154)

# Creates a 75:25 smaple split without replacement from sedanSize
sampleSet <- sample(nrow(boxOfficePrePandemic),
                    round(nrow(boxOfficePrePandemic) * 0.75),
                    replace = FALSE)

# Creates the Training set using 75% of the data
boxOfficeTraining <- boxOfficePrePandemic[sampleSet,]

# Creates the Testing set using 25% of the data
boxOfficeTesting <- boxOfficePrePandemic[-sampleSet,]

# Generates the Naive Bayes Model
boxOfficeNaive <- naiveBayes(formula = Success ~ WinterSeason +
                               SpringSeason +
                               SummerSeason +
                               FallSeason +
                               HolidayWeekend +
                               Streaming +
                               OriginalStreaming +
                               InternationalStreaming,
                             data = boxOfficeTraining,
                             laplace = 1)

# Builds probabilities for each record in the testing data
boxOfficeProbability <- predict(boxOfficeNaive,
                                boxOfficeTesting,
                                type = "raw")

# Displays the probabilities
print(boxOfficeProbability)

# Predicts classes for each record in testing set
boxOfficePrediction <- predict(boxOfficeNaive,
                               boxOfficeTesting,
                               type = "class")

# Displays the predictions
print(boxOfficePrediction)

# Create the Confusion Matrix
confusionMatrix <- table(boxOfficeTesting$Success,
                         boxOfficePrediction)
print(confusionMatrix)

# Calculate the model prediction accuracy.
predictiveAccuracy <- sum(diag(confusionMatrix))/nrow(boxOfficeTesting)

# Displays the predictive accuracy 52
print(predictiveAccuracy)

# Correcting for Dependence -----------------------------------------------
# Streaming ---------------------------------------------------------------
# Fall: 49.04%
# Generates the Naive Bayes Model
boxOfficeNaive <- naiveBayes(formula = Success ~ FallSeason +
                               HolidayWeekend +
                               Streaming,
                             data = boxOfficeTraining,
                             laplace = 1)

# Builds probabilities for each record in the testing data
boxOfficeProbability <- predict(boxOfficeNaive,
                                boxOfficeTesting,
                                type = "raw")

# Displays the probabilities
print(boxOfficeProbability)

# Predicts classes for each record in testing set
boxOfficePrediction <- predict(boxOfficeNaive,
                               boxOfficeTesting,
                               type = "class")

# Displays the predictions
print(boxOfficePrediction)

# Create the Confusion Matrix
confusionMatrix <- table(boxOfficeTesting$Success,
                         boxOfficePrediction)
print(confusionMatrix)

# Calculate the model prediction accuracy.
predictiveAccuracy <- sum(diag(confusionMatrix))/nrow(boxOfficeTesting)

# Displays the predictive accuracy 0.49
print(predictiveAccuracy)

# Winter = 49.04%
# Generates the Naive Bayes Model
boxOfficeNaive <- naiveBayes(formula = Success ~ WinterSeason +
                               HolidayWeekend +
                               Streaming,
                             data = boxOfficeTraining,
                             laplace = 1)

# Builds probabilities for each record in the testing data
boxOfficeProbability <- predict(boxOfficeNaive,
                                boxOfficeTesting,
                                type = "raw")

# Displays the probabilities
print(boxOfficeProbability)

# Predicts classes for each record in testing set
boxOfficePrediction <- predict(boxOfficeNaive,
                               boxOfficeTesting,
                               type = "class")

# Displays the predictions
print(boxOfficePrediction)

# Create the Confusion Matrix
confusionMatrix <- table(boxOfficeTesting$Success,
                         boxOfficePrediction)
print(confusionMatrix)

# Calculate the model prediction accuracy.
predictiveAccuracy <- sum(diag(confusionMatrix))/nrow(boxOfficeTesting)

# Displays the predictive accuracy 0.49
print(predictiveAccuracy)

# Spring = 49.04%
# Generates the Naive Bayes Model
boxOfficeNaive <- naiveBayes(formula = Success ~ SpringSeason +
                               HolidayWeekend +
                               Streaming,
                             data = boxOfficeTraining,
                             laplace = 1)

# Builds probabilities for each record in the testing data
boxOfficeProbability <- predict(boxOfficeNaive,
                                boxOfficeTesting,
                                type = "raw")

# Displays the probabilities
print(boxOfficeProbability)

# Predicts classes for each record in testing set
boxOfficePrediction <- predict(boxOfficeNaive,
                               boxOfficeTesting,
                               type = "class")

# Displays the predictions
print(boxOfficePrediction)

# Create the Confusion Matrix
confusionMatrix <- table(boxOfficeTesting$Success,
                         boxOfficePrediction)
print(confusionMatrix)

# Calculate the model prediction accuracy.
predictiveAccuracy <- sum(diag(confusionMatrix))/nrow(boxOfficeTesting)

# Displays the predictive accuracy 0.49
print(predictiveAccuracy)

# Summer = 50.057%
# Generates the Naive Bayes Model
boxOfficeNaive <- naiveBayes(formula = Success ~ SummerSeason +
                               HolidayWeekend +
                               Streaming,
                             data = boxOfficeTraining,
                             laplace = 1)

# Builds probabilities for each record in the testing data
boxOfficeProbability <- predict(boxOfficeNaive,
                                boxOfficeTesting,
                                type = "raw")

# Displays the probabilities
print(boxOfficeProbability)

# Predicts classes for each record in testing set
boxOfficePrediction <- predict(boxOfficeNaive,
                               boxOfficeTesting,
                               type = "class")

# Displays the predictions
print(boxOfficePrediction)

# Create the Confusion Matrix
confusionMatrix <- table(boxOfficeTesting$Success,
                         boxOfficePrediction)
print(confusionMatrix)

# Calculate the model prediction accuracy.
predictiveAccuracy <- sum(diag(confusionMatrix))/nrow(boxOfficeTesting)

# Displays the predictive accuracy 0.50
print(predictiveAccuracy)


# Original Streaming ------------------------------------------------------
# Fall: 49.04%
# Generates the Naive Bayes Model
boxOfficeNaive <- naiveBayes(formula = Success ~ FallSeason +
                               HolidayWeekend +
                               OriginalStreaming,
                             data = boxOfficeTraining,
                             laplace = 1)

# Builds probabilities for each record in the testing data
boxOfficeProbability <- predict(boxOfficeNaive,
                                boxOfficeTesting,
                                type = "raw")

# Displays the probabilities
print(boxOfficeProbability)

# Predicts classes for each record in testing set
boxOfficePrediction <- predict(boxOfficeNaive,
                               boxOfficeTesting,
                               type = "class")

# Displays the predictions
print(boxOfficePrediction)

# Create the Confusion Matrix
confusionMatrix <- table(boxOfficeTesting$Success,
                         boxOfficePrediction)
print(confusionMatrix)

# Calculate the model prediction accuracy.
predictiveAccuracy <- sum(diag(confusionMatrix))/nrow(boxOfficeTesting)

# Displays the predictive accuracy 0.49
print(predictiveAccuracy)

# Winter = 49.04%
# Generates the Naive Bayes Model
boxOfficeNaive <- naiveBayes(formula = Success ~ WinterSeason +
                               HolidayWeekend +
                               OriginalStreaming,
                             data = boxOfficeTraining,
                             laplace = 1)

# Builds probabilities for each record in the testing data
boxOfficeProbability <- predict(boxOfficeNaive,
                                boxOfficeTesting,
                                type = "raw")

# Displays the probabilities
print(boxOfficeProbability)

# Predicts classes for each record in testing set
boxOfficePrediction <- predict(boxOfficeNaive,
                               boxOfficeTesting,
                               type = "class")

# Displays the predictions
print(boxOfficePrediction)

# Create the Confusion Matrix
confusionMatrix <- table(boxOfficeTesting$Success,
                         boxOfficePrediction)
print(confusionMatrix)

# Calculate the model prediction accuracy.
predictiveAccuracy <- sum(diag(confusionMatrix))/nrow(boxOfficeTesting)

# Displays the predictive accuracy 0.49
print(predictiveAccuracy)

# Spring = 49.04%
# Generates the Naive Bayes Model
boxOfficeNaive <- naiveBayes(formula = Success ~ SpringSeason +
                               HolidayWeekend +
                               OriginalStreaming,
                             data = boxOfficeTraining,
                             laplace = 1)

# Builds probabilities for each record in the testing data
boxOfficeProbability <- predict(boxOfficeNaive,
                                boxOfficeTesting,
                                type = "raw")

# Displays the probabilities
print(boxOfficeProbability)

# Predicts classes for each record in testing set
boxOfficePrediction <- predict(boxOfficeNaive,
                               boxOfficeTesting,
                               type = "class")

# Displays the predictions
print(boxOfficePrediction)

# Create the Confusion Matrix
confusionMatrix <- table(boxOfficeTesting$Success,
                         boxOfficePrediction)
print(confusionMatrix)

# Calculate the model prediction accuracy.
predictiveAccuracy <- sum(diag(confusionMatrix))/nrow(boxOfficeTesting)

# Displays the predictive accuracy 0.49
print(predictiveAccuracy)

# Summer = 50.57%
# Generates the Naive Bayes Model
boxOfficeNaive <- naiveBayes(formula = Success ~ SummerSeason +
                               HolidayWeekend +
                               OriginalStreaming,
                             data = boxOfficeTraining,
                             laplace = 1)

# Builds probabilities for each record in the testing data
boxOfficeProbability <- predict(boxOfficeNaive,
                                boxOfficeTesting,
                                type = "raw")

# Displays the probabilities
print(boxOfficeProbability)

# Predicts classes for each record in testing set
boxOfficePrediction <- predict(boxOfficeNaive,
                               boxOfficeTesting,
                               type = "class")

# Displays the predictions
print(boxOfficePrediction)

# Create the Confusion Matrix
confusionMatrix <- table(boxOfficeTesting$Success,
                         boxOfficePrediction)
print(confusionMatrix)

# Calculate the model prediction accuracy.
predictiveAccuracy <- sum(diag(confusionMatrix))/nrow(boxOfficeTesting)

# Displays the predictive accuracy 0.50
print(predictiveAccuracy)


# International -----------------------------------------------------------
# Fall: 50.10%
# Generates the Naive Bayes Model
boxOfficeNaive <- naiveBayes(formula = Success ~ FallSeason +
                               HolidayWeekend +
                               InternationalStreaming,
                             data = boxOfficeTraining,
                             laplace = 1)

# Builds probabilities for each record in the testing data
boxOfficeProbability <- predict(boxOfficeNaive,
                                boxOfficeTesting,
                                type = "raw")

# Displays the probabilities
print(boxOfficeProbability)

# Predicts classes for each record in testing set
boxOfficePrediction <- predict(boxOfficeNaive,
                               boxOfficeTesting,
                               type = "class")

# Displays the predictions
print(boxOfficePrediction)

# Create the Confusion Matrix
confusionMatrix <- table(boxOfficeTesting$Success,
                         boxOfficePrediction)
print(confusionMatrix)

# Calculate the model prediction accuracy.
predictiveAccuracy <- sum(diag(confusionMatrix))/nrow(boxOfficeTesting)

# Displays the predictive accuracy 0.51
print(predictiveAccuracy)

# Winter = 47.89%
# Generates the Naive Bayes Model
boxOfficeNaive <- naiveBayes(formula = Success ~ WinterSeason +
                               HolidayWeekend +
                               InternationalStreaming,
                             data = boxOfficeTraining,
                             laplace = 1)

# Builds probabilities for each record in the testing data
boxOfficeProbability <- predict(boxOfficeNaive,
                                boxOfficeTesting,
                                type = "raw")

# Displays the probabilities
print(boxOfficeProbability)

# Predicts classes for each record in testing set
boxOfficePrediction <- predict(boxOfficeNaive,
                               boxOfficeTesting,
                               type = "class")

# Displays the predictions
print(boxOfficePrediction)

# Create the Confusion Matrix
confusionMatrix <- table(boxOfficeTesting$Success,
                         boxOfficePrediction)
print(confusionMatrix)

# Calculate the model prediction accuracy.
predictiveAccuracy <- sum(diag(confusionMatrix))/nrow(boxOfficeTesting)

# Displays the predictive accuracy 0.48
print(predictiveAccuracy)

# Spring = 49.04%
# Generates the Naive Bayes Model
boxOfficeNaive <- naiveBayes(formula = Success ~ SpringSeason +
                               HolidayWeekend +
                               InternationalStreaming,
                             data = boxOfficeTraining,
                             laplace = 1)

# Builds probabilities for each record in the testing data
boxOfficeProbability <- predict(boxOfficeNaive,
                                boxOfficeTesting,
                                type = "raw")

# Displays the probabilities
print(boxOfficeProbability)

# Predicts classes for each record in testing set
boxOfficePrediction <- predict(boxOfficeNaive,
                               boxOfficeTesting,
                               type = "class")

# Displays the predictions
print(boxOfficePrediction)

# Create the Confusion Matrix
confusionMatrix <- table(boxOfficeTesting$Success,
                         boxOfficePrediction)
print(confusionMatrix)

# Calculate the model prediction accuracy.
predictiveAccuracy <- sum(diag(confusionMatrix))/nrow(boxOfficeTesting)

# Displays the predictive accuracy 0.49
print(predictiveAccuracy)

# Summer = 50.57%
# Generates the Naive Bayes Model
boxOfficeNaive <- naiveBayes(formula = Success ~ SummerSeason +
                               HolidayWeekend +
                               InternationalStreaming,
                             data = boxOfficeTraining,
                             laplace = 1)

# Builds probabilities for each record in the testing data
boxOfficeProbability <- predict(boxOfficeNaive,
                                boxOfficeTesting,
                                type = "raw")

# Displays the probabilities
print(boxOfficeProbability)

# Predicts classes for each record in testing set
boxOfficePrediction <- predict(boxOfficeNaive,
                               boxOfficeTesting,
                               type = "class")

# Displays the predictions
print(boxOfficePrediction)

# Create the Confusion Matrix
confusionMatrix <- table(boxOfficeTesting$Success,
                         boxOfficePrediction)
print(confusionMatrix)

# Calculate the model prediction accuracy.
predictiveAccuracy <- sum(diag(confusionMatrix))/nrow(boxOfficeTesting)

# Displays the predictive accuracy 0.51
print(predictiveAccuracy)


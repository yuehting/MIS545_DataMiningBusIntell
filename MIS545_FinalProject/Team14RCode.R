# Preliminary Setup -------------------------------------------------------
# Sets the working directory

# K-Nearest Neighbors
# Works well without MASS package
# Installs and loads tidyverse library
# install.packages("tidyverse")
library(tidyverse)

# Installs and loads tidyverse library
# install.packages("lubridate")
library(lubridate)


# Installs and loads corrplot library
# install.packages("corrplot")
library(corrplot)

# Installs and loads e1071 library
# install.packages("e1071")
library(e1071)

# KNN R Code
# Install tidyverse packages
# install.packages("tidyverse")

# Load tidyverse libraries
library(class)
library(olsrr)

# Setting the working directory
setwd("/Users/robertallard/Desktop/MIS 545/Final Project")

# Read weekendsv2.csv into tibble called BoxOffice
# l for logical 
# n for numeric
# i for integer
# c for character
# f for factor
# D for date
# T for datetime

BoxOffice <- read_csv(file = "weekendsv2.csv",
                      col_types = "ffnnn",
                      col_names = TRUE)

# Display the tibble
print(BoxOffice)

# Diplay the structure of the tibble
str(BoxOffice)

# Summary the tibble
summary(BoxOffice)

# Converts data from character into data
BoxOffice$Date <- as.Date(BoxOffice$Date, format="%m/%d/%Y") 

# Explanatory Data Analysis -----------------------------------------------
# Recodes Date into Months
BoxOffice <- BoxOffice %>%
  mutate(Month = months(BoxOffice$Date))

# Recode Months into Season
BoxOffice <- BoxOffice %>%
  mutate(Season = 4)
for (i in 1:length(BoxOffice$Month)) {
  if (BoxOffice$Month[i] == "January" || BoxOffice$Month[i] =="February" 
      || BoxOffice$Month[i] == "December") {
    BoxOffice$Season[i] <- 4
  } else if (BoxOffice$Month[i] == "November" || BoxOffice$Month[i] =="October" 
             || BoxOffice$Month[i] == "September"){
    BoxOffice$Season[i] <-3
  } else if (BoxOffice$Month[i] == "July" || BoxOffice$Month[i] =="June" 
             || BoxOffice$Month[i] == "August"){
    BoxOffice$Season[i] <- 2
  } else {
    BoxOffice$Season[i] <- 1
  }
}

# Dummy codes Seasons into 4 variables
BoxOffice <- BoxOffice %>%
  mutate(WinterSeason = 0)
BoxOffice <- BoxOffice %>%
  mutate(FallSeason = 0)
BoxOffice <- BoxOffice %>%
  mutate(SummerSeason = 0)
BoxOffice <- BoxOffice %>%
  mutate(SpringSeason = 0)
for (i in 1:length(BoxOffice$Month)) {
  if (BoxOffice$Month[i] == "January" || BoxOffice$Month[i] =="February" 
      || BoxOffice$Month[i] == "December") {
    BoxOffice$WinterSeason[i] <- 1
  } else if (BoxOffice$Month[i] == "November" || BoxOffice$Month[i] =="October" 
             || BoxOffice$Month[i] == "September"){
    BoxOffice$FallSeason[i] <- 1
  } else if (BoxOffice$Month[i] == "July" || BoxOffice$Month[i] =="June" 
             || BoxOffice$Month[i] == "August"){
    BoxOffice$SummerSeason[i] <- 1
  } else {
    BoxOffice$SpringSeason[i] <- 1
  }
}
str(BoxOffice)

# Factorizes Winter Season
BoxOffice$WinterSeason <- as.factor(BoxOffice$WinterSeason)

# Factorizes Fall Season
BoxOffice$FallSeason <- as.factor(BoxOffice$FallSeason)

# Factorizes Summer Season
BoxOffice$SummerSeason <- as.factor(BoxOffice$SummerSeason)

# Factorizes Spring Season
BoxOffice$SpringSeason <- as.factor(BoxOffice$SpringSeason)

# Season factorization check
summary(BoxOffice)

# Select data from 2000 to 2021
BoxOffice2K <- filter(.data = BoxOffice,
                      Year > 1999)

# Week numeric check
summary(BoxOffice2K)

# HolidayWeekend Data for 2000-2020
holidayWeekend <- read_csv(file = "HolidayWeek.csv", 
                           col_names = TRUE,
                           col_types = "f")

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

# Create For loop to generate a logical holiday weekend column
BoxOffice2K <- BoxOffice2K %>%
  mutate(HolidayWeekend = 0)
for (i in 1 : length(BoxOffice2K$Date)) {
  for (j in 1 : length(holidayWeekend$HolidayWeekend)) {
    if(BoxOffice2K$Date[i] == holidayWeekend$HolidayWeekend[j]) {
      BoxOffice2K$HolidayWeekend[i] <- 1
    }
  }
}

# Turns the HolidayWeekend into a logical feature
BoxOffice2K$HolidayWeekend <- as.factor(BoxOffice2K$HolidayWeekend)

# j does not match up with the number of holiday weekends
summary(BoxOffice2K$HolidayWeekend)

# Data Subsetting By Year -------------------------------------------------
# Streaming Availablility
BoxOffice2K <- BoxOffice2K %>%
  mutate(Streaming = ifelse(Year > 2007, 1,0))
BoxOffice2K$Streaming <- as.factor(BoxOffice2K$Streaming)

# Original Streaming Availablility
BoxOffice2K <- BoxOffice2K %>%
  mutate(OriginalStreaming  = ifelse(Year > 2012, 1, 0))
BoxOffice2K$OriginalStreaming <- as.factor(BoxOffice2K$OriginalStreaming)

# International Streaming Availablility
BoxOffice2K <- BoxOffice2K %>%
  mutate(InternationalStreaming = ifelse(Year > 2017, 1, 0))
BoxOffice2K$InternationalStreaming <- 
  as.factor(BoxOffice2K$InternationalStreaming)

# Pandemic
BoxOffice2K <- BoxOffice2K %>%
  mutate(Pandemic = ifelse(Year > 2019, 1, 0))
BoxOffice2K$Pandemic <- as.factor(BoxOffice2K$Pandemic)

# Streaming factor check
summary(BoxOffice2K)

# Filter out the outlier data
BoxOfficePrePandemic <- filter(.data = BoxOffice2K, Pandemic ==0)

# Histogram
hist(BoxOfficePrePandemic$Gross)

# Create a new feature called Fourseason
BoxOfficePrePandemic <- BoxOfficePrePandemic %>%
  mutate(Fourseason = 0) 
for (i in 1:length(BoxOfficePrePandemic$Month)) {
  if(BoxOfficePrePandemic$Season[i] == 4){
    BoxOfficePrePandemic$Fourseason[i] <- "Winter"
  } else if(BoxOfficePrePandemic$Season[i] ==3) {
    BoxOfficePrePandemic$Fourseason[i] <- "Fall"
  } else if (BoxOfficePrePandemic$Season[i] ==2) {
    BoxOfficePrePandemic$Fourseason[i] <- "Summer"
  } else{
    BoxOfficePrePandemic$Fourseason[i] <- "Spring"
  }
  
}


# K-nearest model
BoxOfficePrePandemicKnn <- BoxOfficePrePandemic %>% select(-Film)
BoxOfficePrePandemicKnnLabel <- BoxOfficePrePandemic %>% select(Fourseason)
BoxOfficePrePandemicKnn <- BoxOfficePrePandemic %>%
  select(Gross, 
         HolidayWeekend, 
         Streaming, 
         OriginalStreaming, 
         InternationalStreaming,
         Pandemic)

# Set.seed() function
set.seed(370)
# Split into BoxOfficeTraining and BoxOfficeTesting
sampleSet <- sample(nrow(BoxOfficePrePandemicKnn),
                    round(nrow(BoxOfficePrePandemicKnn) * 0.75),
                    replace = FALSE)
# Creating training set
BoxOfficeTraining <- BoxOfficePrePandemicKnn[sampleSet, ]
BoxOfficeTraingLabel <- BoxOfficePrePandemicKnnLabel[sampleSet, ]

# Creating the testing set
BoxOfficeTesting <- BoxOfficePrePandemicKnn[-sampleSet, ]
BoxOfficeTestingLabel <- BoxOfficePrePandemicKnnLabel[-sampleSet, ]

# Generate the model
BoxOfficePrediction <- knn(train = BoxOfficeTraining,
                           test = BoxOfficeTesting,
                           cl = BoxOfficeTraingLabel$Fourseason, 
                           k = 59)

print(BoxOfficePrediction)
summary(BoxOfficePrediction)
# Confusion matrix
BoxOfficeConfusionMatrix <- table(BoxOfficeTestingLabel$Fourseason,
                                  BoxOfficePrediction)
print(BoxOfficeConfusionMatrix)

BoxOfficePredictiveAccuracy <- sum(diag(BoxOfficeConfusionMatrix)) /
  nrow(BoxOfficeTesting)
print(BoxOfficePredictiveAccuracy)

kValuematrix <- matrix(data = NA,
                       ncol = 2,
                       nrow = 0)
colnames(kValuematrix) <- c("k value", "Predictive accuracy")

for (kValue in 1:nrow(BoxOfficeTraining)) {
  if(kValue %% 2 != 0) {
    BoxOfficePrediction <- knn(train = BoxOfficeTraining,
                               test = BoxOfficeTesting,
                               cl = BoxOfficeTraingLabel$Fourseason, 
                               k = 59)
    BoxOfficeConfusionMatrix <- table(BoxOfficeTestingLabel$Fourseason,
                                      BoxOfficePrediction)
    
    BoxOfficePredictiveAccuracy <- sum(diag(BoxOfficeConfusionMatrix)) /
      nrow(BoxOfficeTesting)
    
    kValuematrix <- rbind(kValuematrix, c(kValue, BoxOfficePredictiveAccuracy))
    
  }
  
}
print(kValuematrix)


# EDA and Naive Bayes R Code

# Peter Yeu-Shyang Yeh
# MIS 545 Section 02
# GroupProject.R
# Installs and loads MASS library
# install.packages("MASS")
library(MASS)

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
# set.seed(154)

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

# Summer = 50.57%
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
# Fall: 50.96%
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

# Logistic Regression R Code
# Preliminary Setup -------------------------------------------------------

# Installs and loads tidyverse library
# install.packages("tidyverse")
library(tidyverse)

# Installs and loads tidyverse library
# install.packages("lubridate")
library(lubridate)

# Installs and loads MASS library
# install.packages("MASS")
library(MASS)

# For group by
library(dplyr)

library(corrplot) 
library(olsrr)
library(smotefamily)

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

# Create For loop to generate a logical holiday weekend column
boxOffice2K <- boxOffice2K %>%
  mutate(HolidayWeekend = 0)

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
hist(boxOfficePrePandemic$Gross)

# Correlation check, Use MilleniumWeek since most correlated to Gross
cor(boxOfficePrePandemic$Gross, boxOfficePrePandemic$MilleniumWeek)
cor(boxOfficePrePandemic$Gross, boxOfficePrePandemic$Year)
cor(boxOfficePrePandemic$Gross, boxOfficePrePandemic$Weekend)

# Gross is skewed. Need to generate box cox to determine transformation
boxOfficePrePandemicLinear <- lm(data = boxOfficePrePandemic, Gross ~
                                   MilleniumWeek +
                                   WinterSeason +
                                   SpringSeason +
                                   SummerSeason +
                                   HolidayWeekend +
                                   Streaming +
                                   OriginalStreaming +
                                   InternationalStreaming)

grossBoxCox <- boxcox(boxOfficePrePandemicLinear,
                      lambda = seq(-.5,.3,0.1),
                      interp = F)

boxOfficePrePandemic <- boxOfficePrePandemic %>%
  mutate(NormalizedGross = Gross^(-0.2))

summary(boxOfficePrePandemic)

hist(boxOfficePrePandemic$NormalizedGross)

cor(boxOfficePrePandemic$NormalizedGross, boxOfficePrePandemic$MilleniumWeek)
cor(boxOfficePrePandemic$NormalizedGross, boxOfficePrePandemic$Year)
cor(boxOfficePrePandemic$NormalizedGross, boxOfficePrePandemic$Weekend)

plot(boxOfficePrePandemic$NormalizedGross ~ boxOfficePrePandemic$MilleniumWeek)
boxOfficeNormalLM <- lm(data = boxOfficePrePandemic, NormalizedGross ~
                          MilleniumWeek +
                          WinterSeason +
                          SpringSeason +
                          SummerSeason +
                          HolidayWeekend +
                          Streaming +
                          OriginalStreaming +
                          InternationalStreaming)
abline(boxOfficeNormalLM,
       col = "red")
summary(boxOfficeNormalLM)

boxOfficePrePandemic <- boxOfficePrePandemic %>%
  mutate(SuccessCriteria = fitted(boxOfficeNormalLM))

boxOfficePrePandemic <- boxOfficePrePandemic %>%
  mutate(Success = 0)
for (i in 1:length(boxOfficePrePandemic$NormalizedGross)) {
  if(boxOfficePrePandemic$NormalizedGross[i] <
     boxOfficePrePandemic$SuccessCriteria[i]) {
    boxOfficePrePandemic$Success[i] <- 1
  } else{
    boxOfficePrePandemic$Success[i] <- 0
  }
}

boxOfficePrePandemic$Succes <- as.factor(boxOfficePrePandemic$Success)


summary(boxOfficePrePandemic)

-----------------------------------
  
#seed variable is set to ensure we get the same result every time we run a 
#sampling process
  
# set.seed(203)  

#create a vector of 75% randomly sampled rows from the original data
sample<- sample(nrow(boxOfficePrePandemic),
                      round(nrow(boxOfficePrePandemic) * 0.75),
                      replace = FALSE)

# Put the records (75%) sample into traning data set 
boxOfficePrePandemicTraining <- boxOfficePrePandemic[sample, ]

# Put the records (25%) sample into testing data set 
boxOfficePrePandemicTesting <- boxOfficePrePandemic[-sample, ]

# Check for class imbalance 
summary(boxOfficePrePandemic$Succes)

# Generate the logistic regression model
boxOfficePrePandemicModel <- glm(boxOfficePrePandemic,
                                 family = binomial,
                                 formula = Succes ~ WinterSeason +
                                   SpringSeason +
                                   SummerSeason +
                                   FallSeason +
                                   HolidayWeekend +
                                   Streaming +
                                   OriginalStreaming +
                                   InternationalStreaming)

boxOfficePrePandemicModel <- glm(boxOfficePrePandemic,
                                 family = binomial,
                                 formula = Succes ~ WinterSeason +
                                   SpringSeason +
                                   SummerSeason +
                                   FallSeason +
                                   Year +
                                   HolidayWeekend +
                                   Streaming +
                                   OriginalStreaming +
                                   InternationalStreaming)


summary(boxOfficePrePandemicModel)


# Predicting testing data
boxOfficePrePandemicModelpredict<- predict(boxOfficePrePandemicModel,
                                           boxOfficePrePandemicTesting,
                                           type="response")
# Print predicted data
print(boxOfficePrePandemicModelpredict)

# Anything below or equal to 0.5 as 0, anything above 0.5 as 1.
boxOfficePrePandemicModelpredict<-
  ifelse(boxOfficePrePandemicModelpredict >= 0.5,1,0)

print(boxOfficePrePandemicModelpredict)

# Creating confusion matrix
boxOfficePrePandemicConfusionMatrix <- table(boxOfficePrePandemicTesting$Succes,
                                             boxOfficePrePandemicModelpredict)

print(boxOfficePrePandemicConfusionMatrix)


# False positive rate
boxOfficePrePandemicConfusionMatrix[1,2] /
  (boxOfficePrePandemicConfusionMatrix[1,2]+
     boxOfficePrePandemicConfusionMatrix[1,1] )

# False negative
boxOfficePrePandemicConfusionMatrix[2,1] /
  (boxOfficePrePandemicConfusionMatrix[2,1]+
     boxOfficePrePandemicConfusionMatrix[2,2] )

# Accuracy
sum(diag(boxOfficePrePandemicConfusionMatrix)) /
  nrow(boxOfficePrePandemicTesting)

# Decision Tree R Code
# Preliminary Setup -------------------------------------------------------
# Sets the working directory


# Installs and loads tidyverse library
# install.packages("tidyverse")
library(tidyverse)

# Installs and loads tidyverse library
# install.packages("lubridate")
library(lubridate)

# Installs and loads MASS library
# install.packages("MASS")
library(MASS)

# install.packages("rpart.plot")
library(rpart)
library(rpart.plot)

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

# Exploratory Data Analysis -----------------------------------------------
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

# Create For loop to generate a logical holiday weekend column
boxOffice2K <- boxOffice2K %>%
  mutate(HolidayWeekend = 0)

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
# Streaming Availability
boxOffice2K <- boxOffice2K %>%
  mutate(Streaming = ifelse(Year > 2007, 1, 0))
boxOffice2K$Streaming <- as.factor(boxOffice2K$Streaming)

# Original Streaming Availability
boxOffice2K <- boxOffice2K %>%
  mutate(OriginalStreaming = ifelse(Year > 2012, 1, 0))
boxOffice2K$OriginalStreaming <- as.factor(boxOffice2K$OriginalStreaming)

# International Streaming Availability
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
hist(boxOfficePrePandemic$Gross)

# Correlation check, Use MilleniumWeek since most correlated to Gross
cor(boxOfficePrePandemic$Gross, boxOfficePrePandemic$MilleniumWeek)
cor(boxOfficePrePandemic$Gross, boxOfficePrePandemic$Year)
cor(boxOfficePrePandemic$Gross, boxOfficePrePandemic$Weekend)

# Gross is skewed. Need to generate box cox to determine transformation
boxOfficePrePandemicLinear <- lm(data = boxOfficePrePandemic, Gross ~
                                   MilleniumWeek +
                                   WinterSeason +
                                   SpringSeason +
                                   SummerSeason +
                                   HolidayWeekend +
                                   Streaming +
                                   OriginalStreaming +
                                   InternationalStreaming)

grossBoxCox <- boxcox(boxOfficePrePandemicLinear,
                      lambda = seq(-.5,.3,0.1),
                      interp = F)

boxOfficePrePandemic <- boxOfficePrePandemic %>%
  mutate(NormalizedGross = Gross^(-0.2))

summary(boxOfficePrePandemic)

hist(boxOfficePrePandemic$NormalizedGross)

cor(boxOfficePrePandemic$NormalizedGross, boxOfficePrePandemic$MilleniumWeek)
cor(boxOfficePrePandemic$NormalizedGross, boxOfficePrePandemic$Year)
cor(boxOfficePrePandemic$NormalizedGross, boxOfficePrePandemic$Weekend)

plot(boxOfficePrePandemic$NormalizedGross ~ boxOfficePrePandemic$MilleniumWeek)
boxOfficeNormalLM <- lm(data = boxOfficePrePandemic, NormalizedGross ~
                          MilleniumWeek +
                          WinterSeason +
                          SpringSeason +
                          SummerSeason +
                          HolidayWeekend +
                          Streaming +
                          OriginalStreaming +
                          InternationalStreaming)
abline(boxOfficeNormalLM,
       col = "red")
summary(boxOfficeNormalLM)

boxOfficePrePandemic <- boxOfficePrePandemic %>%
  mutate(SuccessCriteria = fitted(boxOfficeNormalLM))

boxOfficePrePandemic <- boxOfficePrePandemic %>%
  mutate(Success = 0)
for (i in 1:length(boxOfficePrePandemic$NormalizedGross)) {
  if(boxOfficePrePandemic$NormalizedGross[i] <
     boxOfficePrePandemic$SuccessCriteria[i]) {
    boxOfficePrePandemic$Success[i] <- 0
  } else{
    boxOfficePrePandemic$Success[i] <- 1
  }
}

boxOfficePrePandemic$Success <- as.factor(boxOfficePrePandemic$Success)

summary(boxOfficePrePandemic)

# Setting random seed
# set.seed(370)

# Randomly splitting the data into training(75) and testing(25)
sampleSet <- sample(nrow(boxOfficePrePandemic),
                    round(nrow(boxOfficePrePandemic) * .75),
                    replace = FALSE)

# Putting 75% into training
boxOfficePrePandemicTraining <- boxOfficePrePandemic[sampleSet, ]

# Putting 25% into testing
boxOfficePrePandemicTesting <- boxOfficePrePandemic[-sampleSet, ]

# Creating the Decision Tree Model
boxOfficePrePandemicDecisionTreeModel <- rpart(formula = Success ~
                                                 MilleniumWeek +
                                                 WinterSeason +
                                                 SpringSeason +
                                                 SummerSeason +
                                                 HolidayWeekend +
                                                 Streaming +
                                                 OriginalStreaming +
                                                 InternationalStreaming,
                                               method = "class",
                                               cp = .01,
                                               data = boxOfficePrePandemicTraining)

# Displaying the decision tree visualization
rpart.plot(boxOfficePrePandemicDecisionTreeModel)

# Calculating the prediction
boxOfficePrePandemicPrediction <- predict(boxOfficePrePandemicDecisionTreeModel,
                                          boxOfficePrePandemicTesting,
                                          type = "class")

# Displaying the prediction in the console
print(boxOfficePrePandemicPrediction)

# Creating a confusion matrix
boxOfficePrePandemicConfusionMatrix <- table(
  boxOfficePrePandemicTesting$Success,
  boxOfficePrePandemicPrediction)

# Displaying the confusion matrix
print(boxOfficePrePandemicConfusionMatrix)

# Calculating the predictive accuracy
predictiveAccuracy <- sum(diag(boxOfficePrePandemicConfusionMatrix)) /
  nrow(boxOfficePrePandemicTesting)

# Displaying predictive accuracy in the console
print(predictiveAccuracy)

# Neural Network R Code
# Preliminary Setup -------------------------------------------------------
# Sets the working directory


# Installs and loads tidyverse library
# install.packages("tidyverse")
library(tidyverse)

# Installs and loads tidyverse library
# install.packages("lubridate")
library(lubridate)

# Installs and loads MASS library
# install.packages("MASS")
library(MASS)

# install.packages("neuralnet")
library(neuralnet)
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

# Create For loop to generate a logical holiday weekend column
boxOffice2K <- boxOffice2K %>%
  mutate(HolidayWeekend = 0)

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
hist(boxOfficePrePandemic$Gross)

# Correlation check, Use MilleniumWeek since most correlated to Gross
cor(boxOfficePrePandemic$Gross, boxOfficePrePandemic$MilleniumWeek)
cor(boxOfficePrePandemic$Gross, boxOfficePrePandemic$Year)
cor(boxOfficePrePandemic$Gross, boxOfficePrePandemic$Weekend)

# Gross is skewed. Need to generate box cox to determine transformation
boxOfficePrePandemicLinear <- lm(data = boxOfficePrePandemic, Gross ~
                                   MilleniumWeek +
                                   WinterSeason +
                                   SpringSeason +
                                   SummerSeason +
                                   HolidayWeekend +
                                   Streaming +
                                   OriginalStreaming +
                                   InternationalStreaming)

grossBoxCox <- boxcox(boxOfficePrePandemicLinear,
                      lambda = seq(-.5,.3,0.1),
                      interp = F)

boxOfficePrePandemic <- boxOfficePrePandemic %>%
  mutate(NormalizedGross = Gross^(-0.2))

summary(boxOfficePrePandemic)

hist(boxOfficePrePandemic$NormalizedGross)

cor(boxOfficePrePandemic$NormalizedGross, boxOfficePrePandemic$MilleniumWeek)
cor(boxOfficePrePandemic$NormalizedGross, boxOfficePrePandemic$Year)
cor(boxOfficePrePandemic$NormalizedGross, boxOfficePrePandemic$Weekend)

plot(boxOfficePrePandemic$NormalizedGross ~ boxOfficePrePandemic$MilleniumWeek)
boxOfficeNormalLM <- lm(data = boxOfficePrePandemic, NormalizedGross ~
                          MilleniumWeek +
                          WinterSeason +
                          SpringSeason +
                          SummerSeason +
                          HolidayWeekend +
                          Streaming +
                          OriginalStreaming +
                          InternationalStreaming)
abline(boxOfficeNormalLM,
       col = "red")
summary(boxOfficeNormalLM)

boxOfficePrePandemic <- boxOfficePrePandemic %>%
  mutate(SuccessCriteria = fitted(boxOfficeNormalLM))

boxOfficePrePandemic <- boxOfficePrePandemic %>%
  mutate(Success = 0)
for (i in 1:length(boxOfficePrePandemic$NormalizedGross)) {
  if(boxOfficePrePandemic$NormalizedGross[i] <
     boxOfficePrePandemic$SuccessCriteria[i]) {
    boxOfficePrePandemic$Success[i] <- 0
  } else{
    boxOfficePrePandemic$Success[i] <- 1
  }
}

# Turning Success into a logical
boxOfficePrePandemic$Success <- as.logical(boxOfficePrePandemic$Success)
# Turning WinterSeason into numeric
boxOfficePrePandemic$WinterSeason <- as.numeric(
  boxOfficePrePandemic$WinterSeason) - 1
# Turning SpringSeason into numeric
boxOfficePrePandemic$SpringSeason <- as.numeric(
  boxOfficePrePandemic$SpringSeason) - 1
# Turning SummerSeason into numeric
boxOfficePrePandemic$SummerSeason <- as.numeric(
  boxOfficePrePandemic$SummerSeason) - 1
# Turning FallSeason into numeric
boxOfficePrePandemic$FallSeason <- as.numeric(
  boxOfficePrePandemic$FallSeason) - 1
# Turning HolidayWeekend into numeric
boxOfficePrePandemic$HolidayWeekend <- as.numeric(
  boxOfficePrePandemic$HolidayWeekend) -1 
# Turning Streaming into numeric
boxOfficePrePandemic$Streaming <- as.numeric(
  boxOfficePrePandemic$Streaming) - 1
# Turning OriginalStreaming into numeric
boxOfficePrePandemic$OriginalStreaming <- as.numeric(
  boxOfficePrePandemic$OriginalStreaming) - 1
# Turning InternationalStreaming into numeric
boxOfficePrePandemic$InternationalStreaming <- as.numeric(
  boxOfficePrePandemic$InternationalStreaming) -1

# Summary of the boxOfficePrePandemic
summary(boxOfficePrePandemic)

# Setting Random Seed
# set.seed(370)

# Randomly splitting the data into training(75) and testing(25)
sampleSet <- sample(nrow(boxOfficePrePandemic),
                    round(nrow(boxOfficePrePandemic) * .75),
                    replace = FALSE)

# Putting 75% into training
boxOfficePrePandemicTraining <- boxOfficePrePandemic[sampleSet, ]

# Putting 25% into testing
boxOfficePrePandemicTesting <- boxOfficePrePandemic[-sampleSet, ]

# Generating the neural network
boxOfficePrePandemicNeuralNet <- neuralnet(
  formula = Success ~ WinterSeason +
    SpringSeason +
    SummerSeason +
    FallSeason +
    HolidayWeekend +
    Streaming +
    OriginalStreaming +
    InternationalStreaming,
  data = boxOfficePrePandemicTraining,
  hidden = 9,
  act.fct = "logistic",
  linear.output = FALSE)

# Displaying Neural Network numeric results
print(boxOfficePrePandemicNeuralNet$result.matrix)

# Visualizing the neural network
plot(boxOfficePrePandemicNeuralNet)

# Generating the probabilities 
boxOfficePrePandemicProbability <- compute(boxOfficePrePandemicNeuralNet, 
                                           boxOfficePrePandemicTesting)

# Displaying the probabilities 
print(boxOfficePrePandemicProbability$net.result)

# Converting the probabilities to 0 & 1
boxOfficePrePandemicPrediction <- 
  ifelse(boxOfficePrePandemicProbability$net.result > 0.5, 1, 0)

# displaying fishingCharterPrediction
print(boxOfficePrePandemicPrediction)

# Creating the confusion matrix
boxOfficePrePandemicConfusionMatrix <- table(
  boxOfficePrePandemicTesting$Success,boxOfficePrePandemicPrediction)

# Printing the confusion matrix
print(boxOfficePrePandemicConfusionMatrix)

# Calculating predictive accuracy
predictiveAccuracy <- sum(diag(boxOfficePrePandemicConfusionMatrix)) / 
  nrow(boxOfficePrePandemicTesting)

# Printing the predictive accuracy
print(predictiveAccuracy)
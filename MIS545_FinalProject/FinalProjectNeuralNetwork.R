# Peter Yeu-Shyang Yeh
# MIS 545 Section 02
# GroupProject.R

# Preliminary Setup -------------------------------------------------------
# Sets the working directory
setwd("/Users/robertallard/Desktop/MIS 545/Final Project")

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
set.seed(370)

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

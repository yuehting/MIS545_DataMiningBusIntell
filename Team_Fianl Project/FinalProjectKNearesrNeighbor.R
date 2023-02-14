# MIS 545 Final Project

# Install tidyverse packages
# install.packages("tidyverse")

# Load tidyverse libraries
library(tidyverse)
library(class)
library(corrplot)
library(olsrr)

# Set the working directroy
setwd("~/MIS 545/Fianl Project")

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

# # Coding in column of continuous week number since start of 2000
# MilleniumWeek <- c(1 : length(BoxOffice2K$Gross))
# BoxOffice2K <- cbind(BoxOffice2K, MilleniumWeek)

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

# # Plot data of interest
# plot(BoxOfficePrePandemic$Gross ~ BoxOfficePrePandemic$MilleniumWeek)

# Histogram
hist(BoxOfficePrePandemic$Gross)

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

# # Correlation check, Use MilleniumWeek since most correlated to Gross
# cor(BoxOfficePrePandemic$Gross, BoxOfficePrePandemic$MilleniumWeek)
# cor(BoxOfficePrePandemic$Gross, BoxOfficePrePandemic$Year)
# cor(BoxOfficePrePandemic$Gross, BoxOfficePrePandemic$Weekend)

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

BoxOfficeTraining <- BoxOfficePrePandemicKnn[sampleSet, ]
BoxOfficeTraingLabel <- BoxOfficePrePandemicKnnLabel[sampleSet, ]

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

# # Gross is skewed. Need to generate box cox to determine transformation
# BoxOfficePrePandemicLinear <- lm(data = BoxOfficePrePandemic, Gross ~
#                                    MilleniumWeek + 
#                                    WinterSeason +
#                                    SpringSeason +
#                                    SummerSeason +
#                                    HolidayWeekend +
#                                    Streaming +
#                                    OriginalStreaming +
#                                    InternationalStreaming)
# 
# grossBoxCox <- boxcox(boxOfficePrePandemicLinear,
#                       lambda = seq(-.5,.3,0.1),
#                       interp = F)
# 
# boxOfficePrePandemic <- boxOfficePrePandemic %>%
#   mutate(NormalizedGross = Gross^(-0.2))
# 
# summary(boxOfficePrePandemic)
# 
# hist(boxOfficePrePandemic$NormalizedGross)
# 
# cor(boxOfficePrePandemic$NormalizedGross, boxOfficePrePandemic$MilleniumWeek)
# cor(boxOfficePrePandemic$NormalizedGross, boxOfficePrePandemic$Year)
# cor(boxOfficePrePandemic$NormalizedGross, boxOfficePrePandemic$Weekend)
# 
# plot(boxOfficePrePandemic$NormalizedGross ~ boxOfficePrePandemic$MilleniumWeek)
# boxOfficeNormalLM <- lm(data = boxOfficePrePandemic, NormalizedGross ~
#                           MilleniumWeek +
#                           WinterSeason +
#                           SpringSeason +
#                           SummerSeason +
#                           HolidayWeekend +
#                           Streaming +
#                           OriginalStreaming +
#                           InternationalStreaming)
# abline(boxOfficeNormalLM,
#        col = "red")
# summary(boxOfficeNormalLM)
# 
# boxOfficePrePandemic <- boxOfficePrePandemic %>%
#   mutate(SuccessCriteria = fitted(boxOfficeNormalLM))
# 
# boxOfficePrePandemic <- boxOfficePrePandemic %>%
#   mutate(Success = 0)
# for (i in 1:length(boxOfficePrePandemic$NormalizedGross)) {
#   if(boxOfficePrePandemic$NormalizedGross[i] <
#      boxOfficePrePandemic$SuccessCriteria[i]) {
#     boxOfficePrePandemic$Success[i] <- 1
#   } else{
#     boxOfficePrePandemic$Success[i] <- 0
#   }
# }
# 
# boxOfficePrePandemic$Succes <- as.factor(boxOfficePrePandemic$Success)
# 
# summary(boxOfficePrePandemic)

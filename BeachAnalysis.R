# Installing necessary packages

p <- c("readr", "tidyverse", "corrplot")

install.packages(p)

library(readr)
# loading in the data
beach <- read.csv("BeachTan/BeachTan.csv")

library(tidyverse)

# Getting a view of the data

summary(beach)

head(beach)
source("/Users/peterfortunato/Documents/MSBA/data summary.R")
data.summary(beach)
View(beach)

# seeing which variables need to be recoded
str(beach)

# recoding the following variables to factors
beach$UIDStoreLocation <- as.factor(beach$UIDStoreLocation)
beach$Gender <- as.factor(beach$Gender)
beach$DateJoined <- as.Date(beach$DateJoined, "%m/%d/%Y")
beach$MembershipType <- as.factor(beach$MembershipType)
beach$MembershipLevel <- as.factor(beach$MembershipLevel)


str(beach)

data.summary(beach)

# Recoding some factors so they make sense
beach$Gender <- recode_factor(beach$Gender, 
                              "0" = "Female",
                              "1" = "Male")

beach$MembershipType <- recode_factor(beach$MembershipType,
                                      "0" = "Non-Member",
                                      "1" = "Monthly",
                                      "2" = "Annual")

beach$MembershipLevel <- recode_factor(beach$MembershipLevel,
                                       "0" = "Non-Member",
                                       "1" = "Fast",
                                       "2" = "Faster",
                                       "3" = "Fastest",
                                       "4" = "Instant")

# Removing variables that don't need to be in model
beach <- beach %>%
  select(-UIDClient, -DateJoined)

# Removing rows that which don't have a DateJoined (their DaysSinceJoined 
# attribute had over 10,000 days in it)
beach <- beach %>%
  filter(DaysSinceJoined < 10000)

# Removing rows with missing gender values

beach <- na.omit(beach)
str(beach)

# Sample Plots
 

ggplot(beach, aes(x = MembershipType, y = RetailRevenue)) +
  geom_col()

library(DataExplorer)

plot_bar(beach)
plot_histogram(beach)

# Understanding correlations between numeric variables

library(corrplot)

nums <- unlist(lapply(beach, is.numeric))

M<-cor(beach[,nums], use="complete.obs")
corrplot(M, method="color")

# Running a principal component analysis on the numeric variables to see
# if we can reduce dimensions

beach.nums <- beach[,nums]

pcs <- prcomp(beach.nums, scale = T)
summary(pcs)

# It appears that we can keep 81% of the variation of the numeric variables
# if we keep just four out of the six principal components

# Showing that the variance of PCA transformed data is equal to the variance
# of the original data frame (of numeric variables)

scaled.df <- as.data.frame(scale(beach.nums))
sum(var(as.data.frame(pcs$x)))
sum(apply(scaled.df, 2, var))

# Maybe I'll use this maybe I won't

# Creating dummy variables for factors in the data frame
dum1 <- model.matrix(~0 + UIDStoreLocation, data = beach)
dum1 <- as.data.frame(dum1)

dum2 <- model.matrix(~0 + Gender, data = beach)
dum2 <- as.data.frame(dum2)

dum3 <- model.matrix(~0 + MembershipType, data = beach)
dum3 <- as.data.frame(dum3)

dum4 <- model.matrix(~0 + MembershipLevel, data = beach)
dum4 <- as.data.frame(dum4)

# Adding dummy variables to the data frame

beach <- cbind(beach, dum1, dum2, dum3, dum4)

# Removing original variables plus one dummy for each factor

beach <- beach %>%
  select(-UIDStoreLocation, -Gender, -MembershipType, -MembershipLevel,
         -UIDStoreLocation10, -GenderMale, -MembershipTypeAnnual, -MembershipLevelInstant)

# Recoding each dummy variable as factors

beach$UIDStoreLocation1 <- as.factor(beach$UIDStoreLocation1)
beach$UIDStoreLocation2 <- as.factor(beach$UIDStoreLocation2)
beach$UIDStoreLocation3 <- as.factor(beach$UIDStoreLocation3)
beach$UIDStoreLocation4 <- as.factor(beach$UIDStoreLocation4)
beach$UIDStoreLocation5 <- as.factor(beach$UIDStoreLocation5)
beach$UIDStoreLocation6 <- as.factor(beach$UIDStoreLocation6)
beach$UIDStoreLocation7 <- as.factor(beach$UIDStoreLocation7)
beach$UIDStoreLocation8 <- as.factor(beach$UIDStoreLocation8)
beach$UIDStoreLocation9 <- as.factor(beach$UIDStoreLocation9)
beach$GenderFemale <- as.factor(beach$GenderFemale)
beach$`MembershipTypeNon-Member` <- as.factor(beach$`MembershipTypeNon-Member`)
beach$MembershipTypeMonthly <- as.factor(beach$MembershipTypeMonthly)
beach$MembershipLevelFast <- as.factor(beach$MembershipLevelFast)
beach$MembershipLevelFaster <- as.factor(beach$MembershipLevelFaster)
beach$MembershipLevelFastest <- as.factor(beach$MembershipLevelFastest)
beach$`MembershipLevelNon-Member` <- as.factor(beach$`MembershipLevelNon-Member`)

# Partitioning our data into training and validation
# Actually since this is just going to be an explanatory analysis and not predictive, I do not need to
# partition the data into training and validation sets.

# However, I will still be creating two models: one that explains Retail Revenue, and one that explains Upgrade Revenue
# Therefore, I need to make two data sets, one with and one without Retail Revenue

beachRetail <- beach %>%
  select(-UpgradeRevenue)

beachUpgrade <- beach %>%
  select(-RetailRevenue, -`MembershipTypeNon-Member`, -`MembershipLevelNon-Member`)

# Creating linear model 1
modelRetail <- lm(RetailRevenue ~ ., data = beachRetail)
options(scipen = 999)

summary(modelRetail)

# Creating linear model 2
modelUpgrade <- lm(UpgradeRevenue ~ ., data = beachUpgrade)
summary(modelUpgrade)

# Some initial observations:





beach1 <-read.csv("BeachTan/BeachTan.csv")

beach1$UIDStoreLocation <- recode_factor(beach$UIDStoreLocation,
                                         "1" = "A",
                                         "2" = "B",
                                         "3" = "C",
                                         "4" = "D",
                                         "5" = "E",
                                         "6" = "F",
                                         "7" = "G",
                                         "8" = "H",
                                         "9" = "I",
                                         "10" = "J")

ggplot(beach, aes(x = UIDStoreLocation, y = RetailRevenue)) +
  geom_col() +
  scale_x_discrete(limits = c("B", "J", "C", "H", "E", "I", "A", "F", "D", "G"))

aggregate(RetailRevenue~UIDStoreLocation, beach1, mean)
aggregate(RetailRevenue~MembershipLevel, beach1, sum)
aggregate(RetailRevenue~MembershipType, beach1, sum)
aggregate(UpgradeRevenue~MembershipType, beach1, sum)

summary(as.factor(beach1$UIDStoreLocation))
aggregate(RetailRevenue~UIDStoreLocation, beach1, sum)

a <- c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J")
b <- c(814, 1515, 1092, 886, 1197, 778, 848, 1282, 1034, 1595)
c <- c(45279.83, 96623.01, 63807.34, 42244.73, 57229.70, 45162.91, 13968.67, 60553.72, 55214.93, 72635.85)
d <- c/b

a
d
beach1$UIDStoreLocation <- as.factor(beach1$UIDStoreLocation)
avg <- aggregate(RetailRevenue~UIDStoreLocation, beach1, mean)


ggplot(avg, aes(x = UIDStoreLocation, y = RetailRevenue)) +
  geom_col()



#' A2 Household Spend
#' Purpose: Submission for A2
#' Camila Hennessey
#' Feb 2, 2024
 
# Libraries
library(rpart) # For recursive partitioning for classification, regression, and survival trees.
library(caret) # For data splitting, pre-processing, feature selection, model tuning using resampling.
library(dplyr) # A grammar of data manipulation, providing a consistent set of verbs that help you solve the most common data manipulation challenges.
library(MLmetrics) # For evaluating machine learning algorithms' performance metrics.
library(vtreat) # For preparing variables for predictive modeling in a statistically sound manner.
library(DataExplorer) # For simplifying exploratory data analysis.
library(ggplot2) # For creating elegant data visualisations using the Grammar of Graphics.
library(data.table) # Provides an enhanced version of data.frames, which are faster and more convenient for large datasets.
library(corrplot) # For visualizing a correlation matrix.
library(scales) # For graphical scales mapping data to aesthetics.

# WD
# Sets the working directory to a specified path where the project files are located.
setwd("~/Google Drive/MBAN/Visualizing and Analyzing with R/Visualizing and Analyzing with R/personalFiles")

# Data
# Listing all training files located in the specified directory that match the 'training' pattern.
allTrainingFiles <- list.files(path = '~/Google Drive/MBAN/Visualizing and Analyzing with R/Visualizing and Analyzing with R/Cases/A2_Household_Spend/studentTables',
                               pattern = 'training',
                               full.names = T)

# Load the training files and combine them using a left join based on 'tmpID'.
allTrainingDF <- lapply(allTrainingFiles, read.csv)
allTrainingDF <- join_all(allTrainingDF, by='tmpID', type='left')

# Testing data
# Similar to training data, listing all testing files for loading.
allTestingFiles <- list.files(path = '~/Google Drive/MBAN/Visualizing and Analyzing with R/Visualizing and Analyzing with R/Cases/A2_Household_Spend/studentTables',
                              pattern = 'testing',
                              full.names = TRUE)

# Load the testing files and combine them using a left join.
allTestingDF <- lapply(allTestingFiles, read.csv)
allTestingDF <- join_all(allTestingDF, by='tmpID', type='left')

# Prospect data
# Listing files designated as 'prospect' data for future predictions.
allProspectFiles <- list.files(path = '~/Google Drive/MBAN/Visualizing and Analyzing with R/Visualizing and Analyzing with R/Cases/A2_Household_Spend/studentTables',
                               pattern = 'prospect',
                               full.names = TRUE)

# Load the prospect files and combine them using a left join.
allProspectDF <- lapply(allProspectFiles, read.csv)
allProspectDF <- join_all(allProspectDF, by='tmpID', type='left')

# Set the seed for reproducibility
# This ensures that any random operation can be replicated in future runs.
set.seed(123)

# Train 80%/Test 20% Partitioning
# Calculates the number of rows that constitute 80% of the dataset for training.
splitPercent  <- round(nrow(allTrainingDF) %*% .8)
totalRecords <- 1:nrow(allTrainingDF)
totalRecords
# Randomly selects indices for the training set based on the split percentage.
idx          <- sample(totalRecords, splitPercent)
head(idx,15)

# Creates a training set using the selected indices and a validation set with the remaining data.
trainSet <- allTrainingDF[idx, ]
validationSet  <- allTrainingDF[-idx, ]

# Outputs the dimensions of the training and validation sets for verification.
dim(trainSet)
dim(validationSet)


# Exploratory Data Analysis (EDA)
# Generate a summary of the training set to get an overview of the data (min, max, mean, etc. for numeric columns).
summary(trainSet)

# Display the first few rows of the training set to inspect the data format and values.
head(trainSet)

# Display the structure of the training set, including data types and the number of non-NA values for each column.
str(trainSet)

# List of columns to remove
columnsToRemove <- c(
  "ResidenceHHGenderDescription",
  "EthnicDescription",
  "MosaicZ4", 
  "ReligiousContributorInHome",
  "PoliticalContributerInHome",
  "DonatesEnvironmentCauseInHome",
  "DonatesToCharityInHome",
  "DonatestoAnimalWelfare",
  "DonatestoArtsandCulture",
  "DonatestoChildrensCauses",
  "DonatestoHealthcare",
  "DonatestoInternationalAidCauses",
  "DonatestoVeteransCauses",
  "DonatestoHealthcare1",
  "DonatestoInternationalAidCauses1",
  "DonatestoWildlifePreservation",
  "DonatestoLocalCommunity",
  "FirstName","LastName",
  "TelephonesFullPhone",
  "PartiesDescription",
  "ReligionsDescription",
  "LikelyUnionMember",
  "GunOwner",
  "stateFips",
  "supportsAffordableCareAct",
  "supportsGayMarriage",
  "lat",
  "lon",
  "county",
  "supportsGunControl",
  "supportsTaxesRaise",
  "overallsocialviews",
  "DonatestoConservativeCauses",
  "DonatestoLiberalCauses",
  "BroadEthnicGroupings",
  "fips",
  "Veteran",
  "InterestinCurrentAffairsPoliticsInHousehold",
  "ReligiousMagazineInHome",
  "FinancialMagazineInHome",
  "BusinessOwner",
  "Investor",
  "HorseOwner",
  "CatOwner",
  "DogOwner",
  "OtherPetOwner",
  "HomeOffice",
  "BookBuyerInHome",
  "UpscaleBuyerInHome",
  "BuyerofAntiquesinHousehold",
  "BuyerofArtinHousehold",
  "GeneralCollectorinHousehold",
  "BooksAudioReadinginHousehold",
  "FamilyMagazineInHome",
  "FemaleOrientedMagazineInHome",
  "GardeningMagazineInHome",
  "CulinaryInterestMagazineInHome",
  "HealthFitnessMagazineInHome",
  "DoItYourselfMagazineInHome"
  
)

# Remove these columns from all the datasets
trainSet <- trainSet[ , !(names(trainSet) %in% columnsToRemove)]
validationSet <- validationSet[ , !(names(validationSet) %in% columnsToRemove)]
allProspectDF <- allProspectDF[ , !(names(allProspectDF) %in% columnsToRemove)]

# Additionally, to get a quick overview of the data I will use the DataExplorer package considering that it provides an introductory overview of the training set including distributions and missing values.

DataExplorer::plot_intro(trainSet)

# Visualizes missing data patterns in the training set, helping to identify if any columns have significant gaps.
DataExplorer::plot_missing(trainSet)

# Plots histograms for each variable in the training set to examine the distribution of values.
DataExplorer::plot_histogram(trainSet)

# I will repeat the exploratory data analysis for the validation set and all prospect data to ensure consistency in preprocessing.
DataExplorer::plot_intro(validationSet)
DataExplorer::plot_missing(validationSet)
DataExplorer::plot_histogram(validationSet)

DataExplorer::plot_intro(allProspectDF)
DataExplorer::plot_missing(allProspectDF)
DataExplorer::plot_histogram(allProspectDF)

# Re-evaluate the structure and contents of the training set after removing certain columns:
names(trainSet) # Lists all remaining column names in the training set.
str(trainSet) # Shows the updated structure of the training set.
dim(trainSet) # Displays the dimensions (number of rows and columns) of the training set.
head(trainSet) # Views the first few rows of the updated training set.
summary(trainSet) # Generates a summary of the updated training set.
unique(trainSet) # Checks for unique values in the training set, can be computationally intensive depending on data size.
is.na(trainSet) # Checks for NA values in the training set, providing a logical matrix of the same size.

# MODIFY 

# Define a vector of feature names to be used as predictors
informativeFeatures <- c(
  "tmpID",
  "PresenceOfChildrenCode",
  "ISPSA",
  "HomeOwnerRenter",
  "MedianEducationYears",
  "NetWorth",
  "Education",
  "OccupationIndustry",
  "ComputerOwnerInHome",
  "Gender",
  "Age",
  "city",
  "state",
  "HomePurchasePrice",
  "LandValue",
  "DwellingUnitSize",
  "storeVisitFrequency",
  "PropertyType",
  "EstHomeValue"
)

# Define the name of the target variable
targetName <- 'yHat'

# Design a treatment plan for data preprocessing
plan <- designTreatmentsN(dframe      = trainSet, 
                          varlist     = informativeFeatures,
                          outcomename = targetName)

# Apply the treatment plan to different data subsets
treatedTrain <- prepare(plan, trainSet)
treatedValidation <- prepare(plan, validationSet) 

treatedTest <- prepare(plan, allTestingDF) 
treatedProspects <- prepare(plan, allProspectDF) 


# Model No. 1 - Linear Model

# Fit a linear regression model with treatedTrain data
fit1 <- lm(yHat ~ ., data = treatedTrain)
summary(fit1)


# Evaluate the regression model with treatedTrain data
trainingPreds1 <- predict(fit1, treatedTrain)
trainingResults1 <-data.frame(actuals        = treatedTrain$yHat,
                             predicted      = trainingPreds1,
                             residualErrors = treatedTrain$yHat-trainingPreds1 )
head(trainingResults1)

trainRMSE1 <- MLmetrics::RMSE(trainingResults1$predicted, 
                              trainingResults1$actuals)
head(trainRMSE1)


# Fit a linear regression model with treatedValidated data for validation
fit1.1 <- lm(yHat ~ ., data = treatedValidation)
summary(fit1.1)

# Evaluate the regression model with treatedValidated data for validation

ValidationPreds1 <- predict(fit1.1, treatedValidation)
ValidationResults1 <-data.frame(actuals        = treatedValidation$yHat,
                             predicted      = ValidationPreds1,
                             residualErrors = treatedValidation$yHat-ValidationPreds1 )
head(ValidationResults1)

ValidationRMSE1 <- MLmetrics::RMSE(ValidationResults1$predicted, 
                             ValidationResults1$actuals)

head(ValidationRMSE1)

# Evaluate the regression model with treatedTest data to test

fit1.3 <- lm(yHat ~ ., data = treatedTest)
summary(fit1.3)

TestPreds1 <- predict(fit1.3, treatedTest)
TestResults1 <-data.frame(actuals        = treatedTest$yHat,
                                predicted      = TestPreds1,
                                residualErrors = treatedTest$yHat-TestPreds1 )
head(TestResults1)

TestRMSE1 <- MLmetrics::RMSE(TestResults1$predicted, 
                                   TestResults1$actuals)

head(TestRMSE1)

# Model No. 2 - Decision Tree

# More extensive set of features for a different model approach
informativeFeatures2 <- c(
    "tmpID",
    "PresenceOfChildrenCode",
    "ISPSA",
    "HomeOwnerRenter",
    "MedianEducationYears",
    "NetWorth",
    "Investor",
    "BusinessOwner",
    "Education",
    "OccupationIndustry",
    "HorseOwner",
    "CatOwner",
    "DogOwner",
    "OtherPetOwner",
    "HomeOffice",
    "BookBuyerInHome",
    "UpscaleBuyerInHome",
    "BuyerofAntiquesinHousehold",
    "BuyerofArtinHousehold",
    "GeneralCollectorinHousehold",
    "BooksAudioReadinginHousehold",
    "ComputerOwnerInHome",
    "Gender",
    "Age",
    "city",
    "state",
    "HomePurchasePrice",
    "LandValue",
    "DwellingUnitSize",
    "storeVisitFrequency",
    "PropertyType",
    "EstHomeValue",
    "FamilyMagazineInHome",
    "FemaleOrientedMagazineInHome",
    "GardeningMagazineInHome",
    "CulinaryInterestMagazineInHome",
    "HealthFitnessMagazineInHome",
    "DoItYourselfMagazineInHome"
  )
  
targetName2 <- 'yHat'


plan2 <- designTreatmentsN(
  dframe      = trainSet, 
  varlist     = informativeFeatures2,  # Corrected variable name
  outcomename = targetName2
)

# Apply the plan to all sections of the data
treatedTrain2 <- prepare(plan2, trainSet)
treatedValidation2 <- prepare(plan2, validationSet)


fit2 <- train(yHat ~., #formula based
              data = treatedTrain2, #data in
              #"recursive partitioning (trees)
              method = "rpart", 
              #Define a range for the CP to test
              tuneGrid = data.frame(cp = c(0.0001, 0.001,0.005, 0.01, 0.05, 0.07, 0.1)), 
              #ie don't split if there are less than 1 record left and only do a split if there are at least 2+ records
              control = rpart.control(minsplit = 1, minbucket = 2))


# Examine
fit2

# Plot the CP Accuracy Relationship to adust the tuneGrid inputs
plot(fit2)

# Plot a pruned tree
prp(fit2$finalModel, extra = 1)

# Evaluate the decision tree 
trainingPreds2 <- predict(fit2, treatedTrain2)

trainingResults2 <- data.frame(
  actuals = treatedTrain2$yHat,
  predicted = trainingPreds2,
  residualErrors = treatedTrain2$yHat - trainingPreds2
)

head(trainingResults2)

trainRMSE2 <- MLmetrics::RMSE(trainingResults2$predicted, 
                              trainingResults2$actuals)

head(trainRMSE2)

fit2.1 <- train(yHat ~., #formula based
              data = treatedValidation2, #data in
              #"recursive partitioning (trees)
              method = "rpart", 
              #Define a range for the CP to test
              tuneGrid = data.frame(cp = c(0.0001, 0.001,0.005, 0.01, 0.05, 0.07, 0.1)), 
              #ie don't split if there are less than 1 record left and only do a split if there are at least 2+ records
              control = rpart.control(minsplit = 1, minbucket = 2))


# Examine
fit2.1

# Plot the CP Accuracy Relationship to adust the tuneGrid inputs
plot(fit2.1)

# Plot a pruned tree
prp(fit2.1$finalModel, extra = 1)


ValidationPreds2 <- predict(fit2.1, treatedValidation2)
ValidationResults2 <-data.frame(actuals        = treatedValidation2$yHat,
                               predicted      = ValidationPreds2,
                               residualErrors = treatedValidation2$yHat-ValidationPreds2 )
head(ValidationResults2)

ValidationRMSE2 <- MLmetrics::RMSE(ValidationResults2$predicted, 
                                  ValidationResults2$actuals)

head(ValidationRMSE2)

#test 

fit2.3 <- train(yHat ~., #formula based
                data = treatedTest, #data in
                #"recursive partitioning (trees)
                method = "rpart", 
                #Define a range for the CP to test
                tuneGrid = data.frame(cp = c(0.0001, 0.001,0.005, 0.01, 0.05, 0.07, 0.1)), 
                #ie don't split if there are less than 1 record left and only do a split if there are at least 2+ records
                control = rpart.control(minsplit = 1, minbucket = 2))


# Examine
fit2.3

# Plot the CP Accuracy Relationship to adust the tuneGrid inputs
plot(fit2.3)

# Plot a pruned tree
prp(fit2.3$finalModel, extra = 1)

TestPreds2 <- predict(fit2.3, treatedTest)
TestResults2 <-data.frame(actuals        = treatedTest$yHat,
                          predicted      = TestPreds1,
                          residualErrors = treatedTest$yHat-TestPreds1 )
head(TestResults2)

TestRMSE2 <- MLmetrics::RMSE(TestResults2$predicted, 
                             TestResults2$actuals)

head(TestRMSE2)


# Model No. 3 - Linear Model

# Define a vector of feature names to be used as predictors different from the first model

informativeFeatures3 <- c("HomeOwnerRenter", "MedianEducationYears", "NetWorth", 
                             "Investor", "BusinessOwner", "HomePurchasePrice", 
                             "LandValue", "DwellingUnitSize", 
                             "storeVisitFrequency", "EstHomeValue")

                                          
targetName3 <- 'yHat'

plan3 <- designTreatmentsN(dframe      = trainSet, 
                          varlist     = informativeFeatures3,
                          outcomename = targetName)

# Apply the plan to all sections of the data
treatedTrain3 <- prepare(plan, trainSet)
treatedValidation3 <- prepare(plan, validationSet)

# Fit a linear model with all variables

fit3 <- lm(yHat ~ ., data = treatedTrain3)
summary(fit3)

fit3.1 <- lm(yHat ~ ., data = treatedValidation3)
summary(fit3.1)

# Evaluate the model 

trainingPreds3 <- predict(fit3, treatedTrain3)

trainingResults3 <-data.frame(actuals        = treatedTrain3$yHat,
                             predicted      = trainingPreds3,
                             residualErrors = treatedTrain3$yHat-trainingPreds3 )
head(trainingResults3)

trainRMSE3 <- MLmetrics::RMSE(trainingResults3$predicted, 
                             trainingResults3$actuals)

head(trainRMSE3)


ValidationPreds3 <- predict(fit3.1, treatedValidation3)

ValidationResults3 <-data.frame(actuals        = treatedValidation3$yHat,
                               predicted      = ValidationPreds3,
                               residualErrors = treatedValidation3$yHat-ValidationPreds3 )
head(ValidationResults3)

ValidationRMSE3 <- MLmetrics::RMSE(ValidationResults3$predicted, 
                                  ValidationResults3$actuals)
head(ValidationRMSE3)


fit3.2 <- lm(yHat ~ ., data = treatedTest)
summary(fit3.2)

TestPreds3 <- predict(fit3.2, treatedTest)
TestResults3 <-data.frame(actuals        = treatedTest$yHat,
                          predicted      = TestPreds3,
                          residualErrors = treatedTest$yHat-TestPreds3 )
head(TestResults3)

TestRMSE3 <- MLmetrics::RMSE(TestResults3$predicted, 
                             TestResults3$actuals)

head(TestRMSE3)

# Comparing results

# Print RMSE scores for comparison
cat("Training RMSE (Model 1 - Linear):", trainRMSE1, "\n") # From fit model on training data
cat("Validation RMSE (Model 1 - Linear):", ValidationRMSE1, "\n") # From fit model on validation data
cat("Test RMSE (Model 1 - Linear):", TestRMSE1, "\n") # From fit model on test data

cat("Training RMSE (Model 2 - Tree):", trainRMSE2, "\n") # From fit2 model (tree) on training data
cat("Validation RMSE (Model 2 - Tree):", ValidationRMSE2, "\n") # From fit2 model (tree) on validation data
cat("Tesr RMSE (Model 2 - Tree):", TestRMSE2, "\n") # From fit2 model (tree) on test data


cat("Training RMSE (Model 3 - Linear):", trainRMSE3, "\n") # RMSE for fit3 on training data
cat("Validation RMSE (Model 3 - Linear):", ValidationRMSE3, "\n") # RMSE for fit3 on validation data
cat("Test RMSE (Model 3 - Linear):", TestRMSE3, "\n") # RMSE for fit3 on test data


# The best model based on RMSE variability
bestModel <- fit1 

# Make predictions on the test set using the best model
bestModelTestPredictions <- predict(bestModel, treatedTest)

# Make predictions on the prospect set using the best model
prospectPredictions <- predict(bestModel, treatedProspects)

# Append predictions to the original prospect dataset and export
allProspectDF$Predictions <- prospectPredictions
write.csv(allProspectDF, "ProspectPredictions.csv", row.names = FALSE)

# End

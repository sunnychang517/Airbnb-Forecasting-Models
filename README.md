# Airbnb Forecast Rental Prices



library(tidyverse)
library(leaps)
library(caret)
library(ggthemes)
library(glmnet)

analysisData = read.csv("pathway", head = TRUE)

scoringData = read.csv("pathway", head = TRUE)

namevector <- c("price")
scoringData[,namevector] <- 0
scoringData <- scoringData %>%
  select(id,price, everything())
all_equal(analysisData, scoringData, convert = TRUE)

analysisData$zipcode <- as.character(analysisData$zipcode)
scoringData$zipcode <- as.character(scoringData$zipcode)

analysisData$host_is_superhost <- as.logical(analysisData$host_is_superhost)
analysisData$host_has_profile_pic <- as.logical(analysisData$host_has_profile_pic)
analysisData$host_identity_verified <- as.logical(analysisData$host_identity_verified)
analysisData$instant_bookable <- as.logical(analysisData$instant_bookable)
analysisData$require_guest_profile_picture <- as.logical(analysisData$require_guest_profile_picture)
analysisData$require_guest_phone_verification <- as.logical(analysisData$require_guest_phone_verification)
analysisData$is_location_exact <- as.logical(analysisData$is_location_exact)

all_equal(analysisData, scoringData, convert = TRUE)

set.seed(5656)
ksplit <- createDataPartition(y = analysisData$price, p=.7, list=F, groups=50)
train <- analysisData[ksplit,]
test <- analysisData[-ksplit,]
nrow(train) 
nrow(test)

train$train_test_score <- "train"
test$train_test_score <- "test"
scoringData$train_test_score <- "score"
baseData <- bind_rows(train,test,scoringData)

baseData$bed_type <- factor(baseData$bed_type)
baseData$property_type <- factor(baseData$property_type)
baseData$instant_bookable <- factor(baseData$instant_bookable)

#Remove Missing Values
library(mice)
baseData$bathrooms <- as.numeric(baseData$bathrooms)
summary(baseData$bathrooms)

baseData$cleaning_fee <- as.numeric(baseData$cleaning_fee)
summary(baseData$cleaning_fee)

baseData$accommodates <- as.numeric(baseData$accommodates)
is.numeric(baseData$accommodates)

summary(baseData$bathrooms)
summary(baseData$accomodates)
summary(baseData$cleaning_fee)

table(is.na(train$bathrooms))
table(is.na(train$accomodates))
table(is.na(train$cleaning_fee))

baseData$accommodates <- ifelse(is.na(baseData$accomodates),mean(baseData$accomodates,na.rm = TRUE), baseData$accomodates)
baseData$bathrooms <- ifelse(is.na(baseData$bathrooms),mean(baseData$bathrooms,na.rm = TRUE), baseData$bathrooms)
baseData$cleaning_fee <- ifelse(is.na(baseData$cleaning_fee),mean(baseData$cleaning_fee,na.rm = TRUE), baseData$cleaning_fee)
baseData$security_deposit <- ifelse(is.na(baseData$security_deposit),mean(baseData$security_deposit,na.rm = TRUE), baseData$security_deposit)
train$bathrooms <- ifelse(is.na(train$bathrooms),mean(train$bathrooms,na.rm = TRUE), train$bathrooms)
train$accommodates <- ifelse(is.na(train$accommodates),mean(train$accommodates,na.rm = TRUE), train$accommodates)
test$bathrooms <- ifelse(is.na(test$bathrooms),mean(test$bathrooms,na.rm = TRUE), test$bathrooms)
test$accommodates <- ifelse(is.na(test$accommodates),mean(test$accommodates,na.rm = TRUE), test$accommodates)
test$cleaning_fee <- ifelse(is.na(test$cleaning_fee),mean(test$cleaning_fee,na.rm = TRUE), test$cleaning_fee)
train$cleaning_fee <- ifelse(is.na(train$cleaning_fee),mean(train$cleaning_fee,na.rm = TRUE), train$cleaning_fee)
test$security_deposit <- ifelse(is.na(test$security_deposit),mean(test$security_deposit,na.rm = TRUE), test$security_deposit)
train$security_deposit <- ifelse(is.na(train$security_deposit),mean(train$security_deposit,na.rm = TRUE), train$security_deposit)


scoringData$bathrooms <- ifelse(is.na(scoringData$bathrooms),mean(scoringData$bathrooms,na.rm = TRUE), scoringData$bathrooms)
scoringData$accommodates <- ifelse(is.na(scoringData$accommodates),mean(scoringData$accommodates,na.rm = TRUE), scoringData$accommodates)
scoringData$cleaning_fee <- ifelse(is.na(scoringData$cleaning_fee),mean(scoringData$cleaning_fee,na.rm = TRUE), scoringData$cleaning_fee)

summary(baseData$accommodates)
summary(baseData$cleaning_fee)
summary(baseData$bathrooms)

table(is.na(train$bathrooms))
table(is.na(train$accomodates))
table(is.na(train$cleaning_fee))

baseData %>% # Run feature selection first to prevent errors
  count(property_type, train_test_score) %>% 
  group_by(property_type) %>% 
  pivot_wider(names_from=train_test_score, values_from=c(n)) %>% 
  filter(is.na(train) || is.na(test) || is.na(score)) %>%
  mutate(score = coalesce(score, 0)) %>%
  mutate(test = coalesce(test, 0)) %>%
  mutate(train = coalesce(train, 0))

baseData %>% 
  count(property_type, train_test_score) %>% 
  group_by(property_type) %>% 
  pivot_wider(names_from=train_test_score, values_from=c(n)) %>% 
  filter(is.na(train) || is.na(test) || is.na(score)) %>% 
  filter(property_type == 'Cabin')

baseData %>% 
  count(property_type, train_test_score) %>% 
  group_by(property_type) %>% 
  pivot_wider(names_from=train_test_score, values_from=c(n)) %>% 
  filter(is.na(train) || is.na(test) || is.na(score)) %>% 
  filter(property_type == 'Train')

baseData %>% 
  count(property_type, train_test_score) %>% 
  group_by(property_type) %>% 
  pivot_wider(names_from=train_test_score, values_from=c(n)) %>% 
  filter(is.na(train) || is.na(test) || is.na(score)) %>% 
  filter(property_type == 'Farm stay')

baseData %>% 
  count(property_type, train_test_score) %>% 
  group_by(property_type) %>% 
  pivot_wider(names_from=train_test_score, values_from=c(n)) %>% 
  filter(is.na(train)) %>%
  mutate(score = coalesce(score, 0)) %>%
  mutate(test = coalesce(test, 0)) %>%
  mutate(train = coalesce(train, 0))

baseData %>% 
  count(property_type, train_test_score) %>% 
  group_by(property_type) %>% 
  pivot_wider(names_from=train_test_score, values_from=c(n))

pt <- baseData %>%
  select(property_type)  %>% 
  mutate(property_type_char = as.character(property_type)) %>%
  mutate(
    property_type_upd = case_when( 
      property_type_char == "Barn" ~ "Earth house",
      property_type_char == "Dorm" ~ "Hostel",
      property_type_char == "Farm stay" ~ "Earth house", 
      property_type_char == "Yurt" ~ "Earth house",
      property_type_char == "In-law" ~ "Apartment",
      property_type_char == "Bus" ~ "Earth house",
      property_type_char == "Train" ~ "Resort",
      TRUE ~ property_type_char))

pt

baseData$property_type_upd <- as.factor(pt$property_type_upd)

baseData %>% 
  count(property_type_upd, train_test_score) %>% 
  group_by(property_type_upd) %>% 
  pivot_wider(names_from=train_test_score, values_from=c(n)) %>% 
  filter(is.na(train)) %>%
  mutate(score = coalesce(score, 0)) %>%
  mutate(test = coalesce(test, 0)) %>%
  mutate(train = coalesce(train, 0))

train <- baseData  %>% 
  filter(train_test_score == "train")
test <- baseData  %>% 
  filter(train_test_score == "test")
score <- baseData  %>% 
  filter(train_test_score == "score")

nrow(analysisData); nrow(train); nrow(test); nrow(score);

library(caTools)
library(lattice)
library(ggplot2)
library(ISLR)
library(dplyr)
library(caret)

ncol(analysisData)

#Tuned Ranger #189.1483
trControl=trainControl(method="cv",number=5) #189.1483
tuneGrid = expand.grid(mtry=1:3, 
                       splitrule = c('variance','extratrees','maxstat'), 
                       min.node.size = c(2,5,10,15,20,25))
set.seed(5656) 
cvModel = train(price~cleaning_fee+accommodates+bathrooms,
                data=train,
                method="ranger",
                num.trees=1000,
                trControl=trControl,
                tuneGrid=tuneGrid )
cv_forest_ranger = ranger(price~cleaning_fee+accommodates+bathrooms,
                          data=train,
                          num.trees = 1000, 
                          mtry=cvModel$bestTune$mtry, 
                          min.node.size = cvModel$bestTune$min.node.size, 
                          splitrule = cvModel$bestTune$splitrule)
pred_tuned = predict(cv_forest_ranger, data =scoringData, num.trees = 1000)
rmse_cv_forest_ranger = sqrt(mean((pred_tuned$predictions-scoringData$price)^2))
rmse_cv_forest_ranger



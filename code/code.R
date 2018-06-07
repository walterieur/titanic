#------ Titanic -----# 
####Import####
setwd("/Users/walterklaschka/.kaggle/competitions/titanic")
gender <- read.csv("gender_submission.csv", header = T,na.strings=c("","NA"))
train <- read.csv("train.csv",na.strings=c("","NA"))
test <- read.csv("test.csv",na.strings=c("","NA"))
library(tidyr)
library(stringr)
library(dplyr)
library(mlbench)
library(caret)
library(arules)

####Visualisation####
str(train)
hist(train$Age)
####Data Cleaning####
#Column Type
train$Survived <- as.factor(train$Survived)
train$Pclass <- as.factor(train$Pclass)

#Split Columns
train <- separate(data = train, col = Name, into = c("Surname" , "Name"), sep = ",", remove = T)# seperate name and surname
train$Name <- gsub("[^[:alnum:][:blank:].]","",train$Name)# remove symbols and such apart from .
train$Name[514] <-  "Mrs. Martin Elizabeth Barrett"# girl has two . in her name [LOL]
train <- separate(data = train, col = Name, into = c("Title" , "Name"), sep = "\\.", remove = T)# seperate Title and Name
train$Title <- gsub("[^[:alnum:]]","",train$Title)# remove symbols and such apart from .
train$Cabin <- str_sub(train$Cabin, 1,1)
train$CabinType <- as.factor(train$CabinType)
train$Title <- as.factor(train$Title)

#NAs
allNA <- data.frame(summary(is.na(train)))# investigate all data for NA: Age 177 missing values
train$Embarked <- as.character(train$Embarked)# make Embarkation as char
table(train$Embarked)# most embarkations from port S
train$Embarked[is.na(train$Embarked)] <- "S"# apply port S to NA
train$Embarked <- as.factor(train$Embarked)


####Feature Engineering####
train <- rename(train, CabinType = Cabin)
train$Cabin <- !is.na(train$Cabin)
train$CabinType[is.na(train$CabinType)] <- "Z"
train$CabinType <- as.factor(train$CabinType)# make factor
train$Famsize <- as.factor(train$SibSp + train$Parch)# add family_size column
train$SibSp <- as.factor(train$SibSp)#make factor
train$Parch <- as.factor(train$Parch)#make factor
train$Fare.n <- round((train$Fare - min(train$Fare))/(max(train$Fare) - min(train$Fare)),digits = 5)# add relative ticket score. 0 = lowest fare, 1 = highest fare, 0.XX
train$Fare.n <- discretize(train$Fare.n,categories=10)# discretize fare.n for classification
# add solo travel column. 0 = not solo, 1= solo ---- problem -> test data required
# add married column. 0 = not married, 1 = married
#for (i in which(allData$Title!='man' & allData$Surname=='noGroup')) 
#allData$Surname[i] = allData$Surname[allData$Ticket==allData$Ticket[i]][1]

####Feature Selection####
train.r <- train
train.r$Age <- NULL
train.r$Surname <- NULL
train.r$Name <- NULL
train.r$Ticket <- NULL
train.r$Fare <- NULL

#automated
auto.ctrl <- rfeControl(functions=rfFuncs, method="cv", number=10)
auto.feat <- rfe(train.r[,3:ncol(train.r)], train.r[,2],rfeControl = ctrl1)
print(auto.feat)

####Prediction Model####

####Output####

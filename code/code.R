#------ Titanic -----# 
####Import####
setwd("/Users/walterklaschka/.kaggle/competitions/titanic")
gender <- read.csv("gender_submission.csv", header = T)
train <- read.csv("train.csv")
test <- read.csv("test.csv")
library(tidyr)
library(stringr)

####Visualisation####
str(train)
hist(train$Age)
####Data Cleaning####
#Column Type
train$Sex <- as.integer(train$Sex)# make Sex as integer
train$Age <- as.integer(round(train$Age,digits = 0))# make Age as integer
train$Embarked <- as.integer(train$Embarked)# make Embarkation as integer

#Split Columns
train <- separate(data = train, col = Name, into = c("Surname" , "Name"), sep = ",", remove = T)# seperate name and surname
train$Name <- gsub("[^[:alnum:][:blank:].]","",train$Name)# remove symbols and such apart from .
train$Name[514] <-  "Mrs. Martin Elizabeth Barrett"# girl has two . in her name [LOL]
train <- separate(data = train, col = Name, into = c("Title" , "Name"), sep = "\\.", remove = T)# seperate Title and Name
train$Cabin <- str_sub(train$Cabin, 1,1)

#NAs
allNA <- data.frame(summary(is.na(train)))# investigate all data for NA: Age 177 missing values
train$Age[is.na(train$Age)] <- as.integer(round(median(train$Age, na.rm = T),digits = 0))# apply median to Age NAs
train$Embarked[train$Embarked == 1] <- median(train$Age, na.rm = T)# apply median to unknown Embarkation Port
train$Embarked[train$Embarked == 2] <- 1# change value range 2-4 to 1-3
train$Embarked[train$Embarked == 3] <- 2
train$Embarked[train$Embarked == 4] <- 3

#Normalize Fares, PassengerID
train$Fare.n <- round((train$Fare - min(train$Fare))/(max(train$Fare) - min(train$Fare)),digits = 5)

####Summary of Data Set Features####
#Sex: 1 = female, 2 = male
#Embarked: 1 = Cherbourg, 2 = Queenstown, 3 = Southampton



####Feature Engineering####
# add binary cabin column. 0 = no cabin, 1 = cabin
# add family_size column. pyhton: df['Family_Size']=df['SibSp']+df['Parch']
# add relative ticket score. 0 = lowest fare, 1 = highest fare, 0.XX
# add solo travel column. 0 = not solo, 1= solo
# add married column. 0 = not married, 1 = married
#for (i in which(allData$Title!='man' & allData$Surname=='noGroup')) 
#allData$Surname[i] = allData$Surname[allData$Ticket==allData$Ticket[i]][1]

####Prediction Model####

####Output####

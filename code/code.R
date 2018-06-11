#------ Titanic -----# 
####Import####
setwd("/Users/walterklaschka/.kaggle/competitions/titanic")
validation <- read.csv("gender_submission.csv", header = T,na.strings=c("","NA"))
train <- read.csv("train.csv",na.strings=c("","NA"))
train$Data <- as.factor("train")
test <- read.csv("test.csv",na.strings=c("","NA"))
test$Data <- as.factor("test")
test$Survived <- NA
data <- rbind(train,test)
library(tidyr)
library(stringr)
library(dplyr)
library(mlbench)
library(caret)
library(arules)
library(magrittr)

####Visualisation####
str(data)
hist(data$Age)
####Data Cleaning####
#Column Type
data$Survived <- as.factor(data$Survived)
data$Pclass <- as.factor(data$Pclass)

#Split Columns
data <- separate(data = data, col = Name, into = c("Surname" , "Name"), sep = ",", remove = T)# seperate name and surname
data$Name <- gsub("[^[:alnum:][:blank:].]","",data$Name)# remove symbols and such apart from .
data$Name[514] <-  "Mrs. Martin Elizabeth Barrett"# girl has two . in her name [LOL]
data <- separate(data = data, col = Name, into = c("Title" , "Name"), sep = "\\.", remove = T)# seperate Title and Name
data$Title <- gsub("[^[:alnum:]]","",data$Title)# remove symbols and such apart from .
data$Cabin <- str_sub(data$Cabin, 1,1)
data$Title <- as.factor(data$Title)

#NAs
allNA <- data.frame(summary(is.na(data)))
data$Fare[is.na(data$Fare)] <- median(data$Fare, na.rm = T)#apply median to 1 missing value
data$Embarked <- as.character(data$Embarked)# make Embarkation as char
table(data$Embarked)# most embarkations from port S
data$Embarked[is.na(data$Embarked)] <- "S"# apply port S to NA
data$Embarked <- as.factor(data$Embarked)


####Feature Engineering####
data <- rename(data, CabinType = Cabin)
data$Cabin <- !is.na(data$Cabin)
data$CabinType[is.na(data$CabinType)] <- "Z"
data$CabinType <- as.factor(data$CabinType)# make factor
data$Famsize <- as.factor(data$SibSp + data$Parch)# add family_size column
data$SibSp <- as.factor(data$SibSp)#make factor
data$Parch <- as.factor(data$Parch)#make factor
data$Fare.n <- round((data$Fare - min(data$Fare))/(max(data$Fare) - min(data$Fare)),digits = 5)# add relative ticket score. 0 = lowest fare, 1 = highest fare, 0.XX
data$Fare.n <- discretize(data$Fare.n,categories=10)# discretize fare.n for classification
# add solo travel column. 0 = not solo, 1= solo ---- problem -> test data required
# add married column. 0 = not married, 1 = married
#for (i in which(allData$Title!='man' & allData$Surname=='noGroup')) 
#allData$Surname[i] = allData$Surname[allData$Ticket==allData$Ticket[i]][1]

####Feature Selection####
#data$Age <- NULL
data$Surname <- NULL
data$Name <- NULL
data$Ticket <- NULL
data$Fare <- NULL

#### Prediction of AGE NA ####
data$Age %<>% as.integer() %<>% discretize(categories = 8, ordered = T)
data <- data[,c(1,2,6,3,4,5,7:ncol(data))]
data.naomit <- data[ - which(is.na(data$Age)), ]
set.seed(123)
age.part <- createDataPartition(data.naomit$Age, p = 0.6, times = 1, list = F)
age.train <- data.naomit[age.part,]
age.test <- data.naomit[- age.part,]

set.seed(123)
age.auto.ctrl <- rfeControl(functions=rfFuncs, method="cv", number=10)
age.feat <- rfe(age.train[,4:ncol(age.train)], age.train[,3],rfeControl = age.auto.ctrl)
print(age.feat)
age.var <- c("Age","Title","Pclass","Parch","Fare.n")

#### to do get knn working ####
set.seed(123)
age.knn <- train(Age~., data = select(age.train, age.var), 
                 method = "knn", trControl = trainControl(method = "cv",number = 5), 
                 preProcess= c("center", "scale")
                 )
age.knn
age.validation <- subset(data[,3], data$Data == "test")
predict(age.validation)
#apply 

#automated
set.seed(123)
auto.ctrl <- rfeControl(functions=rfFuncs, method="cv", number=10)
auto.feat <- rfe(data.r[,3:ncol(data.r)], data.r[,2],rfeControl = auto.ctrl)
print(auto.feat)

####Prediction Model####

####Output####

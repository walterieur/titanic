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
library(randomForest)

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
# age is hard to predict based on current data -- will get rid of column later

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
data$Surname <- NULL
data$Name <- NULL
data$Ticket <- NULL
data$Fare <- NULL
data$Age %<>% as.integer() %<>% discretize(categories = 8, ordered = T)
data <- data[,c(1,2,6,3,4,5,7:ncol(data))]

#### Prediction of AGE NA #### 
#---> Accuracy 30%, not suitable
# data.naomit <- data[ - which(is.na(data$Age)), ]
# set.seed(123)
# age.part <- createDataPartition(data.naomit$Age, p = 0.8, times = 1, list = F)
# age.train <- data.naomit[age.part,]
# age.test <- data.naomit[- age.part,]
# 
# set.seed(123)
# age.auto.ctrl <- rfeControl(functions=rfFuncs, method="cv", number=10)
# age.feat <- rfe(age.train[,4:ncol(age.train)], age.train[,3],rfeControl = age.auto.ctrl)
# print(age.feat)
# age.var <- c("Age","Title","Pclass","Parch","Fare.n")
# 
# set.seed(123)
# age.knn <- train(Age~., data = select(age.train, age.var),
#                  method = "knn", trControl = trainControl(method = "cv",number = 5),
#                  preProcess= c("center", "scale")
#                  )
# age.knn

#### Auto Feature Prediction ####
data$Age <- NULL

auto.data <- data[which(data$Data %in% "train"),]# of total data set only use "train data"

set.seed(123)
auto.part <- createDataPartition(auto.data$Survived, p = 0.8, times = 1, list = F)# create 80/20 train test size
auto.train <- auto.data[auto.part,]# apply 80% train size
auto.test <- auto.data[-auto.part,]# apply 20% test size

set.seed(123)
auto.ctrl <- rfeControl(functions=rfFuncs, method="cv", number=10)# set control settings
auto.feat <- rfe(auto.train[,3:ncol(auto.train)], auto.train[,2], rfeControl = auto.ctrl)# run the auto model
print(auto.feat)
# Variables Accuracy  Kappa     AccuracySD  KappaSD Selected
# 4        0.7856     0.5384    0.04632     0.11270         
# 8        0.8206     0.6179    0.04006     0.08568 *
# 11       0.8122     0.5993    0.04009     0.08699 

round(postResample(predict(auto.feat, auto.test), auto.test$Survived),digits = 3)# results
# Accuracy    Kappa 
# 0.797       0.584 

auto.var <- c("Survived","Title","Fare.n","Pclass","Sex","Famsize","Data")# select top 5 variables
p.data <- data[, auto.var]


####Prediction Model####
train <- p.data[which(p.data$Data %in% "train"),]#update train data
test <- p.data[which(p.data$Data %in% "test"),]#update test data
validation$Survived <- as.factor(validation$Survived)


set.seed(170)
p.knn <- train(Survived~., data = train[,1:6], method = "knn", trControl = trainControl(method = "repeatedcv",repeats = 5),
               preProcess = c("scale"), tuneLength = 20)
plot(p.knn)
set.seed(170)
res.knn <- c(round(postResample(predict(p.knn,test),validation$Survived),digits = 3))
# Accuracy    Kappa 
# 0.935     0.863 

set.seed(170)
p.svm <- train(Survived~., data = train[,1:6], method = "svmLinear", trControl = trainControl(method = "cv",repeats = 5))
res.svm <- c(round(postResample(predict(p.svm,test),validation$Survived),digits = 3))
# Accuracy    Kappa 
# 0.950    0.893

set.seed(170)
p.rfor <- randomForest(Survived~., data = train[,1:6], trControl = trainControl(method = "cv",repeats = 5))
res.rfor <- c(round(postResample(predict(p.rfor,test),validation$Survived),digits = 3))
# Accuracy    Kappa 
# 0.940    0.873 
#### Result: Use SVM ####

####Output####
final.data <- data
final.data[which(final.data$Data %in% "test"),]$Survived <- predict(p.svm,test)
final.data$Data <- NULL
final.data$Survived <- as.integer(final.data$Survived)

table_comp <- data.frame(rbind(res.knn,res.svm,res.rfor))
colnames(table_comp) <- c("Accuracy","Kappa")
rownames(table_comp) <- c("K-NN","SVM Linear","Random Forest")
table_comp$Accuracy <- table_comp$Accuracy*100
table_comp# comparison table for algorithms run

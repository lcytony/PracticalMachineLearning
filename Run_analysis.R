library(data.table)
library(dplyr)
library(caret)
library(randomForest)
fileUrl="https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
download.file(fileUrl,destfile="e:/uconn/academic/coursera/Practical Machine Learning/ProjectTrain.csv")
fileUrl="https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
download.file(fileUrl,destfile="e:/uconn/academic/coursera/Practical Machine Learning/ProjectTest.csv")
Train<-read.csv("e:/uconn/academic/coursera/Practical Machine Learning/ProjectTrain.csv")
Test<-read.csv("e:/uconn/academic/coursera/Practical Machine Learning/ProjectTest.csv")
Missing <- sapply(Test, function (x) any(is.na(x) | x == ""))
Predictor <- names(Test)[!Missing & grepl("belt|arm|dumbbell|forearm", names(Missing))]
Predictor
Train<-Train[,c("classe",Predictor)]
inTrain<-createDataPartition(y=Train$classe,p=0.7,list=FALSE)
training<-Train[inTrain,]
testing<-Train[-inTrain,]
M<-abs(cor(training[,-1]))
diag(M)<-0
which(M>0.8,arr.ind=T)
PreObj<-preProcess(training[,-1])
trainingSD<-predict(PreObj,training[,-1])
nzv<-nearZeroVar(trainingSD,saveMetrics=TRUE)
preProc<-preProcess(trainingSD,method="pca",thresh=0.8)
trainingPC<-predict(preProc,trainingSD)
modFit<-randomForest(training$classe~.,data=trainingPC)
pred<-predict(modFit,trainingPC,type="class")
training$predictRight<-pred==training$classe
table(pred,training$classe)
testingSD<-predict(PreObj,testing[,-1])
testingPC<-predict(preProc,testingSD)
predTest<-predict(modFit,testingPC,type="class")
testing$predictRight<-predTest==testing$classe
table(predTest,testing$classe)

########### test #########
Test<-Test[,Predictor]
testSD<-predict(PreObj,Test)
testPC<-predict(preProc,testSD)
answer<-predict(modFit,testPC,type="class")

##Practical Machine Learning Project
###Devide train data into training data and testing data

```r
inTrain<-createDataPartition(y=Train$classe,p=0.7,list=FALSE)
training<-Train[inTrain,]
testing<-Train[-inTrain,]
```
###Checking correlation between variable and listed highly correlated data

```r
M<-abs(cor(training[,-1]))
diag(M)<-0
which(M>0.8,arr.ind=T)
```

```
##                  row col
## yaw_belt           3   1
## total_accel_belt   4   1
## accel_belt_y       9   1
## accel_belt_z      10   1
## accel_belt_x       8   2
## magnet_belt_x     11   2
## roll_belt          1   3
## roll_belt          1   4
## accel_belt_y       9   4
## accel_belt_z      10   4
## pitch_belt         2   8
## magnet_belt_x     11   8
## roll_belt          1   9
## total_accel_belt   4   9
## accel_belt_z      10   9
## roll_belt          1  10
## total_accel_belt   4  10
## accel_belt_y       9  10
## pitch_belt         2  11
## accel_belt_x       8  11
## gyros_arm_y       19  18
## gyros_arm_x       18  19
## magnet_arm_x      24  21
## accel_arm_x       21  24
## magnet_arm_z      26  25
## magnet_arm_y      25  26
## accel_dumbbell_x  34  28
## accel_dumbbell_z  36  29
## gyros_dumbbell_z  33  31
## gyros_forearm_z   46  31
## gyros_dumbbell_x  31  33
## gyros_forearm_z   46  33
## pitch_dumbbell    28  34
## yaw_dumbbell      29  36
## gyros_forearm_z   46  45
## gyros_dumbbell_x  31  46
## gyros_dumbbell_z  33  46
## gyros_forearm_y   45  46
```

We can see that there's a lots of data with high correlation. This leads me to proceed with PCA

###Preprocessing with centering and scaling

```r
PreObj<-preProcess(training[,-1])
trainingSD<-predict(PreObj,training[,-1])
```
###Preprocessing with PCA with a explaination of 80% variance

```r
preProc<-preProcess(trainingSD,method="pca",thresh=0.8)
trainingPC<-predict(preProc,trainingSD)
```
###Training data with random forest

```r
modFit<-randomForest(training$classe~.,data=trainingPC)
```
###Listing training data's confusion matrix

```r
pred<-predict(modFit,trainingPC,type="class")
training$predictRight<-pred==training$classe
table(pred,training$classe)
```

```
##     
## pred    A    B    C    D    E
##    A 3906    0    0    0    0
##    B    0 2658    0    0    0
##    C    0    0 2396    0    0
##    D    0    0    0 2252    0
##    E    0    0    0    0 2525
```
###Listing testing data's confusion matrix

```r
testingSD<-predict(PreObj,testing[,-1])
testingPC<-predict(preProc,testingSD)
predTest<-predict(modFit,testingPC,type="class")
testing$predictRight<-predTest==testing$classe
table(predTest,testing$classe)
```

```
##         
## predTest    A    B    C    D    E
##        A 1635   28   11    4    3
##        B   11 1070   19   10    3
##        C   16   29  982   34    7
##        D    9    5   13  915   10
##        E    3    7    1    1 1059
```
###Applied model to test data

```r
Test<-Test[,Predictor]
testSD<-predict(PreObj,Test)
testPC<-predict(preProc,testSD)
answer<-predict(modFit,testPC,type="class")
```

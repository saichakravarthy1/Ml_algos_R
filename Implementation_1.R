library(rpart)
library(e1071)
library(class)
library(nnet)
library(randomForest)
library(ipred)
library(ada)
args <- commandArgs(TRUE)
dataURL<-as.character(args[1])
header<-as.logical(args[2])
d<-read.csv(dataURL,header = header)
ClassCol<-as.integer(args[3])
Class<-d[,ClassCol]
d[,ClassCol]<-as.numeric(d[,ClassCol])
# now create all the classifiers and output accuracy values:

         
# create 10 samples
set.seed(123)
for(i in 1:10) {
  cat("Running sample ",i,"\n")
  sampleI<-sample(1:nrow(d),size = 0.9*nrow(d))
  if(ncol(d)==35)
  {
    d[,35]<-as.numeric(d[,35])
  }
  trainingData<-d[sampleI,]
  ClassName<- names(trainingData[ClassCol])
  f <- as.formula(paste(ClassName," ~."))
  testData<-d[-sampleI,]
  Class1<-testData[,ClassCol]
  # which one is the class attribute
  
  library(rpart)
  #Decision Tree
  fit <- rpart(f, data=trainingData,method='class')
  par(mar=rep(0.1,4))
  prunedTree<-prune(fit,cp=0.01)
  plot(prunedTree)
  text(prunedTree)
  final<-predict(prunedTree,testData,type="class")
  data<-data.frame('predict'=final, 'actual'=Class1)
  count<-nrow(data[data$predict==data$actual,])
  total<-nrow(testData)
  avg = (count*100)/total
  avg =format(round(avg, 2), nsmall = 2)
  #example of how to output
  method<-"Decision Tree" 
  accuracy<-avg
  cat("Method = ", method,", accuracy= ", accuracy,"\n")
  
 # install.packages("e1071")
  library(e1071)
  #SVM
  form <- as.formula(paste("as.factor(",ClassName,") ~."))
  model <- svm(form,data=trainingData,kernel="linear")
  pred<- predict(model,testData)
  data<-data.frame('predict'=pred, 'actual'=Class1)
  count<-nrow(data[data$predict==data$actual,])
  total<-nrow(testData)
  count
  avg = (count*100)/total
  avg =format(round(avg, 2), nsmall = 2)
  avg  #example of how to output
  method<-"SVM" 
  accuracy<-avg
  cat("Method = ", method,", accuracy= ", accuracy,"\n")
  
  
  #Naive Bayesian
  naiv <- naiveBayes(form, data=trainingData) #function
  pred<-predict(naiv,testData) 
  data<-data.frame('predict'=pred, 'actual'=Class1)
  count<-nrow(data[data$predict==data$actual,])
  total<-nrow(testData)
  count
  avg = (count*100)/total
  avg =format(round(avg, 2), nsmall = 2)
  avg  #example of how to output
  method<-"Naive Bayesian" 
  accuracy<-avg
  cat("Method = ", method,", accuracy= ", accuracy,"\n")
  
 # install.packages("class")
  library(class)
  #KNN
  d[,2]<-as.numeric(d[,2])
  #ClassName<- names(trainingData[ClassCol])
  #formula <- as.formula(paste("d[,",ClassCol,"]"))
  Class<-trainingData[,ClassCol]
  Class1<-testData[,ClassCol]
  pred<-knn(train=trainingData, test=testData, cl=Class, k = 3, l = 0, prob = FALSE, use.all = TRUE)
  data<-data.frame('predict'=pred, 'actual'=Class1)
  count<-nrow(data[data$predict==data$actual,])
  total<-nrow(testData)
  count
  avg = (count*100)/total
  avg =format(round(avg, 2), nsmall = 2)
  avg  #example of how to output
  method<-"KNN" 
  accuracy<-avg
  cat("Method = ", method,", accuracy= ", accuracy,"\n")
  
  #Logistic Regression
  d[,ClassCol]<-as.numeric(d[,ClassCol])
  for(i in 1:nrow(d))
  {
    if(d[i,ClassCol]==1)
    {
      d[i,ClassCol]<-0
    }
    else
    {  
      d[i,ClassCol]<-1
    }
  }
  trainingData<-d[sampleI,]
  ClassName<- names(trainingData[ClassCol])
  f <- as.formula(paste(ClassName," ~."))
  testData<-d[-sampleI,]
  Class1<-testData[,ClassCol]
  # which one is the class attribute
  # now create all the classifiers and output accuracy values:
  model <- glm(f,family=binomial,trainingData) #function
  pred<- predict.glm(model,testData,type="response")
  pred<-round(pred,digits=0)
  data<-data.frame('predict'=pred, 'actual'=Class1)
  count<-nrow(data[data$predict==data$actual,])
  total<-nrow(testData)
  avg = (count*100)/total
  avg =format(round(avg, 2), nsmall = 2)
  method<-"Logistic Regression"
  accuracy<-avg
  cat("Method = ", method,", accuracy= ", accuracy,"\n")
  
 # install.packages("nnet")
  library(nnet)
  #neural network
  d[,2]<-as.numeric(d[,2])
  d[,3]<-as.numeric(d[,3])
  d[,1]<-as.numeric(d[,1])
  trainingData<-d[sampleI,]
  output<-trainingData[,ClassCol]
  ClassName<- names(trainingData[ClassCol])
  n <- names(d)
  form <- paste(ClassName, paste(names(trainingData),collapse='+'), sep='~')
  f <- as.formula(paste(ClassName," ~ ."))
  testData<-d[-sampleI,]
  Class1<-testData[,ClassCol]
  neur <- nnet(trainingData,output,size=4,maxit=50) #function
  pred<-predict(neur,testData) 
  pred<-round(pred)
  data<-data.frame('predict'=pred, 'actual'=Class1)
  count<-nrow(data[data$predict==data$actual,])
  total<-nrow(testData)
  avg = (count*100)/total
  avg =format(round(avg, 2), nsmall = 2)
  avg  #example of how to output
  method<-"Neural" 
  accuracy<-avg
  cat("Method = ", method,", accuracy= ", accuracy,"\n")
  
 # install.packages("randomForest")
  library(randomForest)
  #Random Forest
  trainingData<-d[sampleI,]
  ClassName<- names(trainingData[ClassCol])
  f <- as.formula(paste(ClassName," ~."))
  testData<-d[-sampleI,]
  Class1<-testData[,ClassCol]
  # which one is the class attribute
  # now create all the classifiers and output accuracy values:
  model <- randomForest(formula=f,data=trainingData,ntree=100,mtry=2, 
                        keep.forest=TRUE, importance=TRUE,test=Class1) #function
  pred<-predict(model,testData,type="class")
  pred<-round(pred)
  data<-data.frame('predict'=pred, 'actual'=Class1)
  count<-nrow(data[data$predict==data$actual,])
  total<-nrow(testData)
  avg = (count*100)/total
  avg =format(round(avg, 2), nsmall = 2)
  avg  #example of how to output
  method<-"Random Forest"
  accuracy<-avg
  cat("Method = ", method,", accuracy= ", accuracy,"\n")
  
 # install.packages("ipred")
  library(ipred)
  #Bagging
  model <- bagging(f,trainingData) #function
  pred<-predict(model,testData) 
  pred<-round(pred)
  data<-data.frame('predict'=pred, 'actual'=Class1)
  count<-nrow(data[data$predict==data$actual,])
  total<-nrow(testData)
  count
  avg = (count*100)/total
  avg =format(round(avg, 2), nsmall = 2)
  avg  #example of how to output
  method<-"Bagging"
  accuracy<-avg
  cat("Method = ", method,", accuracy= ", accuracy,"\n")
  
 # install.packages("ada")
  library(ada)
  #Boosting
  model <- ada(f,trainingData) #function
  pred<-predict(model,testData) 
  data<-data.frame('predict'=pred, 'actual'=Class1)
  count<-nrow(data[data$predict==data$actual,])
  total<-nrow(testData)
  count
  avg = (count*100)/total
  avg =format(round(avg, 2), nsmall = 2)
  avg  #example of how to output
  method<-"Boosting"
  accuracy<-avg
  cat("Method = ", method,", accuracy= ", accuracy,"\n")  
  
}
  
  
  

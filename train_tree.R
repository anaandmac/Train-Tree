#install.packages("caret")
library(caret)
library(tidyverse)

#Load the dataset into memory using load("segmentationOriginal.RData")
#Usar setwd()->definir o diretorio que esta o arquivo "segmentationOriginal.RData"
load("segmentationOriginal.RData")

#Interrogate the dataset. Remove the first two columns of the matrix;
View(segmentationOriginal)
glimpse(segmentationOriginal)#2019 observations and 119 variables
summary(segmentationOriginal)

#Removendo as duas primeiras colunas
data.2<-select(segmentationOriginal,-c(1,2))
View(data.2)

#Use the "createDataPartition" function within caret to create a vector of indices for a 
  #single split of the data with 75% of the data for a training set and 25% for a testing set;
trainIndex<-createDataPartition(data.2$Class,p=0.75,list=FALSE,times=1)

#Use these indices to create training and testing sets from the original data matrix;
training<- data.2[trainIndex,]
testing<- data.2[-trainIndex,]
View(training)
View(testing)

#Create a training method using the "trainControl" function that invovles repeated ten-fold cross-validation, 
  #repeated 3 times.
#k-fold cross-validation->repeatedcv
ctrl<-trainControl(method="repeatedcv",number=10, repeats = 3)

#Train a decision tree using the training method on the training data with the "rpart" function 
  #as the classification algorithm;
#A variavel resposta e a segmentacao da celula (Class: PS->poorly segmented and WS->well segmented)
library(rpart)
train.tree<-rpart(Class~.,data=training,control="ctrl")#OBS:colocar o metodo em aspas

#Plot the results of training;
library(rpart.plot)
rpart.plot(train.tree)

#Generate predictions using the decision tree on the held-out test dataset;
predictions<-predict(train.tree,newdata=testing,type="prob")

#Probabilidade da celula ser PS ou WS
View(predictions)

#Summarize the results of the predictions of the algorithm using a confusion matrix with the 
#confusionMatrix function in caret
p=0.5#We can change that
pred<-factor(ifelse(predictions[,"PS"]>p, "PS","WS") )
confusionMatrix(pred, testing$Class)


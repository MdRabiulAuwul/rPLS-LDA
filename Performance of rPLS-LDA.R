library(class)
library(plsgenomics)
library(MASS)
library(caret)
library(robustbase)

source("rPLS-LDA Methods.r")

#load all data
Train_Data=DatTrao
Test_Data=DatTeeo
Train_Class=trClass
Test_Class=TrlevelTest
################## PLS-LDA ####################
plsldao<-pls.lda(Xtrain=t(Train_Data),Ytrain=Train_Class,Xtest=t(Test_Data),ncomp=2,nruncv=0)#ncomp=1:4,nruncv=20
pred.plsldao<-plsldao$predclass
Resultplsldao<-confusionMatrix(pred.plsldao,as.factor(Test_Class))
Accuracy_PLS_LDA<-Resultplsldao[[3]][1]

########## rPLS_LDA #############
Train_Datar<-UpdateX(Train_Data,as.factor(Train_Class))
Rplsldao<-pls.lda(Xtrain=t(Train_Datar),Ytrain=Train_Class,Xtest=t(Test_Data),ncomp=2,nruncv=0)
pred.Rplsldao<-Rplsldao$predclass
ResultRplsldao<-confusionMatrix(pred.Rplsldao,as.factor(Test_Class))
Accuracy_rPLS_LDA<-ResultRplsldao[[3]][1]

Accuracy_PLS_LDA
Accuracy_rPLS_LDA


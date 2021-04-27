source("rPLS-LDA Methods.r")

up_DatTra=UpdateX(Train_Data,Train_Class)

rpValue_anova<-NULL ; 
for (j1 in 1:dim(up_DatTra)[1])
{
  rDataYY <- data.frame(YY =up_DatTra[j1,], FactorLevels = factor(Train_Class))
  rpValue_anova[j1] <-  t.test(YY~FactorLevels,data=rDataYY)[[1]]
}

radj.pval<-p.adjust(rpValue_anova,"BH")

rTop40DE<-sort(radj.pval,index.return=TRUE)$ix[1:TopN]
rDatTra=up_DatTra[rTop40DE,]

rDatTee=Test_Data[rTop40DE,]

rPLS-LDA=pls.lda(Xtrain=t(rDatTra),Ytrain=Trlevel,Xtest=t(rDatTee), ncomp=1:4,nruncv=20)

Result_rPLS_LDA=confusionMatrix(as.factor(ryy[[1]]),as.factor(Televel))




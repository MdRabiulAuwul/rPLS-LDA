MeanVar <- function(xxx,Beta)
{
  xx=xxx
  
  nn <- dim(xx)[1] # sample size
  mm <- dim(xx)[2]
  
  Indexcls=xxx[,dim(xxx)[2]]
  
  #####===============Initialization=================================
  Mx<-NULL; Median<-NULL; Dist<-NULL; Data0<-NULL
  for (ii in 1:dim(xx)[2])
  {Median[ii] <- median(xx[,ii])}
  #{Mode <- hist(xx[,ii], nclass = 20, plot = F)
  # Mx[ii] <- mean(Mode$mids[which(Mode$count==max(Mode$count))])
  #}
  for (jj in 1:dim(xx)[1])
  {Dist[jj]<-sqrt(sum((xx[jj,]-Median)^2))}
  for (kk in 1:dim(xx)[1])
  {if (Dist[kk] <= as.numeric(quantile(Dist, p=.5)))
    Data0 <- rbind(Data0, xx[kk,])}
  #plot(Dist)
  #readline('Pls Enter')
  #####==========End Initialization================================
  #source("mpinv.r")
  Mo <-xx[1,]#as.numeric(colMeans(Data0))
  Vo <-diag(dim(xx)[2])#as.matrix(cov(Data0))
  DiffTol = 0.005;
  DiffNorm = +10000;
  Iter = 0;
  #cat("Test=",dim(Vo),"\n")
  ##Wx <- NULL
  while (DiffNorm > DiffTol)
  {
    Wx <- NULL
    for (j1 in 1:nn)
    { zo <- as.numeric(xx[j1,]-Mo)  
    #zz <- bta*(t(zo)%*%solve(Vo)%*%zo)
    #if(det(t(Vo)%*%zo)<0.000005)
    zz <- Beta*(t(zo)%*%ginv(Vo)%*%zo)
    #if(det(t(Vo)%*%zo)>=0.000005)
    #zz <- Beta*(t(zo)%*%solve(Vo)%*%zo)
    Wx[j1] <- exp(-zz/2)
    }
    M.new <- matrix(0,nr=mm)
    V.new <- matrix(0,nr=mm,nc=mm)
    xup<-matrix(0,nr=nn,nc=mm)
    for (j2 in 1:nn)
    { M.new <- M.new + (Wx[j2]*xx[j2,])/sum(Wx)
    V.new <- V.new + (1+Beta)*(Wx[j2]*(xx[j2,]-Mo)%*%t(xx[j2,]-Mo))/sum(Wx)
    xup[j2,]<- (Wx[j2]*xx[j2,])
    }
    #norm1 <- sqrt(sum(M.new^2))+sqrt(sum(V.new^2))
    #norm2 <- sqrt(sum((M.new-Mo)^2))+sqrt(sum((V.new-Vo)^2))
    #DiffNorm <- norm2/norm1  
    DiffNorm <- sqrt(sum((M.new-Mo)^2))/mm + sqrt(sum((V.new-Vo)^2))/mm
    Mo = M.new
    Vo = V.new
    xxup=xup
    Iter = Iter + 1 
    #cat("Iter,Diff=",c(Iter,DiffNorm),"\n")
  }
  Wt.weight <-Wx
  In.Wt<-cbind(Indexcls,Wt.weight)
  plot(Wt.weight)
  return(list(M=M.new, V=V.new, Wt=Wt.weight,Inwt=In.Wt,xxup=xxup))
}



UpdateX<-function(TrDat,cl,beta=c(0.2,0.2)){
  lab <- unique(cl)
  outIndx<-list()
  datup<-list()
  M<-list()
  bwt<-matrix(0,nrow(TrDat),length(lab))
  UpX<-TrDat
  for (ii0 in 1:length(lab)) {
    
    tt=MeanVar(TrDat[,which(cl == lab[ii0])],beta[ii0])
    M[[ii0]]<-tt$M 
    #V=tt$V 
    bwt[,ii0]<-tt$Wt
    bwt[1,ii0]<-0
    bwt[10,ii0]<-0
    outIndx[[ii0]]<-which(bwt[,ii0]<0.2)
    
    if (length(outIndx[[ii0]])>0){
      DataTran<-TrDat
      data0 <- DataTran[outIndx[[ii0]],which(cl == lab[ii0]) ]
      ModTrainx<-NULL
      Beta=1.5
      TrDupdate1<-data0 
      for(g1 in 1:nrow(TrDupdate1)){
        Q1<-as.numeric(quantile(TrDupdate1[g1,], p=.25))
        Q3<-as.numeric(quantile(TrDupdate1[g1,], p=.75))
        IQR<-Q3 - Q1
        UL<- (IQR*Beta) + Q3
        LL<- Q1- (IQR*Beta)
        Indx<-c(which(TrDupdate1[g1,]<LL), which(TrDupdate1[g1,]>UL))
        if (length(Indx)>0){
          TrDupdate1[g1,Indx]<-median(TrDupdate1[g1,])
        }
      }
      ModTrainx=rbind(ModTrainx,TrDupdate1)
      datup[[ii0]]<-ModTrainx
    }
    for (pp in 1:length(outIndx[[ii0]])){
      for (ll in 1:length(which(cl == lab[ii0]))){
        UpX[outIndx[[ii0]][pp],which(cl == lab[ii0])[ll]]<-datup[[ii0]][pp,ll]
      }
    }
  }
  return(UpX)
}

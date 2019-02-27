#BF01 when H0 is true
calBF01medium1<-function(m1,m2,sd1,sd2,N,J,N_SIM,Variances){
  N1<-N2<-N
  samph<-c(N1,N2)
  ngroup<-samph/J
#simulate data
  set.seed(10)
  datalist<-list()
  for(i in 1:N_SIM){
    datalist[[i]]<-cbind(rnorm(N1,m1,sd1),rnorm(N2,m2,sd2))
  }

  ##caculate estimate and variance for group mean
  estimate<-lapply(datalist, function(x) apply(x,2,mean))
  variance<-lapply(datalist, function(x) apply(x,2,function(x) as.matrix(var(x)/N1) ) )


  ##caculate bf
  library(bain)
  bf<-c()
  if(Variances=='equal'){
    for(i in 1:N_SIM){
      names(estimate[[i]]) <- c("mu1","mu2")
      variance_sp<-(variance[[i]][1]+variance[[i]][2])/2
      covlist<-list(matrix(variance_sp),matrix(variance_sp))
      res<-bain(estimate[[i]], n=ngroup, "mu1 = mu2",Sigma=covlist,group_parameters=1,joint_parameters = 0)
      bf[i]<-res$fit$BF[1]
    }
  }
else {
      for(i in 1:N_SIM){
        names(estimate[[i]]) <- c("mu1","mu2")
        covlist<-list(matrix(variance[[i]][1]),matrix(variance[[i]][2]))
        res<-bain(estimate[[i]],n=ngroup, "mu1 = mu2",Sigma=covlist,group_parameters=1, joint_parameters=0)
        bf[i]<-res$fit$BF[1]
      }
    }

  if(m1==m2)
    medianbf<- median(bf)
  else
   medianbf<- 1/median(bf)
  k<-0
  k1<-0
  k2<-0
  for(i in 1:N_SIM){
    if (bf[[i]]<1)
      k<-k+1
    if (bf[[i]]<1/3)
      k1<-k1+1
    if (bf[[i]]>3)
      k2<-k2+1
  }
  error<-k/N_SIM
  e1<-k1/N_SIM
  e2<-k2/N_SIM
  e3<-1-(e1+e2)
  if (abs(m1-m2)>0)
  {
    error<-1-error
  }
  Error<-list(error,e1,e2,e3)
  #calculate CI of median BF01
  if(m1==m2){
    bf_0<-quantile(bf,c(0.2,0.8))
  }
  else{
    bf_0<-quantile(1/bf,c(0.2,0.8))
  }
  CI<-c(bf_0[[1]], bf_0[[2]])

  return(list(medianbf,Error,CI))
}

##BF10 H1 is ture
calBF01medium2<-function(m1,m2,sd1,sd2,N,J,N_SIM,Variances){
  N1<-N2<-N
  samph<-c(N1,N2)
  ngroup<-samph/J
  set.seed(10)
  ##simulate data
  datalist<-list()
  for(i in 1:N_SIM){
    datalist[[i]]<-cbind(rnorm(N1,m1[i],sd1),rnorm(N2,m2[i],sd2))
  }

  ##caculate estimate and variance for group mean
  estimate<-lapply(datalist, function(x) apply(x,2,mean))
  variance<-lapply(datalist, function(x) apply(x,2,function(x) as.matrix(var(x)/N1)))

  ##caculate bf
  library(bain)
  bf<-c()
  if(Variances=='equal'){
    for(i in 1:N_SIM){
      names(estimate[[i]]) <- c("mu1","mu2")
      variance_sp<-(variance[[i]][1]+variance[[i]][2])/2
      covlist<-list(matrix(variance_sp),matrix(variance_sp))
      res<-bain(estimate[[i]],n=ngroup, "mu1 = mu2",Sigma=covlist,group_parameters=1, joint_parameters=0)
      bf[i]<-res$fit$BF[1]
    }
  }
  else {
    for(i in 1:N_SIM){
      names(estimate[[i]]) <- c("mu1","mu2")
      covlist<-list(matrix(variance[[i]][1]),matrix(variance[[i]][2]))
      res<-bain(estimate[[i]],n=ngroup, "mu1 = mu2",Sigma=covlist,group_parameters=1, joint_parameters=0)
      bf[i]<-res$fit$BF[1]
    }
  }
  medianbf<-1/median(bf)

  k<-0
  k1<-0
  k2<-0
  for(i in 1:N_SIM){
    if (bf[[i]]<1)
      k<-k+1
    if (bf[[i]]<1/3)
      k1<-k1+1
    if (bf[[i]]>3)
      k2<-k2+1
  }
  error<-k/N_SIM
  e1<-k1/N_SIM
  e2<-k2/N_SIM
  e3<-1-(e1+e2)
  if (abs(m1-m2)>0)
  {
    error<-1-error
  }
  Error<-list(error,e1,e2,e3)

  bf_0<-quantile(1/bf,c(0.2,0.8))
  CI<-c(bf_0[[1]], bf_0[[2]])
   return(list(medianbf,Error,CI))
}

##define function BF02 H0 is ture H0 VS H2
calBF02medium1<-function(m1,m2,sd1,sd2,N,J,N_SIM,Variances){
  N1<-N2<-N
  samph<-c(N1,N2)
  ngroup<-samph/J
  ##simulate data
  ##Repeat the same simulation results
  set.seed(10)
  ##simulate data
  datalist<-list()
  for(i in 1:N_SIM){
    datalist[[i]]<-cbind(rnorm(N1,m1,sd1),rnorm(N2,m2,sd2))
  }

  ##caculate estimate and variance for group mean
  estimate<-lapply(datalist, function(x) apply(x,2,mean))
  variance<-lapply(datalist, function(x) apply(x,2,function(x) as.matrix(var(x)/N1) ) )

  ##caculate bf
  library(bain)


  ##caculate bf
  if(Variances=='equal'){
    for(i in 1:N_SIM){
      names(estimate[[i]]) <- c("mu1","mu2")
      variance_sp<-(variance[[i]][1]+variance[[i]][2])/2
      covlist<-list(matrix(variance_sp),matrix(variance_sp))
      res<-bain(estimate[[i]],n=ngroup, "mu1=mu2;mu1>mu2",Sigma=covlist,group_parameters=1, joint_parameters=0)
      bf_fit<-res$fit$Fit
      bf_com<-res$fit$Com
      bf1<-bf_fit[[1]]/bf_com[[1]]
      bf2<-bf_fit[[2]]/bf_com[[2]]
      bf[i]<-bf1/bf2
    }
  }
  else {
    for(i in 1:N_SIM){
      names(estimate[[i]]) <- c("mu1","mu2")
      covlist<-list(matrix(variance[[i]][1]),matrix(variance[[i]][2]))
      res<-bain(estimate[[i]],n=ngroup, "mu1=mu2;mu1>mu2",Sigma<-covlist,group_parameters=1, joint_parameters=0)
      bf_fit<-res$fit$Fit
      bf_com<-res$fit$Com
      bf1<-bf_fit[[1]]/bf_com[[1]]
      bf2<-bf_fit[[2]]/bf_com[[2]]
      bf[i]<-bf1/bf2
    }
  }

  if(m1==m2){
    medianbf<- median(bf)
  }
  else{
    medianbf<- 1/median(bf)
  }
  k<-0
  k1<-0
  k2<-0
  for(i in 1:N_SIM){
    if (bf[[i]]<1)
      k<-k+1
    if (bf[[i]]<1/3)
      k1<-k1+1
    if (bf[[i]]>3)
      k2<-k2+1
  }
  error<-k/N_SIM
  e1<-k1/N_SIM
  e2<-k2/N_SIM
  e3<-1-(e1+e2)
  if (abs(m1-m2)>0)
  {
    error<-1-error
  }
  Error<-list(error,e1,e2,e3)

  if(m1==m2){
    bf_0<-quantile(bf,c(0.2,0.8))
  }
  else{
    bf_0<-quantile(1/bf,c(0.2,0.8))
  }
  CI<-c(bf_0[[1]], bf_0[[2]])
  return(list(medianbf,Error,CI))
}

calBF02medium2<-function(m1,m2,sd1,sd2,N,J,N_SIM,Variances){
  N1<-N2<-N
  samph<-c(N1,N2)
  ngroup<-samph/J
  ##simulate data
  set.seed(10)
 # m1<-rnorm(1000,mean=0,sd=sqrt(2))
 # m2<-rnorm(1000,mean=0,sd=sqrt(2))
  ##give an initial value
  miu1<-miu2<-rnorm(N1,0,1)*0
  datalist<-list()
  mu1<-c()
  mu2<-c()
  mu10<-m1[m1>m2]
  mu20<-m2[m1>m2]
  mu1<-mu10[1:N_SIM]
  mu2<-mu20[1:N_SIM]
    for(i in 1:N_SIM){
    miu1<-rnorm(N1,mu1[i],sd1)
    miu2<-rnorm(N2,mu2[i],sd2)
    datalist[[i]]<-cbind(miu1,miu2)
  }

  ##caculate estimate and variance for group mean
  estimate<-lapply(datalist, function(x) apply(x,2,mean))
  variance<-lapply(datalist, function(x) apply(x,2,function(x) as.matrix(var(x)/N1)))

  ##caculate bf
  library(bain)
  bf<-c()
  ERr1<-matrix(c(1,-1,0),nrow=1,ncol=3,byrow=TRUE)
  IRr1<-matrix(0,0,0)
  ERr2<-matrix(0,0,0)
  IRr2<-matrix(c(1,-1,0),nrow=1,ncol=3,byrow=TRUE)
  if(Variances=='equal'){

    for(i in 1:N_SIM){
      names(estimate[[i]]) <- c("mu1","mu2")
      variance_sp<-(variance[[i]][1]+variance[[i]][2])/2
      covlist<-list(matrix(variance_sp),matrix(variance_sp))
      res<-bain(estimate[[i]],n=ngroup, "mu1=mu2;mu1>mu2",Sigma=covlist,group_parameters=1, joint_parameters=0)
      bf_fit<-res$fit$Fit
      bf_com<-res$fit$Com
      bf1<-bf_fit[[1]]/bf_com[[1]]
      bf2<-bf_fit[[2]]/bf_com[[2]]
      bf[i]<-bf1/bf2
    }
  }
  else {
    for(i in 1:N_SIM){
      names(estimate[[i]]) <- c("mu1","mu2")
      covlist<-list(matrix(variance[[i]][1]),matrix(variance[[i]][2]))
      res<-bain(estimate[[i]],n=ngroup, "mu1=mu2;mu1>mu2",Sigma=covlist,group_parameters=1, joint_parameters=0)
      bf_fit<-res$fit$Fit
      bf_com<-res$fit$Com
      bf1<-bf_fit[[1]]/bf_com[[1]]
      bf2<-bf_fit[[2]]/bf_com[[2]]
      bf[i]<-bf1/bf2
    }
  }
  medianbf<-1/median(bf)
  k<-0
  k1<-0
  k2<-0
  for(i in 1:N_SIM){
    if (bf[[i]]>1)
      k<-k+1
    if (bf[[i]]<1/3)
      k1<-k1+1
    if (bf[[i]]>3)
      k2<-k2+1
  }
  error<-k/N_SIM
  e1<-k1/N_SIM
  e2<-k2/N_SIM
  e3<-1-(e1+e2)
  Error<-list(error,e1,e2,e3)

  bf_0<-quantile(1/bf,c(0.2,0.8))
  CI<-c(bf_0[[1]], bf_0[[2]])


  return(list(medianbf,Error,CI))
}

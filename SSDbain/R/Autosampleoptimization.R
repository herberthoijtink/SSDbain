
SSDttest<-function(Population_mean,Variances,MedBF,Hypothesis,T){


  N_SIM=T
  Nmin<-10

  Nmax<-1000
  m11<-m12<-0
  J<-1
  cat('J=1')
  cat('\n')
  if(length(Population_mean)==2)
  {m21<-Population_mean[1]
  m22<-Population_mean[2]}
  else
  {    set.seed(5)
    m21<-rnorm(T*10,mean=0,sd=sqrt(2/J))
    m22<-rnorm(T*10,mean=0,sd=sqrt(2/J))
  }
  if(length(m21)==1){
    cat(paste('initial step: fast estimate the sample size ','\n'))

    #general sample size impression when simulation data set equals to 99
    t1<-system.time(Res<-calSSD_fast(m21,m22,Variances,J,MedBF,Nmin,Nmax,N_SIM,Hypothesis))
    cat('\n')
    #cat(paste('Program running time of step 1 is ',as.character(t1[[3]]),'\n',sep=''))
    #cat(paste('second step:  estimate the sample size based on the true mean and variance','\n'))
    N<-Res[[1]][[3]]

    Nmin<-max(N-100,10)
    Nmax<-N+100
    #final sample size when simulation data set equals 1000
    if(N-100<10)
      cat(paste('final step: simulation',as.character(T), 'data sets with Nmin=10,','Nmax=',as.character(N),'+100','\n'))
    else
      cat(paste('final step: simulation',as.character(T), 'data sets with Nmin=',as.character(N),'-100,','Nmax=',as.character(N),'+100','\n'))
  }


  t2=system.time(Res1<-calSSD(m21,m22,Variances,J=1,MedBF,Nmin,Nmax,N_SIM,Hypothesis))
  #cat(paste('Program running time of step 2 (J=1) is ',as.character(t2[[3]]),'\n',sep=''))
  #cat('\n')
  N<-Res1[[7]]
  Nmin<-max(N-200,10)
  Nmax<-N+200
  J<-2
  cat('J=2')
  cat('\n')
  if(length(Population_mean)==2)
  {m21<-Population_mean[1]
  m22<-Population_mean[2]}
  else
  {    set.seed(5)
    m21<-rnorm(T*10,mean=0,sd=sqrt(2/J))
    m22<-rnorm(T*10,mean=0,sd=sqrt(2/J))
  }
  if(length(m21)==1){
    cat(paste('initial step: fast estimate the sample size ','\n'))

    #general sample size impression when simulation data set equals to 99
    t1<-system.time(Res<-calSSD_fast(m21,m22,Variances,J,MedBF,Nmin,Nmax,N_SIM,Hypothesis))
    cat('\n')
    #cat(paste('Program running time of step 1 is ',as.character(t1[[3]]),'\n',sep=''))
    #cat(paste('second step: detailed estimate the sample size based on the true mean and variance','\n'))


    N<-Res[[1]][[3]]

    Nmin<-max(N-100,10)
    Nmax<-N+100
    #final sample size when simulation data set equals 1000
    if(N-100<10)
      cat(paste('final step: simulation',as.character(T), 'data sets with Nmin=10,','Nmax=',as.character(N),'+100','\n'))
    else
      cat(paste('final step: simulation',as.character(T), 'data sets with Nmin=',as.character(N),'-100,','Nmax=',as.character(N),'+100','\n'))
  }

  if(length(Population_mean)==2)
  {m21<-Population_mean[1]
  m22<-Population_mean[2]}
  else
  {    set.seed(5)
    m21<-rnorm(T*10,mean=0,sd=sqrt(2/J))
    m22<-rnorm(T*10,mean=0,sd=sqrt(2/J))
  }
  t3=system.time(Res2<-calSSD(m21,m22,Variances,J=2,MedBF,Nmin,Nmax,N_SIM,Hypothesis))
  cat('\n')

  J<-3
  cat('J=3')
  cat('\n')
  N<-Res2[[7]]
  Nmin<-max(N-200,10)
  Nmax<-N+200
  if(length(Population_mean)==2)
  {m21<-Population_mean[1]
  m22<-Population_mean[2]}
  else
  {    set.seed(5)
    m21<-rnorm(T*10,mean=0,sd=sqrt(2/J))
    m22<-rnorm(T*10,mean=0,sd=sqrt(2/J))
  }
  if(length(m21)==1){
    cat(paste('initial step: fast estimate the sample size ','\n'))

      #general sample size impression when simulation data set equals to 99
      t1<-system.time(Res<-calSSD_fast(m21,m22,Variances,J,MedBF,Nmin,Nmax,N_SIM,Hypothesis))
    cat('\n')
    #cat(paste('Program running time of step 1 is ',as.character(t1[[3]]),'\n',sep=''))
    #cat(paste('second step: detailed estimate the sample size based on the true mean and variance','\n'))


    N<-Res[[1]][[3]]

    Nmin<-max(N-100,10)
    Nmax<-N+100
    #final sample size when simulation data set equals 1000
    if(N-100<10)
      cat(paste('final step: simulation',as.character(T), 'data sets with Nmin=10,','Nmax=',as.character(N),'+100','\n'))
    else
      cat(paste('final step: simulation',as.character(T), 'data sets with Nmin=',as.character(N),'-100,','Nmax=',as.character(N),'+100','\n'))
  }
  # cat(paste('Program running time of step 2 (J=2) is ',as.character(t3[[3]]),'\n',sep=''))
  # cat('\n')

  if(length(Population_mean)==2)
  {m21<-Population_mean[1]
  m22<-Population_mean[2]}
  else
  {    set.seed(5)
    m21<-rnorm(T*10,mean=0,sd=sqrt(2/J))
    m22<-rnorm(T*10,mean=0,sd=sqrt(2/J))
  }
  t4=system.time(Res3<-calSSD(m21,m22,Variances,J=3,MedBF,Nmin,Nmax,N_SIM,Hypothesis))
  cat('\n')
  #  cat(paste('Program running time of step 2 (J=3) is ',as.character(t4[[3]]),'\n',sep=''))
  cat('\n')
  Res_output(m21,m22,N,J=1,MedBF,Variances,N_SIM,Res1,Hypothesis)
  Res_output(m21,m22,N,J=2,MedBF,Variances,N_SIM,Res2,Hypothesis)
  Res_output(m21,m22,N,J=3,MedBF,Variances,N_SIM,Res3,Hypothesis)
  #   cat(paste('Total program running time is ',as.character(t1[[3]]+t2[[3]]+t3[[3]]+t4[[3]]),'\n',sep=''))
}

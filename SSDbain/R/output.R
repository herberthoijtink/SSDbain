Res_output<-function(m21,m22,N,J,MedBF,Variances,N_SIM,Res,Hypothesis){
  m11<-m12<-0
  if(Variances=='equal'){
    sd1<-sd2<-1
  }
  else
  {
    sd1<-sqrt(1.33)
    sd2<-sqrt(0.67)
  }
  if(Hypothesis=='two-sided'){
  BF1<-Res[[1]]
  BF2<-Res[[2]]
  Error_H0_BF<-Res[[3]]
  CI_H0_BF<-Res[[4]]
  Error_H1_BF<-Res[[5]]
  CI_H1_BF<-Res[[6]]
  N<-Res[[7]]
cat(paste(' H0: mu1 is equal to mu2   H1: mu1 is not equal to mu2'),'\n')
cat(' \n')

cat('populations used for simulating data','\n')
if(length(m21)>1)
  switch(J,

         cat(paste('      mu1=mu2=',as.character(m11),'                mu1~N(',as.character(m11),',','2)',' mu2~N(',as.character(m12),',','2)','\n',sep='')),


         cat(paste('      mu1=mu2=',as.character(m11),'           mu1~N(',as.character(m11),',','1)',' mu2~N(',as.character(m12),',','1)','\n',sep='')),

         cat(paste('      mu1=mu2=',as.character(m11),'           mu1~N(',as.character(m11),',','2/3)',' mu2~N(',as.character(m12),',','2/3)','\n',sep=''))
  )
else
  cat(paste('      mu1=mu2=',as.character(m11), '              mu1=',as.character(m21),'  mu2=', as.character(m22),'\n',sep=''))

cat(paste(' var1=',as.character(sd1^2),    '  var2=', as.character(sd2^2), '          var1=', as.character(sd1^2), '  var2=', as.character(sd2^2), '\n',sep=''     ) )
cat(' \n')
cat(paste('A median Bayes factor of ',as.character(MedBF),' is obtained using N = ',as.character(N),sep=''),'\n')
cat(paste('(J= ',as.character(J), '   T= ',as.character(N_SIM), '  variances= ',as.character(Variances),')',sep=''),'\n')
cat(' \n')
cat(paste('median BF01/BF10 (60% interval)','\n',sep=''))
cat('  ',paste(as.character(signif(BF1,4)),' (',
               as.character(signif(CI_H0_BF[1],4)),',',as.character(signif(CI_H0_BF[2],4)),')      ',
               as.character(signif(BF2,4)),' (',
               as.character(signif(CI_H1_BF[1],4)),',',as.character(signif(CI_H1_BF[2],4)),')',
               '\n',sep=''))
cat(' \n')
cat(paste('   P(BF01<1|H0) = ',as.character(signif(Error_H0_BF[[1]],4)),'    P(BF10<1|H1) = ',as.character(signif(Error_H1_BF[[1]],4)),'\n'))
cat(' \n')
cat(paste('   P(BF01<1/3|H0) = ',as.character(signif(Error_H0_BF[[2]],4)),'\n',sep=''))
cat(paste('   P(BF10<1/3|H1) = ',as.character(signif(Error_H1_BF[[3]],4)),'\n',sep=''))
cat(paste('   (P(1/3<BF01<3|H0)+P(1/3<BF10<3|H1))/2  = ' ,as.character(signif((Error_H0_BF[[4]]+Error_H1_BF[[4]])/2),4),'\n',sep=''))
cat(' \n')
}
else if (Hypothesis=='one-sided'){
  BF1<-Res[[1]]
  BF2<-Res[[2]]
  Error_H0_BF<-Res[[3]]
  CI_H0_BF<-Res[[4]]
  Error_H2_BF<-Res[[5]]
  CI_H2_BF<-Res[[6]]
  N<-Res[[7]]
cat(paste(' H0: mu1 is equal to mu2       H2: mu1 is greater than mu2'),'\n')
cat(' \n')
cat('populations used for simulating data','\n')
if(length(m21)>1)
  switch(J,

         cat(paste('      mu1=mu2=',as.character(m11),'        mu1~N(',as.character(m11),',','2)',' mu2~N(',as.character(m12),',','2)','\n',sep='')),


         cat(paste('      mu1=mu2=',as.character(m11),'        mu1~N(',as.character(m11),',','1)',' mu2~N(',as.character(m12),',','1)','\n',sep='')),

         cat(paste('      mu1=mu2=',as.character(m11),'        mu1~N(',as.character(m11),',','2/3)',' mu2~N(',as.character(m12),',','2/3)','\n',sep=''))
  )
else
  cat(paste('      mu1=mu2=',as.character(m11),'                   mu1=',as.character(m21),'  mu2=', as.character(m22),'\n',sep=''))

cat(paste(' var1=',as.character(sd1^2),    '  var2=', as.character(sd2^2), '          var1=', as.character(sd1^2), '  var2=', as.character(sd2^2), '\n',sep=''     ) )
cat(' \n')

cat(paste('A median Bayes factor of ',as.character(MedBF),' is obtained using N = ',as.character(N),sep=''),'\n')
cat(paste('(J= ',as.character(J), '   T= ',as.character(N_SIM), '  variances= ',as.character(Variances),')',sep=''),'\n')
cat(' \n')
cat(paste('median BF02/BF20 (60% interval)','\n',sep=''))
cat('  ',paste(as.character(signif(BF1,4)),' (',
               as.character(signif(CI_H0_BF[1],4)),',',as.character(signif(CI_H0_BF[2],4)),')      ',
               as.character(signif(BF2,4)),' (',
               as.character(signif(CI_H2_BF[1],4)),',',as.character(signif(CI_H2_BF[2],4)),')',
               '\n',sep=''))
cat(' \n')
cat(paste('   P(BF02<1|H0) = ',as.character(signif(Error_H0_BF[[1]],4)),'        P(BF20<1|H2) = ',as.character(signif(Error_H2_BF[[1]],4)),'\n'))
cat(' \n')
cat(paste('   P(BF02<1/3|H0) =  ',as.character(signif(Error_H0_BF[[2]],4)),'\n',sep=''))
cat(paste('   P(BF20<1/3|H2) =  ',as.character(signif(Error_H2_BF[[3]],4)),'\n',sep=''))
cat(paste('   (P(1/3<BF02<3|H0)+P(1/3<BF20<3|H2))/2  = ',as.character(signif((Error_H0_BF[[4]]+Error_H2_BF[[4]])/2),4),'\n',sep=''))
cat(' \n')
}
else {
  cat('Please input a correct Hypothesis type!',' \n')
}
}

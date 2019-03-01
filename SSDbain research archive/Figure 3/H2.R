m11<-0
m12<-0
sd1<-sd2<-1
for (J in 1:3){
  N_SIM<-10000
  Variances<-'equal'
  string<-'C:/data/R new/figuresforfitting/2018-08-12N10000/figuresforfitting_h2/bf_00_J'
  String_J<-paste(string, 1:3, sep = "")
  sd1<-sd2<-1
  k<-1
  medBF1<-c()
  N_vec<-seq(10, 200, by = 20)
  for (i in 1:length(N_vec)){
    N<-N_vec[i]
    medBF<-calBF02medium1(m11,m12,sd1,sd2,N,J,N_SIM, Variances)
    medBF1[k]<-medBF[[1]]
    k<-k+1
  }
  write.csv(medBF1,file=paste(String_J[J],'.csv'),quote=F,row.names = F)
}


m11<-0.2
m12<-0
sd1<-sd2<-1
for (J in 1:3){
  N_SIM<-10000
  Variances<-'equal'
  string<-'C:/data/R new/figuresforfitting/2018-08-12N10000/figuresforfitting_h2/bf_02_J'
  String_J<-paste(string, 1:3, sep = "")
  k<-1
  medBF1<-c()
  N_vec<-seq(400, 600, by = 20)
  for (i in 1:length(N_vec)){
    N<-N_vec[i]
    medBF<-calBF02medium1(m11,m12,sd1,sd2,N,J,N_SIM, Variances)
    medBF1[k]<-medBF[[1]]
    k<-k+1
  }
  write.csv(medBF1,file=paste(String_J[J],'.csv'),quote=F,row.names = F)
}

m11<-0.5
m12<-0
sd1<-sd2<-1
for (J in 1:3){
  N_SIM<-10000
  Variances<-'equal'
  string<-'C:/data/R new/figuresforfitting/2018-08-12N10000/figuresforfitting_h2/bf_05_J'
  String_J<-paste(string, 1:3, sep = "")
  k<-1
  medBF1<-c()
  N_vec<-seq(20, 80, by = 2)
  for (i in 1:length(N_vec)){
    N<-N_vec[i]
    medBF<-calBF02medium1(m11,m12,sd1,sd2,N,J,N_SIM, Variances)
    medBF1[k]<-medBF[[1]]
    k<-k+1
  }
  write.csv(medBF1,file=paste(String_J[J],'.csv'),quote=F,row.names = F)
}

m11<-0.8
m12<-0
sd1<-sd2<-1
for (J in 1:3){
  N_SIM<-10000
  Variances<-'equal'
  string<-'C:/data/R new/figuresforfitting/2018-08-12N10000/figuresforfitting_h2/bf_08_J'
  String_J<-paste(string, 1:3, sep = "")
  k<-1
  medBF1<-c()
  N_vec<-seq(10, 40, by = 2)
  for (i in 1:length(N_vec)){
    N<-N_vec[i]
    medBF<-calBF02medium1(m11,m12,sd1,sd2,N,J,N_SIM, Variances)
    medBF1[k]<-medBF[[1]]
    k<-k+1
  }
  write.csv(medBF1,file=paste(String_J[J],'.csv'),quote=F,row.names = F)
}

T<-10000
set.seed(5)
m21<-rnorm(T*10,mean=0,sd=sqrt(2/J))
m22<-rnorm(T*10,mean=0,sd=sqrt(2/J))
sd1<-sd2<-1
for (J in 1:3){
  N_SIM<-10000
  Variances<-'equal'
  string<-'C:/data/R new/figuresforfitting/2018-08-12N10000/figuresforfitting_h2/bf_NULL_J'
  String_J<-paste(string, 1:3, sep = "")
  k<-1
  medBF1<-c()
  N_vec<-seq(10, 20, by = 2)
  for (i in 1:length(N_vec)){
    N<-N_vec[i]
    medBF<-calBF02medium2(m21,m22,sd1,sd2,N,J,N_SIM,Variances)
    medBF1[k]<-medBF[[1]]
    k<-k+1
  }
  write.csv(medBF1,file=paste(String_J[J],'.csv'),quote=F,row.names = F)
}


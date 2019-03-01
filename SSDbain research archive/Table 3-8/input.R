# R code when effect size d=0.2, median Bayes factor MedBF=5, Hypothesis: H0: mu1=mu2 vs H1: mu1>mu2, and the number of simulations is 10000.

SSDttest(Population_mean=c(0.2,0),Variances='equal',MedBF=5,Hypothesis='two-sided',T=10000)

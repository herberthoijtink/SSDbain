# SSDbain
    
The SSDbain package computes the sample size for the Bayesian Student's t-test and Bayesian Welch's t-test using bain, which was built under R version 3.5.2.

# install bain, devtools
install.packages('bain')
install.packages('devtools')

# Load bain for compute Bayes factor
library(bain)

# Load devtools package for install_github()
library(devtools)

# get SSDbain from github
install_github("Qianrao-Fu/SSDbain/SSDbain")


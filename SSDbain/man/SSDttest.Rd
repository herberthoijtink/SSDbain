\name{SSDttest}
\alias{SSDttest}
\title{Sample Size Determination for the Bayesian t-test}
\description{
function \code{SSDttest} in R package \code{SSDbain} that can be found at \url{https://informative-hypotheses.sites.uu.nl/software/bain/} computes the sample size for the Bayesian Student's t-test and Welch's t-test as implemented in the R package \code{bain}. The details of the approach can be found in Fu et al. (unpublished). \bold{Users are required to install package} \code{bain} \bold{before using} \code{SSDbain} \bold{package}.}
\usage{
SSDttest(Population_mean,Variances, MedBF,Hypothesis,T)}
\arguments{
  \item{\code{Population_mean}}{a vector giving the means of the null and alternative populations. For example, if m1=0.2 and m2=0, then use \code{Population_mean=c(0.2,0)}, in case of a prior distribution of effect sizes, use \code{Population_mean=NULL}.}
  \item{\code{Variances}}{\code{Variances='equal'} when the variances of two groups are equal; \code{Variances='unequal'} when the variances of two groups are not equal}
  \item{\code{MedBF}}{The desired support in terms of the median Bayes factor e.g. 5 or 10.}
  \item{\code{Hypothesis}}{\code{Hypothesis='two-sided'} when the competing hypotheses are H0:mu1=mu2, H1:mu1,mu2; \code{Hypothesis='one-sided'} when the competing hypotheses are H0:mu1=mu2, H2: mu1>mu2.}
  \item{\code{T}}{a postive integer number indicating the number of data sets sampled from the null and alternative populations to compute the n. Note: a minimum value of \bold{10000} is recommended.}
}
\value{
The output resulting from analyses with SSDbain contains:

1) required sample size n;

2) median Bayes factor (median BF0i) when H0 is true and median Bayes factor (median BFi0) when Hi is true, 60\% intervals of BF0i and BFi0;

3) the error rates P(BF0i<1|H0), P(BFi0<1|Hi);

4) the error rates P(BF0i<1/3|H0), P(BF0i>3|Hi), (P(1/3<BF0i<3|H0)+P(1/3<BF0i<3|Hi))/2.}
\references{
Fu, Q., Mulder, Hoijtink, H. and Mirjam, M. (unpublished). Sample size determination for the Bayesian t-test. doi:10.31234/osf.io/2v4js

Gu, X., Mulder, J., and Hoijtink, H. (2017). Approximated adjusted fractional Bayes factors: A general method for testing informative hypotheses. British Journal of Mathematical and Statistical Psychology. \emph{British Journal of Mathematical and Statistical Psychology}, 71(2), 229-261. doi:10.1111/bmsp.12110

Hoijtink, H., Gu, X., and Mulder, J. (2018). Bayesian evaluation of informative hypotheses for multiple populations. \emph{British Journal of Mathematical and Statistical Psychology}. doi:10.1111/bmsp.12145.
}

\author{
Qianrao Fu, Herbert Hoijtink, Mirjam Moerbeek
}

\examples{
#Example 1: fixed effect size under the alternative hypothesis

Res<-SSDttest(Population_mean=c(0.5,0),Variances='equal',MedBF=5,Hypothesis='two-sided',T=10000)

#Example 2: distribution of effect sizes under the alternative hypothesis

Res<-SSDttest(Population_mean=NULL,Variances='equal',MedBF=5,Hypothesis'two-sided',T=10000)
}

library(stats4)
x=c(1,2,2,3,5)

#MLE DISTRIBUSI BINOMIAL
lbinom = function(p)
{
n= length(x)
-sum(log(choose(n,x)))-sum(x)*log(p)-((n^2-sum(x))*log(1-p))
}
estimasi= mle(minuslog=lbinom, start=list(p=0.3))
estimasi

#MLE DISTRIBUSI NORMAL
lnorm = function(sigma2,myu)
{
n=length(x)
(n/2)*log(2*pi*sigma2)+(1/2)*sum(((x-myu)^2)/sigma2)
}
estimasi=mle(minuslog=lnorm, start=list(sigma2=1, myu=0))
estimasi

#MLE DISTRIBUSI WEIBULL
lweibull = function (beta,teta)
{
n=length(x)
-n*log(beta/teta)-(beta-1)*sum(log(x/teta))+(sum(x/teta))^beta
}
estimasi=mle(minuslog=lweibull, start=list(beta=0.2, teta=0.3))
estimasi

#MLE DISTRIBUSI POISSON
lpoisson = function (lambda)
{
n=length(x)
(n*lambda)-sum(x)*log(lambda)+log(sum(factorial(x)))
}
estimasi=mle(minuslog=lpoisson, start=list(lambda=0.4))
estimasi

#MLE DISTRIBUSI EKSPONENSIAL
lexp = function (lambda)
{
n=length(x)
(n*log(lambda))+((1/lambda)*sum(x))
}
estimasi=mle(minuslog=lexp, start=list(lambda=0.4))
estimasi

#ATAU MLE DISTRIBUSI EKSPONENSIAL
lexp = function (lambda)
{
n=length(x)
(-n*log(lambda))+((lambda)*sum(x))
}
estimasi=mle(minuslog=lexp, start=list(lambda=0.4))
estimasi

#MLE DISTRIBUSI BERNOULLI
x=c(1,1,0,0,0,1)
lbernoulli = function(p)
{
n=length(x)
-sum(x)*log(p)-(n-sum(x))*log(1-p)
}
estimasi=mle(minuslog=lbernoulli, start=list(p=0.2))
estimasi

#MLE DISTRIBUSI GEOMETRIK
lgeo = function(p)
{
n=length(x)
-n*log(p)-(sum(x)-n)*log(1-p)
}
estimasi=mle(minuslog=lgeo, start=list(p=0.2))
estimasi

#MLE DISTRIBUSI BINOMIAL NEGATIF
x=c(1,2,2,3,5)
lbinomneg = function(p)
{
n=length(x)
-sum(log(choose((x-1),(k-1))))-(n*k*log(p))-(sum(x)-(n*k))*log(1-p)
}
estimasi=mle(minuslog=lbinomneg, start=list(p=0.2))
estimasi

#MLE DISTRIBUSI GAMMA
lgamma = function (alpha)
{
n=length(x)
sum(x)-(alpha-1)*log(sum(x))
}
estimasi=mle(minuslog=lgamma, start=list(alpha=5))
estimasi

#PLOT
A=c(100,120,140)
B=c(160,180,200)
plot(A, type="l")
lines(B, lty=2)
plot(0,xlim=c(1,3), ylim=c(100,200), type="l")
lines(A)
lines(B, lty=2)

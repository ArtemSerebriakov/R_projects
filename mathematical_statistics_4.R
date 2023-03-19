x=c(1,3,4,8,1,5,5,6,3,2,0,3,5,3,4,4,2,1,7,8,3,1,5,5,3,4,3,6,5,5,2,3,5,5,4,3,4,3,3,5,5,4,1,6,3,4,5,5,5,5,4,2,2,4,2,4,2,2,5,4,4,5,4,2,2,5,2,4,3,
       4,1,2,4,3,3,3,1,2,5,4,0,2,4,3,3,2,4,5,4,4,2,5,4,1,1,5,4,2,2,2,5,2,5,3,5,3,4,5,3,3,2,4,5,3,2,5,3,6,3,6,3,5,4,
       3,6,3,2,4,5,4,6,3,3,3,1,1,5,1,4,3,1,5,4,6,2,2,5,4,3,6,4,4,2,5,2,5,9,1,4,3)
x
n=length(x)
n
k=12
pd01=qbeta(0.1/2,sum(x),n*k-sum(x)+1)
pd01
pd005=qbeta(0.05/2,sum(x),n*k-sum(x)+1)
pd005
pd002=qbeta(0.02/2,sum(x),n*k-sum(x)+1)
pd002
pu01=qbeta(1-0.1/2,sum(x)+1,n*k-sum(x))
pu01
pu005=qbeta(1-0.05/2,sum(x)+1,n*k-sum(x))
pu005
pu002=qbeta(1-0.02/2,sum(x)+1,n*k-sum(x))
pu002

sum(x)
f1=function(theta)qbinom(0.1/2,n*k,theta)
f2=function(theta)qbinom(1-0.1/2,n*k,theta)
plot(f1,col="red",xlab="p",ylab="",lwd=2)
plot(f2,col="blue",lwd=2,add=T)
abline(h=sum(x),col="green",lwd=2, lty=2)
legend("bottomright",c("quantile(0.1/2)","quantile(1-0.1/2)","571"),lty=c(1,1,2),fill=c("red","blue","green"))

install.packages('rootSolve')
library(rootSolve)
f01d=function(p)(sum(x) - n*p*k)/(sqrt(n*p*(1-p)*k)) - qnorm(1-0.1/2)
f01u=function(p)(sum(x) - n*p*k)/(sqrt(n*p*(1-p)*k)) + qnorm(1-0.1/2)
d01=uniroot(f01d, c(0.0, 1.0))$root
u01=uniroot(f01u, c(0.0, 1.0))$root
d01
u01

f005d=function(p)(sum(x) - n*p*k)/(sqrt(n*p*(1-p)*k)) - qnorm(1-0.05/2)
f005u=function(p)(sum(x) - n*p*k)/(sqrt(n*p*(1-p)*k)) + qnorm(1-0.05/2)
d005=uniroot(f005d, c(0.0, 1.0))$root
u005=uniroot(f005u, c(0.0, 1.0))$root
d005
u005

f002d=function(p)(sum(x) - n*p*k)/(sqrt(n*p*(1-p)*k)) - qnorm(1-0.02/2)
f002u=function(p)(sum(x) - n*p*k)/(sqrt(n*p*(1-p)*k)) + qnorm(1-0.02/2)
d002=uniroot(f002d, c(0.0, 1.0))$root
u002=uniroot(f002u, c(0.0, 1.0))$root
d002
u002

pp=0.3
x1=1:12
plot(pbinom(x1, size = k, prob = pd002), type = "s", lwd = 3,xlab = "x",col="red", ylab = "F(x)")
lines(pbinom(x1, size = k, prob = pu002), type = "s",lty=2, lwd = 3,col="blue")
lines(pbinom(x1, size = k, prob = pp), type = "s",lty=3, lwd = 3,col="green")
legend("bottomright",c("lower","upper","theoretical"),lty=c(1,2,3),lwd=c(3,3,3),fill=c("red","blue","green"))


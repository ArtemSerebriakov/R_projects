addTaskCallback(function(...) {set.seed(2008);TRUE})
N = 160
M = 130
sigma = 2
alpha = -2.5
beta = 1.5

eps_vec = t(replicate(M,sapply(c(1:(N+1)), function(y) rnorm(1, 0, sigma))))
eps_vec[1,]
shapiro.test(eps_vec[1,])
#install.packages("ggpubr")
library("ggpubr")
ggdensity(eps_vec[1,])
#hist(eps_vec[1,], col='steelblue')

plot(eps_vec[1,], type = "l", col = "blue", xlab = "n",
     ylab = "eps_n", lwd = 2)
lines(eps_vec[30,], col='red', lty = 3, lwd = 2)
lines(eps_vec[110,], col='green', lty = 4, lwd = 2)
legend(1, 6, legend=c("trajectory 1", "trajectory 30", "trajectory 110"),
       col=c("blue","red","green"), lty=c(1,3,4), lwd = 2)

eps_vec[30,]
eps_vec[110,]
ggdensity(eps_vec[1,])
ggdensity(eps_vec[30,])
ggdensity(eps_vec[110,])


ksi_vec = array(c(rep(0, M),rep(0, M)),dim=c(M,N+1))
for (k in 1:M) {
  ksi_vec[k,1] = eps_vec[k,1]
  ksi_vec[k,2] = eps_vec[k,1] + eps_vec[k,2] * (1 + alpha * (eps_vec[k,1]))
  for (n in 2:N) {
    sum1 = 0
    for (j in 2:n) {
      sum1 = sum1 + eps_vec[k,j+1] * (1 + alpha * eps_vec[k,j] + beta * eps_vec[k,j-1])
    }
    ksi_vec[k,n+1] = eps_vec[k,1] + eps_vec[k,2] * (1 + alpha * (eps_vec[k,1])) + sum1
  }
}

ksi_vec[1,]
ksi_vec[30,]
ksi_vec[110,]

plot(ksi_vec[1,], type = "l", col = "blue", xlab = "n",
     ylab = "ksi_n", lwd = 2, ylim=c(-180, 300))
lines(ksi_vec[30,], col='red', lty = 3, lwd = 2)
lines(ksi_vec[110,], col='green', lty = 4, lwd = 2)
legend(1, 300, legend=c("trajectory 1", "trajectory 30", "trajectory 110"),
       col=c("blue","red","green"), lty=c(1,3,4), lwd = 2)

ksi_char_vec = array(c(rep(0, M),rep(0, M)),dim=c(M,1))
for (k in 1:M) {
  sum1 = 0
  for (n in 2:N) {
    sum1 = sum1 + (1 + alpha * eps_vec[k, n] + beta * eps_vec[k, n-1]) ^ 2
  }
  ksi_char_vec[k] = ksi_vec[k, N+1] / sqrt(sigma ^ 2 * (sum1 + (1 + alpha * eps_vec[k, 1]) ^ 2))
}

ggdensity(ksi_char_vec)
shapiro.test(ksi_char_vec)
ksi_char_vec

num = 1 + floor(log(M, base = 2))
num
min1 = min(ksi_char_vec)
min1
max1 = max(ksi_char_vec)
max1
hist(ksi_char_vec, breaks = seq(min1, max1, length.out = 9),freq = FALSE)
curve(dnorm(x, mean=0, sd=1), 
      col="blue", lwd=2, add=TRUE)
legend('topright', legend="Standard normal distribution",
       col="blue", lwd = 2, cex = 1)

a = sum(ksi_char_vec) / M
a
var1 = sum((ksi_char_vec - a) ^ 2) / (M - 1)
var1
sqrt(var1)
mean(ksi_char_vec)
var(ksi_char_vec)

d = max1 - min1
d
len1 = d / num
len1
border_vec = c(min1,min1 + len1,min1 + 2*len1,min1 + 3*len1,min1 + 4*len1,
               min1 + 5*len1,min1 + 6*len1,min1 + 7*len1,max1)
border_vec

p_vec = array(c(rep(0, M),rep(0, M)),dim=c(8,1))
p_vec[1] = pnorm(border_vec[2],a,sqrt(var1)) - pnorm(-Inf,a,sqrt(var1))
for (i in 2:7) {
  p_vec[i] = pnorm(border_vec[i+1],a,sqrt(var1)) - pnorm(border_vec[i],a,sqrt(var1))
}
p_vec[8] = pnorm(Inf,a,sqrt(var1)) - pnorm(border_vec[8],a,sqrt(var1))
p_vec
sum(p_vec)

np_vec = M * p_vec
np_vec

hist(ksi_char_vec, breaks = seq(min1, max1, length.out = 9), labels = TRUE)
v_vec = c(5,18,29,34,21,16,6,1)
v_vec
sum(v_vec)

chi2v = sum(((v_vec - np_vec) ^ 2) / np_vec)
chi2v
qchisq(.95, df=num-2-1)

#install.packages('nortest')
library('nortest')
pearson.test(ksi_char_vec)


#eps_vec3 = t(replicate(M,sapply(c(1:(N+1)), function(y) rt(n=1,df=5))))
eps_vec3 = t(replicate(M,sapply(c(1:(N+1)), function(y) runif(n = 1, min = -sqrt(3)*sigma, max = sqrt(3)*sigma))))
ggdensity(eps_vec3[1,])

ksi_vec3 = array(c(rep(0, M),rep(0, M)),dim=c(M,N+1))
for (k in 1:M) {
  ksi_vec3[k,1] = eps_vec3[k,1]
  ksi_vec3[k,2] = eps_vec3[k,1] + eps_vec3[k,2] * (1 + alpha * (eps_vec3[k,1]))
  for (n in 2:N) {
    sum1 = 0
    for (j in 2:n) {
      sum1 = sum1 + eps_vec3[k,j+1] * (1 + alpha * eps_vec3[k,j] + beta * eps_vec3[k,j-1])
    }
    ksi_vec3[k,n+1] = eps_vec3[k,1] + eps_vec3[k,2] * (1 + alpha * (eps_vec3[k,1])) + sum1
  }
}

ksi_char_vec3 = array(c(rep(0, M),rep(0, M)),dim=c(M,1))
for (k in 1:M) {
  sum1 = 0
  for (n in 2:N) {
    sum1 = sum1 + (1 + alpha * eps_vec3[k, n] + beta * eps_vec3[k, n-1]) ^ 2
  }
  ksi_char_vec3[k] = ksi_vec3[k, N+1] / sqrt(sigma ^ 2 * (sum1 + (1 + alpha * eps_vec3[k, 1]) ^ 2))
}
ggdensity(ksi_char_vec3)  
shapiro.test(ksi_char_vec3)
pearson.test(ksi_char_vec3)

num = 1 + floor(log(M, base = 2))
num
min1 = min(ksi_char_vec3)
min1
max1 = max(ksi_char_vec3)
max1
hist(ksi_char_vec3, breaks = seq(min1, max1, length.out = 9),freq = FALSE)
curve(dnorm(x, mean=0, sd=1), 
      col="blue", lwd=2, add=TRUE)
legend('topright', legend="Standard normal distribution",
       col="blue", lwd = 2, cex = 0.7)

a = sum(ksi_char_vec3) / M
a
var1 = sum((ksi_char_vec3 - a) ^ 2) / (M - 1)
var1
sqrt(var1)
mean(ksi_char_vec3)
var(ksi_char_vec3)

d = max1 - min1
d
len1 = d / num
len1
border_vec = c(min1,min1 + len1,min1 + 2*len1,min1 + 3*len1,min1 + 4*len1,
               min1 + 5*len1,min1 + 6*len1,min1 + 7*len1,max1)
border_vec

p_vec = array(c(rep(0, M),rep(0, M)),dim=c(8,1))
p_vec[1] = pnorm(border_vec[2],a,sqrt(var1)) - pnorm(-Inf,a,sqrt(var1))
for (i in 2:7) {
  p_vec[i] = pnorm(border_vec[i+1],a,sqrt(var1)) - pnorm(border_vec[i],a,sqrt(var1))
}
p_vec[8] = pnorm(Inf,a,sqrt(var1)) - pnorm(border_vec[8],a,sqrt(var1))
p_vec
sum(p_vec)

np_vec = M * p_vec
np_vec

hist(ksi_char_vec3, breaks = seq(min1, max1, length.out = 9), labels = TRUE)
v_vec = c(4,7,23,35,34,18,7,2)
v_vec
sum(v_vec)

chi2v = sum(((v_vec - np_vec) ^ 2) / np_vec)
chi2v
qchisq(.95, df=num-1)

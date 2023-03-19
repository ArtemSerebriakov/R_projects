set.seed(12)
N = 1000
a0 = 0.6
a1 = 0.8
b1 = 0.2
sigma = 0.3
l = 18

eps_vec_n = rnorm(N+1, 0, sigma)
eps_vec_r = runif(n = N+1, min = -sqrt(3)*sigma, max = sqrt(3)*sigma)

library("ggpubr")
ggdensity(eps_vec_n)
ggdensity(eps_vec_r)

h_vec_n = rep(0, N+1)
h_vec_r = rep(0, N+1)

from = 2
to = N+1
for (i in from:to) {
  h_vec_n[i] = a0 + eps_vec_n[i] + a1 * h_vec_n[i-1] + 
    b1 * eps_vec_n[i-1]
  h_vec_r[i] = a0 + eps_vec_r[i] + a1 * h_vec_r[i-1] + 
    b1 * eps_vec_r[i-1]
}

plot(h_vec_n, type = "l", col = "blue", xlab = "n",
     ylab = "h_vec_n", pch = 20, ylim = c(-1,6))
abline(h=a0 / (1-a1), col='red', lty = 3, lwd = 2)

plot(h_vec_n[1:200], type = "l", col = "blue", xlab = "n",
     ylab = "h_vec_n", pch = 20, ylim = c(-1,6), xaxp = c(0, 200, 10))
abline(h=a0 / (1-a1), col='red', lty = 3, lwd = 2)

plot(h_vec_r, type = "l", col = "blue", xlab = "n",
     ylab = "h_vec_r", pch = 20, ylim = c(-1,6))
abline(h=a0 / (1-a1), col='red', lty = 3, lwd = 2)

plot(h_vec_r[1:200], type = "l", col = "blue", xlab = "n",
     ylab = "h_vec_r", pch = 20, ylim = c(-1,6), xaxp = c(0, 200, 10))
abline(h=a0 / (1-a1), col='red', lty = 3, lwd = 2)

h_vec_n
h_vec_r

mean_n = 0
from = l+1
to = N+1
for (i in from:to) {
  mean_n = mean_n + h_vec_n[i]
}
mean_n = mean_n / (N-l)
mean_n

mean_r = 0
from = l+1
to = N+1
for (i in from:to) {
  mean_r = mean_r + h_vec_r[i]
}
mean_r = mean_r / (N-l)
mean_r

a0 / (1-a1)

var_n = 0
from = l+1
to = N+1
for (i in from:to) {
  var_n = var_n + (h_vec_n[i]-mean_n) ^ 2
}
var_n = var_n / (N-l)
var_n

var_r = 0
from = l+1
to = N+1
for (i in from:to) {
  var_r = var_r + (h_vec_r[i]-mean_r) ^ 2
}
var_r = var_r / (N-l)
var_r

sigma ^ 2 * ((1 + 2 * a1 * b1 + b1 ^ 2) / (1 - a1 ^ 2))

cov_vec_n = rep(0, 3)
from = l+2
to = N+1
for (j in 1:3) {
  for (i in (from+j):N+1) {
    cov_vec_n[j] = cov_vec_n[j] + (h_vec_n[i]-mean_n) * (h_vec_n[i-j]-mean_n)
  }
  cov_vec_n[j] = cov_vec_n[j] / (N-l)
}
cov_vec_n

cov_vec_r = rep(0, 3)
from = l+2
to = N+1
for (j in 1:3) {
  for (i in (from+j):N+1) {
    cov_vec_r[j] = cov_vec_r[j] + (h_vec_r[i]-mean_r) * (h_vec_r[i-j]-mean_r)
  }
  cov_vec_r[j] = cov_vec_r[j] / (N-l)
}
cov_vec_r

cov_vec = rep(0, 3)
for (j in 1:3) {
  cov_vec[j] = sigma ^ 2 * (a1 + b1) * (a1 ^ (j-1) + (a1 ^ j * (a1 + b1)) /
                                          (1 - a1 ^ 2))
}
cov_vec

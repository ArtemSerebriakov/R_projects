set.seed(12)
tt = 5.5
a = 0.06
b = 0.6
ksi_0 = 1000
n = 20
h = 0.25
sigma = 1

N = tt / h
N
eta_vec_n = rnorm(N, 0, sqrt(h)*sigma)
eta_vec_n
plot(eta_vec_n, type = "l", col = "blue", xlab = "k", lwd = 2)

w_vec = rep(0, N+1)
from = 1
to = N
for (k in from:to) {
  w_vec[k+1] = w_vec[k] + eta_vec_n[k]
}
w_vec
plot(w_vec, type = "l", col = "blue", xlab = "k", lwd = 2)

tk_vec = rep(0, N+1)
from = 2
to = N+1
for (k in from:to) {
  tk_vec[k] = tk_vec[k-1] + h
}
tk_vec[N+1]==tt

ksi_vec_n = rep(0, N+1)
from = 1
to = N+1
for (k in from:to) {
  ksi_vec_n[k] = ksi_0 * exp((a - b^2 / 2) * tk_vec[k] + b * w_vec[k])
}
ksi_vec_n

x_vec_n = rep(0, N+1)
x_vec_n[1] = ksi_0
from = 1
to = N
for (k in from:to) {
  x_vec_n[k+1] = x_vec_n[k] + a * x_vec_n[k] * h + b * x_vec_n[k] * (w_vec[k+1] - w_vec[k])
}
x_vec_n

plot(x_vec_n, type = "l", col = "blue", xlab = "k", lwd = 2, ylim = c(0,1100))

plot(ksi_vec_n, type = "l", col = "blue", xlab = "k", lwd = 2, ylim = c(0,1100))
lines(x_vec_n, col='green', lty = 4, lwd = 2)
legend('topright', legend=c("ksi_tk", "x_k"),
       col=c("blue", "green"), lty=c(1,4), lwd = 2)


#20
ksi_vec_m = array(c(rep(0, n),rep(0, n)),dim=c(n,N+1))
ksi_vec_m[1,] = ksi_vec_n

x_vec_m = array(c(rep(0, n),rep(0, n)),dim=c(n,N+1))
x_vec_m[1,] = x_vec_n

from = 2
to = n
for (i in from:to) {
  #set.seed(i+1)
  #N = tt / h
  eta_vec = rnorm(N, 0, sqrt(h)*sigma)
  
  w_vec = rep(0, N+1)
  from = 1
  to = N
  for (k in from:to) {
    w_vec[k+1] = w_vec[k] + eta_vec[k]
  }
  
  from = 1
  to = N+1
  for (k in from:to) {
    ksi_vec_m[i, k] = ksi_0 * exp((a - b^2 / 2) * tk_vec[k] + b * w_vec[k])
  }
  
  x_vec_m[i, 1] = ksi_0
  from = 1
  to = N
  for (k in from:to) {
    x_vec_m[i, k+1] = x_vec_m[i, k] + a * x_vec_m[i, k] * h + b * x_vec_m[i, k] * (w_vec[k+1] - w_vec[k])
  }
}

ksi_vec_m[, N+1]
x_vec_m[, N+1]
mean(abs(ksi_vec_m[, N+1] - x_vec_m[, N+1]))


#error
519.1073 #0.5
189.8324 #0.25
134.1402 #0.1
218.2204 #0.05
206.588 #0.025
88.55586 #0.01
29.86488 #0.005
16.26568 #0.001

eps_vec = c(519.1073, 189.8324, 134.1402, 218.2204, 206.588, 88.55586,
            29.86488, 16.26568)
h_vec = c(0.5, 0.25, 0.1, 0.05, 0.025, 0.01, 0.005, 0.001)

vec_x = log(h_vec)
vec_x
vec_y = log(eps_vec)
vec_y

beta = sum((vec_y - mean(vec_y)) * (vec_x - mean(vec_x))) / sum((vec_x - mean(vec_x))^2)
beta
alpha = mean(vec_y) - beta * mean(vec_x)
alpha

x_arg = seq(-8, 0, 0.01)
f = alpha + beta * x_arg

plot(vec_x, vec_y, col = "blue", lwd = 2, xlab = "x", ylab = "y")
lines(x_arg, f, type = 'l', col='green', lty = 2, lwd = 2)
legend('topleft', legend=c("vec_x, vec_y", "f"),
       col=c("blue", "green"), lty=c(1,4), lwd = 2)

FF = cbind(c(1,1,1,1,1,1,1,1),vec_x)
FF

library(matlib)
t(FF) %*% FF
inv(t(FF) %*% FF) %*% t(FF) %*% vec_y

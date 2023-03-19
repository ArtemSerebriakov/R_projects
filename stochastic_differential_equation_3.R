set.seed(12)
n = 100
alpha = 0.6
a = 1.8
sigma = 1.2
var_eps = 2.3
var_v = 1.5

#1
ksi_vec_n = rep(0, n)
ksi_vec_n_0 = rnorm(1, a, sigma)
ksi_vec_n_0

eps_vec_n = rnorm(n, 0, sqrt(var_eps))
v_vec_n = rnorm(n, 0, sqrt(var_v))

head(eps_vec_n)
head(v_vec_n)

ksi_vec_n[1] = eps_vec_n[1] + alpha * ksi_vec_n_0
from = 2
to = n
for (i in from:to) {
  ksi_vec_n[i] = eps_vec_n[i] + alpha * ksi_vec_n[i-1]
}
head(ksi_vec_n)

eta_vec_n = ksi_vec_n + v_vec_n
head(eta_vec_n)

ksi_vec_n_est_0 = a
k_vec_n_0 = sigma ^ 2 / var_v
rho = var_eps / var_v

k_vec_n = rep(0, n)
k_vec_n[1] = 1 - 1 / (a ^ 2 * k_vec_n_0 + rho + 1) 
from = 2
to = n
for (i in from:to) {
  k_vec_n[i] = 1 - 1 / (a ^ 2 * k_vec_n[i-1] + rho + 1)
}

ksi_vec_n_est = rep(0, n)
ksi_vec_n_est[1] = alpha * ksi_vec_n_est_0 + k_vec_n[1] * 
  (eta_vec_n[1] - alpha * ksi_vec_n_est_0)
from = 2
to = n
for (i in from:to) {
  ksi_vec_n_est[i] = alpha * ksi_vec_n_est[i-1] + k_vec_n[i] * 
    (eta_vec_n[i] - alpha * ksi_vec_n_est[i-1])
}

mean((ksi_vec_n_est - ksi_vec_n)^2)
var_v

plot(ksi_vec_n, type = "l", col = "blue", xlab = "n", lwd = 2, ylim = c(-4, 7))
#lines(eta_vec_n_r, col='red', lty = 3, lwd = 2)
lines(ksi_vec_n_est, col='green', lty = 4, lwd = 2)
legend(70, 7, legend=c("ksi_n", "ksi_est_n"),
       col=c("blue", "green"), lty=c(1,4), lwd = 2)


#2
ksi_vec_n_r = rep(0, n)
ksi_vec_n_r_0 = rnorm(1, a, sigma)

eps_vec_n_r = runif(n, min = -sqrt(3)*sqrt(var_eps), max = sqrt(3)*sqrt(var_eps))
v_vec_n_r = runif(n, min = -sqrt(3)*sqrt(var_v), max = sqrt(3)*sqrt(var_v))

ksi_vec_n_r[1] = eps_vec_n_r[1] + alpha * ksi_vec_n_r_0
from = 2
to = n
for (i in from:to) {
  ksi_vec_n_r[i] = eps_vec_n_r[i] + alpha * ksi_vec_n[i-1]
}

eta_vec_n_r = ksi_vec_n_r + v_vec_n_r

ksi_vec_n_r_est_0 = a
k_vec_n_r_0 = sigma ^ 2 / var_v
rho = var_eps / var_v

k_vec_n_r = rep(0, n)
k_vec_n_r[1] = 1 - 1 / (a ^ 2 * k_vec_n_r_0 + rho + 1) 
from = 2
to = n
for (i in from:to) {
  k_vec_n_r[i] = 1 - 1 / (a ^ 2 * k_vec_n_r[i-1] + rho + 1)
}

ksi_vec_n_r_est = rep(0, n)
ksi_vec_n_r_est[1] = alpha * ksi_vec_n_r_est_0 + k_vec_n_r[1] * 
  (eta_vec_n_r[1] - alpha * ksi_vec_n_r_est_0)
from = 2
to = n
for (i in from:to) {
  ksi_vec_n_r_est[i] = alpha * ksi_vec_n_r_est[i-1] + k_vec_n_r[i] * 
    (eta_vec_n_r[i] - alpha * ksi_vec_n_r_est[i-1])
}

mean((ksi_vec_n_r_est - ksi_vec_n_r)^2)
var_v

plot(ksi_vec_n_r, type = "l", col = "blue", xlab = "n", lwd = 2, ylim = c(-4, 7))
#lines(eta_vec_n_r, col='red', lty = 3, lwd = 2)
lines(ksi_vec_n_r_est, col='green', lty = 4, lwd = 2)
legend(70, 7, legend=c("ksi_n", "ksi_est_n"),
       col=c("blue", "green"), lty=c(1,4), lwd = 2)

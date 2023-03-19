set.seed(3008)
library(ggplot2)


Tt = 10
H = 0.05
N = 100

n = Tt/H
n

kh = seq(0,Tt,H)
head(kh)
tail(kh)
M = function(t) {1 + exp(-t/2)}
vec = M(kh)
head(vec)
tail(vec)

covar = function(t_1,t_2) {3/(3+20*(t_2-t_1)^2)}
cov_mat = matrix(0,nrow = 201,ncol = 201)
for (i in 1:201)
  for (j in 1:201)
    cov_mat[i,j] = covar(kh[i],kh[j])
cov_mat[1:5,1:5]

eps = rnorm(n+1, mean = 0, sd = 1)
head(eps)
tail(eps)
eps

L = t(chol(cov_mat))
round(L[1:5,1:5],4)
res = L %*% t(L)
res[1:5,1:5]

eta = as.numeric(L %*% eps)
head(eta)
tail(eta)


ksi = vec + eta
head(ksi)
tail(ksi)


trajectories = function(Tt, H)
{
  n = Tt/H
  kh = seq(0,Tt,H)
  vec = M(kh)
  cov_mat = matrix(0,nrow = 201,ncol = 201)
  for (i in 1:201)
    for (j in 1:201)
      cov_mat[i,j] = covar(kh[i],kh[j])
  eps = rnorm(n+1, mean = 0, sd = 1)
  L = t(chol(cov_mat))
  eta = as.numeric(L %*% eps)
  ksi = vec + eta
  return(ksi)
}
traj_list = replicate(N, trajectories(Tt, H), simplify = F)
head(traj_list)

round(traj_list[[1]], 3)
round(traj_list[[75]], 3)
round(traj_list[[97]], 3)

trajs = data.frame(x = kh, m_val = vec,
                   traj_1 = round(traj_list[[1]],3),
                   traj_75 = round(traj_list[[75]],3),
                   traj_97 = round(traj_list[[97]],3))

trajs_pl = ggplot(data = trajs, aes(x=x)) + 
  geom_line(aes(y=m_val,colour="m(t)"), lty = 2, lwd = 2.0) +
  geom_line(aes(y=traj_1,colour="Trajectory 1"), lwd = 1.6) + 
  geom_line(aes(y=traj_75,colour="Trajectory 75"), lwd = 1.5) + 
  geom_line(aes(y=traj_97,colour="Trajectory 97"), lwd = 1.2) +
  scale_color_discrete(name = "", 
                       labels = c("m(t)", 
                                  "Trajectory 1",
                                  "Trajectory 75",
                                  "Trajectory 97")) +
  scale_color_manual(values = c("yellow", "green","red","black")) +
  labs(title = "Совмещенный график трех траекторий и математического ожидания",x = "", y = "") +
  theme_classic()

trajs_pl


t1 = 1
t2 = 2
cut1 = as.data.frame(matrix(c(sapply(traj_list, `[[`, t1),
                              sapply(traj_list, `[[`, t2)),
                            ncol = 2, byrow = F))
colnames(cut1) = c("t1","t2")
head(cut1)

ggplot(cut1, aes(x = t1, y = t2)) + geom_point() +
  labs(title = "Диаграмма рассеяния",
       subtitle = "t1 = 1, t2 = 2",
       x = "t1", y = "t2") +
  theme_bw()


r = (sum((cut1$t1 - mean(cut1$t1))*
           (cut1$t2 - mean(cut1$t2))))/ 
  (sqrt(sum((cut1$t1 - mean(cut1$t1))^2) *
          sum((cut1$t2 - mean(cut1$t2))^2)))
r

tanh(atanh(r)-r/(2*(100-1))-qnorm(0.975)/sqrt(100-3))
tanh(atanh(r)-r/(2*(201-1))+qnorm(0.975)/sqrt(201-3))

cor.test(cut1$t1,cut1$t2,method = "pearson")

r_tr = cov_mat[t1,t2]/sqrt(cov_mat[t1,t1] * cov_mat[t2,t2])
r_tr


t1 = 10
t2 = 15
cut1 = as.data.frame(matrix(c(sapply(traj_list, `[[`, t1),
                              sapply(traj_list, `[[`, t2)),
                            ncol = 2, byrow = F))
colnames(cut1) = c("t1","t2")
head(cut1)

ggplot(cut1, aes(x = t1, y = t2)) + geom_point() +
  labs(title = "Диаграмма рассеяния",
       subtitle = "t1 = 10, t2 = 15",
       x = "t1", y = "t2") +
  theme_bw()


r = (sum((cut1$t1 - mean(cut1$t1))*
           (cut1$t2 - mean(cut1$t2))))/ 
  (sqrt(sum((cut1$t1 - mean(cut1$t1))^2) *
          sum((cut1$t2 - mean(cut1$t2))^2)))
r

cor.test(cut1$t1,cut1$t2,method = "pearson")

r_tr = cov_mat[t1,t2]/sqrt(cov_mat[t1,t1] * cov_mat[t2,t2])
r_tr


t1 = 30
t2 = 90
cut1 = as.data.frame(matrix(c(sapply(traj_list, `[[`, t1),
                              sapply(traj_list, `[[`, t2)),
                            ncol = 2, byrow = F))
colnames(cut1) = c("t1","t2")
head(cut1)

ggplot(cut1, aes(x = t1, y = t2)) + geom_point() +
  labs(title = "Диаграмма рассеяния",
       subtitle = "t1 = 30, t2 = 90",
       x = "t1", y = "t2") +
  theme_bw()


r = (sum((cut1$t1 - mean(cut1$t1))*
           (cut1$t2 - mean(cut1$t2))))/ 
  (sqrt(sum((cut1$t1 - mean(cut1$t1))^2) *
          sum((cut1$t2 - mean(cut1$t2))^2)))
r

cor.test(cut1$t1,cut1$t2,method = "pearson")

r_tr = cov_mat[t1,t2]/sqrt(cov_mat[t1,t1] * cov_mat[t2,t2])
r_tr

paste0(format(round(as.numeric(unlist(cut1['t2'])),3),nsmall = 3), collapse=",")
covar(t1,t1)
vec[t2+1]
vec

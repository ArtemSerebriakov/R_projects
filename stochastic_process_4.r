library("data.table")
addTaskCallback(function(...) {set.seed(2008);TRUE})
### ????????? ??????
tt = 14
n = 100
sigma = 0.25
h = 0.1
z = 1.5
#####
N = tt/h
N

pair_ksi_vec = t(replicate(2*N,sapply(c(1,2), function(y) rnorm(1, 0, sqrt(h/2)*sigma))))
head(pair_ksi_vec)
tail(pair_ksi_vec)
pair_ksi_vec[1,]
shapiro.test(pair_ksi_vec[1,])
s = (replicate(2*N,sapply(c(1,2), function(y) rnorm(1, 0, sqrt(h/2)*sigma))))
head(s)

pair_ksi_vec2 = array(c(rep(0, N),rep(0, N)),dim=c(N,2))
for (k in 1:N) {
  pair_ksi_vec2[k,] = (pair_ksi_vec[2*k-1,]+pair_ksi_vec[2*k,])
}
head(pair_ksi_vec2)
tail(pair_ksi_vec2)

sigma*sqrt(h)
sqrt(var(pair_ksi_vec2[, 1])*h)
mean(pair_ksi_vec2[, 1])


pair_w_vec2 = apply(rbind(c(0,0),pair_ksi_vec2), 2, cumsum)
head(pair_w_vec2)
tail(pair_w_vec2)

# ???????????? ????? ??????????
pairfun = function() 
{
  pair_ksi_vec_k = t(replicate(2*N,sapply(c(1,2), function(y) rnorm(1, 0, sqrt(h/2)*sigma))))
  pair_ksi_vec_k2 = array(c(rep(0, N),rep(0, N)),dim=c(N,2))
  for (k in 1:N) {
    pair_ksi_vec_k2[k,] = (pair_ksi_vec_k[2*k-1,]+pair_ksi_vec_k[2*k,])
  }
  return(apply(rbind(c(0,0),pair_ksi_vec_k2), 2, cumsum))
}

###1
pairs_list1 = replicate(n, pairfun(), F)
pairs_list1


head(pairs_list1[[1]])
tail(pairs_list1[[1]])

head(pairs_list1[[10]])
tail(pairs_list1[[10]])

head(pairs_list1[[20]])
tail(pairs_list1[[20]])

head(pairs_list1[[30]])
tail(pairs_list1[[30]])

head(pairs_list1[[40]])
tail(pairs_list1[[40]])

head(pairs_list1[[50]])
tail(pairs_list1[[50]])

plot(pairs_list1[[1]], type = "l", main = "Trajectory 1", 
     xlab = "Wkh(1)", ylab = "Wkh(2)")

plot(pairs_list1[[10]], type = "l", main = "Trajectory 10", 
     xlab = "Wkh(1)", ylab = "Wkh(2)")

plot(pairs_list1[[20]], type = "l", main = "Trajectory 20", 
     xlab = "Wkh(1)", ylab = "Wkh(2)")

plot(pairs_list1[[30]], type = "l", main = "Trajectory 30", 
     xlab = "Wkh(1)", ylab = "Wkh(2)")

plot(pairs_list1[[40]], type = "l", main = "Trajectory 40", 
     xlab = "Wkh(1)", ylab = "Wkh(2)")

plot(pairs_list1[[50]], type = "l", main = "Trajectory 50", 
     xlab = "Wkh(1)", ylab = "Wkh(2)")


###3

var_fun = function(x)
{
  sum(abs(diff(x)))
}

vars = t(sapply(pairs_list1, function(x) apply(x, 2, var_fun)))
head(vars)
tail(vars)

var12h = colMeans(vars)
var12h

#

sq_var_fun = function(x)
{
  sum(abs(diff(x))^2)
}

sq_vars = t(sapply(pairs_list1, function(x) apply(x, 2, sq_var_fun)))
head(sq_vars)
tail(sq_vars)

sq_var12h = colMeans(sq_vars)
sq_var12h


###4
h = 0.1/2
h
N = tt/h
N

pairs_list2 = replicate(n, pairfun(), F)


vars2 = t(sapply(pairs_list2, function(x) apply(x, 2, var_fun)))
head(vars2)
tail(vars2)

var12h2 = colMeans(vars2)
var12h2

sq_vars2 = t(sapply(pairs_list2, function(x) apply(x, 2, sq_var_fun)))
head(sq_vars2)
tail(sq_vars2)

sq_var12h2 = colMeans(sq_vars2)
sq_var12h2

compare_table = data.table(mean_vars_h1 = var12h,
                           mean_vars_h2 = var12h2,
                           mean_sq_vars_h1 = sq_var12h,
                           mean_sq_vars_h2 = sq_var12h2)
compare_table
compare_table_relate = data.table(compare_table[,1] / compare_table[,2],
                                compare_table[,3] / compare_table[,4])
compare_table_relate


###5
c = 0
for (i in 1:100){
  if (sqrt(pairs_list1[[i]][141,1]^2 + pairs_list1[[i]][141,2]^2) >= z){
    c = c + 1
  }}
c
empiric_prob = c / n
empiric_prob

theor_prob = 1 - pchisq(z^2/(sigma^2 * tt),2)
theor_prob

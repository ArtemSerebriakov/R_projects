alpha=0.1
a0=5.5 
sigma0=2.4
a1=5
sigma1=2.5
eps=0.2
n=90

df=unlist(read.csv(file="C:\\Users\\Sereb\\Downloads\\data.csv", header = F, sep = ";"), use.names = F)
df
df[1]=-2.247
df=as.numeric(df)

df_mean=mean(df)
df_mean
df_sd=sd(df)
df_sd
df_len=length(df)
df_len

#1
stat=(df_mean - a0)/(df_sd) * sqrt(df_len)
stat
-qt(1-alpha,df_len-1)

#2
df_var=var(df)
chi_stat=(df_len-1)*(df_var)/(sigma0^2)
chi_stat
qchisq(1-alpha, df_len-1)

#3
c=a0*n -(qnorm(1-alpha))*(sqrt(df_len))*sigma1
c
sum(df)

#4
beta=pnorm((c-a1*n)/(sigma1*sqrt(df_len)))
beta
1-beta

#5
a11=(c-sigma1*sqrt(n)*qnorm(1-eps))/df_len
a11

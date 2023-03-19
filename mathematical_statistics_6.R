alpha=0.1
a0=5.5 
sigma0=2.4
a1=5
sigma1=2.5
eps=0.2
n=90
beta=0.2690083

df=unlist(read.csv(file="C:\\Users\\Sereb\\Downloads\\data.csv", header = F, sep = ";"), use.names = F)
df
df[1]=-2.247
df=as.numeric(df)
df

df_mean=mean(df)
df_mean
df_sd=sd(df)
df_sd
df_len=length(df)
df_len

A=(1-beta)/alpha
A
B=beta/(1-alpha)
B

M0Yk=-(a0-a1)^2 / (2*sigma1^2)
M0Yk
M1Yk=(a0-a1)^2 / (2*sigma1^2)
M1Yk

M0_nu=(alpha*log(A) + (1-alpha)*log(B))/M0Yk
M0_nu
M1_nu=((1-beta)*log(A) + beta*log(B))/M1Yk
M1_nu

c1=464.6053
c=exp(((a1-a0)*c1)/sigma1^2+(n*(a0^2-a1^2))/(2*sigma1^2))
c

x1=0:10
plot(, type = "s", lwd = 3,xlab = "x",col="red", ylab = "F(x)")
lines(pbinom(x1, size = k, prob = pu002), type = "s",lty=2, lwd = 3,col="blue")
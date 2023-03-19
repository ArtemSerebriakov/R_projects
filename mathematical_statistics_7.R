n=250
X=sqrt(runif(n))
X
min1=min(X)
min1
max1=max(X)
max1
d=max1-min1
d
num=1+floor(log2(n))
num
len=d/num
len
bvector=c(min1,min1+len, min1+2*len, min1+3*len, min1+4*len, min1+5*len, min1+6*len, min1+7*len, max1)
bvector
hist1=hist(X,breaks=bvector,plot=FALSE)
hist1$counts

m=10000
z=c(1:m)
Y=matrix(0,nrow=m,ncol=num)
for (j in z){
  V=sqrt(runif(n))
  min11=min(V)
  max11=max(V)
  d1=max11-min11
  num1=1+floor(log2(n))
  len1=d1/num1
  bvector1=c(min11,min11+len1, min11+2*len1, min11+3*len1, min11+4*len1, min11+5*len1, min11+6*len1, min11+7*len1, max11)
  hist11=hist(V,breaks=bvector1,plot=FALSE)
  for (i in 1:num1){
    Y[j,i]=hist11$counts[i]
  }
}

p_t=c()
for (i in 1:num){
  p_t[i]=(2*i-1)/num^2
}

p_t

D=c()
for (i in z){
  maxx=(1/n)*abs(Y[i,1]-n*p_t[1])
  for (j in 2:num){
    if((1/n)*abs(Y[i,j]-n*p_t[j])>maxx){
      maxx=(1/n)*abs(Y[i,j]-n*p_t[j])
    }
  }
  D[i]=maxx
}

D_s=sort(D)
D_s
View(D_s)


min2=min(D_s)
min2
max2=max(D_s)
max2
d2=max2-min2
d2
num2=1+floor(log2(m))
num2
len2=d2/num2
len2
breaks=c(min2,min2+len2, min2+2*len2, min2+3*len2, min2+4*len2, min2+5*len2, min2+6*len2, min2+7*len2,
           min2+8*len2,min2+9*len2,min2+10*len2,min2+11*len2,min2+12*len2,min2+13*len2,max2)
breaks
hist2=hist(D_s,breaks=breaks,plot=FALSE)
hist2$counts
p=hist2$counts/m
p
f1=p/len2
f1

labs <- sprintf("[%.3f,%.3f)", breaks[-length(breaks)], breaks[-1])
labs[length(labs)] <- gsub("\\)$", "]", labs[length(labs)])
labs
DF <- data.frame(
  abs_freqs =hist2$counts ,
  rel_freqs = f1,
  labs = labs
)
barplot(f1 ~ labs, data = DF, xlab = "",ylab="")



D_s[9000]
D_s[9001]
D_s[9500]
D_s[9501]
D_s[9900]
D_s[9901]

n=250
X1=runif(n)
X1
min3=min(X1)
min3
max3=max(X1)
max3
d3=max3-min3
d3
num3=1+floor(log2(n))
num3
len3=d3/num3
len3
bvector3=c(min3,min3+len3, min3+2*len3, min3+3*len3, min3+4*len3, min3+5*len3, min3+6*len3, min3+7*len3, max3)
bvector3
hist3=hist(X1,breaks=bvector3,plot=FALSE)
hist3$counts
maxx3=(1/n)*abs(hist3$counts[1]-n*p_t[1])
for (j in 2:num3){
  if((1/n)*abs(hist3$counts[j]-n*p_t[j])>maxx3){
    maxx3=(1/n)*abs(hist3$counts[j]-n*p_t[j])
  }
}
maxx3

n=250
X1=sqrt(runif(n))
X1
min3=min(X1)
min3
max3=max(X1)
max3
d3=max3-min3
d3
num3=1+floor(log2(n))
num3
len3=d3/num3
len3
bvector3=c(min3,min3+len3, min3+2*len3, min3+3*len3, min3+4*len3, min3+5*len3, min3+6*len3, min3+7*len3, max3)
bvector3
hist3=hist(X1,breaks=bvector3,plot=FALSE)
hist3$counts
maxx3=(1/n)*abs(hist3$counts[1]-n*p_t[1])
for (j in 2:num3){
  if((1/n)*abs(hist3$counts[j]-n*p_t[j])>maxx3){
    maxx3=(1/n)*abs(hist3$counts[j]-n*p_t[j])
  }
}
maxx3

n=250
X1=rbeta(n,5,2)
X1
min3=min(X1)
min3
max3=max(X1)
max3
d3=max3-min3
d3
num3=1+floor(log2(n))
num3
len3=d3/num3
len3
bvector3=c(min3,min3+len3, min3+2*len3, min3+3*len3, min3+4*len3, min3+5*len3, min3+6*len3, min3+7*len3, max3)
bvector3
hist3=hist(X1,breaks=bvector3,plot=FALSE)
hist3$counts
maxx3=(1/n)*abs(hist3$counts[1]-n*p_t[1])
for (j in 2:num3){
  if((1/n)*abs(hist3$counts[j]-n*p_t[j])>maxx3){
    maxx3=(1/n)*abs(hist3$counts[j]-n*p_t[j])
  }
}
maxx3

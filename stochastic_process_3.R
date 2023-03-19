### Начальные данные:
m=5 # Число состояний марковской цепи
k=6 # время (шаги)
n=120 # траектории


#### Моделирование цепи маркова с m состояниями
# 1. Генерируем (m+1) раз вектор r=(r_1, ..., r_{m-1}) из независимых 
#             и равномерно распределенных на отрезке
#             [0;1] случайных величин

r_vec=replicate((m+1), runif((m-1), min = 0, max = 1), simplify = F)
r_vec

# 2. Для каждого из полученный векторов строим вариационный ряд, то есть упорядочиваем по возрастанию

r=lapply(r_vec, sort)
r
# 3. Находим длины отрезков, на которые вектор r разбивает отрезок [0;1] --
#             получаем вектор вероятностей p

p_vec=lapply(r, diff)
p_heads=lapply(r, head, 1)
p_tails=lapply(r, function(x) (1-tail(x,1)))
p=Map(append, Map(append, p_heads,p_vec),p_tails)
p
mapply(sum, p)


# 4. Первый из полученных векторов p считаем вектором начальных вероятностей, 
#             из остальных составляем матрицу переходов P,
#             записывая их по строкам

p0=p[[1]]
p0
P=t(simplify2array(p))
P=P[-1,]
round(P,3)
P

##### Построение размеченного графа состояний цепи
install.packages("diagram")
install.packages("markovchain")
library(markovchain)
library(diagram)

#png(filename = "../img/1.png",
    #width = 1920, height = 1080,
    #res = 96 * 1.25)

plotmat(round(t(P),3), 
        lwd = 1, box.lwd = 2, 
        cex.txt = 0.9, 
        box.size = 0.04, 
        box.type = "circle", 
        box.prop = 0.5,
        shadow.size = 0,
        arr.length=.25,
        arr.width=.3,
        self.cex = .7,
        self.shifty = -.01,
        self.shiftx = .07)

#### Вычисление безусловных вероятностей состояний цепи на k шаге
install.packages("matrixcalc")
library(matrixcalc)

p_k=p0 %*% matrix.power(P,k)
p_k

#t(matrix.power(P,k))%*%p0
#p0

#p_k2 <- p0 %*% matrix.power(P,2)
#p_k2
#p_k3 <- p0 %*% matrix.power(P,3)
#p_k3
#p_k4 <- p0 %*% matrix.power(P,4)
#p_k4
#p_k5 <- p0 %*% matrix.power(P,5)
#p_k5
#p_k10 <- p0 %*% matrix.power(P,10)
#p_k10

#### Моделирование траектории длины k цепи маркова
#1. Генерируем равномерно распределенную на [0;1] случайную величину r0 и по вектору p0
#             разыгрываем начальное состояние

trajs=list()
for (i in 1:n){
  foo=function(v,j)
  {
    ifelse(v < r[[j+1]][1],1,
           ifelse(v < r[[j+1]][2],2,
                  ifelse(v < r[[j+1]][3],3,
                         ifelse(v < r[[j+1]][4],4,5))))
  }
  r0=runif(1, min = 0, max = 1)
  r0
  j0=foo(r0,0)
  j0
  
  r1=runif(1, min = 0, max = 1)
  r1
  j1=foo(r1,j0)
  j1
  
  r2=runif(1, min = 0, max = 1)
  r2
  j2=foo(r2,j1)
  j2
  
  r3=runif(1, min = 0, max = 1)
  r3
  j3=foo(r3,j2)
  j3
  
  r4=runif(1, min = 0, max = 1)
  r4
  j4=foo(r4,j3)
  j4
  
  r5=runif(1, min = 0, max = 1)
  r5
  j5=foo(r5,j4)
  j5
  
  r6=runif(1, min = 0, max = 1)
  r6
  j6=foo(r6,j5)
  j6
  
  traj=list(c(j0,j1,j2,j3,j4,j5,j6))
  trajs[i]=traj
}

r0
r[[1]]
j0

r1
r[[j0+1]]
j1

r2
r[[j1+1]]
j2

r3
r[[j2+1]]
j3

r4
r[[j3+1]]
j4

r5
r[[j4+1]]
j5

r6
r[[j5+1]]
j6

traj

arr=t(simplify2array(trajs,higher = F))
colnames(arr)=paste("Шаг",as.character(0:k))
rownames(arr)=paste("Траек",as.character(1:n))
arr
head(arr,6)
tail(arr,6)

#arr3=arr[3,]
#arr3

#arr5=arr[5,]
#arr5

#arr118=arr[118,]
#arr118

#arr116=arr[116,]
#arr116

#mtx=rbind(arr3,arr5,arr116,arr118)
#mtx

#matplot(mtx[1,],type='l',xlab="ШАГ",ylab="ТРАЕКТОРИИ",col="green",ylim=c(1.0,5.0),lwd=3)
#matlines(mtx[2,],lty=2,col="red",lwd=4)
#matlines(mtx[3,],lty=3,col="blue",lwd=4)
#matlines(mtx[4,],lty=4,col="yellow",lwd=4)
#legend("topright",legend=c("Траектория 3","Траектория 5","Траектория 116","Траектория 118"),col=c("green","red","blue","yellow"),lty=c(1,2,3,4),lwd=c(3,4,4,4))

df=data.frame(trajectory = rep(c("Траектория 3", "Траектория 5","Траектория 116","Траектория 118"), each=k+1),
               step = rep(paste("ШАГ",as.character(0:k),sep = " "), 2),
               state = c(as.numeric(arr[3,]),as.numeric(arr[5,]),as.numeric(arr[116,]),as.numeric(arr[118,])))
df

library(ggplot2)

ggplot(data=df, aes(x=step, y=state, fill=trajectory)) +
  geom_bar(stat="identity", position=position_dodge()) + 
  scale_fill_manual(values=c("#999999", "#E69F00","#E17204", "#E11E04"))+
  theme_minimal()+ggtitle("Траектории")

### Вычисление эмпирических вероятностей (относительных частот) 
#         состояний  цепи на k шаге.

emp=list()
for(i in 0:k){
hist(arr[,i+1], breaks =0:m)$counts
emp[i]=list(hist(arr[,i+1], breaks =0:m)$counts/n)}
emp

hist(arr[,k+1], breaks =0:m)$counts
empk=hist(arr[,k+1], breaks =0:m)$counts/n
empk

theork=as.numeric(p_k)
theork

df1=data.frame(type = rep(c("Теоретическая", "Эмпирическая"), each=m),
                      state = rep(paste("S",as.character(1:m),sep = ""), 2),
                      prob = c(theork, empk))
df1

#png(filename = "../img/2.png",
   # width = 1920, height = 1080,
   # res = 96 * 2)

ggplot(data=df1, aes(x=state, y=prob, fill=type)) +
  geom_bar(stat="identity", position=position_dodge()) + 
  scale_fill_manual(values=c("#999999", "#E69F00"))+
  theme_minimal()+ggtitle("Шестой шаг")

#dev.off()

empk
theork
max(abs(empk - theork))


hist(arr[,1], breaks =0:m)$counts
emp0=hist(arr[,1], breaks =0:m)$counts/n
emp0

theor0=as.numeric(p0)
theor0

df2=data.frame(type = rep(c("Теоретическая", "Эмпирическая"), each=m),
               state = rep(paste("S",as.character(1:m),sep = ""), 2),
               prob = c(theor0, emp0))
df2

ggplot(data=df2, aes(x=state, y=prob, fill=type)) +
  geom_bar(stat="identity", position=position_dodge()) + 
  scale_fill_manual(values=c("#999999", "#E69F00"))+
  theme_minimal()+ggtitle("Начальное состояние")

emp0
theor0
max(abs(emp0 - theor0))

###Вычисление финальных вероятностей для Марковской цепи из задачи 1 
#       и сравнение их с вероятностями состояний на k шаге
install.packages("matlib")
library(matlib)
P
b=c(rep(0,m-1),1)
b
mat1=rbind((t(P) - diag(m))[-m,],rep(1,m))
mat1
res1=solve(mat1,b)
res1

as.numeric(p_k)
max(abs(res1-as.numeric(p_k)))

P-diag(m)

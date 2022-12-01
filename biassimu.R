(x<-rbinom(10,16,0.5))
sum(x)/10

meanx<-rep(0,1000)
for (i in 1:1000){meanx[i]<-mean(rbinom(10,16,0.5))}
mean(meanx)

ssx<-rep(0,1000)
for (i in 1:1000){
  x<-rbinom(10,16,0.5);ssx[i] <- sum((x-mean(x))^2)}
mean(ssx)
mean(ssx)/10;mean(ssx)/9

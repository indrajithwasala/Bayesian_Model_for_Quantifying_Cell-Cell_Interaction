rm(list=ls())
library(lattice)
library(viridis)
library(gridExtra)
library(ggplot2)


# Load functions
source("functions_potts.R");
Rcpp::sourceCpp('functions_potts.cpp');

MOB.data<-count
str(MOB.data)

#removing rows which has row sum<10
loc<-loc[-(which(rowSums(MOB.data)<10)),] 
loc<-as.data.frame(round(loc))
MOB.data<-MOB.data[!rowSums(MOB.data)<10,]

MOB.data.N <- apply(MOB.data, 1, function(i) i/sum(i)) #normalized data. dividing each element by corresponding rowsum
MOB.data.B <-apply(MOB.data.N, 1, function(i) i>median(i)) #making data into logical based on median
MOB.data.B <- 1*MOB.data.B
MOB.data.B[MOB.data.B=="1"]<-2
MOB.data.B[MOB.data.B=="0"]<-1
str(MOB.data)
str(MOB.data.N)
str(MOB.data.B)

#function to create matrix m (rearranging data )
mf<-function(m,d,loc){
  for(i in 1:260)
  {
    m[loc[i,1],loc[i,2]]<-d[i,]
  }
  return(m)
}

#intializing
L <- 28;
H <- 28;
Q <- 2;
thetat <- 0;
Thetat <- array2matrix_r(thetat);
p<-mu<-upper.bound<-lower.bound<-w<-tstat<-sh<-sd<-c()

name<-colnames(MOB.data)
m<-matrix(0,28,28)

pdf("meadian")
for(i in 1:100)
{
  M<-mf(m,as.data.frame(MOB.data[,i]),loc)
  M1<-mf(m,as.data.frame(MOB.data.N[i,]),loc)
  M2<-mf(m,as.data.frame(MOB.data.B[,i]),loc)
  
  res <- potts_2(M2)
  res1<-res$theta[5020:10000]
  mu[i]<-mean(res1)
  sd[i]<-sd(res1)
  lower.bound[i]<-quantile(res1,0.025)
  upper.bound[i]<-quantile(res1,0.975)
  sh[i]<-shapiro.test(res1)$p.val
  p[i]<-t.test(res1,mu=0,alternative = "less",conf.level = 0.95,var.equal=TRUE)$p.value
  tstat[i]<-t.test(res1,mu=0,alternative = "less",conf.level = 0.95,var.equal=TRUE)$statistic
  w[i]<-wilcox.test(res1, mu = 0, alternative="less")$p.val
  
  plot1<-levelplot(M,col.regions=rev(magma(100)),main=paste(name[i]),ylab="",xlab="plot for original data")
  plot2<-levelplot(M1,col.regions=rev(magma(100)),ylab="",xlab="plot for normalize data")
  plot3<-levelplot(M2,col.regions=rev(magma(100)),ylab="",xlab="plot for binary data")
  plot4<- ggplot(as.data.frame(res1), aes(x=res1)) +
    geom_histogram(binwidth=0.05,colour="black", fill="white")+
    geom_vline(aes(xintercept=thetat),
               color="red", linetype="dashed", size=1)+
    geom_vline(aes(xintercept=quantile(res1,0.025)),  
               color="green", linetype="dashed", size=1)+
    geom_vline(aes(xintercept=quantile(res1,0.975)),
               color="green", linetype="dashed", size=1)+
    ggtitle(paste("Pvalue =", p[i]))
  grid.arrange(plot1,plot2,plot3,plot4,ncol=2,nrow=2)
}

dev.off()

out.median<-data.frame(name[1:100],lower.bound,upper.bound,mu,sd,tstat,p,sh,w)
names(out.median)<-c("gene","lower.bound","upper.bound","mean","std","test.stat","p.val","shapiro.p.val","wilcoxon.p.val")
sum(out.median$lower.bound<0 & out.median$upper.bound>0)

load("C:/Users/user/Desktop/Dr. Li Research/Sun paper datasets/hippocampus_43.Rdata")
b_hippocampus<-((loc[,1]>=203 ) & (loc[,1]<=822)) & ((loc[,2]>=203 ) & (loc[,2]<=822))
fld_hippocampus<-count[b_hippocampus,]
str(fld_hippocampus)

data_N<-apply(fld_hippocampus,1,function(i) i/sum(i))
data_B <-apply(data_N, 1, function(i) i>median(i)) #making data into logical based on median
data_B <- 1*data_B # 1= i>median(i), 0 otherwise
z<-data_B


x<-loc[,1][b_hippocampus]
y<-loc[,2][b_hippocampus]
data<-cbind(x,y,z)
data<-data.frame(data)



R <- max(max(x) - min(data$x), max(data$y) - min(data$y))
x <- (data$x - min(data$x))/R
y <- (data$y - min(data$y))/R
z <- data$X.Tal1.

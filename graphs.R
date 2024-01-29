SE1<-read.csv(file.choose(),header = T)
x<-SE1$Gene
y1<-SE1$One.One
y2<-SE1$Two.Two
y3<-SE1$One.One + SE1$Two.Two
df <- data.frame(x,y1,y2)

require(ggplot2)


ggplot(df, aes(x = x))+
  geom_point(aes(y=y1,color="One.One"))+
  geom_point(aes(y=y2,colour="Two.Two"))+
  labs(title="SE genes identified by both SPARK and SpatialDE",
       x ="Genes", y = "Phi value",color="legend")+
  scale_color_manual(values = c(One.One="blue",Two.Two="red"))


 
ggplot(df, aes(x = x))+
  geom_point(aes(y=y1,color="One.One"))+
  geom_point(aes(y=y2,colour="Two.Two"))+
  geom_point(aes(y=y3,colour="Sum"))+
  labs(title="SE genes identified by both SPARK and SpatialDE",
       x ="Genes", y = "Phi value",color="legend")+
  scale_color_manual(values = c(One.One="blue",Two.Two="red",Sum= "green"))
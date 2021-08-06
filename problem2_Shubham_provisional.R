library(ggplot2)
library(reshape2)

rm(list=ls())
setwd('D:/stat_wk/part_2')

df=read.csv('data/ABIA.csv')
dim(subset(df,df$Origin=='AUS' & df$Dest=='AUS' ))[1]

df_d=subset(df,df$Origin=='AUS')
df_a=subset(df,df$Dest=='AUS')



##Number of low distance flights are significantly more than large distance flights

df$orig_aus=ifelse(df$Origin=='AUS','from_austin','to_austin')


flights = aggregate(df$orig_aus,by=list(orig=df$orig_AUS,dist=df$Distance),FUN=length)
flights=flights[order(flights$x,decreasing = TRUE),]

ggplot(df,aes(x=as.factor(Distance),fill=as.factor(orig_aus)))+
  geom_bar() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


ggplot(df,aes(x=as.factor(DayofMonth),fill=as.factor(orig_aus)))+
  geom_bar() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


ggplot(df,aes(x=as.factor(DayOfWeek),fill=as.factor(orig_aus)))+
  
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


#Evenly distributed between arriving and leaving flights Dow, dom, distance


ggplot(df,aes(x=ArrDelay,fill=as.factor(orig_aus)))+
  geom_bar() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(df,aes(x=DepDelay,fill=as.factor(orig_aus)))+
  geom_bar() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


  +
  geom_vline(aes(xintercept=mean(Distance)),col='red',size=1)
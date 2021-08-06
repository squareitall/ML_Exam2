library(ggplot2)

library(Rcpp)

rm(list=ls())
setwd('D:/stat_wk/part_2')

df=read.csv('data/greenbuildings.csv')




#Analysing the stats GURU's work
par(mfrow=c(1,2))
boxplot(subset(df,df$green_rating==1)$Rent,main='Values of rent for houses with green rating as yes')
boxplot(subset(df,df$green_rating==0)$Rent,main='Values of rent for houses with green rating as no')

cat(median(subset(df,df$green_rating==1)$Rent),median(subset(df,df$green_rating==0)$Rent))

#It seems that The Stat guru only considered the green house as the responsible variable in determining rent prices.
#He made his conclusion on the basis of point estimate statistic (median) of two subsets of data, and didn't even consider the variation in the rent price of two datasets for his conclusion remarks

#There is clear overlapping in rent price for two sets of data; one with the green rating and the other without the one
#Possibly this variation is due to other factors that need to be analysed for precisely deciding what other factors control the rent price



summary(lm(Rent~green_rating,data=df))

#The SLR model trained using only the indicator variable (whether the building is green or not) is not able to explain the variation in rent prices (adj r2 is quite low)
#We surely need to consider other variables 

df_cor=as.data.frame(cor(df,method='pearson'))

col_tbu=names(df)[order(df_cor$Rent,decreasing=TRUE)][2:11]
# par(mfrow=c(round(col/5,0),5))
par(mfrow=c(2,5))
for (i in col_tbu){
  plot(df[,c(i)],df$Rent,ylab='Rent',xlab=i)
}



# Set of Simple Linear regressions
coeff_pre=c()
pval_pre=c()


for (pre in names(df)){
  if (pre =='Rent')
  {next}
  cat(pre,'\n')
  
  temp_pre=df[,pre]
  model_pre=lm(Rent~temp_pre,data=df)
  
  sum_pre=summary(model_pre)
  
  coeff_pre=c(coeff_pre,model_pre$coefficients['temp_pre'])
  pval_pre=c(pval_pre,sum_pre$coefficients[2,4])
  
}

df_pre=data.frame(predictors=names(df)[-1],coeff=coeff_pre,p_vals=pval_pre)

#sorting wrt absolute values of coefficients
df_pre=df_pre[order(abs(df_pre$coeff),decreasing=TRUE),]
print(df_pre)


#Individual predictor analysis suggest that the rent to an appreciable extent depends on the value of cost of electricity, class a and many other variables
#these variables can have same value in two different subgroups (green not green) or can have remarkably different values inside same subset)


rent_clsa=melt(df[,c('Rent','class_a')],id.vars='class_a')

ggplot(rent_clsa, aes(x=factor(class_a),y=value,fill=factor(class_a)))+
  geom_boxplot() +facet_wrap(~variable)

#Class A seems to be more discriminatory when ut comes to determimne prices

rent_clsa_gr=melt(df[,c('Rent','class_a','green_rating')],id.vars=c('class_a','green_rating'))

ggplot(rent_clsa_gr, aes(x=factor(green_rating),y=value,fill=factor(class_a)))+
  geom_boxplot() +facet_wrap(~variable)

#Within each subset of green rating class, class_a variable is sable to differentiate between rent prices
#it can be a strong confounding variable





############IGNORE BELOW THIS ################Rough Space
df_green=subset(df,df$green_rating==1)
df_not_green=df[-c(row(df_green)),]

par(mfrow=c(4,2))
for (i in df_pre$predictors[1:4]){
  
  boxplot(df_green[,i],ylab=i)
  boxplot(df_not_green[,i])
}
par(mfrow=c(2,1))

boxplot(df_green[,df_pre$predictors[1:5]],main='Houses labelled as Green')
boxplot(df_not_green[,df_pre$predictors[1:5]],main='Houses labelled as not Green')



df_tbu=df[,c(df_pre$predictors[1:5],'Rent','green_rating')]#,id=seq(1:5))
df_tbu[,c(1,4,6)]=scale(df_tbu[,c(1,4,6)])#Scaled the numerical values

# df_tbu=df_tbu[,c(1,4,6,7)]#analysing behavior of top numerical variables with green rating

df_melt=melt(df_tbu,id.vars = c('green_rating','class_a'))


ggplot(df_melt, aes(x=factor(green_rating),y=value,fill=factor(green_rating)))+
  geom_boxplot() +facet_wrap(~variable)


ggplot(df,aes(x=Rent,fill=as.factor(green_rating)))+
  geom_histogram(bins=20,binwidth = 0.3)+
  geom_vline(aes(xintercept=mean(Rent)),col='red',size=1)


library(plyr)
mu = aggregate(df$Rent,by=list(green=df$green_rating),FUN=mean)



df_cor=cor(df,method='pearson')
# df_cor=as.data.frame(cor(df,method='pearson'))$Rent



melted_cormat = melt(df_cor)
head(melted_cormat)
ggheatmap = ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()

ggheatmap + 
  geom_text(aes(Var2, Var1, label = round(value,2)), color = "black", size = 4) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
  )+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))



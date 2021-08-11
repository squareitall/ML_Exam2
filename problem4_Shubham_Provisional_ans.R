rm(list=ls())
setwd('D:/stat_wk/part_2')

df=read.csv('data/social_marketing.csv')


create_clusters=function(d,msg=''){
  X_scaled=scale(d,center = TRUE,scale = TRUE)
  dist_X = dist(X_scaled, method='euclidean')
  
  hier_avg = hclust(dist_X, method='average')#lab=)
  hier_min = hclust(dist_X, method='single')
  hier_max = hclust(dist_X, method='complete')
  
  plot(hier_avg, cex=0.8,main=paste0('avg based cluster dendogram',msg))
  plot(hier_min, cex=0.8, main=paste0('min based cluster dendogram',msg))
  plot(hier_max, cex=0.8,main=paste0('max based cluster dendogram',msg))
  
}

X=df[,-c(1)]
#We will take the transpose as we intend to group market segments based on scores by turks
X=t(X)

create_clusters(X)


#Removed the id columns along with spam and adult based columns
X=df[,-c(1,36,37)]

#In order to cluster market on the basis of categories, we take transpose of the customer centric data
#Now we can consider each costumer as feature/contributor and then we will estimate our own clusters of categories
X=t(X)
create_clusters(X,msg=' without spam and adult based columns')



#Removing chatter based columns as it is creating bias

X=df[,-c(1,2,36,37)]

#In order to cluster market on the basis of categories, we take transpose of the customer centric data
#Now we can consider each costumer as faeture/contributor and then we will estimate our own clusters of categories
X=t(X)

create_clusters(X,msg=' without chatter')



#Lets chk presence of each cluster type/segment across all customers
colSums(df[,-c(1)])
# Select balanced customers
table(rowSums(df[,-c(1)]))


#1)Quantile grouping on the basis of number of posts for each market segment
colSums(df[,-c(1)])[order(colSums(df[,-c(1)]),decreasing =TRUE)]


## There can be some users who post a lot more number of times and ones who does not post at all
## We can remove these users on the basis of their frequency of plots

##quantile(df$num_posts, 0.99)
##quantile(df$num_posts, 0.01)

df$num_posts=rowSums(df[,-c(1)])

lower_lim=0.01
upper_lim=0.99
mask_low=(df$num_posts > quantile(df$num_posts, lower_lim))
mask_up=(df$num_posts < quantile(df$num_posts, upper_lim))

df_sub=df[(mask_low & mask_up),]
df_sub=df_sub[,-c(38)]

X=t(df_sub[,-c(1,2,36,37)])

create_clusters(X,msg=' with subset of turks')










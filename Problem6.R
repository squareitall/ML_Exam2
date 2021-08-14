library(tidyverse)
library(arules)  # has a big ecosystem of packages built around it
library(arulesViz)

library(reshape2)


#For ans 6
temp=read.csv('data/groceries.txt',header=FALSE)
temp=rownames_to_column(temp)
tt=melt(temp,id.vars = 'rowname')
tt= tt[tt$value!='',]

tt=tt[,c('rowname','value')]
colnames(tt)=c('user_id','product')

tt$user_id=strtoi(tt$user_id)
tt=tt[order(tt$user_id),]


tt$user_id=factor(tt$user_id)

prod_lt_by_user = split(x=tt$product, f=tt$user_id)

prod_lt_by_user = lapply(prod_lt_by_user, unique)


prod_dist = as(prod_lt_by_user, "transactions")
summary(prod_dist)

prod_for_users = apriori(prod_dist, 
                     parameter=list(support=.005, confidence=.1, maxlen=5))

plot(prod_for_users)

#Seems product combination with confidence above 0.3 can be further inspected
inspect(subset(prod_for_users, confidence > 0.3))


inspect(subset(prod_for_users, subset=lift > 1 & support > 0.03))
#People generally buy whole milk and other vegetables together

sub1 = subset(prod_for_users, subset=confidence > 0.01 & support > 0.005)
plot(head(sub1, 100, by='lift'), method='graph')

# export
saveAsGraph(head(prod_for_users, n = 1000, by = "lift"), file = "groceries.graphml")



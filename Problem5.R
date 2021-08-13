rm(list=ls())

library(tm)

readerPlain = function(fname){
  readPlain(elem=list(content=readLines(fname)), 
            id=fname, language='en') }

## Rolling two directories together into a single corpus
author_dirs = Sys.glob('data/ReutersC50/C50train/*')
author_dirs = author_dirs[1:3]
file_list = NULL
labels = NULL
for(author in author_dirs) {
  author_name = substring(author, first=29)
  files_to_add = Sys.glob(paste0(author, '/*.txt'))
  file_list = append(file_list, files_to_add)
  labels = append(labels, rep(author_name, length(files_to_add)))
}

# Need a more clever regex to get better names here
all_docs = lapply(file_list, readerPlain) 
names(all_docs) = file_list
names(all_docs) = sub('.txt', '', names(all_docs))


my_corpus = Corpus(VectorSource(all_docs))

# Preprocessing
my_corpus = tm_map(my_corpus, content_transformer(tolower)) # make everything lowercase
my_corpus = tm_map(my_corpus, content_transformer(removeNumbers)) # remove numbers
my_corpus = tm_map(my_corpus, content_transformer(removePunctuation)) # remove punctuation
my_corpus = tm_map(my_corpus, content_transformer(stripWhitespace)) ## remove excess white-space
my_corpus = tm_map(my_corpus, content_transformer(removeWords), stopwords("SMART"))

DTM = DocumentTermMatrix(my_corpus)

DTM = removeSparseTerms(DTM, 0.985)


tfidf = weightTfIdf(DTM)


#Final X
X = as.matrix(tfidf)
summary(colSums(X))

dim(X)

scrub_cols = which(colSums(X) == 0)
X = X[,-scrub_cols]



words_from_trainset=colnames(X)
#We will use setdiff to find words present in test set but not in the train set
#setdiff(c('access','chips','consumers'),words_from_trainset)

#Lets create a pseudo word assigning quantile value of 0.2 (we will select only non zero values) across rows to that column


quant_2_non_zero=function(r){
  quantile(r[r!=0],0.2)
}
X=cbind(X,pseudo_word=apply(X,1,quant_2_non_zero))


pca_ = prcomp(X, scale=TRUE)

summary(pca_) 
plot(pca_)

# pca_$rotation[c(1:10,2520),1:10]
rot_transformer_10=pca_$rotation[,1:10]

rot_transformer_30=pca_$rotation[,1:30]


X_train=pca_$x[,1:30]
# y_train=c(rep(0,50),rep(1,50),rep(2,50))


###MODEL
library(randomForest)
ddf=cbind(X_train,author=c(rep(0,50),rep(1,50),rep(2,50)))
model=randomForest(as.factor(author)~.,data=ddf,ntree=100,maxnodes=15)


#########################
#TEST SET

####################################



library(tm)

readerPlain = function(fname){
  readPlain(elem=list(content=readLines(fname)), 
            id=fname, language='en') }

## Rolling two directories together into a single corpus
author_dirs = Sys.glob('data/ReutersC50/C50test/*')
author_dirs = author_dirs[1:3]
file_list = NULL
labels = NULL
for(author in author_dirs) {
  author_name = substring(author, first=29)
  files_to_add = Sys.glob(paste0(author, '/*.txt'))
  file_list = append(file_list, files_to_add)
  labels = append(labels, rep(author_name, length(files_to_add)))
}


all_docs = lapply(file_list, readerPlain) 
names(all_docs) = file_list
names(all_docs) = sub('.txt', '', names(all_docs))


my_corpus = Corpus(VectorSource(all_docs))

# Preprocessing
my_corpus = tm_map(my_corpus, content_transformer(tolower)) # make everything lowercase
my_corpus = tm_map(my_corpus, content_transformer(removeNumbers)) # remove numbers
my_corpus = tm_map(my_corpus, content_transformer(removePunctuation)) # remove punctuation
my_corpus = tm_map(my_corpus, content_transformer(stripWhitespace)) ## remove excess white-space
my_corpus = tm_map(my_corpus, content_transformer(removeWords), stopwords("SMART"))

DTM = DocumentTermMatrix(my_corpus)

DTM = removeSparseTerms(DTM, 0.985)



#Changes
tfidf_test = weightTfIdf(DTM)



###Set manipulation
length(intersect(words_from_trainset,colnames(tfidf_test)))

length(words_from_trainset)


bth=intersect(words_from_trainset,colnames(tfidf_test))#1529 for three authors
not_in_train=setdiff(colnames(tfidf_test),bth)


wrds_tbu=c(bth,'pseudo_word')
pca_transformer_tbu=rot_transformer_30[wrds_tbu,]#+ pseudo_word



X_test=as.matrix(tfidf_test[,bth])


mean_non_zer_diff_set=function(r){
  mean(r[r!=0])
}

X_test=cbind(X_test,test_pseudo=apply(tfidf_test[,not_in_train],1,mean_non_zer_diff_set))


y_test=c(rep(0,50),rep(1,50),rep(2,50))
X_test_final=X_test%*%pca_transformer_tbu
pred=predict(model,X_test_final)


cat(sum(y_test==pred))










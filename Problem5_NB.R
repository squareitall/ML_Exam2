library(tm)


readerPlain = function(fname){
  readPlain(elem=list(content=readLines(fname)), 
            id=fname, language='en') }

process_X=function(num,folder){
  tbu=paste('../data/ReutersC50/C50',folder,'/*',sep='')
author_dirs = Sys.glob(tbu)
author_dirs = author_dirs[num]
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

X = as.matrix(DTM)
smooth_count = 1/nrow(X)
w_AP = colSums(X + smooth_count)
w_AP = w_AP/sum(w_AP)

if (folder=='train'){
return(w_AP)}
else{
  return(X)
}
}


tbu=function(xx,temp_df=temp,lt_wrd=bth){
  sum(xx*log(temp_df[lt_wrd]))}

W_mat=c()
acc=c()
for (i in seq(1:50)){
  cat('\n',i,'\n')
  test=process_X(i,'test')
  #Could have created a placeholder, instead trying it with nested loop
  
  result_wt=c()
  
  for (j in seq(1:50)){
    cat(j)
    train=process_X(j,'train')
    bth=intersect(colnames(test),names(train))
    
    temp_r=apply(test[,bth],1,tbu,temp_df=train,lt_wrd=bth)
    
    result_wt=cbind(result_wt,temp_r)
  }
  colnames(result_wt)=seq(1:50)
  
  res_final=apply(result_wt,1,which.min)
  
  acc=c(acc,(sum(res_final==i)/length(res_final)))
    
  }



plot(acc,type='l')

cat(which.max(acc),max(acc),sep='->')
cat(which.min(acc),min(acc),sep='->')





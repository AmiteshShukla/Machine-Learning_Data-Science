library(text2vec)
library(glmnet)
library(pROC)
require(Matrix)
require(data.table)

set.seed(9661)

rm(list = ls())
start.time = proc.time()
all = read.table("data.tsv",stringsAsFactors = F,header = T)
all$review = gsub('<.*?>', ' ', all$review)
splits = read.table("splits.csv", header = T)
s = 3  # Here we get the 3rd training/test split. 
train = all[-which(all$new_id%in%splits[,s]),]
test = all[which(all$new_id%in%splits[,s]),]


#https://cran.r-project.org/web/packages/text2vec/vignettes/text-vectorization.html

# define preprocessing function and tokenization function
prep_fun = tolower
tok_fun = word_tokenizer

it_tmp = itoken(train$review,
                preprocessor = prep_fun, 
                tokenizer = tok_fun,
                ids = train$new_id,
                progressbar = FALSE)

stop_words = c("i", "me", "my", "myself", 
               "we", "our", "ours", "ourselves", 
               "you", "your", "yours", 
               "their", "they", "his", "her", 
               "she", "he", "a", "an", "and",
               "is", "was", "are", "were", 
               "him", "himself", "has", "have", 
               "it", "its", "of", "one", "for", 
               "the", "us", "this")

myvocab = create_vocabulary(it_tmp, ngram = c(1L, 2L), 
                            stopwords = stop_words)

pruned_vocab = prune_vocabulary(myvocab,
                                term_count_min = 5,
                                doc_proportion_max = 0.5,
                                doc_proportion_min = 0.001)

#pruned_vocab = prune_vocabulary(myvocab, 
#                                 vocab_term_max = 10000)

# create dtm_train with new pruned vocabulary vectorizer
vectorizer = vocab_vectorizer(pruned_vocab)

dtm_train  = create_dtm(it_tmp, vectorizer)

it_test = itoken(test$review, 
                 preprocessor = prep_fun, 
                 tokenizer = tok_fun, 
                 ids = test$id, 
                 progressbar = FALSE)

dtm_test = create_dtm(it_test, vectorizer)

v.size = dim(dtm_train)[2]
ytrain = train$sentiment

RowVar <- function(x, ...) {
  rowSums((x - rowMeans(x, ...))^2, ...)/(dim(x)[2] - 1)
}

summ = matrix(0, nrow=v.size, ncol=4)
summ[,1] = apply(dtm_train[ytrain==1, ], 2, mean)
summ[,2] = apply(dtm_train[ytrain==1, ], 2, var)
summ[,3] = apply(dtm_train[ytrain==0, ], 2, mean)
summ[,4] = apply(dtm_train[ytrain==0, ], 2, var)
#summ1<-rowMeans(dtm_train[ytrain==1, ])

n1=sum(ytrain); 
n=length(ytrain)
n0= n - n1

myp = (summ[,1] - summ[,3])/
  sqrt(summ[,2]/n1 + summ[,4]/n0)

words = colnames(dtm_train)
#pos.list = words[id[myp[id]>0]]
#neg.list = words[id[myp[id]<0]]
id = order(abs(myp), decreasing=TRUE)[1:2500]
if (s == 3) {
    res<-matrix(0,nrow = 2500,ncol = 2)
    res[,1]<-id
    res[,2]<-words[id]
    res<-as.data.frame(res)
    setnames(res, old=c("V1","V2"),new=c("new_id", "word"))
    write.table(res, file = "myVocab.txt", sep = ",",
                row.names = FALSE)
}

NFOLDS = 10
mycv = cv.glmnet(x=dtm_train[, id], y=train$sentiment, 
                 family='binomial',type.measure = "auc", 
                 nfolds = NFOLDS, alpha=0)
myfit = glmnet(x=dtm_train[, id], y=train$sentiment, 
               lambda = mycv$lambda.min, family='binomial', alpha=0)
logit_pred = predict(myfit, dtm_test[, id], type = "response")
res2<-matrix(0,nrow = 25000,ncol = 2)
res2[,1]<-test$new_id
res2[,2]<-logit_pred
res2<-as.data.frame(res2)
setnames(res2, old=c("V1","V2"),new=c("new_id", "prob"))
write.table(res2, file = "mysubmission.txt", sep = ",",
            row.names = FALSE)
roc_obj = roc(test$sentiment, as.vector(logit_pred))
auc(roc_obj) 
etime<- proc.time() - start.time
etime

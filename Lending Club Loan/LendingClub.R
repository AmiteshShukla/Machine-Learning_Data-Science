library(gbm)
library(xgboost)
library(caret)
require(Matrix)
library(forcats)
library(data.table)
rm(list = ls())
start.time = Sys.time()
code_flag = 0

if (code_flag == 1) {
   data<-read.csv("loan_stat542.csv")
   split_id<-read.csv("Project3_test_id.csv")

   test1 = subset(data, id %in% split_id$test1)
   train1 = subset(data, !(id %in% split_id$test1))
   test2 = subset(data, id %in% split_id$test2)
   train2 = subset(data, !(id %in% split_id$test2))
   test3 = subset(data, id %in% split_id$test3)
   train3 = subset(data, !(id %in% split_id$test3))
}

data_prep = function(ttrain,ttest) {
  
  levels(ttrain$loan_status)[grepl("Charged Off",levels(ttrain$loan_status))]<-"Default"
  levels(ttest$loan_status)[grepl("Charged Off",levels(ttest$loan_status))]<-"Default"
  
  for (col in 1:length(ttrain)) {
    facNA = paste(c(col*100), collapse = " ")
    if (class(ttrain[,col]) == "factor") {
      ttrain[,col]<-fct_explicit_na(ttrain[,col], facNA)
    }
  }
  
  for (col in 1:length(ttest)) {
    facNA = paste(c(col*100), collapse = " ")
    if (class(ttest[,col]) == "factor") {
      ttest[,col]<-fct_explicit_na(ttest[,col], facNA)
    }
  }
  
  ttrain[is.na(ttrain)] <- 999
  ttest[is.na(ttest)] <- 999
  
  ttrain$loan_status<-as.numeric(ttrain$loan_status)
  ttest$loan_status<-as.numeric(ttest$loan_status)
  ttrain$loan_status<<-ifelse(ttrain$loan_status == 1, 0, 1)
  ttest$loan_status<<-ifelse(ttest$loan_status == 1, 0, 1)
  
  tt <<- sparse.model.matrix(loan_status ~ ., data = ttrain)[,-1]
  zz <<- sparse.model.matrix(loan_status ~ ., data = ttest)[,-1]
}

if (code_flag == 1) {
    ttrain = train1
    ttest = test1
} else {
   ttrain<-read.csv("train.csv")
   ttest<-read.csv("test.csv")
}

tid<-grep("title", colnames(ttrain))
ttid<-grep("title", colnames(ttest))
ttrain<-ttrain[,-tid]
ttest<-ttest[,-ttid]

tid<-grep("zip_code", colnames(ttrain))
ttid<-grep("zip_code", colnames(ttest))
ttrain<-ttrain[,-tid]
ttest<-ttest[,-ttid]

tid<-grep("earliest_cr", colnames(ttrain))
ttid<-grep("earliest_cr", colnames(ttest))
ttrain<-ttrain[,-tid]
ttest<-ttest[,-ttid]

data_prep(ttrain,ttest)

bst <- xgboost(data = tt, label = ttrain$loan_status, max_depth = 5,
               eta = 0.2, nthread = 2, nrounds = 250,
               verbose=0,objective = "binary:logistic")

xb.pred = predict(bst,zz)
row=nrow(ttest)
res1<-matrix(nrow = row,ncol = 2)
res1[,1]<-ttest$id
res1[,2]<-xb.pred
res1<-as.data.frame(res1)
setnames(res1, old=c("V1","V2"),new=c("id", "prob"))
write.table(res1, file = "mysubmission1.txt", sep = ",",
            row.names = FALSE)
if (code_flag == 1) {
    rm(tt)
    rm(zz)
    rm(ttrain)
    rm(ttest)
    ttrain = train2
    ttest = test2
    tid<-grep("title", colnames(ttrain))
    ttid<-grep("title", colnames(ttest))
    ttrain<-ttrain[,-tid]
    ttest<-ttest[,-ttid]
    
    tid<-grep("zip_code", colnames(ttrain))
    ttid<-grep("zip_code", colnames(ttest))
    ttrain<-ttrain[,-tid]
    ttest<-ttest[,-ttid]
    
    tid<-grep("earliest_cr", colnames(ttrain))
    ttid<-grep("earliest_cr", colnames(ttest))
    ttrain<-ttrain[,-tid]
    ttest<-ttest[,-ttid]
    data_prep(ttrain,ttest)
    bst <- xgboost(data = tt, label = ttrain$loan_status, max_depth = 5,
                   eta = 0.2, nthread = 2, nrounds = 250,
                   verbose=0,objective = "binary:logistic")
    diff<-setdiff(bst[["feature_names"]],colnames(zz))
    xb.pred = predict(bst,zz)
    row=nrow(ttest)
    res1<-matrix(nrow = row,ncol = 2)
    res1[,1]<-ttest$id
    res1[,2]<-xb.pred
    res1<-as.data.frame(res1)
    setnames(res1, old=c("V1","V2"),new=c("id", "prob"))
    write.table(res1, file = "mysubmission2.txt", sep = ",",
                row.names = FALSE)

    rm(tt)
    rm(zz)
    rm(ttrain)
    rm(ttest)
    ttrain = train3
    ttest = test3
    tid<-grep("title", colnames(ttrain))
    ttid<-grep("title", colnames(ttest))
    ttrain<-ttrain[,-tid]
    ttest<-ttest[,-ttid]
    
    tid<-grep("zip_code", colnames(ttrain))
    ttid<-grep("zip_code", colnames(ttest))
    ttrain<-ttrain[,-tid]
    ttest<-ttest[,-ttid]
    
    tid<-grep("earliest_cr", colnames(ttrain))
    ttid<-grep("earliest_cr", colnames(ttest))
    ttrain<-ttrain[,-tid]
    ttest<-ttest[,-ttid]
    data_prep(ttrain,ttest)
    bst <- xgboost(data = tt, label = ttrain$loan_status, max_depth = 5,
                   eta = 0.2, nthread = 2, nrounds = 250,
                   verbose=0,objective = "binary:logistic")

    xb.pred = predict(bst,zz)
    row=nrow(ttest)
    res1<-matrix(nrow = row,ncol = 2)
    res1[,1]<-ttest$id
    res1[,2]<-xb.pred
    res1<-as.data.frame(res1)
    setnames(res1, old=c("V1","V2"),new=c("id", "prob"))
    write.table(res1, file = "mysubmission3.txt", sep = ",",
            row.names = FALSE)

    end.time = Sys.time()
    run.time = as.numeric(difftime(end.time, start.time, units = 'min'))
    print(run.time)
    run.time
}

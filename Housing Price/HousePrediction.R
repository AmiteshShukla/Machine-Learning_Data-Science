library(gbm)
library(xgboost)
library(psych)
library(caret)
library(plyr)
library(mice)
library(DescTools)
require(Matrix)
require(data.table)
library(randomForest)
library(dplyr)
library(data.table)

set.seed(123)

train<-read.csv("train.csv")
test<-read.csv("test.csv")

# AmesData<-read.csv("Ames_data.csv")
# data<-read.csv("ttsplit.csv",header = FALSE)
# test<-subset(AmesData, PID %in% data$V5)
# train<-subset(AmesData, !(PID %in% data$V5))
# train$Sale_Price<-log(train$Sale_Price)
# test$Sale_Price<-log(test$Sale_Price)

train$Sale_Price<-log(train$Sale_Price)


test$Sale_Price<-0

size<-ncol(train)
row=nrow(test)
res1<-matrix(nrow = row,ncol = 2)
res2<-matrix(nrow = row,ncol = 2)
train_label<-train[,size]
test_label<-test[,size]

train[is.na(train)] <- 0
test[is.na(test)] <- 0

train<-train[!(train$Lot_Frontage>200),]

train<-train[!(train$BsmtFin_SF_2)>1250,]
train<-train[!(train$Gr_Liv_Area>4500),]
train<-train[!(train$Garage_Yr_Blt>2020),]
train<-train[!(train$Open_Porch_SF>400),]

train<-train[!(train$Wood_Deck_SF>800),]

train<-train[!(train$Screen_Porch>500),]

ltrain<-train$Sale_Price

train$Lot_Area<-log(train$Lot_Area)
test$Lot_Area<-log(test$Lot_Area)

train$Mo_Sold <- as.factor(train$Mo_Sold)
test$Mo_Sold <- as.factor(test$Mo_Sold)

train$Exter_Cond <- as.numeric(train$Exter_Cond)
test$Exter_Cond<- as.numeric(test$Exter_Cond)

train$Pool_QC <- as.numeric(train$Pool_QC)
test$Pool_QC<- as.numeric(test$Pool_QC)

train$Garage_Cond <- as.numeric(train$Garage_Cond)
test$Garage_Cond<- as.numeric(test$Garage_Cond)

train$Paved_Drive <- as.numeric(train$Paved_Drive)
test$Paved_Drive<- as.numeric(test$Paved_Drive)

train$Fireplace_Qu <- as.numeric(train$Fireplace_Qu)
test$Fireplace_Qu<- as.numeric(test$Fireplace_Qu)

train$BsmtFin_Type_1 <- as.numeric(train$BsmtFin_Type_1)
test$BsmtFin_Type_1<- as.numeric(test$BsmtFin_Type_1)

train$Lot_Shape <- as.numeric(train$Lot_Shape)
test$Lot_Shape<- as.numeric(test$Lot_Shape)

train$Bsmt_Cond <- as.numeric(train$Bsmt_Cond)
test$Bsmt_Cond<- as.numeric(test$Bsmt_Cond)

train$Bsmt_Cond <- as.numeric(train$Bsmt_Cond)
test$Bsmt_Cond<- as.numeric(test$Bsmt_Cond)

train$Fireplace_Qu <- as.numeric(train$Fireplace_Qu)
test$Fireplace_Qu<- as.numeric(test$Fireplace_Qu)

train$Total_Sq_Feet <- train$Gr_Liv_Area + train$Total_Bsmt_SF
test$Total_Sq_Feet <- test$Gr_Liv_Area + test$Total_Bsmt_SF

train$Total_Br <- train$Bedroom_AbvGr + train$TotRms_AbvGrd
test$Total_Br <- test$Bedroom_AbvGr + test$TotRms_AbvGrd

train$Total_Fl_SF <- train$First_Flr_SF + train$Second_Flr_SF
test$Total_Fl_SF <- test$First_Flr_SF + test$Second_Flr_SF

train$Total_Porch_SF <- train$Screen_Porch + train$Three_season_porch + train$Enclosed_Porch
test$Total_Porch_SF <- test$Screen_Porch + test$Three_season_porch + test$Enclosed_Porch

train$Tot_Bathrooms <- train$Full_Bath + (train$Half_Bath*0.5) + (train$Bsmt_Full_Bath*1) + (train$Bsmt_Half_Bath*0.5)
test$Tot_Bathrooms <- test$Full_Bath + (test$Half_Bath*0.5) + (test$Bsmt_Full_Bath*1) + (test$Bsmt_Half_Bath*0.5)

levels(train$Kitchen_Qual)[grepl("Poor",levels(train$Kitchen_Qual))]<-"Typical"
levels(test$Kitchen_Qual)[grepl("Poor",levels(test$Kitchen_Qual))]<-"Typical"
levels(train$Kitchen_Qual)[grepl("Fair",levels(train$Kitchen_Qual))]<-"Typical"
levels(test$Kitchen_Qual)[grepl("Fair",levels(test$Kitchen_Qual))]<-"Typical"

levels(train$Mas_Vnr_Type)[grepl("BrkCmn",levels(train$Mas_Vnr_Type))]<-"None"
levels(test$Mas_Vnr_Type)[grepl("BrkCmn",levels(test$Mas_Vnr_Type))]<-"None"
levels(train$Mas_Vnr_Type)[grepl("CBlock",levels(train$Mas_Vnr_Type))]<-"None"
levels(test$Mas_Vnr_Type)[grepl("CBlock",levels(test$Mas_Vnr_Type))]<-"None"

levels(train$MS_Zoning)[grepl("A_agr",levels(train$MS_Zoning))]<-"Others"
levels(test$MS_Zoning)[grepl("A_agr",levels(test$MS_Zoning))]<-"Others"
levels(train$MS_Zoning)[grepl("I_all",levels(train$MS_Zoning))]<-"Others"
levels(test$MS_Zoning)[grepl("I_all",levels(test$MS_Zoning))]<-"Others"
levels(train$MS_Zoning)[grepl("C_all",levels(train$MS_Zoning))]<-"Others"
levels(test$MS_Zoning)[grepl("C_all",levels(test$MS_Zoning))]<-"Others"

tc<-c("Utilities","Street","Land_Slope", "Heating",
      "Pool_QC",
      "Roof_Matl",
      "Enclosed_Porch","Misc_Feature", "Screen_Porch", "Three_season_porch",
      "Misc_Val","Full_Bath","Half_Bath","Bsmt_Full_Bath",
      "Bsmt_Half_Bath","Gr_Liv_Area","First_Flr_SF",
      "Second_Flr_SF", "Street","BsmtFin_SF_2",
      "Low_Qual_Fin_SF","Bedroom_AbvGr","TotRms_AbvGrd","Heating_QC",
      "Sale_Type","Lot_Frontage","Mas_Vnr_Area","Total_Bsmt_SF","Fence"
      ,"Condition_2")

train<-train[ , -which(names(train) %in% tc)]
test<-test[ , -which(names(test) %in% tc)]

xtrain<<-train
xtest<<-test

factorLevel <- function (dataset) {
  i=0
  for (lst in colnames(dataset)) {
    i = i+1
    if (class(dataset[,i]) == "factor") {
      #https://stackoverflow.com/questions/50726091/convert-levels-of-factor-to-na-based-on-count
      to_exclude <- names(table(dataset[i])[100*(table(dataset[i])/length(dataset[,i])) < 0.5])
      levels(train[,i])[levels(train[,i]) %in% to_exclude] <<- "Others"
      levels(test[,i])[levels(test[,i]) %in% to_exclude] <<- "Others"
      
    }
  }
  return(dataset)
}
combined<-rbind(train,test)
cmb<-factorLevel(combined)


start.time = proc.time()
ttrain <- model.matrix(Sale_Price ~ ., data = train)[,-1]
ttest <- model.matrix(Sale_Price ~ ., data = test)[,-1]

suppressMessages(library(glmnet))
lambda <- 10^seq(15, -20, by = -.1)
cv.out = cv.glmnet(ttrain, ltrain, alpha = 1,lambda = lambda)
best.lam = cv.out$lambda.min
Ytest.pred = predict(cv.out, s = best.lam, newx = ttest)

etime<- proc.time() - start.time
#print("Completed in time:")
etime[3]
res1[,1]<-test$PID
res1[,2]<-exp(Ytest.pred)
res1<-as.data.frame(res1)
setnames(res1, old=c("V1","V2"),new=c("PID", "Sale_Price"))
write.table(res1, file = "mysubmission1.txt", sep = ",",
            row.names = FALSE)


etime<- proc.time() - start.time
#set.seed(123)
bag.ames<-randomForest(train$Sale_Price ~ .,data = train,mtry=15,ntree=75,importance=TRUE)
rf.pred = predict(bag.ames,newdata = test[,-size])

etime<- proc.time() - start.time
#print("Completed in time:")
etime[3]


etime<- proc.time() - start.time
tt <- sparse.model.matrix(Sale_Price ~ ., data = train)[,-1]
zz <- sparse.model.matrix(Sale_Price ~ ., data = test)[,-1]

bst <- xgboost(data = tt, label = ltrain, max_depth = 4,
               eta = 0.2, nthread = 2, nrounds = 500,
               verbose=0,objective = "reg:linear")

xb.pred = predict(bst,zz)
rmse=sqrt(mean((xb.pred - ltest)^2))
#print(paste0("RMSE with xgboost: ",rmse))
etime<- proc.time() - start.time
#print("Completed in time:")
etime[3]

res2[,1]<-test$PID
res2[,2]<-exp(xb.pred)
res2<-as.data.frame(res2)
setnames(res2, old=c("V1","V2"),new=c("PID", "Sale_Price"))
write.table(res2, file = "mysubmission2.txt", sep = ",",
            row.names = FALSE)
         
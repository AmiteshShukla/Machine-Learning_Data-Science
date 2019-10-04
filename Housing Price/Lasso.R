library(psych)
library(caret)
library(DescTools)
library(plyr)
library(mice)
require(Matrix)
library(xgboost)
require(data.table)

set.seed(9661)
# AmesData<-read.csv("Ames_data.csv")

train<-read.csv("train.csv")
test<-read.csv("test.csv")

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
#count(AmesData,AmesData$Mas_Vnr_Type)
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
)

train<-train[ , -which(names(train) %in% tc)]
test<-test[ , -which(names(test) %in% tc)]
train_label<-train$Sale_Price
test_label<-test$Sale_Price

factorLevel <- function (dataset) {
  i=0
  for (lst in colnames(dataset)) {
    i = i+1
    if (class(dataset[,i]) == "factor") {
      #https://stackoverflow.com/questions/50726091/convert-levels-of-factor-to-na-based-on-count
      to_exclude <- names(table(dataset[i])[100*(table(dataset[i])/length(dataset[,i])) < 0.77])
      levels(train[,i])[levels(train[,i]) %in% to_exclude] <<- "Others"
      levels(test[,i])[levels(test[,i]) %in% to_exclude] <<- "Others"
      
    }
  }
  return(dataset)
}
combined<-rbind(train,test)
cmb<-factorLevel(combined)

tt <- sparse.model.matrix(Sale_Price ~ ., data = train)[,-1]
zz <- sparse.model.matrix(Sale_Price ~ ., data = test)[,-1]

xVar<-as.numeric(nearZeroVar(tt))

X <- scale(tt,center = TRUE, scale = TRUE)

X.s<<-attr(X,'scaled:scale')
X.t<-matrix(X.s)
mean_sd <- unlist(attributes(X)[-1])

p<- ncol(X)
beta      <- rep(0,p)
Y <<- scale(zz,center = TRUE, scale = TRUE)

train_label<<- scale(train_label,center = TRUE, scale = FALSE)
y.c<<-attr(train_label,'scaled:center')

nl      <- 1

lambda<-0.05011872
#lambda<-0.005706853
beta_cd <- matrix(0,nl,p)
beta0 <- matrix(0,nl,1)

for(l in 1:nl){
  # Initial values
  b <- rep(0,p)
  #r <- train_label
  r<-train_label
  # Coordiante descent
  for(step in 1:50){
    for(j in 1:p){
      # partial residuals
      r <- r + X[,j]*b[j]
      # soft-threshold solution
      xr <- sum(X[,j]*r)
      xx <- sum(X[,j]^2)   
      b[j] <- (abs(xr)-lambda[l]/2)/xx
      b[j] <- sign(xr)*ifelse(b[j]>0,b[j],0)
      # residuals
      r <- r - X[,j]*b[j]
    }
  }
  beta_cd[l,]<-b
  
  # reverse the z-transformation
  beta_transform <- (b * as.numeric(mean_sd[2]) + as.numeric(mean_sd[1]))
  # this value will have a name, remove it
  names(beta_transform) <- NULL
  #beta_cd[l,]<-beta_transform
  #beta0[l,]<-y.c - sum(X%*%(beta_cd[l,]/X.t))
  beta0[l,]<-y.c - sum(X%*%(beta_transform/X.t))
}
pred = 0

for (k in 1:nrow(Y)) {
  pred[k]=0
  for (s in 1:ncol(Y)) {
    pred[k] = pred[k] + Y[k,s]*beta_cd[1,s]
  }
  pred[k] = pred[k] + beta0[1]
}

err=0
for (se in 1:nrow(Y)) {
  err[se] = (pred[se] - test_label[se])^2
}

rmse<-sqrt(mean(err))

row=nrow(Y)
res3<-matrix(nrow = row,ncol = 2)
res3[,1]<-test$PID
res3[,2]<-exp(pred)
res3<-as.data.frame(res3)
setnames(res3, old=c("V1","V2"),new=c("PID", "Sale_Price"))
write.table(res3, file = "mysubmission3.txt", sep = ",",
            row.names = FALSE)


library(caret)
library(gbm)
library(randomForest)
library(caTools)
library(foreach)
library(doMC)
library(e1071)
library(ROCR)

# parallel process
registerDoMC(cores=16)

# Random Forest
buildRFModel <- function(X,y) {
  
  gc(reset=TRUE)
  set.seed(825)
  RF <- foreach(ntree=rep(200,16), .combine=combine,
                .multicombine=TRUE,
                .packages="randomForest") %dopar% {
                  randomForest(X,
                               factor(y),
                               ntree=ntree,
                               strata=factor(y),
                               do.trace=TRUE, importance=TRUE, forest=TRUE,
                               replace=TRUE)
                }
  RF
}

RFmodel<-buildRFModel(X,y)
predRF<-predict(RFmodel, newdata=dataTest, type="prob")
predRF<-as.data.frame(predRF)
pred<-prediction(predRF[,2],yTest)
auc.tmp <- performance(pred,"auc")
aucRF <- as.numeric(auc.tmp@y.values)
aucRF
#0.85344

# GBM
buildGBMModel <- function(X,y) {
  gc()
  data<-cbind(y,X)
  set.seed(825)
  GB <- gbm(y~., data=data, n.trees=1500,
            distribution="bernoulli",
            keep.data=FALSE, shrinkage=0.01, bag.fraction=0.3,
            interaction.depth=12,verbose = TRUE)
  GB
}

GBMmodel<-buildGBMModel(X,y)
predGBM<-predict(GBMmodel, newdata=dataTest, n.trees=1500, type="response")
pred<-prediction(predGBM,yTest)
auc.tmp <- performance(pred,"auc")
aucGBM <- as.numeric(auc.tmp@y.values)
aucGBM
#0.8673255 

predictFinal<-(aucRF*predRF[,2]
               +aucGBM*predGBM)/(aucRF+aucGBM)

pred<-prediction(predictFinal,yTest)
auc.tmp <- performance(pred,"auc")
aucPred <- as.numeric(auc.tmp@y.values)
#0.88

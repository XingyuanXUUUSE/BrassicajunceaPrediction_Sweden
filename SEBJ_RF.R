set.seed(5)

tuneRF(x=all.cov[,1:19],y=as.factor(all.cov$Pres))
rf.Bradypus <- randomForest(as.factor(Pres)~.,mtry=4,ntree=500,data=all.cov)
eRF<-list()
par(mfrow=c(2,3))

for (i in 1:folds) {
  train <- Pres.cov[kfold_pres != i,]
  test <- Pres.cov[kfold_pres == i,]
  backTrain <- Back.cov[kfold_back != i,]
  backTest <- Back.cov[kfold_back == i,]
  dataTrain <- rbind(train, backTrain)
  dataTest <- rbind(test, backTest)
  dataTrain$Pres <- as.factor(dataTrain$Pres)
  RF_eval <- randomForest(Pres ~ ., data = dataTrain)
  rf.pred <- predict(RF_eval, type = "prob")[, 2]
  eRF[[i]] <- evaluate(p = rf.pred[which(dataTrain$Pres == "1")],
                       a = rf.pred[which(dataTrain$Pres == "0")])
  
  plot(eRF[[i]], 'ROC')
}
eRF
aucRF <- sapply( eRF, function(x){slot(x, 'auc')} )
mean(aucRF)
Opt_RF<-sapply( eRF, function(x){ x@t[which.max(x@TPR + x@TNR)] } )
Opt_RF
Mean_OptRF<-mean(Opt_RF)
Mean_OptRF
prRF <- predict(BIO, RF_eval)
par(mfrow=c(1,2))
plot(prRF, main='Random Forest Prediction')
plot(prRF > Mean_OptRF, main='presence/absence')

output_folder <- "RF_Output"

if (!file.exists(output_folder)) {
  dir.create(output_folder)
}

output_file_1 <- file.path(output_folder, "PrRF_P.tif")
output_file_2 <- file.path(output_folder, "PrRF_PA.tif")

writeRaster(prRF, filename=output_file_1, format="GTiff")
writeRaster(prRF > Mean_OptRF, filename=output_file_2, format="GTiff")
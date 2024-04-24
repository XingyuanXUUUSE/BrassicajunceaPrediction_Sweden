maxnetMod <- maxnet(all.cov$Pres, all.cov[,1:19],
                    maxnet.formula(all.cov$Pres,all.cov[,1:8],classes="lq"))

maxnet.cloglog.map <- predict(BIO, maxnetMod, clamp = F, type = "cloglog")

plot(maxnet.cloglog.map, axes = F, box = F, main = "Maxnet-clog")

par(mfrow=c(2,3))

eMAX<-list()

for (i in 1:folds) {
  train <- Pres.cov[kfold_pres!= i,]
  test <- Pres.cov[kfold_pres == i,]
  backTrain<-Back.cov[kfold_back!=i,]
  backTest<-Back.cov[kfold_back==i,]
  dataTrain<-rbind(train,backTrain)
  dataTest<-rbind(test,backTest)
  maxnet_eval <- maxnet(dataTrain$Pres, dataTrain[,1:8])
  eMAX[[i]] <- evaluate(p=dataTest[which(dataTest$Pres==1),],a=dataTest[which(dataTest$Pres==0),], maxnet_eval)
  plot(eMAX[[i]],'ROC')
  
}
aucMAX <- sapply( eMAX, function(x){slot(x, 'auc')} )
aucMAX
mean(aucMAX)
Opt_MAX <- sapply(eMAX, function(x){ x@t[which.max(x@TPR + x@TNR)] })
Opt_MAX
Mean_OptMAX <- mean(Opt_MAX)
Mean_OptMAX
prMAX <- predict(BIO, maxnet_eval)
par(mfrow=c(1,2))
plot(prMAX, main='Maxent Prediction')
plot(prMAX > Mean_OptMAX, main='presence/absence')

output_folder <- "MaxEnt_Output"

if (!file.exists(output_folder)) {
  dir.create(output_folder)
}

output_file_1 <- file.path(output_folder, "PrMax_P.tif")
output_file_2 <- file.path(output_folder, "PrMax_PA.tif")

writeRaster(prMAX, filename=output_file_1, format="GTiff")
writeRaster(prMAX > Mean_OptMAX, filename=output_file_2, format="GTiff")

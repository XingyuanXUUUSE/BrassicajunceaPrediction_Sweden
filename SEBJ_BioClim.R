set.seed(5)

#set number of folds to use
bc_K<-list()
par(mfrow=c(2,3))
# for loop to iterate over folds
for (i in 1:folds) {
  train <- Pres.cov[kfold_pres!= i,]
  test <- Pres.cov[kfold_pres == i,]
  backTrain<-Back.cov[kfold_back!=i,]
  backTest<-Back.cov[kfold_back==i,]
  dataTrain<-rbind(train,backTrain)
  
  dataTest<-rbind(test,backTest)#bind test data together
  bc_bradypus_K<-bioclim(train[,1:8])#for bioclim we only need presence data to train the model!
  bc_K[[i]] <- evaluate(p=test[,1:8],a=backTest[,1:9], bc_bradypus_K)#use testing data (kfold==i) for model evaluation
  
  #check the AUC by plotting ROC values
  
  plot(bc_K[[i]],'ROC')
  
}

aucBIO <- sapply( bc_K, function(x){slot(x, 'auc')} )
mean(aucBIO)
Opt_BIO<-sapply( bc_K, function(x){ x@t[which.max(x@TPR + x@TNR)] } )
Opt_BIO
Mean_OptBIO<- mean(Opt_BIO)
Mean_OptBIO

bc_K

prBIO <- predict(BIO, bc_bradypus_K,type = "response")
par(mfrow=c(1,2))
plot(prBIO, main='BIO_K')
prPBIO <- prBIO > Mean_OptBIO
plot(prBIO > Mean_OptBIO, main='presence/absence')

output_folder <- "BioClim_Output"

if (!file.exists(output_folder)) {
  dir.create(output_folder)
}

output_file_1 <- file.path(output_folder, "PrBIO_P.tif")
output_file_2 <- file.path(output_folder, "PrBIO_PA.tif")

writeRaster(prBIO, filename=output_file_1, format="GTiff")
writeRaster(prPBIO, filename=output_file_2, format="GTiff")

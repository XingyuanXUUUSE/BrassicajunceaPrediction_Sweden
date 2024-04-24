glm.juncea <- glm(Pres~.,binomial(link = 'logit'), data = all.cov)
summary(glm.juncea)

glm.map <- predict(BIO, glm.juncea, type = 'response')
plot(glm.map, main = 'juncea')

eGLM<-list()
par(mfrow=c(2,3))

for (i in 1:folds) {
  train <- Pres.cov[kfold_pres!= i,]
  test <- Pres.cov[kfold_pres == i,]
  backTrain<-Back.cov[kfold_back!=i,]
  backTest<-Back.cov[kfold_back==i,]
  dataTrain<-rbind(train,backTrain)
  dataTest<-rbind(test,backTest)
  glm_eval <- glm(Pres~.,binomial(link = "logit"), data=dataTrain)#this is our glm model trained on presence and absence points
  eGLM[[i]] <- evaluate(p=dataTest[ which(dataTest$Pres==1),],a=dataTest[which(dataTest$Pres==0),], glm_eval)#use testing data (kfold==i) for model evaluation
  
  #check the AUC by plotting ROC values
  
  plot(eGLM[[i]],'ROC')
  
}

eGLM
aucGLM <- sapply( eGLM, function(x){slot(x, 'auc')} )
mean(aucGLM)
Opt_GLM<-sapply( eGLM, function(x){ x@t[which.max(x@TPR + x@TNR)] } )
Opt_GLM
Mean_OptGLM<- mean(Opt_GLM)
trGLM<-plogis(Mean_OptGLM)
trGLM

prGLM <- predict(BIO, glm_eval,type = "response")
par(mfrow=c(1,2))
plot(prGLM, main='GLM, regression')
prPGLM <- prGLM > trGLM
plot(prPGLM, main='presence/absence')

output_folder <- "GLM_Output"

if (!file.exists(output_folder)) {
  dir.create(output_folder)
}

output_file_1 <- file.path(output_folder, "PrGLM_P.tif")
output_file_2 <- file.path(output_folder, "PrGLM_PA.tif")

writeRaster(prGLM, filename=output_file_1, format="GTiff")
writeRaster(prPGLM, filename=output_file_2, format="GTiff")


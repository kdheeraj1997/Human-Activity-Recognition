library("caret")

ds<-read.csv(file="C:/Users/Aruna Mulugur/Documents/dm_assignment/har.csv")
dataset<-ds[,1:561]


nzv <- nearZeroVar(dataset, saveMetrics = TRUE)
dataset_nzv <- dataset[c(rownames(nzv[nzv$percentUnique > 99,])) ]
#write.csv(dataset_nzv, "C:/Users/Aruna Mulugur/Documents/dm_assignment/har_nzv.csv")

pmatrix <- scale(dataset_nzv)
princ <- prcomp(pmatrix)
nComp <- 10 
dfComponents <- predict(princ, newdata=pmatrix)[,1:nComp]
dfEvaluate <- cbind(as.data.frame(dfComponents))
write.csv(dfEvaluate, "C:/Users/Aruna Mulugur/Documents/dm_assignment/har_pca.csv")
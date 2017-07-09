library("caret")

har<-read.csv(file="C:/Users/Aruna Mulugur/Documents/dm_assignment/har_pca.csv")
dt=sort(sample(nrow(har),nrow(har)*.7))
train<-har[dt,]
test<-har[-dt,]


#lda
library("MASS")
start.time <- Sys.time()

har_lda <-lda(Activity ~ .- Activity, data=train)
summary(har_lda)
p <-predict(har_lda, test)$class
table_lda <-table(p,test$Activity)
error_lda <- (1-sum(diag(table_lda))/sum(table_lda))
p_lda <- (sum(diag(table_lda))/sum(table_lda))*100
cm <- confusionMatrix(p,test$Activity)
a_lda<-cm$overall['Accuracy']

end.time <- Sys.time()
lda.time <- end.time - start.time


#ibk
library("RWeka")
start.time <- Sys.time()

har_ibk <-IBk(Activity ~ .- Activity, data=train)
summary(har_IBk)
p <-predict(har_ibk, test,type="class")
table_ibk <-table(p,test$Activity)
error_ibk <- (1-sum(diag(table_ibk))/sum(table_ibk))
p_ibk <- (sum(diag(table_ibk))/sum(table_ibk))*100
cm <- confusionMatrix(p,test$Activity)
a_ibk<-cm$overall['Accuracy']

end.time <- Sys.time()
ibk.time <- end.time - start.time



#J48
library("RWeka")
start.time <- Sys.time()

har_j48 <-J48(Activity ~ .- Activity, data=train)
summary(har_J48)
p <-predict(har_j48, test,type="class")
table_J48 <-table(p,test$Activity)
error_J48 <- (1-sum(diag(table_J48))/sum(table_J48))
p_J48 <- (sum(diag(table_J48))/sum(table_J48))*100
cm <- confusionMatrix(p,test$Activity)
a_j48<-cm$overall['Accuracy']

end.time <- Sys.time()
j48.time <- end.time - start.time



#svm
library("e1071")
start.time <- Sys.time()

har_svm <-svm(Activity ~ ., data=train)
summary(har_svm)
p <-predict(har_svm, test, type="class")
table_svm <-table(p,test$Activity)
error_svm <- (1-sum(diag(table_svm))/sum(table_svm))
p_svm <- (sum(diag(table_svm))/sum(table_svm))*100
cm <- confusionMatrix(p,test$Activity)
a_svm<-cm$overall['Accuracy']

end.time <- Sys.time()
svm.time <- end.time - start.time


#ctree
library("party")
start.time <- Sys.time()

har_ctree <- ctree(Activity ~ ., data=train)
table(predict(har_ctree), train$Activity)
testpred <-predict(har_ctree,newdata=test)
testp <-table(testpred, test$Activity)
error_ctree <- (1-sum(diag(testp))/sum(testp))
p_ctree<-(sum(diag(testp))/sum(testp))*100
cm <- confusionMatrix(p,test$Activity)
a_ctree<-cm$overall['Accuracy']

end.time <- Sys.time()
ctree.time <- end.time - start.time

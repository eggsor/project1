library(e1071)
setwd("C:\\Users\\hp\\Desktop\\mgs")
txtdata<-read.table("tfidfvec.txt",head=TRUE);
labeltree<- read.table("labels.txt", head=TRUE);



dset=data.frame(tree=labeltree[,4], txtdata);
set.seed(4)
train<-sample(nrow(dset), 0.7*nrow(dset))
dset.train=dset[train,]
dset.val=dset[-train,]

svm2=svm(tree~., data=dset.train)
vx=subset(dset.val, select=-tree)
vy=subset(dset.val, select=tree)
pred2=predict(svm2,vx)
accu=table(pred2, dset.val$tree)
accu

lm=lm(tree~., data=dset.train)
predlm=predict(lm, vx)
acculm=table(predlm,dset.val$tree)
acculm






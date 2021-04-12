library(e1071)
#image
setwd("C:\\Users\\hp\\Desktop\\mgs")
txtdata<-read.table("a.txt",head=FALSE);
labeltree<- read.table("part.txt", head=FALSE);


dset=data.frame(tree=labeltree[,2], txtdata);
set.seed(3416)

train<-sample(nrow(dset), 0.7*nrow(dset))
dset.train=dset[train,]
dset.val=dset[-train,]
vx=subset(dset.val, select=-tree)
vy=subset(dset.val, select=tree)
#dset.train$tree=factor(dset.train$tree)
#dset.val$tree=factor(dset.val$tree)
imglin2<-lm(tree~., data=dset.train)
imgpred2=predict(imglin2,vx)
lmaccu=table(imgpred2,dset.val$tree)
lmaccu

svm3=svm(tree~., data=dset)

pred2=predict(svm3,vx)
svm3accu=table(pred2, dset.val$tree)
svm3accu



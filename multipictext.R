library(e1071)
#multi
setwd("C:\\Users\\hp\\Desktop\\mgs\\double")
txtdata<-read.table("textmul.txt",head=FALSE);
picdata<-read.table("imagemul.txt",head=FALSE);
labeltree<- read.table("labels.txt", head=FALSE);
datatree=data.frame(tree=labeltree[,3], txtdata, picdata);

x=subset(datatree, select=-tree)
x=subset(datatree, select=tree)

svm1=svm(tree~., data=datatree)
summary(svm1)
svm1$labels

pred=predict(svm1,x)
table(pred,y)

dset=data.frame(tree=labeltree[,3], txtdata);
set.seed(123)
train<-sample(nrow(dset), 0.7*nrow(dset))
dset.train=dset[train,]
dset.val=dset[-train,]

svm2=svm(tree~., data=dset)
vx=subset(dset.val, select=-tree)
vy=subset(dset.val, select=tree)
pred2=predict(svm2,vx)
accu=table(pred2, dset.val$tree)
accu

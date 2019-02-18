#### PH 245 LAB 13 ####
### GSI: Courtney ###


# Example : Wisconsin breast cancer data
# Background information:
# Breast cancer continues to be a common and deadly form of cancer among women. 
# The diagnosis of breast tumors has traditionally been performed by biopsy, an invasive and 
# often traumatic surgical procedure. Alternatively, diagnosis might be based on fine needle 
# aspiration (FNA) which can often be performed on an outpatient basis. In 1994 researchers at 
# the University of Wisconsin developed what has become known as the "Wisconsin Breast Cancer Data"
# to study FNA as a method of diagnosis. The dataset, which was obtained from 
# ftp://ftp.ics.uci.edu/pub contains information on 569 FNAs. There are two diagnoses (classes),
# 212 malignant(y=1) and 357 benign(y=0). The 30 predictors correspond to the mean, standard deviation 
# and a tail average of the empirical distributions of 10 characteristics of the cells extracted
# by FNA.
setwd("/Users/BettyFriedan/Google Drive/Berkeley Drive (manual import)/Graduate school (manual)/Fall 15/Multivariate stats/Lab/Lab 13")
data<-read.table(file="Data_WBreastCancer.dat", header=TRUE, quote="")
X<-as.matrix(data[,2:31])
G<-data[,1]
table(G)    # 212 malignant cases (G=1) vs 357 benign cases (G=0)

#### Dividing data into training and testing set ####
# the randomness in computer is pseudorandom. With same seed, different computer will have same random numbers generated
set.seed(100) 

# randomly select 300 samples as training set
# the index of these 300 selected observations
id.tr<-sort(sample(seq(1, nrow(X)), size=300))

# subset of the data to obtain the training set
X.tr<-X[id.tr, ]
G.tr<-G[id.tr]

# sbuset of the data to obtain the testing set
X.te<-X[-id.tr,]
G.te<-G[-id.tr]
table(G.tr)
table(G.te)



#### LDA ####
library(MASS)
# lda: first argument is predictors, the second argument is grouping
# build lda model based on training set
out.lda<-lda(X.tr, G.tr)
# predict grouping based on the lda result on training set
G.tr.lda<-predict(out.lda, as.data.frame(X.tr))$class
# misclassification: the proportion of the predicted grouping the is different from the observed
# grouping. Think them as errors
sum(G.tr.lda != G.tr) / length(G.tr)

# predict grouping based on the lda result on testing set
G.te.lda<-predict(out.lda, as.data.frame(X.te))$class
sum(G.te.lda != G.te) / length(G.te)


#### QDA ####
# same coding set up as lda, see comments above
out.qda<-qda(X.tr, G.tr)
G.tr.qda<-predict(out.qda, as.data.frame(X.tr))$class
sum(G.tr.qda != G.tr) / length(G.tr)
G.te.qda<-predict(out.qda, as.data.frame(X.te))$class
sum(G.te.qda != G.te) / length(G.te)


#### MDA ####
# install the package mda if you haven't before loading the library
# To install the package: install.packages("mda")
library(mda)

# first argument: formula in the format Response(group) ~ predictors
# second argument: Number of subclasses per class. Can be a vector with a number for each class.
# build mda model using training set
out.mda<-mda(G.tr~X.tr, subclasses=c(5,5))

# type: kind of predictions. type = "class" produces a fitted factor
G.tr.mda<-predict(out.mda, X.tr, type="class")
sum(G.tr.mda != G.tr) / length(G.tr)
G.te.mda<-predict(out.mda, X.te, type="class")
sum(G.te.mda != G.te) / length(G.te)


#### knn ####
library(class)
#
k<-10
# First argument: matrix or data frame of training set cases.
# second argument: matrix or data frame of test set cases
# Third argument cl: factor of true classifications of training set
# fourth argument k: number of neighbours considered (in our case, we are consider 10 neighbors)

# use training set to train the model and obtain predictions on the training set
G.tr.knn<-knn(X.tr, X.tr, G.tr, k=k)
sum(G.tr.knn != G.tr) / length(G.tr)

# use training set to train the model and obtain predictions on the testing set
G.te.knn<-knn(X.tr, X.te, G.tr, k=k)
sum(G.te.knn != G.te) / length(G.te)


#### CART ####
# install the package mda if you haven't before loading the library
# To install the package: install.packages("tree")
library(tree)

# first argument: The left-hand-side (response) should be either a numerical vector when 
# a regression tree will be fitted or a factor, when a classification tree is produced. 
# The right-hand-side should be a series of numeric or factor variables separated by +. The "."
# here is a simplified coding for all predcitors in the data x1+x2+...+x30
# second argument: your data set
# third argument subset: An expression specifying the subset of cases to be used (here we provide
# the index of the training set)

# train the tree using training set
out.tre<-tree(as.factor(y)~., data, subset=id.tr)

# plot the tree
plot(out.tre)
text(out.tre)

# predict grouping of training set using the model built by training set
G.tr.tre<-predict(out.tre, data.frame(X[id.tr,]), type="class")
sum(G.tr.tre != G.tr) / length(G.tr)

# predict grouping of testing set using the model built by training set
G.te.tre<-predict(out.tre, data.frame(X[-id.tr,]), type="class")
sum(G.te.tre != G.te) / length(G.te)


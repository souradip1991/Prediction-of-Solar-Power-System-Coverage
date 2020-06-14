#installing required packages
if(!require(ggplot2))
{
  install.packages("ggplot2")
  library(ggplot2)
}
if(!require(GGally))
{
  install.packages("GGally")
  library(GGally)
}
if(!require(corrplot))
{
  install.packages("corrplot")
  library(corrplot)
}
if(!require(nnet))
{
  install.packages("nnet")
  library(nnet)
}
if(!require(car))
{
  install.packages("car")
  library(car)
}
if(!require(ROCR))
{
  install.packages("ROCR")
  library(ROCR)
}
if(!require(rpart))
{
  install.packages("rpart")
  library(rpart)
}
if(!require(partykit))
{
  install.packages("partykit")
  library(partykit)
}
if(!require(randomForest))
{
  install.packages("randomForest")
  library(randomForest)
}
if(!require(adabag))
{
  install.packages("adabag")
  library(adabag)
}
if(!require(kernlab))
{
  install.packages("kernlab")
  library(kernlab)
}
##Load dataset 
solar.data<-read.csv(file.choose(),header=TRUE)
##EDA and Data Preprocessing
str(solar.data[,c(1,2,76,79)])
#Checking for Null and duplicated observations
table(is.na(solar.data))
table(duplicated(solar.data))
#Convert FALSE/TRUE to numerical factor levels
solar.data$voting_2012_dem_win<-toupper(solar.data$voting_2012_dem_win)
solar.data$voting_2016_dem_win<-toupper(solar.data$voting_2016_dem_win)
solar.data$voting_2016_dem_win<-ifelse(solar.data$voting_2016_dem_win=="FALSE",0,1)
table(solar.data$voting_2016_dem_win)
solar.data$voting_2016_dem_win<-factor(solar.data$voting_2016_dem_win,levels = c(0,1))
solar.data$voting_2012_dem_win<-ifelse(solar.data$voting_2012_dem_win=="FALSE",0,1)
solar.data$voting_2012_dem_win<-factor(solar.data$voting_2012_dem_win,levels = c(0,1))
table(solar.data$voting_2012_dem_win)
#Check structure of converted variables
str(solar.data[,c(1,2,76,79)])
##Correlation check
cor(solar.data[,c(44,46)])
cor(solar.data[,c(49:60)])
cor(solar.data[,c(74,75,77,78)])
cor(solar.data[,c(6,10)])
cor(solar.data[,c(3,4,7,8,9)])
######Splitting Training, Validation and Test dataset
set.seed(19200368)
#Remove variables with high correlation
new.df<-solar.data[,-c(7,8,10,27,35:43,46,49,52,53,55,59,60,77,78)]
#Scale Data
new.df[,-c(1,2,56,57)]<-scale(new.df[,-c(1,2,56,57)])
#CV split
N <- nrow(new.df)
train_samps <- sample(1:N, size = 0.50*N)
validation_samps<- sample( setdiff(1:N, train_samps), size = 0.25*N )
test_samps <- setdiff(1:N, union(train_samps, validation_samps))
train<-new.df[train_samps,]
validation<-new.df[validation_samps,]
test<-new.df[test_samps,]

####Logistic Regression
#Fit Multinomial Logistic Regression to training dataset
fit.lr<-glm(solar_system_count~.,data=train, family = binomial)
summary(fit.lr)
#Fit the model into validation dataset and classify the observations
pred_scores<-predict(fit.lr,validation,"response")
tau=0.5
pred_class<-ifelse(pred_scores>tau,1,0)
#create confusion matrix for the predicted classes and the actual classes
tab<-table(validation$solar_system_count,pred_class)
#Plot ROC curve to check AUC
predictedObject<-prediction(pred_scores,validation$solar_system_count)
perform<-performance(predictedObject,"tpr","fpr")
plot(perform, xlab="False Positive Rate", ylab="True Positive Rate", main="Receiver Operator Characteristics Curve")
abline(0,1,col="red",lty=2)
auc<-performance(predictedObject,"auc")
auc@y.values
#Select Optimal tau based on specificity and sensitivity

sensitivity<-performance(predictedObject,"sens")
specificity<-performance(predictedObject,"spec")
tau<-sensitivity@x.values[[1]]
SS <- sensitivity@y.values[[1]] + specificity@y.values[[1]]
best <- which.max(SS)
tau[best]
#Classify based on optimal tau
pred_class2<-ifelse(pred_scores>tau[best],1,0)
#create confusion matrix for the predicted classes and the actual classes
tab2<-table(validation$solar_system_count,pred_class2)
#calculate accuracy and misclassification error based on the confusion matrix
accuracy<-sum( diag(tab2) ) / sum(tab2)
cat("Confusion matrix and Accuracy: \n")
tab2
accuracy

####Classification Trees
#Fit Classification tree to training dataset
fit.ct <- rpart(solar_system_count ~ ., data = train)
#Plot the tree modeled
plot( as.party(fit.ct), cex = 0.5 )
#Get the summary of trained model
fit.ct
summary(fit.ct)
#Predict outcome of validation dataset
pred_class<-predict(fit.ct,validation,type = "class")
#Create confusion matrix and calculate validation accuracy
tab<-table(validation$solar_system_count,pred_class)
accuracy<-sum( diag(tab) ) / sum(tab)
cat("Confusion matrix: \n")
tab
cat("\n Accuracy = ", accuracy)

#####Bagging
#Fir Bagging to training dataset
fit.bag<-bagging(solar_system_count~.,data=train)
#Fit the model into validation dataset and classify the observations
pred_class<-predict(fit.bag,validation,type="class")
#create confusion matrix for the predicted classes and the actual classes
tab<-table(validation$solar_system_count,pred_class$class)
accuracy<-sum( diag(tab) ) / sum(tab) 
cat("Confusion matrix: \n")
tab
cat("\n Accuracy = ", accuracy)

#Plot 6 trees created by bagging method
par(mfrow=c(3,2))
for (j in 1:6)
{
  plot(fit.bag$trees[[j]],main=paste("Example ",j,sep=""))
  text(fit.bag$trees[[j]],use.n=TRUE,xpd=TRUE,col="magenta")
}

######Random Forest
#Fit Random Forest algorithm  to training dataset
fit.rf<-randomForest(solar_system_count~.,data=train,importance=TRUE)
#Fit the model into validation dataset and classify the observations
pred_class<-predict(fit.rf,validation)
#create confusion matrix for the predicted classes and the actual classes
tab<-table(validation$solar_system_count,pred_class)
accuracy<-sum( diag(tab) ) / sum(tab)
cat("Confusion matrix: \n")
tab
cat("\n Accuracy = ", accuracy)
#Plot the Gini index to check the important variables
varImpPlot(fit.rf,main="Random Forest")

####### Boosting
fit.boost<-boosting(solar_system_count~.,data=train,
                    coeflearn = "Breiman",boos = FALSE)
#Check if the model has been overfitted on training dataset
eBoostTrain <- errorevol(fit.boost, train)$error
# compute test classification error as function of number of trees
eBoostTest <- errorevol(fit.boost, validation)$error
# plot error paths
mat <- cbind(eBoostTrain,  eBoostTest)
cols <- c("deepskyblue4", "darkorange3")
matplot(mat, type = "l", lty = rep(2:1, each = 2), col = cols,
        lwd = 2, xlab = "Number of trees", ylab = "Classification error")
legend(x = 60, y = 0.14, cex = 1,
       legend = c( "Boosting train",  "Boosting test"),
       lty = rep(2:1, each = 2), col = cols, lwd = 2, bty = "n")
points(apply(mat, 2, which.min), apply(mat, 2, min), col = cols,
       pch = rep(c(15, 17), each = 2), cex = 1.5)

# Fit the model into validation dataset and classify the observations
pred_class<-predict(fit.boost,validation,type="class")
#create confusion matrix for the predicted classes and the actual classes and calculate the accuracy
tab<-table(validation$solar_system_count,pred_class$class)
accuracy<-sum( diag(tab) ) / sum(tab)
#Display the confusion matrix and accuracy
cat("Confusion matrix: \n")
tab
cat("\n Accuracy = ", accuracy)

###### Support Vector Machines
#Train the model using SVM on training dataset and Non-linear kernel
fit.svm <- ksvm(solar_system_count~.,data=train,kernel="rbfdot")
# Fit the model into validation dataset and classify the observations
pred_class<-predict(fit.svm,validation)
#create confusion matrix for the predicted classes and the actual classes
tab<-table(validation$solar_system_count,pred_class)
accuracy<-sum( diag(tab) ) / sum(tab)
cat("Confusion matrix: \n")
tab
cat("\n Accuracy = ", accuracy)

##### Running the entire process 100 times to select best model
##### Applying parallel programming to run the code efficiently on available processes
accuracy<-rep(NA,8)
no_cores <- detectCores() - 1  
registerDoParallel(no_cores)
accuracy_mat<-foreach (i=1:100,.combine = 'rbind',.multicombine = TRUE,.packages = c("nnet","car","ROCR","partykit","randomForest","adabag","kernlab","rpart","MASS")) %dopar% {
  N <- nrow(new.df)
  train_samps2 <- sample(1:N, size = 0.50*N)
  validation_samps2<- sample( setdiff(1:N, train_samps2), size = 0.25*N )
  test_samps2 <- setdiff(1:N, union(train_samps2, validation_samps2))
  train2<-new.df[train_samps2,]
  validation2<-new.df[validation_samps2,]
  test2<-new.df[test_samps2,]
  #####Logistic####
  fit.lr2<-glm(solar_system_count~.,data=train2, family = binomial)
  pred_scores<-predict(fit.lr2,validation2,"response")
  tau=0.5
  pred_class<-ifelse(pred_scores>tau,1,0)
  predictedObject<-prediction(pred_scores,validation2$solar_system_count)
  sensitivity<-performance(predictedObject,"sens")
  specificity<-performance(predictedObject,"spec")
  tau<-sensitivity@x.values[[1]]
  SS <- sensitivity@y.values[[1]] + specificity@y.values[[1]]
  best <- which.max(SS)
  pred_class2<-ifelse(pred_scores>tau[best],1,0)
  tab2<-table(validation2$solar_system_count,pred_class2)
  accuracy[1]<-sum( diag(tab2) ) / sum(tab2)
  ####Decision tree
  fit.ct2 <- rpart(solar_system_count ~ ., data = train2)
  pred_class<-predict(fit.ct2,validation2,type = "class")
  tab<-table(validation2$solar_system_count,pred_class)
  accuracy[2]<-sum( diag(tab) ) / sum(tab)
  #####Bagging
  fit.bag2<-bagging(solar_system_count~.,data=train2)
  pred_class<-predict(fit.bag2,validation2,type="class")
  tab<-table(validation2$solar_system_count,pred_class$class)
  accuracy[3]<-sum( diag(tab) ) / sum(tab)
  ####Randomforest
  fit.rf2<-randomForest(solar_system_count~.,data=train2,importance=TRUE)
  pred_class<-predict(fit.rf2,validation2)
  tab<-table(validation2$solar_system_count,pred_class)
  accuracy[4]<-sum( diag(tab) ) / sum(tab)
  ######Boosting
  fit.boost2<-boosting(solar_system_count~.,data=train2,
                       coeflearn = "Breiman",boos = FALSE)
  pred_class<-predict(fit.boost2,validation2,type="class")
  tab<-table(validation2$solar_system_count,pred_class$class)
  accuracy[5]<-sum( diag(tab) ) / sum(tab)
  #####SVM
  fit.svm2 <- ksvm(solar_system_count~.,data=train2,kernel="rbfdot")
  pred_class<-predict(fit.svm2,validation2)
  tab<-table(validation2$solar_system_count,pred_class)
  accuracy[6]<-sum( diag(tab) ) / sum(tab)
  
  acc<-c(logistic=accuracy[1],decision_tree=accuracy[2],
         bagging_method=accuracy[3],random_forest=accuracy[4],
         boosting_method=accuracy[5],svm=accuracy[6])
  best<-names( which.max(acc) )
  switch(best,
         logistic = {
           pred_test_scores<-predict(fit.lr2,test2,"response")
           tau=0.5
           pred_test_class<-ifelse(pred_test_scores>tau,1,0)
           predictedObject<-prediction(pred_test_scores,test2$solar_system_count)
           sensitivity<-performance(predictedObject,"sens")
           specificity<-performance(predictedObject,"spec")
           tau<-sensitivity@x.values[[1]]
           SS <- sensitivity@y.values[[1]] + specificity@y.values[[1]]
           best <- which.max(SS)
           pred_class2<-ifelse(pred_test_scores>tau[best],1,0)
           tab2<-table(test2$solar_system_count,pred_class2)
           accBest<-sum( diag(tab2) ) / sum(tab2)
         },
         decision_tree = {
           pred_ct<-predict(fit.ct2,test2,type = "class")
           tab<-table(test2$solar_system_count,pred_ct)
           accBest<-sum( diag(tab) ) / sum(tab)
         },
         random_forest = {
           pred_rf<-predict(fit.rf2,test2)
           tab<-table(test2$solar_system_count,pred_rf)
           accBest<-sum( diag(tab) ) / sum(tab)
         },
         bagging_method = {
           pred_bag<-predict(fit.bag2,test2,type="class")
           tab<-table(test2$solar_system_count,pred_bag$class)
           accBest<-sum( diag(tab) ) / sum(tab)
         },
         boosting_method = {
           pred_boost<-predict(fit.boost2,test2,type="class")
           tab<-table(test2$solar_system_count,pred_boost$class)
           accBest<-sum( diag(tab) ) / sum(tab)
         },
         svm = {
           pred_svm<-predict(fit.svm2,test2)
           tab<-table(test2$solar_system_count,pred_svm)
           accBest<-sum( diag(tab) ) / sum(tab)
         }
         
  )
  
  accuracy[7] <- best
  accuracy[8] <- accBest
  return(accuracy)
}
##Statistical Summary of validation and test accuracies
colnames(accuracy_mat) <- c("val_logistic", "val_decisiontree","val_bagging",
                            "val_random_forest","val_boosting","val_svm",
                            "best","test")
accuracy_mat<-as.data.frame(accuracy_mat)
accuracy_mat[,1]<-as.numeric(as.character(accuracy_mat[,1]))
accuracy_mat[,2]<-as.numeric(as.character(accuracy_mat[,2]))
accuracy_mat[,3]<-as.numeric(as.character(accuracy_mat[,3]))
accuracy_mat[,4]<-as.numeric(as.character(accuracy_mat[,4]))
accuracy_mat[,5]<-as.numeric(as.character(accuracy_mat[,5]))
accuracy_mat[,6]<-as.numeric(as.character(accuracy_mat[,6]))
accuracy_mat[,8]<-as.numeric(as.character(accuracy_mat[,8]))
str(accuracy_mat)

#Statistical summary of both models
cat("Logistic Regression: \n")
summary(accuracy_mat[,1])
cat("Classification Tree: \n")
summary(accuracy_mat[,2])
cat("Bagging:\n")
summary(accuracy_mat[,3])
cat("Random Forest: \n")
summary(accuracy_mat[,4])
cat("Boosting:\n")
summary(accuracy_mat[,5])
cat("Support Vector Machine: \n")
summary(accuracy_mat[,6])

#Plotting the accuracy rate of the best model selected in each iteration
table(accuracy_mat[,7])
tapply(accuracy_mat[,8], accuracy_mat[,7], summary)
boxplot(accuracy_mat[,8] ~ accuracy_mat[,7], xlab="Random Forest", ylab="Accuracy",main="Accuracy rate statistics")
stripchart(accuracy_mat[,8] ~ accuracy_mat[,7], add = TRUE, vertical = TRUE,
           method = "jitter", pch = 19, col = adjustcolor("blue", 0.2))


#Plotting the variation in accuracy
meanAcc<-colMeans(accuracy_mat[,1:6])
matplot(accuracy_mat[,c(2,3)],type="o",cex=0.5,lty=2,lwd=1,pch=1,col=c("blue","red"),
        ylab="Accuracy",xlab="Iterations",main="Accuracy variation plot")
abline(h=meanAcc[c(2,3)],col=c("blue","red"))
legend("right",col=c("blue","red"),lty=1,pch=1,cex=0.8,title ="Classification Type",legend =c("CT","Bagging"))

matplot(accuracy_mat[,c(1,4,5,6)],type="o",cex=0.5,lty=2,lwd=1,pch=1,col=c("magenta","orange","brown","darkgreen"),
        ylab="Accuracy",xlab="Iterations",main="Accuracy variation plot")
abline(h=meanAcc[c(1,4,5,6)],col=c("magenta","orange","brown","darkgreen"))
legend("right",col=c("magenta","orange","brown","darkgreen"),lty=1,pch=1,cex=0.8,title ="Classification Type",legend =c("LR","RF","Boosting","SVM"))

######### Applying the best model selected on the test dataset
#Fit the model into validation dataset and classify the observations
pred_class<-predict(fit.rf,test[,-1])
#create confusion matrix for the predicted classes and the actual classes
tab<-table(test$solar_system_count,pred_class)
#Calculate accuracy of test dataset classification
accuracy<-sum( diag(tab) ) / sum(tab)
cat("Confusion matrix: \n")
tab
cat("\n Accuracy = ", accuracy)



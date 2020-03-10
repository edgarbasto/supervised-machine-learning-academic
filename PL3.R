wd <- "/home/president/ISCTE/ML-Supervised/Wholesale/"
setwd(wd)


## options  ####
options("scipen" = 999)
wholesale<-read.csv("Wholesale customers data.csv", header=TRUE, dec=".")

#1) Descriptives
names(wholesale)

str(wholesale)
summary(wholesale)# first two variables should be factors!!
typeof(wholesale$Channel)

wholesale$Channel <- factor(wholesale$Channel , levels = c(1,2), labels = c("Horeca", "Retail"))
wholesale$Region<- factor(wholesale$Region , levels = c(1,2,3), labels = c("Lisbon", "Oporto", "Other"))

knitr::kable(summary(wholesale))
attach(wholesale)

#2) Simple logistic model based on all dataset
contrasts(Channel)
#2b)logistic model
glm.wholesale<-glm(Channel ~ Grocery, data=wholesale ,family =binomial )
str(glm.wholesale)

glm.wholesale$coef

#2c) logistic model predictions
probs.glm.wholesale <-predict (glm.wholesale ,type ="response")
?predict

probs.glm.wholesale[1]
Grocery[1]
?exp

#2d) Logistic model graph
plot(Grocery,probs.glm.wholesale,pch =20)# sigmoid curve

#2e) Logistic model classification result
pred.glm.wholesale <- rep (1 ,nrow(wholesale))
pred.glm.wholesale [probs.glm.wholesale >.5]=2
head(pred.glm.wholesale)

pred.glm.wholesale<-factor(pred.glm.wholesale , levels = c(1,2), labels = c("Horeca", "Retail"))
(confusion_mat<-table(Channel,pred.glm.wholesale))
confusion_mat

#3) Metrics to evaluate the classification
#accuracy
(accuracy<-sum(diag(confusion_mat))/sum(confusion_mat))
## [1] 0.8590909
#Classification Error = 1-accuracy
(ce<- 1-accuracy)
## [1] 0.1409091
# In order to define precision and recall one has to determine which category is the "positive" event. Considering Horeca as "positive":
# Recall or Sensitivity is TP/(TP+FN):
recall_Horeca<- confusion_mat[1,1]/sum(confusion_mat[1,])
recall_Horeca
# Precision is TP/(TP+FP):
precision_Horeca <-confusion_mat[1,1]/sum(confusion_mat[,1])
precision_Horeca
#F score is defined as 2 * precision * recall/(precision+recall)
Fscore_Horeca<-2*precision_Horeca*recall_Horeca/(precision_Horeca+recall_Horeca)#
Fscore_Horeca
# Specificity TN/(FP+TN)
specificity_Retail <-confusion_mat[2,2]/sum(confusion_mat[2,])
specificity_Retail

# Huberty
default_p<-max(mean(Channel == "Horeca"), mean(Channel == "Retail"))# majority class frequency
(Huberty<-(accuracy-default_p)/(1-default_p))

install.packages('e1071')
install.packages('caret')
library(knitr)
library(psych)# to provide descriptive statistics
library(ggplot2)# to provide graphics
library(e1071)# classification with naive bayes
library(caret)# to provide folds for cross-validation

#1a) Descriptives
data(iris)
names(iris)
dim(iris)
knitr::kable(summary(iris))
attach(iris)
#1b) Continuing Descriptives
knitr::kable(round(describe(iris[,1:4]),3))

knitr::kable(round(describe(iris[Species=="setosa",-5]),3))
knitr::kable(round(describe(iris[Species=="versicolor",-5]),3))
knitr::kable(round(describe(iris[Species=="virginica",-5]),3))

knitr::kable(round(cor(iris[,1:4]),2))

#1c) Some graphics
c <- ggplot(iris, aes(x=Sepal.Length, fill=Species, color=Species)) +  geom_histogram(binwidth = 1) + labs(title="Sepal.Length distribution by species") 
c + theme_bw()


p <- ggplot(iris, aes(x=Sepal.Width, fill=Species, color=Species)) + geom_histogram(binwidth = 0.5) + labs(title="Sepal.Width distribution by species")
p + theme_bw()

#2) Naive Bayes classifier
#2a) naive bayes based on all dataset
x <- iris[,-5]
head(x)
y <- iris$Species
head(y)
nb.iris<-naiveBayes(x, y)
nb.iris


#2b) naive bayes predictions
pred_y <- predict(nb.iris, iris)
iris_pred <- cbind(iris, pred_y)
knitr::kable(head(iris_pred))
iris_pred

#2c) naive bayes performance
knitr::kable(confusion_mat<-table(iris$Species,pred_y))
(accuracy<-sum(diag(confusion_mat))/sum(confusion_mat))

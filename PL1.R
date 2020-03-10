install.packages('Metrics')
library(MASS)
library(Metrics)
library(ggplot2)

#1a)
data(Boston)
dim(Boston)
names(Boston)

knitr::kable(summary(Boston))

Boston$chas <- as.factor(Boston$chas)
knitr::kable(summary(Boston))
attach(Boston)

#1b)
ggplot(Boston, aes(x = "", y = medv)) + geom_boxplot()

which(medv>quantile(medv,prob=0.75)+1.5*(quantile(medv,prob=0.75)-quantile(medv,prob=0.25)))
which(medv<quantile(medv,prob=0.25)-1.5*(quantile(medv,prob=0.75)-quantile(medv,prob=0.25)))

knitr::kable(corr.Boston<-round(cor(Boston[,-4]),2))
corr.Boston[,13]

which.max( abs(corr.Boston[-13,13]) )
corr.Boston[12,13]

#2) Simple linear regression
#2a)
lm.Boston1 <-lm(medv~lstat ,data=Boston )
lm.Boston1
lm.Boston1$coefficients

#2b)
plot.new()
plot(lstat ,medv,pch =20)
abline (lm.Boston1)
abline (lm.Boston1 ,lwd =3)
abline (lm.Boston1 ,lwd =3, col ="red ")



#2c)
pred.lm.Boston1<-predict (lm.Boston1)
residuals.Boston1<-medv-pred.lm.Boston1
head(residuals.Boston1)

residuals.Boston1<-residuals (lm.Boston1)
round(head(residuals.Boston1),3)
View(residuals.Boston1)

#2d)
plot(predict (lm.Boston1), residuals (lm.Boston1))
which.max(abs(residuals (lm.Boston1)))
lm.Boston1$residuals[which.max(abs(residuals (lm.Boston1)))]


#2e) R-square
(rsq<-summary(lm.Boston1)$r.squared)


#3
#3.1)
set.seed(777)
ind_train <- sample(nrow(Boston),.65*nrow(Boston))
Boston_train <- Boston[ind_train,]
dim(Boston_train)

Boston_test <- Boston[-ind_train,]
dim(Boston_test)

#3.2)
lm.Boston1_train <-lm(medv~lstat ,data=Boston_train )
lm.Boston1_train$coefficients
summary(lm.Boston1_train)$r.squared

#em teste
head(Boston_test$medv)
pred.lm.Boston1_test<-predict (lm.Boston1_train ,Boston_test)
head(pred.lm.Boston1_test)
(rsq_test<-1-sse(medv[-ind_train], pred.lm.Boston1_test)/sse(medv[-ind_train],mean(medv[-ind_train])))


#4 Testar sem um outlier
View(lm.Boston1_train)
which.max(abs(residuals (lm.Boston1_train)))
lm.Boston1_train$residuals[which.max(abs(residuals (lm.Boston1_train)))]

#Novo dataset de treino sem a posição 373
rownames(Boston_train)
Boston2_train <- Boston_train[!(rownames(Boston_train) == 373),]
dim(Boston2_train)

lm.Boston2_train <-lm(medv~lstat ,data=Boston2_train )
lm.Boston2_train$coefficients
summary(lm.Boston2_train)$r.squared

#teste
pred.lm.Boston2_test<-predict (lm.Boston2_train ,Boston_test)
head(pred.lm.Boston2_test)
(rsq_test<-1-sse(medv[-ind_train], pred.lm.Boston2_test)/sse(medv[-ind_train],mean(medv[-ind_train]))) #ERRADO por causa do ind_train













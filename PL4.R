wd <- "/home/president/ISCTE/ML-Supervised/"
setwd(wd)
options(scipen = 999)

library(MASS)
library(car)
library(Metrics)

data("Boston")
knitr::kable(summary(Boston))
corr.Boston<-round(cor(Boston),2)
(corr.Boston[,14])# correlations with target

#2) Linear Regression model with 2 predictors
#Beggining with rm and lstat have better correlations with the target vardiable (medv)
lm.Boston2 <-lm(medv~ lstat+rm,data=Boston )
names(lm.Boston2)
summary(lm.Boston2)

#2b) predicting medv
pred.medv<-predict(lm.Boston2,Boston)
View(pred.medv)

#2c) Evaluating regression performance
attach(Boston)
se(medv, predict(lm.Boston2)) # elementwise squared error

sse(medv, predict(lm.Boston2))# sum of squared errors
sse(medv,mean(medv))# total variance

(rsq<-1-sse(medv, predict(lm.Boston2))/sse(medv,mean(medv))) #r-squared

(rmse <- sqrt(sse(medv, predict(lm.Boston2))/nrow(Boston))) # root mean squared error RMSE

#mean absolute error MAE
ae(medv, predict(lm.Boston2))#elementwise absolute error
(mae <- sum(ae(medv, predict(lm.Boston2)))/nrow(Boston))

#relative absoule error RAE
rae(medv,predict(lm.Boston2))
(rae_<-sum(ae(medv, predict(lm.Boston2)))/sum(ae(medv, mean(medv)))) #alternative way to calculate

#relative squared error RSE
(rse <- sse(medv, predict(lm.Boston2))/sse(medv,mean(medv)))

#summary of the performance
eval_res <- c(rsq,rmse,mae,rae_,rse)
write.table(eval_res,'Regression_metrics.csv', sep='', row.names=c('R2', 'RMSE', 'MAE', 'RAE', 'RSE'), col.names='Regression metrics')


# R2 63,85% percentagem da variação total de medv (em torno da média) que é explicada pelo modelo de regressão linear multipla
# RMSE 5.523 estimativa de um erro por observação (entre os valores previstos e os valores observados) calculado pela raiz do erro quadrático
# MAE 3.953 estimativa do erro por observação
# RAE 0.5946
# RSE 0.361





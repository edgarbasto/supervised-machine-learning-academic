install.packages('Metrics')

library(MASS)
library(Metrics)
library(ggplot2)

data(Boston)
dim(Boston)
names(Boston)

knitr::kable(summary(Boston))

Boston$chas <- as.factor(Boston$chas)
knitr::kable(summary(Boston))

attach(Boston)

#1b)
ggplot(Boston, aes(x = "", y = medv)) + geom_boxplot()
quantile(medv)
which(medv>quantile(medv,prob=0.75)+1.5*(quantile(medv,prob=0.75)-quantile(medv,prob=0.25)))
which(medv<quantile(medv,prob=0.25)-1.5*(quantile(medv,prob=0.75)-quantile(medv,prob=0.25)))


knitr::kable(corr.Boston<-round(cor(Boston[,-4]),2))
corr.Boston[,13]
which.max( abs(corr.Boston[-13,13]) )

corr.Boston[12,13]

#2)
?lm
lm.Boston1 <- lm(medv~lstat, data=Boston)
lm.Boston1

lm.Boston1$coefficients

#2b)
plot.new()
plot(lstat ,medv,pch =20)
abline(lm.Boston1)
abline (lm.Boston1 ,lwd =3)# line width ...
abline (lm.Boston1 ,lwd =3, col ="red ")#...and colour


#2c)
pred.lm.Boston1<-predict (lm.Boston1)
pred.lm.Boston1
residuals.Boston1<-medv-pred.lm.Boston1
head(residuals.Boston1)
residuals.Boston1<-residuals (lm.Boston1)
round(head(residuals.Boston1),3)

#2d)
plot(predict (lm.Boston1), residuals (lm.Boston1))
which.max(abs(residuals (lm.Boston1)))



lm.Boston1$residuals[which.max(abs(residuals (lm.Boston1)))]


#2e)



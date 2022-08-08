Data = read.csv('baseball.csv')
c<-Data[,-18]

library(leaps)

p<-regsubsets(salary~.,data=c, nvmax=6, really.big=T) # find best subset of data
summary(p)

baseball_subset<-Data[,c(1,8,9,11,12,14,16)]

fit = lm(salary ~.,data=baseball_subset) # use the data frame baseball

baseball.stdres = rstandard(fit) # get standardized residuals
index<-1:337 # index running from 1 to 337
plot(index, baseball.stdres,ylab="Standardized Residuals",xlab="index from 1-337",main="Standardized Residual Model")
abline(0, 0)
abline(3, 0, col="red") # abline at 3 and -3 helps see which players have resids > 3
abline(-3, 0, col="red")

# Provide a plot of standardized residuals versus predicted values
resid = fit$residuals
predict = fit$fitted.value # predicted values vector
plot(predict,resid,main="Predicted vs Residual plot")

# Normal probability plot of standardized residuals and comment on the plot
out = qqnorm((resid-mean(resid))/sd(resid)) # normal probability plot
x = range(out$x)
lines(c(x[1],x[2]),c(x[1],x[2]))

output=cooks.distance(fit)
plot(output,ylab='Cooks D',main="Plot of Cook's D Values")

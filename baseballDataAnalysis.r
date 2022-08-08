Data = read.csv('baseball.csv')
c<-Data[,18]

library(leaps)

p<-regsubsets(salary~.,data=c, nvmax=6, really.big=T) # find best subset of data
summary(p)

fit = lm(salary ~.,data=baseball) # use the data frame baseball

# coefficient of independent variable "hits": -2.698
# according to this, getting 10 extra hits means a salary DECREASE of 10*2.7 = $27.
# This does not make sense. By my intuition, one would expect that more hits means better payday.
# There should be a positive coefficient for hits, not negative.   

variation_percentage = summary(lm(salary ~.,data=baseball))$r.squared 
# 70.14% of variation in salaries explained by linear model

baseball_none = Data[,c(1,13)] # testing free agent status vs salary had the highest p-value. But this p-value of 0.02 was still < 0.05. 
fit_none = lm(salary ~.,data=baseball_none) # use data from the eleven columns above 

baseball_eleven = Data[,c(1,8:17)] # This takes data from x8..x17 and compares with salary in x1 
fit_eleven = lm(salary ~.,data=baseball_eleven) # use data from the eleven columns above 

variation_percentage_eleven = summary(lm(salary ~.,data=baseball_eleven))$r.squared
# 69.73% of variation in salaries explained by linear model of given 11 columns

resid = fit_eleven$residuals
# put predicted values into a predict vector
predict = fit$fitted.value 

plot(predict,resid, main="Predicted vs Residual Plot", xlab="Predicted values", ylab="Residuals") 
# plot predicted values vs residuals

plot(density(fit_eleven$residuals), main = "Kernel Density plot") # kernel density estimate

out = qqnorm((resid-mean(resid))/sd(resid)) # normal probability plot
x = range(out$x)
lines(c(x[1],x[2]),c(x[1],x[2]))


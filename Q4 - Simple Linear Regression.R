data <- read.csv('sales.csv',header=TRUE,sep=',')

x <- data$budget
y <- data$totalUnits

mean_x <- sum(x)/nrow(data)
mean_y <- sum(y)/nrow(data)

b_1 <- sum((x-mean_x)*(y-mean_y))/sum((x-mean_x)^2)
b_0 <- mean_y - b_1*mean_x

getPred<-function(x){
  return (b_0 + (b_1*x))
}


x_test<-c(3000,4000,5000,6000,7000)
predicted<-sapply(x_test,getPred)
predicted
par(mfrow=c(1,2))    # set the plotting area into a 1*2 array
plot(x_test,predicted)


##Library Function
linearMod<-lm(totalUnits~budget,data=data)
print(linearMod)
modelSummary<-summary(linearMod)
modelCoeffs<-modelSummary$coefficients
modelCoeffs
x_test_lib = data.frame(budget=c(3000,4000,5000,6000,7000))
predictions_lib<-predict(linearMod,x_test_lib)

plot(x_test,predictions_lib)


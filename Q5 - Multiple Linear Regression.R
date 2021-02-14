#source - http://faculty.cas.usf.edu/mbrannick/regression/Part3/Reg2.html

data <- read.csv('salesmlr.csv')

x1 <- data$budgetNews
x2 <- data$budgetTv
y <- data$totalUnits

x1_ <- sum(x1)/length(x1)
x2_ <- sum(x2)/length(x2)
y_ <- sum(y)/length(y)


productSum <- function(x1,x2){
  #find summation of (x1-x1_)(x2-x2_)
  return(sum(x1*x2)-(sum(x1)*sum(x2))/length(x1))
}

findBk <- function(xk,xn,y){
  num <- productSum(xn,xn)*productSum(xk,y) - productSum(xk,xn)*productSum(xn,y)
  den <- productSum(xk,xk)*productSum(xn,xn) - productSum(xk,xn)^2
  return(num/den)
}

b_1 <- findBk(x1,x2,y)
b_2 <- findBk(x2,x1,y)
b_0 <- y_ - b_1*x1_ - b_2*x2_

getPred<-function(x){
  return (b_0+(b_1*x[1])+(b_2*x[2]))
}

x_test = data.frame("budgetNews"=c(150,300,450,600,750),"budgetTv"=c(3000,4000,5000,6000,7000))

predicted<-apply(x_test,1,getPred)
predicted

#using library function for mlr 
model <- lm(totalUnits ~ budgetNews+budgetTv,data=data)
summary(model)
predictions_lib<-predict(model,x_test)
predictions_lib

#Comparison Table
library(grid)
library(gridExtra)
result = data.frame("budgetTv"=c(3000,4000,5000,6000,7000),"budgetNews"=c(150,300,450,600,750),"predictions"=predicted,"predictions_lib"=predictions_lib)

coeff_table <- data.frame("Non Predefined Function"=c(b_0,b_1,b_2),"Predefined Function"=summary(model)$coefficients[1:3])
rownames(coeff_table) <- c("Intercept", "budgetTv", "budgetNews")
grid.arrange(tableGrob(coeff_table),tableGrob(result))


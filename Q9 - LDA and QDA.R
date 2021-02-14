#LDA 
# install.packages('ISLR')

library(ISLR) #credit card dataset
library(MASS)
dfr = Default

index = sample(x = 1:nrow(dfr), size = round(nrow(dfr) * 0.5))

train_dfr = dfr[index, ]
test_dfr = dfr[-index, ]

model = lda(default ~ student + balance + income, data = train_dfr)
summary(model)

pred = predict(model, test_dfr)
test_dfr$pred = pred$class
head(test_dfr)

table(test_dfr$pred, test_dfr$default)

head(dfr)


#QDA

library(ISLR)  #credit card - banking dataset 
library(MASS)

dfr = Default
dfr$student = as.numeric(dfr$student)
dfr$student = as.factor(dfr$student)

index = sample(1:nrow(dfr), round(nrow(dfr) * 0.5))
train_dfr = dfr[index, ]
test_dfr = dfr[-index, ]

model = qda(default ~ student + balance + income, train_dfr)
summary(model)

pred = predict(model, test_dfr)
test_dfr$pred = pred$class
head(test_dfr)

table(test_dfr$pred, test_dfr$default)

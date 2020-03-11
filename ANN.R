setwd("F://Rsoftware/RNN")
getwd()
library("neuralnet")
heart_dis <- read.csv("whas1.csv", head=TRUE, sep=",")
View(heart_dis)
str(heart_dis)
heart_dis <- heart_dis[, -1]
str(heart_dis)
summary(heart_dis)
heart_dis[1:12] <- scale(heart_dis[1:12])
summary(heart_dis)
set.seed(12345)
ind <- sample(2, nrow(heart_dis), replace = TRUE, prob = c(0.7, 0.3))
train.data <- heart_dis[ind == 1, ]
test.data <- heart_dis[ind == 2, ]
nn <- neuralnet(formula = FSTAT ~ AGE + SEX + CPK + SHO + CHF + MIORD + 
                  MITYPE + YEAR + YRGRP + LENSTAY + DSTAT + LENFOL, 
                data = train.data, hidden = 2, 
                err.fct = "ce", linear.output = FALSE)

summary(nn)
nn$response[1:20]
nn$net.result[[1]][1:20]
nn$result.matrix
plot(nn)
mypredict <- compute(nn, nn$covariate)$net.result
mypredict <- apply(mypredict, c(1), round)
mypredict[1:20]
table(mypredict, train.data$FSTAT, dnn =c("Predicted", "Actual"))
testPred <- compute(nn, test.data[, 0:12])$net.result
testPred <- apply(testPred, c(1), round)
table(testPred, test.data$FSTAT, dnn = c("Predicted", "Actual"))
set.seed(12345)
nn <- neuralnet(formula = FSTAT ~ AGE + SEX + CPK + SHO + CHF + MIORD + 
                  MITYPE + YEAR + YRGRP + LENSTAY + DSTAT + LENFOL, 
                data = train.data, hidden = c(4, 2), 
                err.fct = "ce", linear.output = FALSE)
nn$net.result[[1]][1:20]
nn$result.matrix
plot(nn)
mypredict <- compute(nn, nn$covariate)$net.result
mypredict <- apply(mypredict, c(1), round)
table(mypredict, train.data$FSTAT, dnn =c("Predicted", "Actual"))
testPred <- compute(nn, test.data[, 0:12])$net.result
testPred <- apply(testPred, c(1), round)
table(testPred, test.data$FSTAT, dnn = c("Predicted", "Actual"))
set.seed(12345)
nn <- neuralnet(formula = FSTAT ~ AGE + SEX + CPK + SHO + CHF + MIORD + 
                  MITYPE + YEAR + YRGRP + LENSTAY + DSTAT + LENFOL, 
                data = train.data, hidden = 1, 
                err.fct = "ce", linear.output = FALSE)
nn$net.result[[1]][1:20]
nn$result.matrix
plot(nn)
mypredict <- compute(nn, nn$covariate)$net.result
mypredict <- apply(mypredict, c(1), round)
table(mypredict, train.data$FSTAT, dnn =c("Predicted", "Actual"))
testPred <- compute(nn, test.data[, 0:12])$net.result
testPred <- apply(testPred, c(1), round)
table(testPred, test.data$FSTAT, dnn = c("Predicted", "Actual"))

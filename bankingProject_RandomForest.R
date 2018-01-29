######################### Train Data ###########################

bank_train = read.csv("bank-full_train.csv")

glimpse(bank_train)

table(bank_train$default)
bank_train$default = as.numeric(bank_train$default=="yes")

table(bank_train$housing)
bank_train$housing = as.numeric(bank_train$housing=="yes")

table(bank_train$loan)
bank_train$loan = as.numeric(bank_train$loan=="yes")

## bank_train$y = as.numeric(bank_train$y == "yes")

for(i in 1:ncol(bank_train)){
  if(class(bank_train[,i])=="character"){
    bank_train[,i]=as.factor(bank_train[,i])
  }
}

na.omit(bank_train)

#################### Test Data ###################

bank_test = read.csv("bank-full_test.csv")

table(bank_test$default)
bank_test$default = as.numeric(bank_test$default=="yes")

table(bank_test$housing)
bank_test$housing = as.numeric(bank_test$housing=="yes")

table(bank_test$loan)
bank_test$loan = as.numeric(bank_test$loan=="yes")


for(i in 1:ncol(bank_test)){
  if(class(bank_test[,i])=="character"){
    bank_test[,i]=as.factor(bank_test[,i])
  }
}

library(dplyr)

glimpse(bank_train)
glimpse(bank_test)

############################# Building MOdel using Random Forest ################################

library(randomForest) 
rf_bank=randomForest(y~.,data=bank_train)
rf_bank

################################ Prediction to Test data ###############################

forest.pred=predict(rf_bank,newdata=bank_test)

table(forest.pred)

View(cbind.data.frame(bank_test, forest.pred))

x = forest.pred
writeClipboard(as.character(x))

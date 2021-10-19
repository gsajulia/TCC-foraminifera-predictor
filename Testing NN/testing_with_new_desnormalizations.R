library(devtools)
require(neuralnet)
library(stringr)
library(caret)
library(e1071)
library(dplyr)

outputExpected = "RES_jul-sep_0-100m"

df <- read.csv("last_forams_data_clean_last.csv",
               header = TRUE,
               sep = ",")

outputExpected = gsub("-", ".", outputExpected, fixed = TRUE)

# Keep only the RES column of outputExpected
outputResult = df[, outputExpected]
df <- df[, -grep("RES", colnames(df))]
df[outputExpected] <- outputResult

# Names of the attributes from csv
dfNames <- names(df)

# NA coluns receive 0
df[is.na(df)] = 0

# Function to normalize the dataset
normalize <- function(x) {
  z = x
  if(min(x) < max(x)){ 
    z = (x - min(x)) / (max(x) - min(x))
  }
  return(z)
}

desnormalize <- function(normalizedValue, originalValue) {
  z = normalizedValue * (max(originalValue) - min(originalValue)) + min(originalValue)
  return(z)
}

# Normalization of dataset, dimension values between 0 and 1
dataset <- as.data.frame(normalize(df))

# Creating formula that pick the names of the columns
# And concat each one with "+"
formula = str_c(dfNames[!dfNames %in% outputExpected],
                collapse = "+");

# Separation of dataset, train of 90% and test of 10%
index = sample(seq_len(nrow(dataset)), size = 0.90 * nrow(dataset))
train <- dataset[index,];
test <- dataset[-index,];

desnormTest <- data.frame(desnormalize(test, df))  

# Neural Network equation
nn = neuralnet(str_c(outputExpected, " ~ ", formula), data=train,
               algorithm = "rprop+", startweights = NULL,
               hidden = c(5, 2), stepmax = 1e+06,
               lifesign = "none", threshold = 0.01,
);

# plot neural network
plot(nn)

# NN Result
predict = neuralnet::compute(nn, test);

# Denormalizing values from result
desnormResult <- data.frame(desnormalize(predict$net.result, df))

# Predicions as a new column of dataframe
colnames(result) <- c("Prediction")
result <- predict$net.result
colnames(desnormResult) <- c("Denormalized prediction")
# Table of all informations
finalResult <- cbind(desnormTest, desnormResult)

##Option 2

actual <- test[outputExpected]
prediction <- nn.results$net.result

accuracy <- ifelse(prediction<actual, (prediction/actual)*100, ((prediction/actual)-2)*-100)

# sprintf("Médiaaa: %i", mean(accuracy))

results <- data.frame(actual = actual, prediction = prediction, accuracy = accuracy)

##Option 3

predicted=desnormResult
actual=desnormTest[outputExpected]
comparison=data.frame(predicted,actual)
deviation=((actual-predicted)/actual)
comparison=data.frame(predicted,actual,deviation)
#This if is to avoid the problem with division with 0 resulting in Inf
deviation<- deviation[!abs(deviation) == Inf]
accuracy=1-abs(mean(data.matrix(deviation)))
sprintf("Error: %f", nn$result.matrix[1,])
sprintf("Médiaaa: %f", mean(accuracy))
sprintf("%1.2f%%", accuracy*100)

# Result Plot
plot(test[outputExpected][,1], predict$net.result[,1],col='red',main='Real vs predicted NN', xlab="Real", ylab="DNN")
abline(0,1,lwd=2)


# Predict function (separate file)
teste = df[, -grep(outputExpected, colnames(df))]
predict = neuralnet::compute(nn, teste);
#TODO -inf problem, sometimes occurs, problem with division by 0
cbind(teste, Valor_predito=desnormalize(predict$net.result, df))
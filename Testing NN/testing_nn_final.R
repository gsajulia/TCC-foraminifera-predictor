library(devtools)
require(neuralnet)
library(stringr)
library(caret)
library(e1071)
library(dplyr)
library(Metrics)

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

##Calculating error, precision and accuracy

predicted=desnormResult
actual=desnormTest[outputExpected]
comparison=data.frame(predicted,actual)
deviation=((actual-predicted)/actual)
comparison=data.frame(predicted,actual,deviation)
#This if is to avoid the problem with division with 0 resulting in Inf
deviation<- deviation[!abs(deviation) == Inf]
accuracy=1-abs(mean(data.matrix(deviation)))
sprintf("Médiaaa: %f", mean(accuracy))
sprintf("%1.2f%%", accuracy*100)


# Calculating precision
accurate = 1-abs(data.matrix(deviation))
pred <- ifelse(accurate>0.9, 1, 0)
truePositives <- length(pred[pred == 1])
sprintf("/n/nSão: %i de %i ", truePositives, length(pred))
falsePositives <- length(pred)-truePositives
precision = truePositives/(truePositives + falsePositives)

# Calculating error
sprintf("MSE: %f", nn$result.matrix[1,])
sprintf("MAE: %f", mae(actual[,1], predicted[,1]))
sprintf("RMSE: %f", rmse(actual[,1], predicted[,1]))
sprintf("RAE: %f", rae(actual[,1], predicted[,1]))

# Result Plot
plot(test[outputExpected][,1], predict$net.result[,1],col='red',main='Real vs predicted NN', xlab="Real", ylab="DNN")
abline(0,1,lwd=2)


# Predict function (separate file)
teste = df[, -grep(outputExpected, colnames(df))]
newpredict = neuralnet::compute(nn, teste);
cbind(teste, Valor_predito=desnormalize(predict$net.result, df))
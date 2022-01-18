library(devtools)
require(neuralnet)
library(stringr)
library(caret)
library(e1071)
library(dplyr)
library(Metrics)
library("rpart")
library("rpart.plot")


##To use to others models will have "CHANGE"

outputExpected = "RES_Annual_0m" #CHANGE the string

df <- read.csv("last_forams_data_clean_last.csv",
               header = TRUE,
               sep = ",")

outputExpected = gsub("-", ".", outputExpected, fixed = TRUE)

# Keep only the RES column of outputExpected
outputResult = df[, outputExpected]
df <- df[,-grep("RES", colnames(df))]
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

    nn$result.matrix

    # NN Result
    predict = neuralnet::compute(nn, test);

    # Denormalizing values from result
    desnormResult <- data.frame(desnormalize(predict$net.result, df))

    colnames(desnormResult) <- c("Denormalized prediction")

    # Table of all informations
    finalResult <- cbind(desnormTest, desnormResult)
    
    # Test the resulting output of predict and the real value
    temp_test <- subset(test, select = c(dfNames[1:length(dfNames)]))
    head(temp_test)
    nn.results <- neuralnet::compute(nn, temp_test)

    # Calculating how accurate the model is

    results <- data.frame(actual = test[outputExpected], prediction = nn.results$net.result)

    predicted=desnormResult
    actual=desnormTest[outputExpected]
    comparison=data.frame(predicted,actual)
    deviation=((actual-predicted)/actual)
    comparison=data.frame(predicted,actual,deviation)
    

    # Calculating precision
    accurate = 1-abs(data.matrix(deviation))
    pred <- ifelse(accurate>0.9, 1, 0)
    truePositives <- length(pred[pred == 1])
    sprintf("/n/nSão: %i de %i ", truePositives, length(pred))
    falsePositives <- length(pred)-truePositives
    precision = truePositives/(truePositives + falsePositives)

    #This if is to avoid the problem with division with 0 resulting in Inf
    deviation<- deviation[!abs(deviation) == Inf]
    accuracy=1-abs(mean(data.matrix(deviation)))
    mse = nn$result.matrix[1,]
    mae = mae(actual[,1], predicted[,1])
    rmse = rmse(actual[,1], predicted[,1])
    rae = rae(actual[,1], predicted[,1])
    sprintf("%1.2f%%", accuracy*100)

formula = str_c(dfNames[!dfNames %in% outputExpected],
                collapse = "+")
foramTree <- rpart(str_c(outputExpected, " ~ ", formula), data = df, method='anova')

# CHANGE variable name
NN_RES_Annual_0m = list(
        `accuracy`=round(accuracy*100, 2), 
        `precision`= round(precision*100, 2), 
        `table`=finalResult, 
        `cleanDf`= df, 
        `nn`= nn, 
        `test`=test,
        `mse`=round(mse, 2),
        `mae`=round(mae, 2),
        `rmse`=round(rmse, 2),
        `rae`=round(rae, 2),
        `foramTree`=foramTree
        )
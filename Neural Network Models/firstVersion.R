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
outputExpectedOptions = c(
    "RES_Annual_0m",
    "RES_Annual_50m",
    "RES_Annual_75m",
    "RES_Annual_100m",
    "RES_Annual_0-75m",
    "RES_Annual_0-100m",
    "RES_Annual_0-200m",
    ###################
    "RES_jan-mar_0m",
    "RES_jan-mar_50m",
    "RES_jan-mar_75m",
    "RES_jan-mar_100m",
    "RES_jan-mar_0-75m",
    "RES_jan-mar_0-100m",
    "RES_jan-mar_0-200m",
    ###################
    "RES_jul-sep_0m",
    "RES_jul-sep_50m",
    "RES_jul-sep_75m",
    "RES_jul-sep_100m",
    "RES_jul-sep_0-75m",
    "RES_jul-sep_0-100m",
    "RES_jul-sep_0-200m"
)

i = c(6, 13, 11, 13, 15, 11, 14,
      14, 15, 6, 7, 15, 15, 8,
      10, 11, 14, 9, 14, 14, 9)

j = c(12, 8, 9, 13, 7, 10, 10,
      11, 9, 14, 9, 14, 9, 8,
      6, 9, 5, 15, 12, 11, 9)

for (k in 1:length(outputExpectedOptions)) {

    outputExpected = outputExpectedOptions[k] #CHANGE the string

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
        if (min(x) < max(x)) {
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
                    collapse = "+")
    
    
    # Separation of dataset, train of 90% and test of 10%
    index = sample(seq_len(nrow(dataset)), size = 0.90 * nrow(dataset))
    train <- dataset[index, ]
    
    test <- dataset[-index, ]
    
    
    desnormTest <- data.frame(desnormalize(test, df))

    # Neural Network equation
    nn = neuralnet(
        str_c(outputExpected, " ~ ", formula),
        data = train,
        algorithm = "rprop+",
        startweights = NULL,
        hidden = c(i[k], j[k]),
        stepmax = 1e+06,
        lifesign = "none",
        threshold = 0.01,
    )

    # NN Result
    predict = neuralnet::compute(nn, test)
    
    
    # Denormalizing values from result
    desnormResult <-
        data.frame(desnormalize(predict$net.result, df))
    
    finalResult <- cbind(desnormTest, desnormResult)
    
    predicted = desnormResult
    actual = desnormTest[outputExpected]
    comparison = data.frame(predicted, actual)
    deviation = ((actual - predicted) / actual)
    comparison = data.frame(predicted, actual, deviation)
    
    # Calculating precision
    accurate = 1 - abs(data.matrix(deviation))
    pred <- ifelse(accurate > 0.9, 1, 0)
    truePositives <- length(pred[pred == 1])
    sprintf("/n/nSão: %i de %i ", truePositives, length(pred))
    falsePositives <- length(pred) - truePositives
    precision = truePositives / (truePositives + falsePositives)
    
    #This if is to avoid the problem with division with 0 resulting in Inf
    deviation <- deviation[!abs(deviation) == Inf]
    accuracy = 1 - abs(mean(data.matrix(deviation)))
    mse = mse(actual[, 1], predicted[, 1])
    mae = mae(actual[, 1], predicted[, 1])
    rmse = rmse(actual[, 1], predicted[, 1])
    rae = rae(actual[, 1], predicted[, 1])
    sprintf("%1.2f%%", accuracy * 100)

    formula = str_c(dfNames[!dfNames %in% outputExpected],
                    collapse = "+")
    foramTree <-
        rpart(str_c(outputExpected, " ~ ", formula),
              data = df,
              method = 'anova')
    
    # CHANGE variable name

    NN = list(
        `accuracy` = round(accuracy * 100, 2),
        `precision` = round(precision * 100, 2),
        `table` = finalResult,
        `cleanDf` = df,
        `nn` = nn,
        `test` = test,
        `mse` = round(mse, 2),
        `mae` = round(mae, 2),
        `rmse` = round(rmse, 2),
        `rae` = round(rae, 2),
        `foramTree` = foramTree
    )

    print(str_interp("${outputExpected} ------ (${i[k]}, ${j[k]}) MSE: ${round(mse(actual[,1], predicted[,1]), 2)}, MAE: ${round(mae(actual[,1], predicted[,1]), 2)},RMSE: ${round(rmse(actual[,1], predicted[,1]), 2)}, RAE: ${round(rae(actual[,1], predicted[,1]), 2)},accuracy: ${round(accuracy*100, 2)}, precision: ${round(precision*100, 2)}"))
    
    if (outputExpected == "RES_Annual_0m") {
        NN_RES_Annual_0m = NN
    } else if (outputExpected == "RES_Annual_50m") {
        NN_RES_Annual_50m = NN
    } else if (outputExpected == "RES_Annual_75m") {
        NN_RES_Annual_75m = NN
    }  else if (outputExpected == "RES_Annual_100m") {
        NN_RES_Annual_100m = NN
    }  else if (outputExpected == "RES_Annual_0.75m") {
        NN_RES_Annual_0_75m = NN
    }  else if (outputExpected == "RES_Annual_0.100m") {
        NN_RES_Annual_0_100m = NN
    }  else if (outputExpected == "RES_Annual_0.200m") {
        NN_RES_Annual_0_200m = NN
    } else if (outputExpected == "RES_jan.mar_0m") {
        NN_RES_JanMar_0m = NN
    } else if (outputExpected == "RES_jan.mar_50m") {
        NN_RES_JanMar_50m = NN
    } else if (outputExpected == "RES_jan.mar_75m") {
        NN_RES_JanMar_75m = NN
    }  else if (outputExpected == "RES_jan.mar_100m") {
        NN_RES_JanMar_100m = NN
    }  else if (outputExpected == "RES_jan.mar_0.75m") {
        NN_RES_JanMar_0_75m = NN
    }  else if (outputExpected == "RES_jan.mar_0.100m") {
        NN_RES_JanMar_0_100m = NN
    }  else if (outputExpected == "RES_jan.mar_0.200m") {
        NN_RES_JanMar_0_200m = NN
    } else if (outputExpected == "RES_jul.sep_0m") {
        NN_RES_JulSep_0m = NN
    } else if (outputExpected == "RES_jul.sep_50m") {
        NN_RES_JulSep_50m = NN
    } else if (outputExpected == "RES_jul.sep_75m") {
        NN_RES_JulSep_75m = NN
    }  else if (outputExpected == "RES_jul.sep_100m") {
        NN_RES_JulSep_100m = NN
    }  else if (outputExpected == "RES_jul.sep_0.75m") {
        NN_RES_JulSep_0_75m = NN
    }  else if (outputExpected == "RES_jul.sep_0.100m") {
        NN_RES_JulSep_0_100m = NN
    }  else if (outputExpected == "RES_jul.sep_0.200m") {
        NN_RES_JulSep_0_200m = NN
    }
}
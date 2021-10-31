library(devtools)
require(neuralnet)
library(stringr)
library(caret)
library(e1071)
library(dplyr)

useNeuralNetwork <- function(outputExpected) {
    if(outputExpected == "RES_Annual_0m")
        response <<- NN_RES_Annual_0m
        
    return(response)
}
library(devtools)
require(neuralnet)
library(stringr)
library(caret)
library(e1071)
library(dplyr)

load("./data.RData")

useNeuralNetwork <- function(outputExpected) {
  if (outputExpected == "RES_Annual_0m") {
    response <<- NN_RES_Annual_0m
  } else if (outputExpected == "RES_Annual_50m") {
    response <<- NN_RES_Annual_50m
  } else if (outputExpected == "RES_Annual_75m") {
    response <<- NN_RES_Annual_75m
  }  else if (outputExpected == "RES_Annual_100m") {
    response <<- NN_RES_Annual_100m
  }  else if (outputExpected == "RES_Annual_0-75m") {
    response <<- NN_RES_Annual_0_75m
  }  else if (outputExpected == "RES_Annual_0-100m") {
    response <<- NN_RES_Annual_0_100m
  }  else if (outputExpected == "RES_Annual_0-200m") {
    response <<- NN_RES_Annual_0_200m
  } else if (outputExpected == "RES_jan-mar_0m") {
    response <<- NN_RES_JanMar_0m
  } else if (outputExpected == "RES_jan-mar_50m") {
    response <<- NN_RES_JanMar_50m
  } else if (outputExpected == "RES_jan-mar_75m") {
    response <<- NN_RES_JanMar_75m
  }  else if (outputExpected == "RES_jan-mar_100m") {
    response <<- NN_RES_JanMar_100m
  }  else if (outputExpected == "RES_jan-mar_0-75m") {
    response <<- NN_RES_JanMar_0_75m
  }  else if (outputExpected == "RES_jan-mar_0-100m") {
    response <<- NN_RES_JanMar_0_100m
  }  else if (outputExpected == "RES_jan-mar_0-200m") {
    response <<- NN_RES_JanMar_0_200m
  } else if (outputExpected == "RES_jul-sep_0m") {
    response <<- NN_RES_JulSep_0m
  } else if (outputExpected == "RES_jul-sep_50m") {
    response <<- NN_RES_JulSep_50m
  } else if (outputExpected == "RES_jul-sep_75m") {
    response <<- NN_RES_JulSep_75m
  }  else if (outputExpected == "RES_jul-sep_100m") {
    response <<- NN_RES_JulSep_100m
  }  else if (outputExpected == "RES_jul-sep_0-75m") {
    response <<- NN_RES_JulSep_0_75m
  }  else if (outputExpected == "RES_jul-sep_0-100m") {
    response <<- NN_RES_JulSep_0_100m
  }  else if (outputExpected == "RES_jul-sep_0-200m") {
    response <<- NN_RES_JulSep_0_200m
  }
  
  return(response)
}
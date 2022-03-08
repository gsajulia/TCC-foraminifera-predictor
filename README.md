This project was made to help researchers in the prediction of foraminifera temperature

See more at: https://vfhnkg-julia0gabriela-santi0acosta.shinyapps.io/foraminifera-predictor/

The use of each folder

1- Neural Network Models | To make changes in database you have to modify data.RData

    a. Comment the load("./data.RData") in both files

    b. Run Neural Network Model in console with your modifications

    c.Run the project with "Default Models" selected

    d.Verify the result in the tabs

    e. If everything is ok as you spected you can save with the modifications in data.Rdata

2- Testing NN
    a. Allows test the NN models in console (separated from shiny)

3- R
    a. This files represent the functions that are called in the server.R

4- www
    a. Allow create a styles.css to change the styles in ui

5- Training different nn parameters

    a. v1 is the algorithm that test the NN with different configurations from 5 to 15 hidden neurons and save in a spreedshet
    b. The spredsheets resulting from this training is also saved in this folder
#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

# Define UI for application that draws a histogram
## Horsepower Prediction: ui.R Part 1

#       Sepal.Length Sepal.Width Petal.Length Petal.Width
#min          4.3         2.0          1.0         0.1
#max          7.9         4.4          6.9         2.5

library(shiny)
shinyUI(fluidPage(
  titlePanel("Predict Species or Iris"),
  sidebarLayout(
    sidebarPanel(
      h3("1: Select the value for the 4 inputs, to generate the probability for each species. Finally the winning Species wil be returned as the  Final Prediction"),
      sliderInput("sliderSepalL", "What is the lenght of the Sepal?", 3, 8, value = 6, step = 0.1),
      sliderInput("sliderSepalW", "What is the Width of the Sepal?", 2, 5, value = 3, step = 0.1),
      sliderInput("sliderPetalL", "What is the lenght of the Petal", 1, 7, value = 3, step = 0.1),
      sliderInput("sliderPetalW", "What is the Width of the Petal?", 0.1, 3, value = 1, step = 0.1)
    ),
    mainPanel(
      h3("2: Each of the plots show the 3 models against one of the 4 predictors"),
      plotOutput("plot1"),
      
      h3("Probability of belonging to the setosa Iris Species:"),
      textOutput("pred_setosa"),
      h3("Probability of belonging to the versicolor Iris Species:"),
      textOutput("pred_versicolor"),
      h3("Probability of belonging to the virginica Iris Species:"),
      textOutput("pred_virginica"),
      h3("Final prediction:"),
      textOutput("predfinal")
    )
  )
))
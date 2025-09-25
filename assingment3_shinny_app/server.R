#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#
library(shiny)
library(plotmo)

shinyServer(function(input, output) {

  # 1. Modelo para Setosa vs No-Setosa
  iris$Setosa <- ifelse(iris$Species == "setosa", 1, 0)
  modelo_setosa <- glm(Setosa ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width,
                       data = iris, family = binomial)
  
  # 2. Modelo para Versicolor vs No-Versicolor
  iris$Versicolor <- ifelse(iris$Species == "versicolor", 1, 0)
  modelo_versicolor <- glm(Versicolor ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width,
                           data = iris, family = binomial)

  # 3. Modelo para Virginica vs No-Virginica
  iris$Virginica <- ifelse(iris$Species == "virginica", 1, 0)
  modelo_virginica <- glm(Virginica ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width,
                          data = iris, family = binomial)

  model1pred <- reactive({
    sliderSepalLInput <- input$sliderSepalL
    sliderSepalWInput <- input$sliderSepalW
    sliderPetalLInput <- input$sliderPetalL
    sliderPetalWInput <- input$sliderPetalW
    
    nueva_obs <- data.frame(
      Sepal.Length = sliderSepalLInput,
      Sepal.Width  = sliderSepalWInput,
      Petal.Length = sliderPetalLInput,
      Petal.Width  = sliderPetalWInput
    )
    
    predict(modelo_setosa, newdata = nueva_obs,type = "response")
  })
  
  model2pred <- reactive({
    sliderSepalLInput <- input$sliderSepalL
    sliderSepalWInput <- input$sliderSepalW
    sliderPetalLInput <- input$sliderPetalL
    sliderPetalWInput <- input$sliderPetalW
    
    nueva_obs <- data.frame(
      Sepal.Length = sliderSepalLInput,
      Sepal.Width  = sliderSepalWInput,
      Petal.Length = sliderPetalLInput,
      Petal.Width  = sliderPetalWInput
    )
    
    predict(modelo_versicolor, newdata = nueva_obs,type = "response")
  })
  
  model3pred <- reactive({
    sliderSepalLInput <- input$sliderSepalL
    sliderSepalWInput <- input$sliderSepalW
    sliderPetalLInput <- input$sliderPetalL
    sliderPetalWInput <- input$sliderPetalW
    
    nueva_obs <- data.frame(
      Sepal.Length = sliderSepalLInput,
      Sepal.Width  = sliderSepalWInput,
      Petal.Length = sliderPetalLInput,
      Petal.Width  = sliderPetalWInput
    )
    
    predict(modelo_virginica, newdata = nueva_obs,type = "response")
  })
  
  output$plot1 <- renderPlot({
    
    sliderSepalLInput <- input$sliderSepalL
    sliderSepalWInput <- input$sliderSepalW
    sliderPetalLInput <- input$sliderPetalL
    sliderPetalWInput <- input$sliderPetalW
    
    par(mfrow = c(2,2))
    
    #===========================
    #PLOT1
    #===========================
    plot(iris$Sepal.Length, iris$Setosa, pch = 16, col = "gray",
         xlab = "Sepal.Length", ylab = "Probability", main = "Logistic Curve for Sepal.Length",
         ylim = c(0,1))
    
    # Secuencia de valores para la variable
    x_vals <- seq(3, 8, length.out = 200)
    newdata <- data.frame(
      Sepal.Length = x_vals,
      Sepal.Width  = mean(iris$Sepal.Width),
      Petal.Length = mean(iris$Petal.Length),
      Petal.Width  = mean(iris$Petal.Width)
    )
    
    # Probabilidades predichas
    p_setosa     <- predict(modelo_setosa, newdata = newdata, type = "response")
    p_versicolor <- predict(modelo_versicolor, newdata = newdata, type = "response")
    p_virginica  <- predict(modelo_virginica, newdata = newdata, type = "response")
    
    lines(x_vals, p_setosa,     col = "red",   lwd = 2)
    lines(x_vals, p_versicolor, col = "blue",  lwd = 2)
    lines(x_vals, p_virginica,  col = "green", lwd = 2)
    
    legend("topright", legend = c("Setosa", "Versicolor", "Virginica"),
           col = c("red", "blue", "green"), lwd = 2, bty = "n")
    
    points(sliderSepalLInput, model1pred(), col = "red", pch = 16, cex = 2)
    points(sliderSepalLInput, model2pred(), col = "blue", pch = 16, cex = 2)
    points(sliderSepalLInput, model3pred(), col = "green", pch = 16, cex = 2)
    
    #===========================
    #PLOT 2
    #===========================
    plot(iris$Sepal.Width, iris$Setosa, pch = 16, col = "gray",
         xlab = "Sepal.Width", ylab = "Probability", main = "Logistic Curve for Sepal.Width",
         ylim = c(0,1))
    
    # Secuencia de valores para la variable
    x_vals <- seq(2, 5, length.out = 200)
    newdata <- data.frame(
      Sepal.Length = mean(iris$Sepal.Length),
      Sepal.Width  = x_vals,
      Petal.Length = mean(iris$Petal.Length),
      Petal.Width  = mean(iris$Petal.Width)
    )
    
    # Probabilidades predichas
    p_setosa     <- predict(modelo_setosa, newdata = newdata, type = "response")
    p_versicolor <- predict(modelo_versicolor, newdata = newdata, type = "response")
    p_virginica  <- predict(modelo_virginica, newdata = newdata, type = "response")
    
    lines(x_vals, p_setosa,     col = "red",   lwd = 2)
    lines(x_vals, p_versicolor, col = "blue",  lwd = 2)
    lines(x_vals, p_virginica,  col = "green", lwd = 2)
    
    legend("topright", legend = c("Setosa", "Versicolor", "Virginica"),
           col = c("red", "blue", "green"), lwd = 2, bty = "n")
    
    points(sliderSepalWInput, model1pred(), col = "red", pch = 16, cex = 2)
    points(sliderSepalWInput, model2pred(), col = "blue", pch = 16, cex = 2)
    points(sliderSepalWInput, model3pred(), col = "green", pch = 16, cex = 2)
    
    #===========================
    #PLOT 3
    #===========================
    plot(iris$Petal.Length, iris$Setosa, pch = 16, col = "gray",
         xlab = "Petal.Length", ylab = "Probability", main = "Logistic Curve for Petal.Length",
         ylim = c(0,1))
    
    # Secuencia de valores para la variable
    x_vals <- seq(1, 7, length.out = 200)
    newdata <- data.frame(
      Sepal.Length = mean(iris$Sepal.Length),
      Sepal.Width  = mean(iris$Sepal.Width),
      Petal.Length = x_vals,
      Petal.Width  = mean(iris$Petal.Width)
    )
    
    # Probabilidades predichas
    p_setosa     <- predict(modelo_setosa, newdata = newdata, type = "response")
    p_versicolor <- predict(modelo_versicolor, newdata = newdata, type = "response")
    p_virginica  <- predict(modelo_virginica, newdata = newdata, type = "response")
    
    lines(x_vals, p_setosa,     col = "red",   lwd = 2)
    lines(x_vals, p_versicolor, col = "blue",  lwd = 2)
    lines(x_vals, p_virginica,  col = "green", lwd = 2)
    
    legend("topright", legend = c("Setosa", "Versicolor", "Virginica"),
           col = c("red", "blue", "green"), lwd = 2, bty = "n")
    
    points(sliderPetalLInput, model1pred(), col = "red", pch = 16, cex = 2)
    points(sliderPetalLInput, model2pred(), col = "blue", pch = 16, cex = 2)
    points(sliderPetalLInput, model3pred(), col = "green", pch = 16, cex = 2)
    
    
    #===========================
    #PLOT 4
    #===========================
    plot(iris$Petal.Width, iris$Setosa, pch = 16, col = "gray",
         xlab = "Petal.Width", ylab = "Probability", main = "Logistic Curve for Petal.Width",
         ylim = c(0,1))
    
    # Secuencia de valores para la variable
    x_vals <- seq(0.1, 3, length.out = 200)
    newdata <- data.frame(
      Sepal.Length = mean(iris$Sepal.Length),
      Sepal.Width  = mean(iris$Sepal.Width),
      Petal.Length = mean(iris$Petal.Length),
      Petal.Width  = x_vals
    )
    
    # Probabilidades predichas
    p_setosa     <- predict(modelo_setosa, newdata = newdata, type = "response")
    p_versicolor <- predict(modelo_versicolor, newdata = newdata, type = "response")
    p_virginica  <- predict(modelo_virginica, newdata = newdata, type = "response")
    
    lines(x_vals, p_setosa,     col = "red",   lwd = 2)
    lines(x_vals, p_versicolor, col = "blue",  lwd = 2)
    lines(x_vals, p_virginica,  col = "green", lwd = 2)
    
    legend("topright", legend = c("Setosa", "Versicolor", "Virginica"),
           col = c("red", "blue", "green"), lwd = 2, bty = "n")
    
    points(sliderPetalWInput, model1pred(), col = "red", pch = 16, cex = 2)
    points(sliderPetalWInput, model2pred(), col = "blue", pch = 16, cex = 2)
    points(sliderPetalWInput, model3pred(), col = "green", pch = 16, cex = 2)
    
  })
  
  # Vector con probabilidades
  especie_predicha <- reactive({
    probs <- c(setosa = model1pred(),
               versicolor = model2pred(),
               virginica = model3pred())
    names(which.max(probs))
  })
  
  output$pred_setosa <- renderText({
    model1pred()
  })
  
  output$pred_versicolor <- renderText({
    model2pred()
  })
  output$pred_virginica <- renderText({
    model3pred()
  })
  
  output$predfinal <- renderText({
    especie_predicha()
  })
  
})
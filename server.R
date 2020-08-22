library(shiny)
library(corrplot)
#libraries to load along with server code

Server <- (function(input, output) {

    # Reactive element to create initial dataframe using example dataset and selected inputs
    thedata <- reactive({
     rawdata <- mtcars
     thedata <- data.frame(rawdata[,c("mpg",input$Predictors)])

    })   

    # Reactive element to run the JRW model
    model <- reactive({
    multRegress(thedata())
    })

    # Reactive element to create correlation matrix to be used for corrplot
    thedata.cor <- reactive({
    corr <- round(cor(thedata()),3)
    corr <- corrplot(corr, method="circle")
    })

    # Reactive element to create numeric correlation matrix for file download
    raw.cor <- reactive({
    round(cor(thedata()),3)
    })

   # Print data summary
   output$summary <- renderPrint({
   summary(thedata())
   })

   # Download handler for correlation csv
   output$corr_csv <- downloadHandler(
   filename = function() {
   paste(input$Dependent, "_Corr.csv", sep="")
    },
   content = function(correl) {
      a <- raw.cor()
      write.csv(a, correl)
    })

  # Generate corrplot
  output$corr <- renderPlot({
  thedata.cor()
  })

  # Update and print model summary
  output$model <- renderPrint({
    model()
    result
  })

  # Update and print model R2
  output$rsquare <- renderText({
    model()
    paste("R2=",round(rsquare,3))
  })

  # Download handler for model report csv
   output$report <- downloadHandler(
   filename = function() {
   paste(input$Dependent, "_Report.csv", sep="")
    },
   content = function(MReport) {
      model()
      b <- result
      c <- rsquare
      write.table(b, MReport, col.names=TRUE, sep=",") 
      write.table(c, MReport, col.names=FALSE, sep=",", append=TRUE)     

    })

    
})
shinyApp(ui = UI, server = Server)
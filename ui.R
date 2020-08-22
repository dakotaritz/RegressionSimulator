library(shiny)
library(shinythemes)

    #Pull names of predictor columns for easy reference in groupinput and server code
    predictors <- colnames(mtcars[,2:11])

    #Create function to run JRW model - includes all calculation steps
    multRegress<-function(mydata){
    numVar<<-NCOL(mydata)
    Variables<<- names(mydata)[2:numVar]

    mydata<-cor(mydata, use="pairwise.complete.obs")
    RXX<-mydata[2:numVar,2:numVar]
    RXY<-mydata[2:numVar,1]

    RXX.eigen<-eigen(RXX)
    D<-diag(RXX.eigen$val)
    delta<-sqrt(D)

    lambda<-RXX.eigen$vec%*%delta%*%t(RXX.eigen$vec)
    lambdasq<-lambda^2
    beta<-solve(lambda)%*%RXY
    rsquare<<-sum(beta^2)

    RawWgt<-lambdasq%*%beta^2
    import<-(RawWgt/rsquare)*100

    result<<-data.frame(Variables, Raw.RelWeight=RawWgt, Rescaled.RelWeight=import)
    } 

UI <- (fluidPage(
  theme = shinytheme('darkly'),
  # Title of the app
  titlePanel("Multiple Regression via Relative Weights Analysis (mtcars dataset)"),

  sidebarLayout(
    sidebarPanel(

      # Selection input for Dependent
      selectInput("Dependent", "Dependent Variable:", choices = c("Miles per Gallon (mpg)" = "mpg"), selected = "mpg"),

      # Group input for predictor variables
      checkboxGroupInput("Predictors", "Predictor Variables Included:", choices = c("cyl: Number of Cylinders" = "cyl", "disp: Displacement (cu.in.)" = "disp",
                         "hp: Gross Horsepower" = "hp", "drat: Rear Axle Ratio" = "drat", "wt: Weight (1,000 lbs)" = "wt", "qsec: 1/4 Mile Time" = "qsec", "vs: V/S Engine" = "vs", 
                         "am: Transmission Type" = "am", "gear: Number of Gears" = "gear", "carb: Number of Carburetors" = "carb"), selected = predictors),
      br(),
      p("Relative Weights Analysis is a statistical method used to estimate the relative importance of predictor variables in multiple regression. This method transforms the predictor variables into a set of orthogonal (uncorrelated) variables, which allows the model to produce clear results even if high collinearity were to exist between the original predictors. When the dependent variable is regressed on this new set of transformed variables, the raw coefficients from the regression represent the individual contribution of each predictor to the overall R2 value. In the final output, these coefficients are rescaled so that the relative importance for all variables sums to 100%."),
      p("(Un)check the items above to change the variables included in the model. The outputs will update accordingly."),
      p("The correlation matrix and the model results for the current selections can be downloaded as a .csv file.")),


    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Data Summary",
                           div(style="width:600px;",(verbatimTextOutput("summary")))),
                  tabPanel("Correlations",
                           downloadButton("corr_csv", "Download Raw Matrix"),
                           plotOutput("corr", height = 600, width = 800)),
                  tabPanel("Model",
                           downloadButton("report", "Download Model Report"),                  
                           div(style="width:500px;",(verbatimTextOutput("model"))),
                           div(style="width:500px;",(verbatimTextOutput("rsquare")))))
  ))
))
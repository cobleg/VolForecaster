#
# Objective: generate GARCH forecasts based on parameter inputs

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  tags$head(tags$style(
    HTML("hr {border-top: 1px solid #000000;}")
  )),
  
  # Application title
  titlePanel("Volatility Forecaster"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      selectInput("ticker",
                  "Select stock code:",
                  c('S&P 500' = "SPX")),
      
      hr(),
      numericInput(
        "ar",
        "Select maximum number of Auto-Regressive parameters: ",
        2,
        min = 2,
        max = 5
      ),
      
      numericInput(
        "ma",
        "Select maximum number of Moving Average parameters: ",
        2,
        min = 2,
        max = 5
      ),
      
      selectInput(
        "criterion",
        "Select model selection statistic:",
        c(
          'AIC' = 'AIC',
          'BIC' = 'BIC',
          'HQIC' = 'HQIC',
          'SIC' = 'SIC'
        )
      ),
      hr(),
      
      
      selectInput(
        "varianceModelType",
        "Select type of variance model:",
        c(
          'eGARCH' = 'eGARCH',
          'apGARCH' = 'apGARCH',
          'csGARCH' = 'csGARCH',
          'fGARCH' = 'fGARCH',
          
          'gjrGARCH' = 'gjrGARCH',
          'iGARCH' = 'iGARCH',
          'mcsGARCH' = 'mcsGARCH',
          'realGARCH' = 'realGARCH',
          'sGARCH' = 'sGARCH'
        ),
        selected = 'eGARCH'
      ),
      hr(),
      
      sliderInput(
        "forecastLength",
        "Select forecast length:",
        5,
        min = 2,
        max = 20,
        step = 1
      )
    ),
    
    # Show a plot of the generated distribution
    mainPanel(tabsetPanel(
      tabPanel("Data", tableOutput('tickerDataTable')),
      tabPanel("ARFIMA model spec", tableOutput('meanModelTable')),
      tabPanel("Diagnostics", plotOutput("diagnosticPlots")),
      tabPanel("Forecast", plotOutput("forecastPlot"))
      
    ))
  )
))

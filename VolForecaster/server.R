#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
source('getData.R')

library(kableExtra)
library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  # get data
  tickerReturnsData <- reactive({
    
    dat1 <- diff(log(getData(input$ticker)), 1)
    
    return(dat1)
    
  })
  
  meanModelData <- reactive({
    # find the best fitting ARFIMA model for returns
    ARFIMA.fit = autoarfima(
      data =  tickerReturnsData(),
      ar.max = input$ar,
      ma.max = input$ma,
      criterion = input$criterion,
      method = "full"
    )
    data.frame(head(ARFIMA.fit$rank.matrix))
  })
  
  output$tickerDataTable <- reactive({
    df <- data.frame(tickerReturnsData())
    df1 <- tail(df, 20)
    names(df1) <- paste0(input$ticker, " (returns)")
    df1 %>%
      knitr::kable("html") %>%
      kable_styling(bootstrap_options = "striped",
                    full_width = F)
  })
  
  
  
  output$meanModelTable <- reactive({
    df2 <- meanModelData()
    df2 %>%
      knitr::kable("html") %>%
      kable_styling(bootstrap_options = "striped",
                    full_width = F)
  })
  
})

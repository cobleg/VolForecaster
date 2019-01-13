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
  
  # create list object to assign best fitting mean model to GARCH model
  meanModel <- reactive({
    headings <- colnames(meanModelData())
    
    # get the specification of the best fitting model
    arLength <- length(headings[grepl("^ar", headings)]) - 1
    maLength <- length(headings[grepl("^ma", headings)])
    firstRow <- meanModelData()[1,]
    
    myList <- headings[1:(arLength + maLength)]
    myRow <- firstRow[1:(arLength + maLength)]
    
    # if the sum of the AR indicators is zero, then remove the AR part of the vector
    myRow2 <- myRow
    if (sum(myRow[1:arLength]) == 0) {
      myRow2 <- myRow[-c(1:arLength)]
    }
    
    # if the sum of the MA indicators is zero, then remove the MA part of the vector
    if (sum(myRow2[grepl('^ma', names(myRow2))]) == 0) {
      myRow2 <- myRow2[-which(c(grepl('^ma', names(myRow2))))]
    }
    return(myRow2)
  })
  
  # convert meanModel() into a list object
  meanModelList <- reactive({
    ## now construct a list object consisting of fixed AR and/or MA components
    myList <- as.list(unname(meanModel()))
    names(myList) <- names(meanModel())
    # remove elements not equal to 0
    myList <- myList[myList == 0]
    return(myList)
  })
  
  # get number of AR terms in mean model
  arNumber <- reactive({
    sum(grepl('^ar', names(meanModel())))
  })
  
  # get number of MA terms in mean model
  maNumber <- reactive({
    sum(grepl('^ma', names(meanModel())))
  })
  
  # create GARCH model specification
  spec <- reactive({
    ugarchspec(
      mean.model = list(armaOrder = c(arNumber(), maNumber())),
      variance.model = list(model = input$varianceModelType),
      distribution = "jsu"
    )
  })
  
  # indicate which AR/MA terms to keep fixed in the GARCH model
  myList <- reactive({
    # if myList is not empty then define fixed compoents in the GARCH model specification
    if (length(meanModelList()) != 0) {
      setfixed(spec()) <- meanModelList()
    }
    return(myList)
  })
  
  # fit the GARCH model
  fittedModel <- reactive({
    fit <- ugarchfit(spec(), tickerReturnsData())
  })
  
  # create forecasts
  bootp <- reactive({
    ugarchboot(
      fit,
      method = c("Partial", "Full")[1],
      n.ahead = input$forecastLength,
      n.bootpred = input$forecastLength
    )
    
  })
  
  # create tables
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
  
  output$meanModeListTable <- reactive({
    fittedModel()@model$modelinc %>%
      knitr::kable("html") %>%
      kable_styling(bootstrap_options = "striped",
                    full_width = F)
  })
  
  # create plots
  output$diagnosticPlots <- renderPlot({
    par(mar = c(1, 1, 1, 1))
    plot(fittedModel(), which = 'all') # diagnostics
    
  })
  
  output$forecastPlot <- renderPlot({
    dateSeries <- seq(as.Date(colnames(bootp()@forc@forecast$seriesFor) ), length.out = input$forecastLength, by = 1)
    forecast <- xts( bootp()@forc@forecast$seriesFor * 100, order.by = dateSeries)
    plot(forecast, xlab = 'Date' , ylab = '%', main = paste0('Point forecast of ', input$ticker))
    
  })
})

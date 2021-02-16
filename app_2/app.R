library(PerformanceAnalytics)
library(quantmod)
library(xts)
library(ggplot2)
library(shiny)
library(shinyWidgets)
library(shinythemes)
library(plotly)
library(tidyverse)
library(tidyquant)

ui <- fluidPage(theme = shinytheme("yeti"),
                titlePanel("Shiny Portfolio Analysis"),
                tabsetPanel(
                  type = "tabs",
                  tabPanel("Explore", sidebarLayout(
                    sidebarPanel(
                      helpText("select a stock"),
                      selectInput(inputId = "stock", label = "Stock",choices = c(
                        "MSFT","AAPL","AMZN","FB","GOOG","JPM","GOOGL","BILI","NIO",
                        "V","NFLX","T","HD","VZ","MA","BAC","DIS","CVX","INTC","MRK",
                        "KO","CSCO","BA","UNH"
                      ), multiple = F, selected = c("AMZN"),
                      selectize = TRUE),
                      dateRangeInput("dates",
                                     "Date range",
                                     start = "2013-01-01",
                                     end = as.character(Sys.Date())),
                      actionButton(inputId = "go",
                                   label = "Update", icon("refresh"))),
                    mainPanel(plotOutput("plot", height = 800))
                  )),
                  tabPanel("skewness and kurtosis", sidebarLayout(
                    sidebarPanel(
                      helpText("Detecting non-normality using skewness and kurtosis"),
                      selectInput(inputId = "skewstock", label = "Stock Analysis",choices = c(
                        "MSFT","AAPL","AMZN","FB","GOOG","JPM","GOOGL","BILI","NIO",
                        "V","NFLX","T","HD","VZ","MA","BAC","DIS","CVX","INTC","MRK",
                        "KO","CSCO","BA","UNH"
                      ), multiple = F, selected = c("AAPL"),
                      selectize = TRUE),
                      dateRangeInput("dates2",
                                     "Date range",
                                     start = "2016-01-01",
                                     end = as.character(Sys.Date())),
                      actionButton(inputId = "analysis",
                                   label = "Analysis", icon("list-alt"))),
                    mainPanel(
                      tableOutput("table"),
                      br(),
                      plotlyOutput("anaplot2"))
                  )))
)

server <- function(input, output){
  
  dataInput <- eventReactive(input$go, {
    getSymbols(input$stock, src = "yahoo",
               from = input$dates[1],
               to = input$dates[2],
               auto.assign = FALSE)
  })
  
  output$plot <- renderPlot({
    chartSeries(dataInput(), theme = chartTheme("black"),
                name = input$stock)
  })
  
  skewData <- eventReactive(input$analysis, {
    stock_close <- lapply(input$skewstock, function(symb)
      get.hist.quote(instrument= symb,
                     start = input$dates[1], 
                     quote="Close", provider = "yahoo",
                     retclass="zoo"))
    stock_close
  })
  output$table <- renderTable({
    sp500_monthly <- data.frame(skewData())
    sp500_monthly <- to.monthly(sp500_monthly, indexAt = "startof")
    sp500_monthly <- Return.calculate(sp500_monthly)
    sp500_monthly <- sp500_monthly[,-1]
    sp500_monthly <- sp500_monthly[,-1]
    sp500_monthly <- sp500_monthly[-1, ]
    colnames(sp500_monthly) <- "sp500"
    skew_table <- table(skewness(sp500_monthly),kurtosis(sp500_monthly))
    #rownames(skew_table) <- c("skewness","kurtosis")
    print(skew_table)
  })
  output$anaplot2 <-renderPlotly({
    #sp500 <- as.data.frame(skewData())
    #sp500$Date <- rownames(sp500)
    #sp500$Date <- as.Date(sp500$Date, format("%m/%d/%Y"))
    #sp500 <- sp500[order(sp500$Date), ]
    #sp500 <- as.xts(sp500[, 2], order.by = sp500$Date)
    #sp500_monthly <- to.monthly(sp500)
    #sp500_returns <- Return.calculate(sp500_monthly$sp500.Close)
    #plot.zoo(sp500_returns)
    close_data <- as.data.frame(skewData())
    close_data$Dates <- rownames(close_data)
    rownames(close_data) <- NULL
    p <- close_data %>%
      plot_ly(
        color = ~Close,
        x = ~Dates,
        y = ~Close, yaxis = list(type = "log"),
        mode = 'markers+lines'
      ) %>%
      layout(showlegend = F, title = "Stock Close Values")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
                
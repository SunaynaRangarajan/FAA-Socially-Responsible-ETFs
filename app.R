library(shiny)
library(shinythemes)
library(data.table)

source("FAA.R")

# Define UI for app 

ui <- fluidPage(
  theme = shinytheme("flatly"),

  # App title ----
  titlePanel("Flexible Asset Allocation for Socially Responsible ETF portfolio", windowTitle = "FAA ESG ETFs"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Numeric input for number of months lookback ----
      numericInput(inputId = "monthsLookback",
                  label = "Months Lookback:",
                  value = 0.67),
      
      # Input: Numeric input for number of ETFs selected ----
      numericInput(inputId = "bestN",
                   label = "Best N ETFs:",
                   value = 3),
      
      # Input: Numeric input for Momentum weight ----
      numericInput(inputId = "weightMom",
                   label = "Momentum weight",
                   value = 1),
      
      # Input: Numeric input for Volatility weight ----
      numericInput(inputId = "weightVol",
                   label = "Volatility weight",
                   value = 0.5),
      
      # Input: Numeric input for Correlation weight ----
      numericInput(inputId = "weightCor",
                   label = "Correlation weight",
                   value = 0.5),
      
      # Input: Numeric input for ESG weight ----
      numericInput(inputId = "weightESG",
                   label = "ESG score weight",
                   value = 0.5),
      
      #Input: Text input for benchmark ---
      textInput(inputId = "benchmark", label = "Benchmark", value = "SPY"),
      
      submitButton("Apply changes")
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Performance summary
      plotOutput(outputId = "performancePlot"),
      
      tableOutput(outputId = "performanceStats"),
      
      textOutput(outputId = "VGSHinvestment")
    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  

  output$performancePlot <- renderPlot({
    
    charts.PerformanceSummary(cbind(FAA(adPrices, weightMom= input$weightMom, weightVol=input$weightVol, 
                                  weightCor=input$weightCor, weightesg=input$weightESG,
    bestN = input$bestN, monthsLookback = input$monthsLookback), benchmark(input$benchmark)), color=c("forestgreen","orange"),
    main="Performance Summary")
    

  })
  
  output$performanceStats <- renderTable({
    
    strategy = cbind(FAA(adPrices, weightMom= input$weightMom, weightVol=input$weightVol, weightCor=input$weightCor, weightesg=input$weightESG,
                                  bestN = input$bestN, monthsLookback = input$monthsLookback), benchmark(input$benchmark))
    colnames(strategy) = c("Chosen strategy","Benchmark")
    
    stats <-  data.frame(t(rbind(Return.annualized(strategy)*100,
                       StdDev.annualized(strategy)*100,
                       maxDrawdown(strategy),
                       SharpeRatio.annualized(strategy))))
    colnames(stats) = c("Annualized_return","Annualized_vol","Worst_drawdown","Annualized_sharpe_ratio")
    stats
    
    
  }, rownames = TRUE)
  
  output$VGSHinvestment = renderText({
    
    paste("On", Cash_inv(adPrices, weightMom= input$weightMom, weightVol=input$weightVol, weightCor=input$weightCor, weightesg=input$weightESG,
              bestN = input$bestN, monthsLookback = input$monthsLookback), "of the days, there was an investment in VGSH - Vanguard Short-Term Treasury ETF")
  })
  
  
  
}

shinyApp(ui = ui, server = server)

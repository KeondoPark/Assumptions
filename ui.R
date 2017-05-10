library(shiny)

shinyUI(fluidPage(
  titlePanel("Claim Assumption - Trend"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("The plot shows the trend of claim assumptions. Dots imply the actual data, red lines imply the fitted model by regression method selected from below."),
      
      selectInput("RegMethod", 
                  label = "Select a Regression Method",
                  choices = c("Linear regression", 
                              "Log linear regression"
                              ),
                  selected = "Linear regression"),
      
      sliderInput("UYear", label = h3("Ultimate Year"),
                  min = 0, max = 100, value = 10)
    ),
    
    mainPanel(plotOutput("plot"),
              textOutput("text1"),
              dataTableOutput("Table1")
              )
  )
))





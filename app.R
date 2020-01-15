library(tidyverse)
library(shinydashboard)
ui <- dashboardPage(
  dashboardHeader(title = "Customer Coversion Rate Dahsboard"),
  dashboardSidebar(),
  dashboardBody(
    fluidRow(
      valueBoxOutput("total"),
      valueBoxOutput("client"),
      valueBoxOutput("percentage")
    ),
    fluidRow(
      
    )
  )
)

server <- function(input, output) {
  
  conversionsDF <- read.csv(
    file="~/Documents/Dev_master/R_projects/marketing/conversion/data/bank-additional-full.csv",
    header=TRUE,
    sep=";"
  )
  
  # Encode conversions as 0s and 1s
  conversionsDF$conversion <- as.integer(conversionsDF$y) - 1
  
  # total number of conversions
  total <- sum(conversionsDF$conversion)
  
  output$total <- renderValueBox(
    valueBox(total, "Total conversion", icon = icon("pencil"), color = "red" )
  )
  
  # total number of clients in the data (= number of records in the data)
  client <- nrow(conversionsDF)
  
  output$client <- renderValueBox(
    valueBox(client, "Total clients", icon = icon("users") )
  )
  
  percentage <- round(total / client * 100, 2)
  
  output$percentage <- renderValueBox(
    valueBox(percentage, "Percentage of conversion", icon = icon("percent"), color = "yellow")
  )
}

shinyApp(ui, server)


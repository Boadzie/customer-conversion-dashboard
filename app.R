library(tidyverse)
library(shinydashboard)
library(DT)


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
      DTOutput("table")
    ),
    fluidRow(
      box(width=12, title = "The data is from UCI's Bank Marketing Dataset. 
                It can be found at the link below: ",
                tags$a("https://archive.ics.uci.edu/ml/datasets/bank+marketing"))
    ),
    fluidRow(
      box("age", title = "Conversion by Age", plotOutput("age")),
      box("agegroup", title = "Conversion by Age Group", plotOutput("agegroup")),
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
  
  # The table
  output$table <- renderDT(
    datatable(conversionsDF)
  )
  
  # The Visualizations
  # Conversion rates by age
  
  conversionsByAge <- conversionsDF %>%
    group_by(Age=age) %>%
    summarise(TotalCount=n(), NumConversions=sum(conversion)) %>%
    mutate(ConversionRate=NumConversions/TotalCount*100.0)
  
  # line chart
  output$age <- renderPlot(
    ggplot(data=conversionsByAge, aes(x=Age, y=ConversionRate)) +
      geom_line() +
      ggtitle('Conversion Rates by Age') +
      xlab("Age") +
      ylab("Conversion Rate (%)") +
      theme(plot.title = element_text(hjust = 0.5))
  )
  
  # Conversion by Age Group
  conversionsByAgeGroup <- conversionsDF %>%
    group_by(AgeGroup=cut(age, breaks=seq(20, 70, by = 10)) ) %>%
    summarise(TotalCount=n(), NumConversions=sum(conversion)) %>%
    mutate(ConversionRate=NumConversions/TotalCount*100.0)
  conversionsByAgeGroup$AgeGroup <- as.character(conversionsByAgeGroup$AgeGroup)
  conversionsByAgeGroup$AgeGroup[6] <- "70+"
  
  
  output$agegroup <- renderPlot(
    # bar chart
    ggplot(conversionsByAgeGroup, aes(x=AgeGroup, y=ConversionRate)) +
      geom_bar(width=0.5, stat="identity") +
      ggtitle('Conversion Rates by Age Groups') +
      xlab("Age") +
      ylab("Conversion Rate (%)") +
      theme(plot.title = element_text(hjust = 0.5))
    
  )
  
  
}

shinyApp(ui, server)


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
    ),
    
    fluidRow(
      box("non", title = "Conversion vs Non-conversion by Marital Status", plotOutput("non")),
      box("agemarital", title = "Conversions by Age and Marital status", plotOutput("agemarital"))
    ),
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
  
  # Conversion vs Non-conversion
  conversionsByMaritalStatus <- conversionsDF %>%
    group_by(Marital=marital, Conversion=conversion) %>%
    summarise(Count=n())
  
  output$non <- renderPlot(
    # pie chart
    ggplot(conversionsByMaritalStatus, aes(x="", y=Count, fill=Marital)) +
      geom_bar(width=1, stat = "identity", position=position_fill()) +
      geom_text(aes(x=1.25, label=Count), position=position_fill(vjust = 0.5)) +
      coord_polar("y") +
      facet_wrap(~Conversion) +
      ggtitle('Marital Status (0: Non Conversions, 1: Conversions)') +
      theme(
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        plot.title=element_text(hjust=0.5),
        legend.position='bottom'
      )
  )
  
  
  # Conversions by age and marital status
  
  #### 5. Conversions by Age Groups & Marital Status ####
  conversionsByAgeMarital <- conversionsDF %>%
    group_by(AgeGroup=cut(age, breaks= seq(20, 70, by = 10)), Marital=marital) %>%
    summarise(Count=n(), NumConversions=sum(conversion)) %>%
    mutate(TotalCount=sum(Count)) %>%
    mutate(ConversionRate=NumConversions/TotalCount)
  conversionsByAgeMarital$AgeGroup <- as.character(conversionsByAgeMarital$AgeGroup)
  conversionsByAgeMarital$AgeGroup[is.na(conversionsByAgeMarital$AgeGroup)] <- "70+"
  
  output$agemarital <- renderPlot(
    # bar chart
    ggplot(conversionsByAgeMarital, aes(x=AgeGroup, y=ConversionRate, fill=Marital)) +
      geom_bar(width=0.5, stat="identity", position="dodge") +
      ylab("Conversion Rate (%)") +
      xlab("Age") +
      ggtitle("Conversion Rates by Age and Marital Status") +
      theme(plot.title=element_text(hjust=0.5))
  )
  
}

shinyApp(ui, server)


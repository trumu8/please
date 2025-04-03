library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(dplyr)
library(tidyr)

getwd()
setwd("/Users/trumu/Desktop/TNC")

Data<- read.csv("~/Desktop/TNC/DACCountries.csv")




# Ensure Time.period is properly formatted
Data <- Data %>%
  mutate(
    TIME_PERIOD = as.numeric(TIME_PERIOD),
    OBS_VALUE = as.numeric(OBS_VALUE)
  )

# UI
ui <- dashboardPage(
  dashboardHeader(title = "OECD Aid Disbursements (2000-2023)"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("globe")),
      menuItem("Data Explorer", tabName = "explorer", icon = icon("table")),
      menuItem("Trend Analysis", tabName = "trends", icon = icon("chart-line"))
    ),
    
    # Filters
    selectInput("donor", "Select Donor(s):", 
                choices = sort(unique(Data$Donor)),
                multiple = TRUE,
                selected = unique(Data$Donor)[1]),
    
    selectInput("recipient", "Select Recipient(s):", 
                choices = sort(unique(Data$Recipient)),
                multiple = TRUE,
                selected = unique(Data$Recipient)[1]),
    
    sliderInput("years", "Select Year Range:",
                min = 2000,  # Fixed to your data range
                max = 2023,
                value = c(2000, 2023),
                step = 1,
                sep = ""),  # Remove comma in year display
    
    selectInput("measure", "Select Measure:",
                choices = sort(unique(Data$Measure)),
                selected = unique(Data$Measure)[1])
  ),
  
  dashboardBody(
    tabItems(
      # Overview Tab
      tabItem(tabName = "overview",
              fluidRow(
                valueBoxOutput("totalAid", width = 4),
                valueBoxOutput("avgAid", width = 4),
                valueBoxOutput("yearRange", width = 4)
              ),
              fluidRow(
                box(width = 12, plotlyOutput("annualTrend")),
                box(width = 6, plotlyOutput("donorShare")),
                box(width = 6, plotlyOutput("recipientShare"))
              )
      ),
      
      # Data Explorer Tab
      tabItem(tabName = "explorer",
              fluidRow(
                box(width = 12,
                    downloadButton("downloadData", "Download Data"),
                    DTOutput("dataTable"))
              )
      ),
      
      # Trend Analysis Tab
      tabItem(tabName = "trends",
              fluidRow(
                box(width = 12, plotlyOutput("detailedTrend")),
                box(width = 12,
                    radioButtons("trendType", "Trend Display:",
                                 choices = c("By Donor" = "donor",
                                             "By Recipient" = "recipient",
                                             "By Measure" = "measure"),
                                 selected = "donor",
                                 inline = TRUE))
              )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive filtered data
  filtered_data <- reactive({
    req(input$donor, input$recipient, input$measure)
    
    Data %>%
      filter(
        Donor %in% input$donor,
        Recipient %in% input$recipient,
        TIME_PERIOD >= input$years[1],
        TIME_PERIOD <= input$years[2],
        Measure %in% input$measure
      )
  })
  
  # Update recipient choices based on selected donors
  observe({
    recipients <- Data %>%
      filter(Donor %in% input$donor) %>%
      pull(Recipient) %>%
      unique() %>%
      sort()
    
    updateSelectInput(session, "recipient",
                      choices = recipients,
                      selected = head(recipients, 1))
  })
  
  # Value boxes
  output$totalAid <- renderValueBox({
    total <- filtered_data() %>%
      summarise(sum(OBS_VALUE, na.rm = TRUE)) %>%
      pull() %>%
      round(2)
    
    valueBox(
      paste0("$", total, "M"),
      "Total Disbursement",
      icon = icon("money-bill-wave"),
      color = "green"
    )
  })
  
  output$avgAid <- renderValueBox({
    avg <- filtered_data() %>%
      summarise(mean(OBS_VALUE, na.rm = TRUE)) %>%
      pull() %>%
      round(2)
    
    valueBox(
      paste0("$", avg, "M"),
      "Annual Average",
      icon = icon("calculator"),
      color = "blue"
    )
  })
  
  output$yearRange <- renderValueBox({
    valueBox(
      paste(input$years[1], "-", input$years[2]),
      "Year Range",
      icon = icon("calendar"),
      color = "yellow"
    )
  })
  
  # Plots
  output$annualTrend <- renderPlotly({
    df <- filtered_data() %>%
      group_by(TIME_PERIOD) %>%
      summarise(Total = sum(OBS_VALUE, na.rm = TRUE))
    
    plot_ly(df, x = ~TIME_PERIOD, y = ~Total, type = 'scatter', mode = 'lines+markers') %>%
      layout(title = "Annual Aid Disbursement Trend",
             xaxis = list(title = "Year", range = c(2000, 2023)),
             yaxis = list(title = "Amount (Millions USD)"))
  })
  
  output$donorShare <- renderPlotly({
    df <- filtered_data() %>%
      group_by(Donor) %>%
      summarise(Total = sum(OBS_VALUE, na.rm = TRUE))
    
    plot_ly(df, labels = ~Donor, values = ~Total, type = 'pie') %>%
      layout(title = "Share by Donor")
  })
  
  output$recipientShare <- renderPlotly({
    df <- filtered_data() %>%
      group_by(Recipient) %>%
      summarise(Total = sum(OBS_VALUE, na.rm = TRUE))
    
    plot_ly(df, labels = ~Recipient, values = ~Total, type = 'pie') %>%
      layout(title = "Share by Recipient")
  })
  
  output$detailedTrend <- renderPlotly({
    df <- filtered_data()
    
    if(input$trendType == "donor") {
      df <- df %>% group_by(TIME_PERIOD, Donor)
    } else if(input$trendType == "recipient") {
      df <- df %>% group_by(TIME_PERIOD, Recipient)
    } else {
      df <- df %>% group_by(TIME_PERIOD, Measure)
    }
    
    df <- df %>% summarise(Total = sum(OBS_VALUE, na.rm = TRUE))
    
    p <- plot_ly(df, x = ~TIME_PERIOD, y = ~Total, color = ~get(input$trendType),
                 type = 'scatter', mode = 'lines') %>%
      layout(title = "Detailed Trend Analysis",
             xaxis = list(title = "Year", range = c(2000, 2023)),
             yaxis = list(title = "Amount (Millions USD)"))
    
    p
  })
  
  # Data table
  output$dataTable <- renderDT({
    datatable(filtered_data(),
              options = list(scrollX = TRUE, pageLength = 10))
  })
  
  # Download handler
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("oecd-aid-data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(filtered_data(), file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)

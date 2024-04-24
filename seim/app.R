library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(readr)

data <- read_rds("C://guacodemoleh/IS415-GAA/seim/data/rds/seim_est.rds")
data <- data %>%
  mutate(Group = case_when(
    startsWith(Variable, "O_") ~ "Origin",
    startsWith(Variable, "D_") ~ "Destination",
    TRUE ~ "Others"
  ))
data <- data %>%
  group_by(Variable) %>%
  mutate(Stat.Sig = ifelse(any(Statistic == "p.val" & Value < 0.05), 1, 0)) %>%
  ungroup()

# Define the UI
ui <- fluidPage(
  titlePanel("Spatial Econometric Interaction Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("statistic", "Select Statistic:", choices = unique(data$Statistic)),
      actionButton("showSignificantBtn", "Show statistically significant"),
      actionButton("showAllBtn", "Show All Variables")
    ),
    
    mainPanel(
      plotlyOutput("plot", height = "700px", width = "100%")
    )
  )
)

# Define the server logic
server <- function(input, output, session) {
  values <- reactiveValues(showSignificant = FALSE)
  
  # Filter the data based on selected statistic
  filteredData <- reactive({
    data %>%
      filter(Statistic == input$statistic)
  })
  
  # Show statistically significant variables
  observeEvent(input$showSignificantBtn, {
    if (input$showSignificantBtn > 0) {
      values$showSignificant <- TRUE
    } else {
      values$showSignificant <- FALSE
    }
    
    output$plot <- renderPlotly({
      if (values$showSignificant) {
        plotData <- filteredData() %>%
          filter(Stat.Sig < 0.05)
      } else {
        plotData <- filteredData()
      }
      
      p <- ggplot(plotData, aes(x = Variable, y = Value, fill = Group)) +
        geom_bar(stat = "identity", position = "dodge", width = 0.5) +
        coord_flip() +
        labs(x = NULL, y = "Value") +
        theme_minimal() +
        theme(legend.position = "top")
      
      p <- ggplotly(p, tooltip = c("Value"))
      
      p
    })
  })
  
  # Show all variables
  observeEvent(input$showAllBtn, {
    values$showSignificant <- FALSE
    
    output$plot <- renderPlotly({
      plotData <- filteredData()
      
      p <- ggplot(plotData, aes(x = Variable, y = Value, fill = Group)) +
        geom_bar(stat = "identity", position = "dodge", width = 0.5) +
        coord_flip() +
        labs(x = NULL, y = "Value") +
        theme_minimal() +
        theme(legend.position = "top")
      
      p <- ggplotly(p, tooltip = c("Value"))
      
      p
    })
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
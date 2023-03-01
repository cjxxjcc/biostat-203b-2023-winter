library(shiny)
library(dplyr)
library(ggplot2)

# setwd("/Users/chengjiaxin/Desktop/Biostat203b/HW/HW3")
icu_cohort <- readRDS("icu_cohort.rds")
ui <- pageWithSidebar(
  headerPanel("ICU Cohort Explorer"),
  sidebarPanel(
    selectInput("variable", "Variable:", 
                choices = c("Age at Adimission", 
                            "Gender",
                            "Ethnicity", 
                            "Language",
                            "Insurance", 
                            "Marital Status",
                            "Lab Measurement", 
                            "Vital Measurement", 
                            "30-Day Mortality")),
    conditionalPanel(
      condition = "input.variable == 'Lab Measurement'",
      selectInput("lab", "Select a Lab Test:",
                  choices = c("L50912",
                              "L50971", 
                              "L50983", 
                              "L50902", 
                              "L50882", 
                              "L51221", 
                              "L51301", 
                              "L50931"))
    ),
    conditionalPanel(
      condition = "input.variable == 'Vital Measurement'",
      selectInput("vital", "Select a Vital:",
                  choices = c("C220045", 
                              "C220181", 
                              "C220179", 
                              "C223761", 
                              "C220210"))
    ),
    actionButton("plot_button", "Plot"),
    downloadButton("download_button", "Download Data")
  ),
  mainPanel(
    tabsetPanel(
      tabPanel("Plot", plotOutput("plot")),
      tabPanel("Table", tableOutput("table"))
    )
  )
)

server <- function(input, output) {
  output$plot <- renderPlot({
    if (input$variable == "Age at Adimission") {
      ggplot(icu_cohort, aes(x = age_at_admission)) +
        geom_histogram(binwidth = 5) +
        labs(x = "Age at Hospital Admission", y = "Frequency")
    } else if (input$variable == "Gender") {
      ggplot(icu_cohort, aes(x = gender)) +
        geom_bar() +
        labs(x = "Gender", y = "Count")
    } else if (input$variable == "Ethnicity") {
      ggplot(icu_cohort, aes(x = ethnicity)) +
        geom_bar() +
        labs(x = "Ethnicity", y = "Count")
    } else if (input$variable == "Language") {
      ggplot(icu_cohort, aes(x = language)) +
        geom_bar() +
        labs(x = "Language", y = "Count")
    } else if (input$variable == "Insurance") {
      ggplot(icu_cohort, aes(x = insurance)) +
        geom_bar() +
        labs(x = "Insurance", y = "Count")
    } else if (input$variable == "Marital Status") {
      ggplot(icu_cohort, aes(x = marital_status)) +
        geom_bar() +
        labs(x = "Marital Status", y = "Count")
    } else if (input$variable == "Lab Measurement") {
      ggplot(icu_cohort, aes(x = !!sym(input$lab))) +
        geom_histogram(binwidth = 1) +
        labs(x = input$lab, y = "Frequency")
    } else if (input$variable == "Vital Measurement") {
      ggplot(icu_cohort, aes(x = !!sym(input$vital))) +
        geom_histogram(binwidth = 1) +
        labs(x = input$vital, y = "Frequency")
    } else if (input$variable == "30-Day Mortality") {
      ggplot(icu_cohort, aes(x = thirty_day_mort)) +
        geom_bar() +
        labs(x = "30-Day Mortality", y = "Count")
    }
  })
  
  output$table <- renderTable({
    if (input$variable == "Age at Adimission") {
      icu_cohort %>% group_by(age_at_admission) %>%
        summarise(n = n()) %>% arrange(desc(n))
    } else if (input$variable == "Gender") {
      icu_cohort %>% group_by(gender) %>%
        summarise(n = n()) %>% arrange(desc(n))
    } else if (input$variable == "Ethnicity") {
      icu_cohort %>% group_by(ethnicity) %>%
        summarise(n = n()) %>% arrange(desc(n))
    } else if (input$variable == "Language") {
      icu_cohort %>% group_by(language) %>%
        summarise(n = n()) %>% arrange(desc(n))
    } else if (input$variable == "Insurance") {
      icu_cohort %>% group_by(insurance) %>%
        summarise(n = n()) %>% arrange(desc(n))
    } else if (input$variable == "Marital Status") {
      icu_cohort %>% group_by(marital_status) %>%
        summarise(n = n()) %>% arrange(desc(n))
    } else if (input$variable == "Lab Measurement") {
      icu_cohort %>% select(!!sym(input$lab)) %>% 
        na.omit() %>% summarise(Mean = mean(!!sym(input$lab)), 
                                SD = sd(!!sym(input$lab)), 
                                Median = median(!!sym(input$lab)), 
                                Min = min(!!sym(input$lab)), 
                                Max = max(!!sym(input$lab)))
    } else if (input$variable == "Vital Measurement") {
      icu_cohort %>% select(!!sym(input$vital)) %>% 
        na.omit() %>% summarise(Mean = mean(!!sym(input$vital)), 
                                SD = sd(!!sym(input$vital)), 
                                Median = median(!!sym(input$vital)), 
                                Min = min(!!sym(input$vital)), 
                                Max = max(!!sym(input$vital)))
    } else if (input$variable == "30-Day Mortality") {
      icu_cohort %>% group_by(thirty_day_mort) %>%
        summarise(n = n()) %>% arrange(desc(n))
    }
  })
}
shinyApp(ui, server)
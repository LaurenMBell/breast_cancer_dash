## Lauren practice dashboard for shiny!
## visualizes breast cancer data

## how could you connect a ML or LR 
## model that predicts survival time on this data?

library(shiny)
library(bslib)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(mapDK)

#source("bar_treatment.R")

stage_theme <- c("1" = "yellow", 
                 "2" = "orange", 
                 "3" = "red", 
                 "4" = "darkred")

treatment_theme <- c("Chemotherapy" = "#E69F00",
                     "Hormone Therapy" = "#56B4E9",
                     "Radio Therapy" = "#009E73")


cancer_data <- read_csv("data/METABRIC_RNA_Mutation.csv")

######################### UI #######################################

ui <- page_navbar(
  title = "Breast Cancer Dashboard",
  
  nav_panel(
    title = "Clinical Data Exploration",
    layout_sidebar(
      sidebar = sidebar(
        title = "Selection Menu",
        
        selectInput("cancer_subtype", 
                    label = "Select breast cancer subtype:",
                    choices = c("All Cancer Subtypes",
                                "All Carcinoma Subtypes",
                                "Breast Invasive Ductal Carcinoma", 
                                "Breast Mixed Ductal and Lobular Carcinoma", 
                                "Breast Invasive Lobular Carcinoma", 
                                "Breast Invasive Mixed Mucinous Carcinoma", 
                                "Metaplastic Breast Cancer"),
                    selected = "All Cancer Subtypes"),
        
        sliderInput("age",
                    label = "Select age at diagnosis range:",
                    min = min(cancer_data$age_at_diagnosis), 
                    max = max(cancer_data$age_at_diagnosis),
                    value = c(min(cancer_data$age_at_diagnosis), 
                              max(cancer_data$age_at_diagnosis))),
        
        checkboxGroupInput("surgery_type",
                           label = "Select surgery type:",
                           choices = c("Masectomy", "Breast Conserving"),
                           selected = c("Masectomy", "Breast Conserving")
        )
      ), #closes data exploration selection menu
      
      fluidRow(
        card(
          card_header("Tumor Size for Selected Cancer Type"), 
          plotOutput("tumor_size_hist")
        )
      ) # closes main page for data exploration
    )
  ),#closes data exploration page
  
  nav_panel(
    title = "Cohort Statistics", 
    layout_columns(
      value_box(
        title = "Patients (n)",
        value = nrow(cancer_data) - 1, 
        showcase = bsicons::bs_icon("capsule-pill"),
        theme = "primary"
      ), 
      value_box(
        title = "Average Age at Diagnosis in Years",
        value = round(mean(cancer_data$age_at_diagnosis, na.rm = TRUE), 1),
        showcase = bsicons::bs_icon("clock"),
        theme = "info"
      ), 
      value_box(
        title = "Average Tumor Size in cm", 
        value = round(mean(cancer_data$tumor_size, na.rm = TRUE), 1), 
        showcase = bsicons::bs_icon("arrows"), 
        theme = "success"
      ),
      value_box(
        title = "Average Survival in Months", 
        value = round(mean(cancer_data$overall_survival_months, na.rm = TRUE), 1), 
        showcase = bsicons::bs_icon("calendar-event"), 
        theme = "warning"
      ),
    ), #closes column layout
    card(
      plotOutput("cancer_subtype_bar")
    )
  ), #closes quick facts page
  
  
  nav_panel(
    title = "About the Data",
    navset_card_pill(
      nav_panel("Breast Cancer", "about breast cancer, 
                the subtypes, and cancer bio"),
      nav_panel("METABRIC", "all abut metabric and the papers using this data"), 
      nav_panel("Transcriptomics", "about transcriptomics and radiomics")
    )
  ), #closes 'about the data' page
  
  
  nav_panel(
    title = "Survival Calculator",
    card(
      card_header("Survival Time Prediction"),
      p("Survival calculator goes here...")
    )
  ) #closes survival calculator page
)


######################### SERVER #######################################

server <- function(input, output) {
  
  filtered_data <- reactive({
    data <- cancer_data
    if (input$cancer_subtype == "All Cancer Subtypes") {
      data <- data
    } else if (input$cancer_subtype == "All Carcinoma Subtypes") {
      data <- data %>%
        filter(grepl("Carcinoma", cancer_type_detailed, ignore.case = TRUE))
    } else {
      data <- data %>%
        filter(cancer_type_detailed == input$cancer_subtype)
    }
      data <- data %>%
      filter(age_at_diagnosis >= input$age[1] & age_at_diagnosis <= input$age[2])
    
    if (length(input$surgery_type) > 0) {
      data <- data %>%
        filter(toupper(type_of_breast_surgery) %in% toupper(input$surgery_type))
    }
    return(data)
  })
  
  
  #tumor size histogram: filter to selected type, age, and surgery type
  output$tumor_size_hist <- renderPlot({
    summary <- filtered_data()
    
    ggplot(summary, aes(x = tumor_size)) + 
      geom_histogram(bins=15, fill="steelblue", color="white") +
      labs(
        #title = "Distribution of Tumor Size\nfor Selected Cancer Type", 
        x = "Tumor Size (cm)", 
        y = "Count") +
      theme_minimal()
  })
  
  #cohort quick facts bar chart of cancer subtypes
  output$cancer_subtype_bar <- renderPlot({
    title = "Count of Cancer Subtypes"
    cancer_data %>%
      count(cancer_type_detailed) %>%
      ggplot(aes(x = reorder(cancer_type_detailed, n), y = n)) + 
      geom_col(fill = "steelblue") +
      coord_flip() +
      labs(
        x = "Cancer Subtype", 
        y = "Count"
      ) +
      theme_minimal()
  })
  
}

shinyApp(ui = ui, server = server)
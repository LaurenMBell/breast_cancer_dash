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

source("bar_treatment.R")

cancer_data <- read_csv("data/METABRIC_RNA_Mutation.csv")

######################### UI #######################################

ui <- page_navbar(
  title = "Breast Cancer Data Dashboard",
  
  
  nav_panel(
    title = "Breast Cancer Data",
    layout_sidebar(
      sidebar = sidebar(
        title = "Selection Menu",
        
        selectInput("type", 
                    label = "Select breast cancer subtype:",
                    choices = c("Invasive Ductal Carcinoma", 
                                "Mixed Ductal and Lobular Carcinoma", 
                                "Invasive Lobular Carcinoma", 
                                "Invasive Mixed Mucinous Carcinoma", 
                                "Metaplastic Breast Cancer"),
                    selected = "Invasive Ductal Carcinoma"
        ),
        
        sliderInput("age",
                    label = "Select age at diagnosis range:",
                    min = min(cancer_data$age_at_diagnosis), 
                    max = max(cancer_data$age_at_diagnosis),
                    value = c(min, max)
        ),
        
        checkboxGroupInput("surgery_type",
                           label = "Select surgery type:",
                           choices = c("Masectomy", "Breast Conserving"),
                           selected = choices
        ),
        
        
      ),
      
      
      layout_columns(
        value_box(
          title = "Patients (n)",
          value = nrow(cancer_data_denmark) - 1, 
          showcase = bsicons::bs_icon("capsule-pill"),
          theme = "primary"
        ), 
        value_box(
          title = "Average Age at Diagnosis in Years",
          value = round(mean(cancer_data_denmark$Age, na.rm = TRUE), 1),
          showcase = bsicons::bs_icon("clock"),
          theme = "info"
        ), 
        value_box(
          title = "Average Tumor Size in cm", 
          value = round(mean(cancer_data_denmark$Tumor_Size_cm, na.rm = TRUE), 1), 
          showcase = bsicons::bs_icon("arrows"), 
          theme = "success"
        ),
        value_box(
          title = "Average Survival in Months", 
          value = round(mean(cancer_data_denmark$Survival_Months, na.rm = TRUE), 1), 
          showcase = bsicons::bs_icon("calendar-event"), 
          theme = "warning"
        ),
        value_box(
          title = "Average BMI", 
          value = round(mean(cancer_data_denmark$BMI, na.rm = TRUE), 1), 
          showcase = bsicons::bs_icon("person"), 
          theme = "secondary"
        ),
        card(
          card_header("Treatments Given"),
          plotOutput("bar_treatment")
        ),
        card(
          card_header("Cancer Stage of Selected Cancer Type"),
          plotOutput("bar_stage")
        ),
        card(
          card_header("Number of Diagnoses Across Denmark"),
          p("Number of Selected Cancer Diagnoses across Denmark"),
          plotOutput("diagnosis_map")
        ),
        card(
          p("put another graph here! something with dates")
        )
      )
    )
  ),
  
  
  nav_panel(
    title = "Survival Calculator",
    card(
      card_header("Survival Time Prediction"),
      p("Survival calculator goes here...")
    )
  )
)


######################### SERVER #######################################

server <- function(input, output) {
  
  output$bar_stage <- renderPlot({
    summary <- cancer_data_denmark %>% 
      filter(Cancer_Type == input$type) %>% 
      count(Stage) 
    
    ggplot(summary, aes(x = Stage, y = n, fill = Stage)) + 
      geom_col() +
      scale_fill_manual(values = stage_theme) +
      labs(
        title = "# of Cancer Stage Diagnoses for Selected Cancer Type", 
        x = "Cancer Stage", 
        y = "Count") +
      theme_minimal()
  })
  
  output$bar_treatment <- renderPlot({
    summary <- cancer_data_denmark %>% 
      filter(Cancer_Type == input$type) %>% 
      count(Treatment_Type) 
    
    ggplot(summary, aes(x = reorder(Treatment_Type, n), y = n, fill = Treatment_Type)) + 
      geom_col() +
      coord_flip() + 
      scale_fill_manual(values = treatment_theme) +
      labs(
        title = "Treatments for Selected Cancer Type", 
        x = "Treatment", 
        y = "Count") +
      theme_minimal()
  })
  
  output$diagnosis_map <- renderPlot({
    selected_type <- cancer_data_denmark %>% filter(Cancer_Type == input$type)
    region <- c("region hovedstaden", "region sjælland","region syddanmark", "region midtjylland", "region nordjylland")
    count <- c(sum(selected_type$Region == "Hovedstaden"),
               sum(selected_type$Region == "Sjælland"), 
               sum(selected_type$Region == "Syddanmark"), 
               sum(selected_type$Region == "Midtjylland"),
               sum(selected_type$Region == "Nordjylland"))
    
    city_data <- data.frame(region, count)
    
    mapDK(
      detail = "region", 
      data = city_data, id = "region", values = "count", 
      guide.label = "Number of Diagnoses")  
  })
  
}

shinyApp(ui = ui, server = server)
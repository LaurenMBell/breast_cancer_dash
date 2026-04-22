## Lauren practice dashboard for shiny!
## visualizes breast cancer data

library(shiny)
library(bslib)
library(tidyverse)

#source("bar_treatment.R")
source("helper.R")
#source("survival_calc.R")

stage_theme <- c("0" = "#f9c74f",
                 "1" = "#EE9B00", 
                 "2" = "#CA6702", 
                 "3" = "#BB3E03", 
                 "4" = "#Ae2012", 
                 "NA" = "#E9D8A6")

treatment_theme <- c("Chemotherapy" = "#E69F00",
                     "Hormone Therapy" = "#56B4E9",
                     "Radio Therapy" = "#009E73")

type_theme <- c("All Cancer Subtypes" = "#577590",
                "All Carcinoma Subtypes" = "#76c893",
                "Breast Invasive Ductal Carcinoma" = "#001219", 
                "Breast Mixed Ductal and Lobular Carcinoma" = "#005F73", 
                "Breast Invasive Lobular Carcinoma" = "#0A9396", 
                "Breast Invasive Mixed Mucinous Carcinoma" = "#94D2BD", 
                "Metaplastic Breast Cancer" = "#8ecae6")


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
                           choices = c("Mastectomy", "Breast Conserving"),
                           selected = c("Mastectomy", "Breast Conserving")
        )
        
      ), #closes data exploration selection menu
      
      fluidRow(
        card(
          card_header("Tumor Size for Selected Cancer Type"), 
          plotOutput("tumor_size_hist"),
          sliderInput("bins", 
                      label = "Select number of bins:", 
                      min = 5, 
                      max = 50, 
                      value = 30
          ) 
        ), #closes tumor size histogram card
        card(
          card_header("Age and Tumor Size Correlation by Stage"),
          plotOutput("age_size_scatter"),
          checkboxGroupInput("stage_scatter", 
                      label = "Select stage(s) to visualize:", 
                      choices = c(0, 1, 2, 3, 4),
                      selected = c(0, 1, 2, 3, 4))
  
        ) #closes age and tumor size scatter plot
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
      )
    ), #closes column layout
    layout_columns(
      card(
        plotOutput("cancer_subtype_bar"),
      ), 
      card(
        plotOutput("cancer_type_by_survival")
      )
      # TO DO: add a "create your own plot" card HERE
    )
    
  ), #closes quick facts page
  
  
  nav_panel(
    title = "About the Data",
    navset_card_pill(
      nav_panel("Overview", about_overview()),
      nav_panel("Breast Cancer", about_breast_cancer()),
      nav_panel("METABRIC", about_metabric()), 
      nav_panel("Transcriptomics", about_transcriptomics())
    )
  ), #closes 'about the data' page
  
  
  nav_panel(
    title = "Survival Calculator",
    card(
      card_header("Survival Time Prediction"),
      p("Naive Bayes survival calculator under construction")
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
    selected_color <- type_theme[input$cancer_subtype]
    
    ggplot(summary, aes(x = tumor_size)) + 
      geom_histogram(bins=input$bins, fill = selected_color, color ="white") +
      labs(
        # if 'all cancer subtypes' or 'all carcinoma' is selected, show those in a 
        # stacked bar chart 
        x = "Tumor Size (cm)", 
        y = "Count") +
      theme_minimal()
  })
  
  #cohort quick facts bar chart of cancer subtypes
  output$cancer_subtype_bar <- renderPlot({
    plot_data <- cancer_data %>%
      mutate(tumor_stage = as.character(tumor_stage)) %>% 
      mutate(tumor_stage = ifelse(is.na(tumor_stage), "NA", tumor_stage)) %>% 
      count(cancer_type_detailed, tumor_stage) %>%
      drop_na() 
    
    ggplot(plot_data, aes(x = reorder(cancer_type_detailed, n, FUN = sum), 
                          y = n, 
                          fill = tumor_stage)) +  
      geom_col(position = "stack") +
      scale_fill_manual(values = stage_theme, 
                        na.value = "darkgrey") +  
      labs(
        title = "Count of Cancer Subtypes by Tumor Stage",
        x = "Cancer Subtype", 
        y = "Count",
        fill = "Tumor Stage"
      ) +
      theme_minimal() +
      theme(legend.position = "right")
  })
  
  output$age_size_scatter <- renderPlot({
    data <- filtered_data() %>%
      mutate(
        tumor_stage = as.character(tumor_stage),
        tumor_stage = ifelse(is.na(tumor_stage), "NA", tumor_stage)
      )
    # filter data by selected stages from the checkbox input
    
    data <- data %>%
      filter(tumor_stage %in% as.character(input$stage_scatter))
    
    if (nrow(data) > 2) {
      corr <- cor(data$age_at_diagnosis, data$tumor_size, use = "complete.obs")
      corr_text <- paste0("r = ", round(corr, 3))
    } else {
      corr_text <- "Not enough data"
    }
    
    ggplot(data, aes(x= age_at_diagnosis, y= tumor_size, 
                            color = as.character(tumor_stage))) + 
      geom_jitter() + 
      labs(
        title = "Correlation of Age and Tumor Size", 
        x = "Age at Diagnosis", 
        y = "Tumor Size (cm)", 
        color = "Tumor Stage") + 
      scale_color_manual(values = stage_theme) + 
      geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed") +
      annotate("text", x = Inf, y = Inf, label = corr_text, 
               hjust = 1.1, vjust = 1.5, size = 5, color = "black")
  })
  
  output$cancer_type_by_survival <- renderPlot({
    ggplot(cancer_data, aes(x = cancer_type_detailed, 
                            y = overall_survival_months, 
                            fill = cancer_type_detailed)) + 
      geom_boxplot() + 
      scale_fill_manual(values = type_theme) +
      labs(
        title = "Overall Survival by Cancer Subtype",
        x = "Cancer Subtype",
        y = "Overall Survival (months)",
        fill = "Cancer Subtype"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "none")
  })
  
}

shinyApp(ui = ui, server = server)
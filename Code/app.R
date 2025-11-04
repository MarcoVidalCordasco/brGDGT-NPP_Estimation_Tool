library(shiny)
library(openxlsx)
library(compositions)
library(randomForest)
library(ggplot2)
library(reshape2)
library(dplyr)


# Load brGDGT Random Forest model
model_cv <- readRDS("model_brGDGT-NPP_cv.rds")

sampletypes <- c("Lacustrine Sediment", "Lacustrine SPM", 
                 "Low DO Lacustrine SPM", "Peat", 
                 "Riverine Sediment and SPM", "Soil")

# Corrected Miami model
miami_model <- function(MAT, MAP) {
  NPP_MAT <- 3000 / (1 + exp(1.315 - 0.119 * MAT))
  NPP_MAP <- 3000 * (1 - exp(-0.000664 * MAP))
  pmin(NPP_MAT, NPP_MAP)
}

ui <- fluidPage(
  titlePanel("NPP Estimation App"),
  
  tabsetPanel(
    id = "main_tabs",
    
    # Step 1
    tabPanel("Step 1: Select Context and Method",
             sidebarLayout(
               sidebarPanel(
                 radioButtons("context_type", "Select NPP context:",
                              choices = c("Archaeological Site" = "archaeo",
                                          "Natural Archive (e.g., lake, peat bog)" = "natural")),
                 conditionalPanel(
                   condition = "input.context_type == 'natural'",
                   selectInput("sampletype", "Select Sample Type", choices = sampletypes)
                   
                 ),
                 radioButtons("method", "Select NPP estimation method:",
                              choices = c("brGDGT" = "brGDGT",
                                          "Climate-derived" = "climate",
                                          "Both" = "both")),
                 conditionalPanel(
                   condition = "input.method == 'both'",
                   checkboxInput("hanpp", "Compute HANPP?*", value = TRUE)
                 ),
                 checkboxGroupInput("outputs", "Select outputs:",
                                    choices = c("Plots" = "plots",
                                                "Summary Statistics" = "summary")),
                 actionButton("next_btn", "Next")
               ),
               mainPanel(
                 span("* To compute HANPP, brGDGT-derived and Climate-derived data must come from the same context and have matching IDs or Levels (= Stratigraphic level number)",
                      style = "font-size: 12px; color: #555;")
               )
             )
    ),
    
    # Step 2
    tabPanel("Step 2: Upload Data and Compute",
             sidebarLayout(
               sidebarPanel(
                 conditionalPanel(
                   condition = "input.method == 'brGDGT' || input.method == 'both'",
                   fileInput("file_br", "Upload brGDGT Excel file (.xlsx)", accept = ".xlsx")
                 ),
                 conditionalPanel(
                   condition = "input.method == 'climate' || input.method == 'both'",
                   fileInput("file_clim", "Upload climate Excel file (.xlsx)", accept = ".xlsx")
                 ),
                 actionButton("compute_btn", "Compute NPP"),
                 br(),
                 tableOutput("results_table"),
                 downloadButton("download_results", "Download Results")
               ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("Plots",
                            plotOutput("npp_plot", height = "400px"),
                            conditionalPanel(
                              condition = "input.hanpp == true",
                              plotOutput("hanpp_plot", height = "400px")
                            )
                   ),
                   tabPanel("Summary Statistics",
                            tableOutput("summary_table")
                   )
                 )
               )
             )
    )
  )
)

server <- function(input, output, session) {
  
  # Move to Step 2
  observeEvent(input$next_btn, {
    updateTabsetPanel(session, "main_tabs", selected = "Step 2: Upload Data and Compute")
  })
  
  # Determine sample type
  selected_sampletype <- reactive({
    if(input$context_type == "archaeo") "Soil" else input$sampletype
  })
  
  # Determine ID column (ID or Level)
  id_column <- reactive({
    if(input$context_type == "archaeo") "Level" else "ID"
  })
  
  # Load brGDGT file
  br_data <- reactive({
    req(input$file_br)
    df <- read.xlsx(input$file_br$datapath, sheet = 1)
    if(!(id_column() %in% colnames(df))) {
      showNotification(paste("brGDGT file must contain column", id_column()), type = "error")
      return(NULL)
    }
    df
  })
  
  # Load climate file
  clim_data <- reactive({
    req(input$file_clim)
    df <- read.xlsx(input$file_clim$datapath, sheet = 1)
    required_cols <- c(id_column(), "MAT","MAP")
    if(!all(required_cols %in% colnames(df))) {
      showNotification(paste("Climate file must contain columns:", paste(required_cols, collapse=", ")), type = "error")
      return(NULL)
    }
    df
  })
  
  # Main computation
  results <- eventReactive(input$compute_btn, {
    df_list <- list()
    col_id <- id_column()
    
    # brGDGT NPP
    if(input$method %in% c("brGDGT","both")) {
      df <- br_data()
      req(df)
      br_cols <- colnames(df)[2:16]
      comp_new <- df[, br_cols] / 100 + 1e-6
      clr_df <- as.data.frame(clr(comp_new))
      clr_df$Sampletype <- factor(selected_sampletype(), levels = sampletypes)
      
      newdata_model <- clr_df
      clr_cols <- colnames(clr_df)
      newdata_model <- newdata_model[, clr_cols, drop = FALSE]
      for(stype in sampletypes){
        colname <- paste0("Sampletype", stype)
        newdata_model[[colname]] <- ifelse(clr_df$Sampletype == stype, 1, 0)
      }
      model_cols <- c(clr_cols, paste0("Sampletype", sampletypes))
      model_cols <- model_cols[model_cols %in% colnames(newdata_model)]
      newdata_model <- newdata_model[, model_cols]
      rf_preds <- predict(model_cv$finalModel, newdata = newdata_model, predict.all = TRUE)
      df$NPP_brGDGT <- apply(rf_preds$individual, 1, mean)
      df$NPP_sd <- apply(rf_preds$individual, 1, sd)
      df$`95%CI_low` <- df$NPP_brGDGT - 1.96 * df$NPP_sd
      df$`95%CI_high` <- df$NPP_brGDGT + 1.96 * df$NPP_sd
      df_list$brGDGT <- df
      df_list$rf_preds <- rf_preds
    }
    
    # Climate NPP
    if(input$method %in% c("climate","both")) {
      dfc <- clim_data()
      req(dfc)
      dfc$NPP_climate <- miami_model(dfc$MAT, dfc$MAP)
      df_list$climate <- dfc
    }
    
    # HANPP
    if(input$method=="both" & input$hanpp){
      df_br <- df_list$brGDGT
      df_clim <- df_list$climate
      req(df_br, df_clim)
      if(!(col_id %in% colnames(df_br)) | !(col_id %in% colnames(df_clim))){
        showNotification(paste("Both datasets must have column", col_id, "for HANPP"), type="error")
      } else {
        df_hanpp <- merge(df_br, df_clim, by=col_id)
        df_hanpp$HANPP <- df_hanpp$NPP_climate - df_hanpp$NPP_brGDGT
        df_list$hanpp <- df_hanpp
      }
    }
    
    df_list
  })
  
  # Main results table
  output$results_table <- renderTable({
    req(results())
    col_id <- id_column()
    if(input$method=="brGDGT") {
      results()$brGDGT %>% select(all_of(col_id), NPP_brGDGT, NPP_sd, `95%CI_low`, `95%CI_high`)
    } else if(input$method=="climate") {
      results()$climate %>% select(all_of(col_id), NPP_climate)
    } else {
      if(input$hanpp & "hanpp" %in% names(results())) {
        results()$hanpp %>% select(all_of(col_id), NPP_brGDGT, NPP_climate, HANPP)
      } else {
        merge(results()$brGDGT, results()$climate, by = col_id) %>% 
          select(all_of(col_id), NPP_brGDGT, NPP_climate)
      }
    }
  })
  
  # Summary statistics table
  output$summary_table <- renderTable({
    req(results())
    if("summary" %in% input$outputs){
      df_list <- results()
      sum_stats <- list()
      if(input$method %in% c("brGDGT","both") & !is.null(df_list$brGDGT)){
        df <- df_list$brGDGT
        sum_stats$brGDGT <- data.frame(
          Variable="NPP_brGDGT",
          Mean=mean(df$NPP_brGDGT, na.rm=TRUE),
          Median=median(df$NPP_brGDGT, na.rm=TRUE),
          SD=sd(df$NPP_brGDGT, na.rm=TRUE)
        )
      }
      if(input$method %in% c("climate","both") & !is.null(df_list$climate)){
        df <- df_list$climate
        sum_stats$climate <- data.frame(
          Variable="NPP_climate",
          Mean=mean(df$NPP_climate, na.rm=TRUE),
          Median=median(df$NPP_climate, na.rm=TRUE),
          SD=sd(df$NPP_climate, na.rm=TRUE)
        )
      }
      if(input$method=="both" & input$hanpp & "hanpp" %in% names(df_list)){
        df <- df_list$hanpp
        sum_stats$HANPP <- data.frame(
          Variable="HANPP",
          Mean=mean(df$HANPP, na.rm=TRUE),
          Median=median(df$HANPP, na.rm=TRUE),
          SD=sd(df$HANPP, na.rm=TRUE)
        )
      }
      do.call(rbind, sum_stats)
    }
  })
  
  # NPP plot
  output$npp_plot <- renderPlot({
    req(results())
    col_id <- id_column()
    if("plots" %in% input$outputs){
      df <- NULL
      if(input$method=="brGDGT") df <- results()$brGDGT
      else if(input$method=="climate") df <- results()$climate
      else df <- merge(results()$brGDGT, results()$climate, by=col_id)
      
      df_long <- df %>% select(all_of(c(col_id, intersect(c("NPP_brGDGT","NPP_climate"), colnames(df))))) %>% melt(id.vars=col_id)
      
      ggplot(df_long, aes_string(x=col_id, y="value", color="variable")) +
        geom_line(size=1.5) +
        labs(title="NPP Predictions", y="NPP") + theme_minimal()
    }
  })
  
  # HANPP plot (separate)
  output$hanpp_plot <- renderPlot({
    req(results())
    col_id <- id_column()
    if("plots" %in% input$outputs & input$method=="both" & input$hanpp){
      df <- results()$hanpp
      ggplot(df, aes_string(x=col_id, y="HANPP")) +
        geom_line(color="red", size=1.5) +
        labs(title="HANPP", y="HANPP") + theme_minimal()
    }
  })
  
  # Download
  output$download_results <- downloadHandler(
    filename = function() { paste0("NPP_results_", Sys.Date(), ".xlsx") },
    content = function(file){
      df_to_save <- NULL
      col_id <- id_column()
      if(input$method=="brGDGT") df_to_save <- results()$brGDGT
      else if(input$method=="climate") df_to_save <- results()$climate
      else df_to_save <- if(input$hanpp & "hanpp" %in% names(results())) results()$hanpp else merge(results()$brGDGT, results()$climate, by=col_id)
      write.xlsx(df_to_save, file)
    }
  )
  
}

shinyApp(ui, server)





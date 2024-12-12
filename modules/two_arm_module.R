# modules/two_arm_module.R

# UI Module
twoArmUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    h2("Two-Arm Olfactometer"),
    br(),
    fluidRow(
      column(6, h4("Trial Timer"), verbatimTextOutput(ns("trial_timer")))
    ),
    fluidRow(
      class = "centered-buttons",
      column(2, actionButton(ns("start"), "Start Recording", class = "btn btn-start")),
      column(2, actionButton(ns("stop"), "Stop Recording", class = "btn btn-stop")),
      column(2, actionButton(ns("reset_trial"), "Reset Trial", class = "btn btn-reset")),
      column(2, actionButton(ns("add_trial"), "Add New Trial", class = "btn btn-add")),
      column(2, downloadButton(ns("downloadData"), "Download Data as CSV", class = "btn btn-download"))
    ),
    tags$br(), tags$br(),
    fluidRow(
      class = "centered-text-inputs",
      class = "centered-buttons",
      column(1, textInput(ns("arm1_count"), "Arm 1 Count", value = "0")),
      column(1, textInput(ns("arm2_count"), "Arm 2 Count", value = "0")),
      column(1, textInput(ns("central_count"), "Center Count", value = "0"))
    ),
    fluidRow(
      class = "centered-buttons",
      actionButton(ns("submit"), "Submit Counts")
    ),
    tags$br(), tags$br(),
    h4("Trial Summary"),
    tableOutput(ns("results"))
  )
}

# Server Module
twoArmServer <- function(id, parent_input) {
  moduleServer(id, function(input, output, session) {
    # Initialize reactive values
    arms <- reactiveValues(
      data = data.frame(
        Trial = character(0),
        Arm = character(0),
        Odour_Source = character(0),
        Count = numeric(0),
        stringsAsFactors = FALSE
      ),
      start_time = NULL,
      trial_running = FALSE,
      elapsed_time = 0,
      total_elapsed_time = 0,
      # Add default odour sources
      arm1_odour = "Odour A",
      arm2_odour = "Odour B",
      center_odour = "Center"
    )
    
    # Start recording
    observeEvent(input$start, {
      # Set default values if inputs are empty
      shiny::insertUI(
        selector = "body",
        where = "afterBegin",
        ui = tags$script(HTML(sprintf(
          "if (!document.getElementById('two_arm_arm1_odour').value) {
             Shiny.setInputValue('two_arm_arm1_odour', '%s');
             document.getElementById('two_arm_arm1_odour').value = '%s';
           }
           if (!document.getElementById('two_arm_arm2_odour').value) {
             Shiny.setInputValue('two_arm_arm2_odour', '%s');
             document.getElementById('two_arm_arm2_odour').value = '%s';
           }",
          arms$arm1_odour, arms$arm1_odour,
          arms$arm2_odour, arms$arm2_odour
        )))
      )
      
      arms$start_time <- Sys.time()
      arms$trial_running <- TRUE
      session$sendCustomMessage(type = "attachKeyListener", message = list())
      shinyjs::disable(selector = 'input')
    })
    
    # Stop recording
    observeEvent(input$stop, {
      arms$trial_running <- FALSE
      arms$start_time <- NULL
      session$sendCustomMessage(type = "detachKeyListener", message = list())
      shinyjs::enable(selector = 'input')
    })
    
    # Add new trial
    observeEvent(input$add_trial, {
      reset_trial_data()
      
      shiny::insertUI(
        selector = "body",
        where = "afterBegin",
        ui = tags$script(sprintf(
          "Shiny.setInputValue('trial_number_two', '%s');",
          as.character(as.numeric(parent_input$trial_number_two) + 1)
        ))
      )
    })
    
    # Reset trial
    observeEvent(input$reset_trial, {
      reset_trial_data()
      reset_counts()
    })
    
    # Helper function to reset trial data
    reset_trial_data <- function() {
      arms$start_time <- NULL
      arms$trial_running <- FALSE
      arms$elapsed_time <- 0
      arms$total_elapsed_time <- 0
      arms$data <- data.frame(
        Trial = character(0),
        Arm = character(0),
        Odour_Source = character(0),
        Count = numeric(0),
        stringsAsFactors = FALSE
      )
    }
    
    # Helper function to reset counts
    reset_counts <- function() {
      updateTextInput(session, "arm1_count", value = "0")
      updateTextInput(session, "arm2_count", value = "0")
      updateTextInput(session, "central_count", value = "0")
    }
    
    # Submit counts
    observeEvent(input$submit, {
      # Convert counts to numeric
      arm1_count <- as.numeric(input$arm1_count)
      arm2_count <- as.numeric(input$arm2_count)
      central_count <- as.numeric(input$central_count)
      
      # Calculate total and validate against released count
      total_count <- arm1_count + arm2_count + central_count
      if (total_count != as.numeric(parent_input$num_released)) {
        showModal(modalDialog(
          title = "Error",
          "The total number of individuals in all arms does not match the number released.",
          easyClose = TRUE,
          footer = NULL
        ))
      } else {
        # Get odour sources with defaults if empty
        arm1_odour <- if (parent_input$two_arm_arm1_odour == "") arms$arm1_odour else parent_input$two_arm_arm1_odour
        arm2_odour <- if (parent_input$two_arm_arm2_odour == "") arms$arm2_odour else parent_input$two_arm_arm2_odour
        
        # Add data
        arms$data <- arms$data %>%
          add_row(Trial = parent_input$trial_number_two, Arm = "Arm 1", 
                  Odour_Source = arm1_odour, Count = round(arm1_count, 0)) %>%
          add_row(Trial = parent_input$trial_number_two, Arm = "Arm 2", 
                  Odour_Source = arm2_odour, Count = round(arm2_count, 0)) %>%
          add_row(Trial = parent_input$trial_number_two, Arm = "Center", 
                  Odour_Source = "Center", Count = round(central_count, 0))
      }
    })
    
    # Timer output
    autoInvalidate <- reactiveTimer(1000)
    
    output$trial_timer <- renderText({
      autoInvalidate()
      if (arms$trial_running) {
        elapsed <- as.numeric(difftime(Sys.time(), arms$start_time, units = "secs")) + 
          arms$total_elapsed_time
      } else {
        elapsed <- arms$total_elapsed_time
      }
      sprintf("%02d:%02d:%02d", floor(elapsed / 3600), floor((elapsed %% 3600) / 60), round(elapsed %% 60))
    })
    
    # Results table
    output$results <- renderTable({
      arms$data %>%
        mutate(across(where(is.numeric), \(x) round(x, 0))) %>%
        rename(
          "Trial" = Trial,
          "Arm" = Arm,
          "Odour Source" = Odour_Source,
          "Count" = Count
        ) %>%
        mutate(Count = as.integer(Count))
    })
    
    # Download handler
    output$downloadData <- downloadHandler(
      filename = function() {
        paste("two_arm_olfactometer_data_", parent_input$trial_number_two, "_", 
              Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        # Summarize and prepare data
        summarized_data <- arms$data %>%
          group_by(Trial, Arm, Odour_Source) %>%
          summarize(
            Count = sum(Count),
            .groups = 'drop'
          )
        
        # Replace underscores in column names with spaces
        colnames(summarized_data) <- gsub("_", " ", colnames(summarized_data))
        
        # Write to CSV
        write.csv(summarized_data, file, row.names = FALSE)
      }
    )
  })
}

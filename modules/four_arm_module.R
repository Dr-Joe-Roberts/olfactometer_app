# modules/four_arm_module.R

# UI Module
fourArmUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    h2("Four-Arm Olfactometer"),
    br(),
    fluidRow(
      column(6, h4("Current Arm"), verbatimTextOutput(ns("current_arm"))),
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
    h4("Trial Summary"),
    tableOutput(ns("arm_times"))
  )
}

# Server Module
fourArmServer <- function(id, parent_input) {
  moduleServer(id, function(input, output, session) {
    # Initialize reactive values
    arms <- reactiveValues(
      current_arm = "Center",
      start_time = NULL,
      times = data.frame(
        Arm = character(0),
        Duration = numeric(0),
        Count = integer(0),
        Odour_Source = character(0),
        Trial = character(0),
        stringsAsFactors = FALSE
      ),
      all_times = data.frame(
        Arm = character(0),
        Duration = numeric(0),
        Count = integer(0),
        Odour_Source = character(0),
        Trial = character(0),
        stringsAsFactors = FALSE
      ),
      trial_start = NULL,
      trial_running = FALSE,
      elapsed_time = 0,
      total_elapsed_time = 0,
      # Add default odour sources
      arm1_odour = "Odour A",
      arm2_odour = "Odour B",
      arm3_odour = "Odour C",
      arm4_odour = "Odour D",
      center_odour = "Center"
    )
    
    # Start recording
    observeEvent(input$start, {
      # Set default values if inputs are empty
      shiny::insertUI(
        selector = "body",
        where = "afterBegin",
        ui = tags$script(HTML(sprintf(
          "if (!document.getElementById('arm1_odour').value) {
             Shiny.setInputValue('arm1_odour', '%s');
             document.getElementById('arm1_odour').value = '%s';
           }
           if (!document.getElementById('arm2_odour').value) {
             Shiny.setInputValue('arm2_odour', '%s');
             document.getElementById('arm2_odour').value = '%s';
           }
           if (!document.getElementById('arm3_odour').value) {
             Shiny.setInputValue('arm3_odour', '%s');
             document.getElementById('arm3_odour').value = '%s';
           }
           if (!document.getElementById('arm4_odour').value) {
             Shiny.setInputValue('arm4_odour', '%s');
             document.getElementById('arm4_odour').value = '%s';
           }",
          arms$arm1_odour, arms$arm1_odour,
          arms$arm2_odour, arms$arm2_odour,
          arms$arm3_odour, arms$arm3_odour,
          arms$arm4_odour, arms$arm4_odour
        )))
      )
      
      arms$start_time <- Sys.time()
      if (is.null(arms$trial_start)) {
        arms$trial_start <- Sys.time()
      }
      arms$trial_running <- TRUE
      arms$current_arm <- "Center"
      session$sendCustomMessage(type = "attachKeyListener", message = list())
      shinyjs::disable(selector = 'input')
    })
    
    # Stop recording
    observeEvent(input$stop, {
      if (!is.null(arms$current_arm) && !is.null(arms$start_time)) {
        record_arm_duration()
      }
      arms$current_arm <- NULL
      arms$trial_running <- FALSE
      arms$start_time <- NULL
      session$sendCustomMessage(type = "detachKeyListener", message = list())
      shinyjs::enable(selector = 'input')
    })
    
    # Add new trial
    observeEvent(input$add_trial, {
      arms$all_times <- bind_rows(arms$all_times, arms$times)
      reset_trial_data()
      
      shiny::insertUI(
        selector = "body",
        where = "afterBegin",
        ui = tags$script(sprintf(
          "Shiny.setInputValue('trial_number_four', '%s');",
          as.character(as.numeric(parent_input$trial_number_four) + 1)
        ))
      )
    })
    
    # Reset trial
    observeEvent(input$reset_trial, {
      reset_trial_data()
    })
    
    # Key press handler
    observeEvent(parent_input$key, {
      if (arms$trial_running) {
        handle_key_press(parent_input$key)
      }
    })
    
    # Helper function to record arm duration
    record_arm_duration <- function() {
      duration <- round(as.numeric(difftime(Sys.time(), arms$start_time, units = "secs")), 2)
      odour_source <- get_odour_source(arms$current_arm)
      
      arms$times <- arms$times %>%
        add_row(
          Arm = arms$current_arm,
          Duration = duration,
          Count = 1,
          Odour_Source = odour_source,
          Trial = parent_input$trial_number_four
        )
      arms$total_elapsed_time <- arms$total_elapsed_time + duration
    }
    
    # Helper function to get odour source
    get_odour_source <- function(arm) {
      switch(arm,
             "Arm 1" = if (parent_input$arm1_odour == "") arms$arm1_odour else parent_input$arm1_odour,
             "Arm 2" = if (parent_input$arm2_odour == "") arms$arm2_odour else parent_input$arm2_odour,
             "Arm 3" = if (parent_input$arm3_odour == "") arms$arm3_odour else parent_input$arm3_odour,
             "Arm 4" = if (parent_input$arm4_odour == "") arms$arm4_odour else parent_input$arm4_odour,
             "Center" = if (parent_input$arm5_odour == "") arms$center_odour else parent_input$arm5_odour)
    }
    
    # Helper function to reset trial data
    reset_trial_data <- function() {
      arms$current_arm <- "Center"
      arms$start_time <- NULL
      arms$times <- data.frame(
        Arm = character(0),
        Duration = numeric(0),
        Count = integer(0),
        Odour_Source = character(0),
        Trial = character(0),
        stringsAsFactors = FALSE
      )
      arms$trial_start <- NULL
      arms$elapsed_time <- 0
      arms$total_elapsed_time <- 0
    }
    
    # Helper function to handle key press
    handle_key_press <- function(key_data) {
      key_time <- as.POSIXct(key_data$time, format="%Y-%m-%dT%H:%M:%OSZ", tz="UTC")
      new_arm <- switch(key_data$key,
                        `1` = "Arm 1",
                        `2` = "Arm 2",
                        `3` = "Arm 3",
                        `4` = "Arm 4",
                        `5` = "Center",
                        arms$current_arm)
      
      if (new_arm != arms$current_arm) {
        if (!is.null(arms$current_arm) && !is.null(arms$start_time)) {
          duration <- round(as.numeric(difftime(key_time, arms$start_time, units = "secs")), 2)
          odour_source <- get_odour_source(arms$current_arm)
          
          arms$times <- arms$times %>%
            add_row(
              Arm = arms$current_arm,
              Duration = duration,
              Count = 1,
              Odour_Source = odour_source,
              Trial = parent_input$trial_number_four
            )
          arms$total_elapsed_time <- arms$total_elapsed_time + duration
        }
        arms$start_time <- key_time
        arms$current_arm <- new_arm
      }
    }
    
    # Outputs
    output$current_arm <- renderText({
      arms$current_arm
    })
    
    # Create summary of times
    summarized_times <- reactive({
      arms$times %>%
        group_by(Trial, Arm, Odour_Source) %>%
        summarize(
          Total_Duration = sum(Duration, na.rm = TRUE),
          Total_Entries = n(),
          .groups = 'drop'
        )
    })
    
    output$arm_times <- renderTable({
      summarized_times() %>%
        rename(
          "Trial" = Trial,
          "Arm" = Arm,
          "Odour Source" = Odour_Source,
          "Total Duration (seconds)" = Total_Duration,
          "Total Entries" = Total_Entries
        )
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
    
    # Download handler
    output$downloadData <- downloadHandler(
      filename = function() {
        paste("four_arm_olfactometer_data_", parent_input$trial_number_four, "_", 
              Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        # Summarize the data
        summarized_data <- bind_rows(arms$all_times, arms$times) %>%
          group_by(Trial, Arm) %>%
          summarize(
            Total_Duration = sum(Duration, na.rm = TRUE),
            Total_Entries = n(),
            Odour_Source = first(Odour_Source),
            .groups = 'drop'
          )
        
        # Create complete dataset with all combinations
        complete_data <- create_complete_dataset(summarized_data)
        
        # Write to CSV
        write.csv(complete_data, file, row.names = FALSE)
      }
    )
    
    # Helper function to create complete dataset
    create_complete_dataset <- function(summarized_data) {
      all_arms <- expand.grid(
        Trial = unique(summarized_data$Trial),
        Arm = c("Arm 1", "Arm 2", "Arm 3", "Arm 4", "Center"),
        stringsAsFactors = FALSE
      ) %>%
        mutate(
          Odour_Source = case_when(
            Arm == "Arm 1" ~ if (parent_input$arm1_odour == "") arms$arm1_odour else parent_input$arm1_odour,
            Arm == "Arm 2" ~ if (parent_input$arm2_odour == "") arms$arm2_odour else parent_input$arm2_odour,
            Arm == "Arm 3" ~ if (parent_input$arm3_odour == "") arms$arm3_odour else parent_input$arm3_odour,
            Arm == "Arm 4" ~ if (parent_input$arm4_odour == "") arms$arm4_odour else parent_input$arm4_odour,
            Arm == "Center" ~ if (parent_input$arm5_odour == "") arms$center_odour else parent_input$arm5_odour
          )
        )
      
      summarized_data %>%
        right_join(all_arms, by = c("Trial", "Arm", "Odour_Source")) %>%
        mutate(
          Total_Duration = ifelse(is.na(Total_Duration), 0, Total_Duration),
          Total_Entries = ifelse(is.na(Total_Entries), 0, Total_Entries)
        ) %>%
        select(Trial, Arm, Odour_Source, Total_Duration, Total_Entries) %>%
        arrange(Trial, Arm)
    }
  })
}

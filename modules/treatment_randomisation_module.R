# modules/treatment_randomisation_module.R

# UI Module
treatmentRandomisationUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    h2("Treatment Randomisation", class = "large-text"),
    br(),
    selectInput(ns("num_treatments"), "Number of Treatments", choices = c(2, 4, 6), selected = 4),
    uiOutput(ns("treatment_inputs")),
    actionButton(ns("randomise"), "Randomise Treatments", class = "btn btn-randomise"),
    tags$br(), tags$br(),
    h4("Randomised Positions", class = "large-text"),
    tableOutput(ns("randomised_positions"))
  )
}

# Server Module
treatmentRandomisationServer <- function(id, parent_input) {
  moduleServer(id, function(input, output, session) {
    # Reactive values to store treatments and randomised positions
    treatments <- reactiveValues(
      original = list(),
      randomised = list()
    )
    
    # Generate treatment input fields based on the number of treatments selected
    output$treatment_inputs <- renderUI({
      ns <- session$ns
      num_treatments <- as.numeric(input$num_treatments)
      input_fields <- lapply(1:num_treatments, function(i) {
        textInput(ns(paste0("treatment", i)), paste("Treatment", i), value = "")
      })
      do.call(tagList, input_fields)
    })
    
    # Observe randomise button click
    observeEvent(input$randomise, {
      # Get treatments from inputs
      num_treatments <- as.numeric(input$num_treatments)
      treatments$original <- lapply(1:num_treatments, function(i) {
        input[[paste0("treatment", i)]]
      })
      
      # Randomise treatments
      treatments$randomised <- sample(treatments$original)
      
      # Update randomised positions in the data collection tabs
      if (num_treatments == 2) {
        updateTextInput(session, "two_arm_arm1_odour", value = treatments$randomised[1])
        updateTextInput(session, "two_arm_arm2_odour", value = treatments$randomised[2])
      } else if (num_treatments == 4) {
        updateTextInput(session, "arm1_odour", value = treatments$randomised[1])
        updateTextInput(session, "arm2_odour", value = treatments$randomised[2])
        updateTextInput(session, "arm3_odour", value = treatments$randomised[3])
        updateTextInput(session, "arm4_odour", value = treatments$randomised[4])
      } else if (num_treatments == 6) {
        updateTextInput(session, "arm1_six_odour", value = treatments$randomised[1])
        updateTextInput(session, "arm2_six_odour", value = treatments$randomised[2])
        updateTextInput(session, "arm3_six_odour", value = treatments$randomised[3])
        updateTextInput(session, "arm4_six_odour", value = treatments$randomised[4])
        updateTextInput(session, "arm5_six_odour", value = treatments$randomised[5])
        updateTextInput(session, "arm6_six_odour", value = treatments$randomised[6])
      }
    })
    
    # Render randomised positions table
    output$randomised_positions <- renderTable({
      data.frame(
        "Position" = 1:length(treatments$randomised),
        "Treatment" = treatments$randomised
      )
    })
  })
}

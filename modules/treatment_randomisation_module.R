# modules/treatment_randomisation_module.R

# UI Module
treatmentRandomisationUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    h2("Treatment Randomisation", class = "large-text"),
    br(),
    fluidRow(
      column(6, textInput(ns("treatment1"), "Treatment 1", value = "")),
      column(6, textInput(ns("treatment2"), "Treatment 2", value = ""))
    ),
    fluidRow(
      column(6, textInput(ns("treatment3"), "Treatment 3", value = "")),
      column(6, textInput(ns("treatment4"), "Treatment 4", value = ""))
    ),
    fluidRow(
      column(6, textInput(ns("treatment5"), "Treatment 5", value = ""))
    ),
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
    
    # Observe randomise button click
    observeEvent(input$randomise, {
      # Get treatments from inputs
      treatments$original <- list(
        input$treatment1,
        input$treatment2,
        input$treatment3,
        input$treatment4,
        input$treatment5
      )
      
      # Randomise treatments
      treatments$randomised <- sample(treatments$original)
      
      # Update randomised positions in the data collection tabs
      updateTextInput(session, "arm1_odour", value = treatments$randomised[1])
      updateTextInput(session, "arm2_odour", value = treatments$randomised[2])
      updateTextInput(session, "arm3_odour", value = treatments$randomised[3])
      updateTextInput(session, "arm4_odour", value = treatments$randomised[4])
      updateTextInput(session, "arm5_odour", value = treatments$randomised[5])
    })
    
    # Render randomised positions table
    output$randomised_positions <- renderTable({
      data.frame(
        "Position" = 1:5,
        "Treatment" = treatments$randomised
      )
    })
  })
}

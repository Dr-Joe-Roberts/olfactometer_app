# app.R

library(shiny)
library(dplyr)
library(lubridate)
library(shinyjs)

# Source the module files
source("modules/four_arm_module.R")
source("modules/two_arm_module.R")
source("modules/six_arm_module.R")
source("modules/treatment_randomisation_module.R")

# UI
ui <- fluidPage(
  useShinyjs(),
  tags$script(HTML("
    function attachKeyListener() {
      document.addEventListener('keydown', keyListener);
    }
    function detachKeyListener() {
      document.removeEventListener('keydown', keyListener);
    }
    function keyListener(event) {
      if(['1', '2', '3', '4', '5'].includes(event.key)) {
        Shiny.onInputChange('key', {key: event.key, time: new Date()});
      } else {
        alert('Invalid key! Please press a key between 1 and 5.');
      }
    }
    document.addEventListener('visibilitychange', function() {
      if (document.visibilityState === 'visible') {
        attachKeyListener();
      } else {
        detachKeyListener();
      }
    });
    Shiny.addCustomMessageHandler('attachKeyListener', function(message) {
      attachKeyListener();
    });
    Shiny.addCustomMessageHandler('detachKeyListener', function(message) {
      detachKeyListener();
    });
  ")),
  includeCSS("custom.css"),
  tags$head(
    tags$link(rel = 'stylesheet', type = 'text/css', href = 'https://fonts.googleapis.com/css?family=Montserrat&display=swap'),
    tags$link(rel = 'icon', type = 'image/png', href = 'HAU-shield.png')
  ),
  titlePanel(
    title = tags$div(
      class = "title-panel",
      tags$div(
        class = "main-container",
        tags$div(
          class = "left-side",
          tags$div(
            tags$span("olfactometeR", class = "left-side-title")
          ),
          tags$div(
            tags$span("An App to Record and Analyse Insect Behavioural Responses in Olfactometer Bioassays", class = "left-side-subtitle")
          )
        ),
        tags$div(
          class = "right-side",
          tags$a(
            href = 'https://www.harper-adams.ac.uk/',
            tags$img(
              src = 'HAU.png',  
              height = 70,
              width = 215
            ),
            target = '_blank',
            class = 'right-side-logo'
          )
        )
      )
    ),
    windowTitle = 'olfactometeR'
  ),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h3("Olfactometer Type", class = "large-text"),
      selectInput("olfactometer_type", "Select Olfactometer Type:", 
                  choices = list("Four-Arm Olfactometer" = "four", 
                                 "Two-Arm Olfactometer" = "two", 
                                 "Six-Arm Olfactometer" = "six")),
      
      # Four-arm olfactometer inputs
      conditionalPanel(
        condition = "input.olfactometer_type == 'four'",
        h3("Arm Assignments", class = "large-text"),
        fluidRow(
          column(4, tags$span("Arm 1", class = "arm-key"),
                 tags$div("Key 1", align = "left", class = "key-label")),
          column(8, textInput("arm1_odour", "Odour Source:", value = ""))
        ),
        fluidRow(
          column(4, tags$span("Arm 2", class = "arm-key"),
                 tags$div("Key 2", align = "left", class = "key-label")),
          column(8, textInput("arm2_odour", "Odour Source:", value = ""))
        ),
        fluidRow(
          column(4, tags$span("Arm 3", class = "arm-key"),
                 tags$div("Key 3", align = "left", class = "key-label")),
          column(8, textInput("arm3_odour", "Odour Source:", value = ""))
        ),
        fluidRow(
          column(4, tags$span("Arm 4", class = "arm-key"),
                 tags$div("Key 4", align = "left", class = "key-label")),
          column(8, textInput("arm4_odour", "Odour Source:", value = ""))
        ),
        fluidRow(
          column(4, tags$span("Center", class = "arm-key"),
                 tags$div("Key 5", align = "left", class = "key-label")),
          column(8, textInput("arm5_odour", "Center:", value = "Center"))
        ),
        textInput("trial_number_four", "Trial Number", value = "1")
      ),
      
      # Two-arm olfactometer inputs
      conditionalPanel(
        condition = "input.olfactometer_type == 'two'",
        textInput("num_released", "Number of Individuals Released", value = "1"),
        h3("Arm Assignments", class = "large-text"),
        fluidRow(
          column(4, tags$span("Arm 1", class = "arm-key")),
          column(8, textInput("two_arm_arm1_odour", "Odour Source:", value = ""))
        ),
        fluidRow(
          column(4, tags$span("Arm 2", class = "arm-key")),
          column(8, textInput("two_arm_arm2_odour", "Odour Source:", value = ""))
        ),
        fluidRow(
          column(4, tags$span("Center", class = "arm-key")),
          column(8, textInput("two_arm_arm5_odour", "Center:", value = "Center"))
        ),
        textInput("trial_number_two", "Trial Number", value = "1")
      ),
      
      # Six-arm olfactometer inputs
      conditionalPanel(
        condition = "input.olfactometer_type == 'six'",
        textInput("num_released_six", "Number of Individuals Released", value = "1"),
        h3("Arm Assignments", class = "large-text"),
        fluidRow(
          column(4, tags$span("Arm 1", class = "arm-key")),
          column(8, textInput("arm1_six_odour", "Odour Source:", value = ""))
        ),
        fluidRow(
          column(4, tags$span("Arm 2", class = "arm-key")),
          column(8, textInput("arm2_six_odour", "Odour Source:", value = ""))
        ),
        fluidRow(
          column(4, tags$span("Arm 3", class = "arm-key")),
          column(8, textInput("arm3_six_odour", "Odour Source:", value = ""))
        ),
        fluidRow(
          column(4, tags$span("Arm 4", class = "arm-key")),
          column(8, textInput("arm4_six_odour", "Odour Source:", value = ""))
        ),
        fluidRow(
          column(4, tags$span("Arm 5", class = "arm-key")),
          column(8, textInput("arm5_six_odour", "Odour Source:", value = ""))
        ),
        fluidRow(
          column(4, tags$span("Arm 6", class = "arm-key")),
          column(8, textInput("arm6_six_odour", "Odour Source:", value = ""))
        ),
        fluidRow(
          column(4, tags$span("Center", class = "arm-key")),
          column(8, textInput("arm7_six_odour", "Center:", value = "Center"))
        ),
        textInput("trial_number_six", "Trial Number", value = "1")
      )
    ),
    mainPanel(
      tabsetPanel(id = "tabs",
                  tabPanel("Instructions", 
                           uiOutput("introduction"),
                           h3("Using the App"),
                           tags$ol(
                             tags$li("Select Olfactometer Type: Choose the type of olfactometer you are using from the dropdown menu in the sidebar."),
                             tags$li("Enter Arm Assignments: Depending on the selected olfactometer type, enter the odour source for each arm."),
                             tags$li("Set Trial Information: Provide the trial number and other relevant details."),
                             tags$li("Start Recording: Press the 'Start Recording' button to begin the trial."),
                             tags$li("Record Arm Entries: Press the number key corresponding to each arm (1-6) as insects move between arms."),
                             tags$li("Stop Recording: Press the 'Stop Recording' button to end the trial."),
                             tags$li("Review and Download Data: Review the trial summary and download the data as a CSV file."),
                             tags$li("Reset or Add New Trial: Use the 'Reset Trial' or 'Add New Trial' buttons as needed.")
                           ),
                           br(),
                           h3("Best Practices"),
                           HTML('<p>The following table summarises best practices for conducting insect olfactometer bioassays. These guidelines are based on the comprehensive review provided in the paper by <a href="https://doi.org/10.1111/eea.13351" target="_blank">Roberts et al. (2023)</a>, which outlines critical considerations to ensure the reliability and reproducibility of bioassay results.</p>'),
                           tableOutput("best_practices")
                  ),
                  tabPanel("Four-Arm Olfactometer",
                           fourArmUI("four_arm")
                  ),
                  tabPanel("Two-Arm Olfactometer",
                           twoArmUI("two_arm")
                  ),
                  tabPanel("Six-Arm Olfactometer",
                           sixArmUI("six_arm")
                  ),
                  tabPanel("Treatment Randomisation",
                           treatmentRandomisationUI("treatment_randomisation")
                  )
      )
    )
  ),
  tags$footer(
    class = "footer",
    tags$a(href = 'https://github.com/Dr-Joe-Roberts/olfactometer_app', target = '_blank',
           tags$img(src = 'https://github.githubassets.com/favicons/favicon.png', height = '25', width = '25')
    )
  )
)

# Server
server <- function(input, output, session) {
  # Tab synchronization
  observeEvent(input$olfactometer_type, {
    if (input$olfactometer_type == "four") {
      updateTabsetPanel(session, "tabs", selected = "Four-Arm Olfactometer")
    } else if (input$olfactometer_type == "two") {
      updateTabsetPanel(session, "tabs", selected = "Two-Arm Olfactometer")
    } else if (input$olfactometer_type == "six") {
      updateTabsetPanel(session, "tabs", selected = "Six-Arm Olfactometer")
    }
  })
  
  observeEvent(input$tabs, {
    if (input$tabs == "Four-Arm Olfactometer") {
      updateSelectInput(session, "olfactometer_type", selected = "four")
    } else if (input$tabs == "Two-Arm Olfactometer") {
      updateSelectInput(session, "olfactometer_type", selected = "two")
    } else if (input$tabs == "Six-Arm Olfactometer") {
      updateSelectInput(session, "olfactometer_type", selected = "six")
    }
  })
  
  # Call the module servers
  fourArmServer("four_arm", input)
  twoArmServer("two_arm", input)
  sixArmServer("six_arm", input)
  treatmentRandomisationServer("treatment_randomisation", input)
  
  # Introduction UI
  output$introduction <- renderUI({
    HTML('
    <br>
    <h2>Introduction</h2>
    <p>This Shiny web app is designed to help you collect data from an insect olfactometer bioassays. There are currently three olfactometer designs supported: (1) four-arm olfactometers, (2) two-arm olfactometers, and (3) six-arm olfactometers. Data can be downloaded as a CSV file.</p>
    <br>')
  })
  
  # Best practices table
  output$best_practices <- renderTable({
    data.frame(
      "Consideration" = c(
        "Olfactometer Design", 
        "Environment", 
        "Stimuli Presentation", 
        "Cleaning Procedures", 
        "Physiological State", 
        "Releasing Organisms"
      ),
      "Best Practices" = c(
        "Ensure the design suits the research question and species; use chemically inert materials; pilot test configurations.", 
        "Report and control temperature and humidity; use consistent lighting; minimise external stimuli.",
        "Avoid mechanical damage to biological materials; consider solvent properties for synthetic chemicals.",
        "Thoroughly clean between replicates; use fragrance-free detergents; appropriately clean or replace activated charcoal filters.",
        "Report and standardise the physiological state; account for time-of-day effects; use blocking in design and analysis.",
        "Decide between individual or group releases based on interaction and social behaviors; treat each group release as a single replicate."
      ),
      stringsAsFactors = FALSE
    )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

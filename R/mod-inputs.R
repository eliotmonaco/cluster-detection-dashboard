# Modules for user inputs

# # Data selection
# data_select_ui <- function(id, choices) {
#   dateInput(
#     inputId = NS(id, "date"),
#     label = input_tooltip(
#       "Analysis date",
#       paste(
#         "Data is downloaded and analyses are run daily. Changing the analysis",
#         "date will change the dataset and analysis results that are displayed",
#         "throughout the dashboard."
#       )
#     ),
#     value = max(choices),
#     min = min(choices),
#     max = max(choices)
#   )
# }

# data_select_server <- function(id, rv, data) {
#   moduleServer(id, function(input, output, session) {
#     observe({
#       rv$date <- input$date
#     })
#
#     # Get all data for a specific analysis date
#     data <- reactive({
#       get_db_data(data, rv$date)
#     })
#
#     observe({
#       rv$data <- data()
#     })
#
#     # Update selected analysis date when analysis date input is changed
#     observeEvent(rv$date, {
#       updateDateInput(session, "date", value = rv$date)
#     })
#   })
# }

# Syndrome selection
syn_select_ui <- function(id, choices, icons) {
  div(
    selectizeInput(
      inputId = NS(id, "syn"),
      label = input_tooltip(
        "Syndrome",
        paste(
          "The color of the circle next to a syndrome indicates the highest",
          "recurrence interval (RI) level of any clusters detected for that",
          "syndrome. Refer to the RI selector for the level associated with",
          "each color."
        )
      ),
      choices = choices,
      options = list(
        render = I("{
        item: function(item, escape) {
          return (
            '<div class=\"syn-flex-container\" '
            + 'style=\"display:flex; flex-wrap:nowrap; align-items:baseline;\">'
            + item.label + '</div>'
          );
        },
        option: function(item, escape) {
          return (
            '<div style=\"display:flex; flex-wrap:nowrap;'
            + 'align-items:baseline;\">' + item.label + '</div>'
          );
        }
      }")
      )
    ),
    class = "syn-select-input"
  )
}

syn_select_server <- function(id, rv, color) {
  moduleServer(id, function(input, output, session) {
    observe({
      rv$syn <- input$syn
    })

    # # Get syndrome list with icons for syndrome input
    # synchoices <- reactive({
    #   get_syn_choices(
    #     df = rv$data$syndromes,
    #     ls = rv$data$satscan_results,
    #     colors = rv$aux$ri_bg
    #   )
    # })
    #
    # # Update syndrome choices when syndrome list changes
    # observeEvent(synchoices(), {
    #   updateSelectInput(
    #     session, "syn",
    #     choices = synchoices(), selected = rv$syn
    #   )
    # })

    # Update selected syndrome when syndrome input is changed
    observeEvent(rv$syn, {
      updateSelectInput(session, "syn", selected = rv$syn)
    })
  })
}

# Recurrence interval selection
ri_select_ui <- function(id, choices) {
  div(
    radioButtons(
      inputId = NS(id, "ri"),
      label = input_tooltip(
        "Minimum recurrence interval (RI)",
        paste(
          "The recurrence interval (RI) reflects the frequency that a cluster",
          "of the observed likelihood would occur by chance. An RI of 100 days",
          "indicates that a false positive is expected once in 100 days, while",
          "an RI of 100 years indicates that a false positive is expected once",
          "in 100 years."
        )
      ),
      choices = choices,
      selected = choices[[2]]
    ),
    class = "ri-select-input"
  )
}

ri_select_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    observe({
      rv$ri <- as.numeric(input$ri)
    })

    observeEvent(rv$ri, {
      updateRadioButtons(session, "ri", selected = rv$ri)
    })
  })
}

# Map zoom level selection
zoom_select_ui <- function(id) {
  numericInput(
    inputId = NS(id, "zoom"),
    label = input_tooltip(
      "Default map zoom level",
      "Increase or decrease the default map magnification for the session."
    ),
    value = 9,
    min = 0
  )
}

zoom_select_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    observe({
      rv$zoom <- input$zoom
    })
  })
}

# Days selection
days_select_ui <- function(id, choices) {
  radioButtons(
    inputId = NS(id, "days"),
    label = input_tooltip(
      "Date range",
      "Choose the amount of time to display in the time series."
    ),
    choices = choices
  )
}

days_select_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    observe({
      rv$days <- as.numeric(input$days)
    })
  })
}


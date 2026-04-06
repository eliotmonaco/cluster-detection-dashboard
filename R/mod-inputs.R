# Modules for user inputs

# Data selection
data_select_ui <- function(id, choices) {
  dateInput(
    inputId = NS(id, "date"),
    label = "Analysis date",
    value = max(choices),
    min = min(choices),
    max = max(choices)
  )
}

data_select_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    observe({
      rv$date <- input$date
    })

    # Get all data for a specific analysis date
    data <- reactive({
      get_db_data(dbdata, rv$date)
    })

    observe({
      rv$data <- data()
    })

    # Update selected analysis date when analysis date input is changed
    observeEvent(rv$date, {
      updateDateInput(session, "date", value = rv$date)
    })
  })
}

# Syndrome selection
syn_select_ui <- function(id, choices) {
  selectInput(
    inputId = NS(id, "syn"),
    label = "Syndrome",
    choices = choices,
    multiple = FALSE
  )
}

syn_select_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    observe({
      rv$syn <- input$syn
    })

    # Get syndrome list when data is updated
    synselect <- reactive({
      syn_select_list(rv$data$syndromes)
    })

    # Update syndrome selections when syndrome list changes
    observeEvent(synselect(), {
      rv$synselect <- synselect()

      updateSelectInput(
        session, "syn", choices = rv$synselect, selected = rv$syn
      )
    })

    # Update selected syndrome when syndrome input is changed
    observeEvent(rv$syn, {
      updateSelectInput(session, "syn", selected = rv$syn)
    })
  })
}

# P-value selection
pval_select_ui <- function(id) {
  checkboxInput(
    inputId = NS(id, "pval"),
    label = HTML("Show clusters where p&nbsp;<&nbsp;0.05 only"),
    value = TRUE
  )
}

pval_select_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    observe({
      rv$pval <- input$pval
    })
  })
}

# Map zoom level selection
zoom_select_ui <- function(id) {
  numericInput(
    inputId = NS(id, "zoom"),
    label = "Default map zoom level",
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
    label = "Date range",
    choices = choices
  )
}

days_select_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    observe({
      rv$days <- input$days
    })
  })
}


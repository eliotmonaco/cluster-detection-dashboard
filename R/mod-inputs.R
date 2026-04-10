# Modules for user inputs

# Data selection
data_select_ui <- function(id, choices) {
  dateInput(
    inputId = NS(id, "date"),
    label = input_tooltip(
      "Analysis date",
      paste(
        "Data is downloaded and analyses are run daily. Changing the analysis",
        "date will change the dataset and analysis results that are displayed",
        "on the dashboard."
      )
    ),
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
syn_select_ui <- function(id, choices, icons) {
  div(
    selectizeInput(
      inputId = NS(id, "syn"),
      label = input_tooltip(
        "Syndrome",
        paste(
          "The dropdown list contains the syndromes available for the current",
          "analysis date. The icon color indicates the maximum recurrence",
          "interval strength of any clusters detected for a given syndrome."
        )
      ),
      choices = choices,
      multiple = FALSE,
      options = list(
        render = I("{
        item: function(item, escape) {
          return (
            '<div style=\"display:flex; flex-wrap:nowrap;'
            + 'align-items:baseline;\">' + item.label + '</div>'
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

    # Get syndrome list when data is updated
    synlist <- reactive({
      get_syn_choices(rv$data$syndromes)
    })

    # Get syndrome list with icons for syndrome input
    synchoices <- reactive({
      add_ri_icons(
        syn = synlist(),
        str = get_syn_cluster_strength(rv$data$satscan_results),
        colors = ri_bg_color
      )
    })

    observeEvent(synlist(), {
      rv$synlist <- synlist()
    })

    # Update syndrome choices when syndrome list changes
    observeEvent(synchoices(), {
      updateSelectInput(
        session, "syn",
        choices = synchoices(), selected = rv$syn
      )
    })

    # Update selected syndrome when syndrome input is changed
    observeEvent(rv$syn, {
      updateSelectInput(session, "syn", selected = rv$syn)
    })
  })
}

# Recurrence interval selection
ri_select_ui <- function(id, choices) {
  radioButtons(
    inputId = NS(id, "ri"),
    label = input_tooltip(
      "Minimum recurrence interval",
      paste(
        "The recurrence interval (RI) is a measure of the likelihood of a",
        "cluster. A higher RI indicates a higher likelihood and vice versa."
      )
    ),
    choices = choices,
    selected = choices[[2]]
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
      rv$days <- as.numeric(input$days)
    })
  })
}


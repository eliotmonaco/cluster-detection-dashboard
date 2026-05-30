# Functions for user inputs

# Get data and analysis results for one date from dashboard data
get_db_data <- function(ls, date, name = NULL) {
  ls <- ls[[names(ls)[grepl(gsub("-", "", date), names(ls))]]]

  if (!is.null(name)) {
    ls[[name]]
  } else {
    ls
  }
}

# Wrap an input label in `bslib::tooltip()`
input_tooltip <- function(label, tooltip_text) {
  bslib::tooltip(
    list(
      label,
      fontawesome::fa(
        "circle-question", fill = "#007bc2",
        height = ".7em", vertical_align = "top"
      )
    ),
    tooltip_text,
    placement = "right"
  )
}

# Custom sidebar
sidebar <- function(...) {
  bslib::sidebar(
    ...,
    width = 310,
    bg = "#e4f3ff"
  )
}


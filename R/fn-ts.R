# Functions for time series plot

# Configure data for time series plot
config_ts_plot_data <- function(
  ls,
  src = c("hospital", "patient"),
  syndrome,
  n_days
) {
  src <- match.arg(src)

  df <- ls[[src]][[syndrome]]

  start <- max(df$date) - n_days + 1

  df |>
    dplyr::filter(date >= start)
}


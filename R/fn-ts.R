# Functions for time series plot

# Convert time series dataframe to a list for Highcharter function
df_to_hc_list <- function(df) {
  list(
    list(
      data = lapply(1:nrow(df), \(r) {
        list(
          x = highcharter::datetime_to_timestamp(df[r, "date"]),
          y = df[r, "count"],
          color = df[r, "alert_fill"],
          marker = list(
            symbol = df[r, "alert_symbol"],
            radius = df[r, "alert_radius"],
            lineWidth = df[r, "alert_line"],
            lineColor = df[r, "alert_color"]
          ),
          alert_status = df[r, "alert_status"]
        )
      })
    )
  )
}

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
    dplyr::filter(date >= start) |>
    df_to_hc_list()
}

# Time series plot
ts_plot <- function(ls, title) {
  highchart() |>
    hc_add_series_list(ls) |>
    hc_xAxis(
      type = "datetime",
      title = list(text = "Date"),
      labels = list(format = "{value:%b %d}")
    ) |>
    hc_yAxis(
      title = list(text = "Count")
    ) |>
    hc_legend(enabled = FALSE) |>
    hc_tooltip(formatter = JS(
      "function() {
        const dt = new Date(this.x);
        return dt.toDateString() + '<br>' +
        `Count: <b>${this.y}</b>` + '<br>' +
        `Alert status: <b>${this.point.alert_status}</b>`;
      }"
    )) |>
    hc_title(text = title)
}


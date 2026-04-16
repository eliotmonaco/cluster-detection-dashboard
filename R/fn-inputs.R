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

# Create a list of choices for syndrome select input
get_syn_choices <- function(ls) {
  syn <- as.list(names(ls))

  names(syn) <- lapply(ls, \(x) x$name1)

  syn
}

# Create a list with the max RI level of any cluster for each syndrome
# ls = satscan results
get_max_ri_level <- function(ls) {
  # Get `shapeclust` dataframe for each syndrome
  ss <- lapply(ls, \(src) {
    lapply(src, \(syn) {
      sf::st_drop_geometry(syn$shapeclust)
    })
  })

  # Within each syndrome, combine the dataframes from each source
  ss <- purrr::map2(ss$patient, ss$hospital, rbind)

  # Get the max RI level for each syndrome
  lapply(ss, \(df) {
    if (is.data.frame(df)) {
      row <- suppressWarnings(
        which(df$recurr_int == max(df$recurr_int))[1]
      )

      as.character(df[row, "ri_level"])
    } else {
      NA
    }
  })
}

# Create icons indicating the max RI level for the syndrome input list
# syn = syndrome input choice list
# str = output of `get_max_ri_level()`
add_ri_icons <- function(syn, str, colors) {
  ls <- mapply(str, names(syn), FUN = \(x, y) {
    syn_html <- paste(
      "<div class='syn-icon' style='flex-shrink:0; background: %s;",
      "width: 12px; height: 12px; border: 1px solid %s; border-radius: 50%%;",
      "margin: 0 5px;'></div><div class='syn-text'>%s</div>"
    )

    border <- "#aaa"

    if (is.na(x)) {
      sprintf(syn_html, "white", border, y)
    } else if (x == "very weak") {
      sprintf(syn_html, colors[1], border, y)
    } else if (x == "weak") {
      sprintf(syn_html, colors[2], border, y)
    } else if (x == "moderate") {
      sprintf(syn_html, colors[3], border, y)
    } else if (x == "strong") {
      sprintf(syn_html, colors[4], border, y)
    } else if (x == "very strong") {
      sprintf(syn_html, colors[5], border, y)
    }
  })

  ls2 <- names(ls)

  names(ls2) <- ls

  ls2
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


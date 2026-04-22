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
# df = syndrome table
# ls = satscan results
# colors = RI background colors
get_syn_choices <- function(df, ls, colors) {
  max_ri <- get_max_ri_level(ls)

  if (nrow(df) != length(max_ri)) {
    stop("The number of syndromes in `df` and `ls` must be equal")
  }

  df$html <- mapply(max_ri, df$name1, FUN = \(x, y) {
    add_ri_icon(lvl = x, text = y, colors = colors)
  })

  # Divide syndromes by category
  cats <- unique(df$category)

  ls2 <- lapply(cats, \(x) {
    df2 <- df |>
      dplyr::filter(category == x)

    ls <- as.list(df2$abbr)

    names(ls) <- df2$html

    ls
  })

  names(ls2) <- stringr::str_to_sentence(cats)

  ls2
}

# Create a list with the max RI level of any cluster for each syndrome
# ls = satscan results
get_max_ri_level <- function(ls) {
  # Get `shapeclust` dataframe for each syndrome
  ss <- lapply(ls, \(src) {
    lapply(src, \(syn) {
      if (is.data.frame(syn$shapeclust) && nrow(syn$shapeclust) > 0) {
        sf::st_drop_geometry(syn$shapeclust)
      } else {
        NA
      }
    })
  })

  # Within each syndrome, combine the dataframes from each source
  ss <- purrr::map2(ss$patient, ss$hospital, rbind)

  # Get the max RI level for each syndrome
  lapply(ss, \(df) {
    if (is.data.frame(df)) {
      row <- suppressWarnings(
        which(df$recurr_int == max(df$recurr_int, na.rm = TRUE))[1]
      )

      as.character(df[row, "ri_level"])
    } else {
      NA
    }
  })
}

# Wrap syndrome name in div with icon representing RI level
add_ri_icon <- function(lvl, text, colors) {
  syn_html <- paste(
    "<div class='syn-icon' style='flex-shrink:0; background: %s;",
    "width: 12px; height: 12px; border: 1px solid %s; border-radius: 50%%;",
    "margin: 0 5px;'></div><div class='syn-text'>%s</div>"
  )

  border <- "#8D959E"

  if (is.na(lvl)) {
    sprintf(syn_html, "white", border, text)
  } else if (lvl == "very weak") {
    sprintf(syn_html, colors[1], border, text)
  } else if (lvl == "weak") {
    sprintf(syn_html, colors[2], border, text)
  } else if (lvl == "moderate") {
    sprintf(syn_html, colors[3], border, text)
  } else if (lvl == "strong") {
    sprintf(syn_html, colors[4], border, text)
  } else if (lvl == "very strong") {
    sprintf(syn_html, colors[5], border, text)
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


# Functions for using text in the dashboard UI

# Extract text from a markdown file
get_md_text <- function(file, delim) {
  txt <- shiny::markdown(readLines(file))

  strsplit(txt, split = delim, fixed = TRUE)[[1]][2]
}

# Return a heading tag for a syndrome
syndrome_title_tag <- function(x, df) {
  shiny::tags$h3(
    df[df$abbr == x, "name1"],
    class = "cluster-tab-title"
  )
}


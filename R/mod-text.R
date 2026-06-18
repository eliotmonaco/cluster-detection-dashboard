# Modules for displaying text

# Markdown text
md_text_ui <- function(id) {
  textOutput(NS(id, "mdtext"))
}

md_text_server <- function(id, file, delim) {
  moduleServer(id, function(input, output, session) {
    output$mdtext <- renderText({
      get_md_text(file, delim)
    })
  })
}

# Heading with syndrome name
syn_heading_ui <- function(id) {
  htmlOutput(NS(id, "synheader"))
}

syn_heading_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    # Syndrome heading on clusters page
    output$synheader <- renderUI({
      syndrome_title_tag(rv$syn, rv$data$syndromes)
    })
  })
}


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

# Create list options for syndrome select input
syn_select_list <- function(ls) {
  ls2 <- as.list(names(ls))

  names(ls2) <- lapply(ls, \(x) x$name1)

  ls2
}


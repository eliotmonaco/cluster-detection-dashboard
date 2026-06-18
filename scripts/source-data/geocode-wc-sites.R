# Geocode World Cup sites

library(tidygeocoder)
library(tidyverse)

wcsites <- read.csv("data/prep/worldcup-sites-20260414.csv")

wcsites <- wcsites |>
  mutate(
    address = str_trim(sub("/", " and ", address)),
    address = str_replace(address, "KCMO", "Kansas City, Missouri")
  )

wcsites_gc <- geocode(
  wcsites,
  address = "address",
  method = "arcgis",
  full_results = TRUE
)

saveRDS(wcsites_gc, "data/prep/worldcup_sites.rds")


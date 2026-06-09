# Get data needed to determine values for `residence` in data details

library(sf)
library(tigris)
library(tidyverse)

options(tigris_use_cache = TRUE)

# Get Missouri places shapefile
mo_places <- places(state = 29)

# Get KC and Columbia centroids
cent_kc <- st_centroid(filter(mo_places, GEOID == 2938000))

cent_col <- st_centroid(filter(mo_places, GEOID == 2915670))

# Get distance between KC and Columbia and create a buffer around KC
dist <- st_distance(cent_kc, cent_col)

kc_buff <- st_buffer(cent_kc, dist)

# Get intersection of ZCTAs and `kc_buff`
us_zctas <- zctas()

kc_region_zctas <- st_intersection(us_zctas, kc_buff)

ggplot(kc_region_zctas) +
  geom_sf()

kc_region_zctas <- kc_region_zctas$ZCTA5CE20

# Get places within buffer
places <- list(
  mo = places(state = 29),
  ks = places(state = 20),
  ia = places(state = 19),
  ne = places(state = 31)
)

places <- do.call(rbind, places)

kc_region_places <- st_intersection(places, kc_buff)

# Create list for regex pattern matching
kc_region_places <- kc_region_places |>
  st_drop_geometry() |>
  mutate(state = case_when(
    STATEFP == "29" ~ "MO",
    STATEFP == "20" ~ "KS",
    STATEFP == "19" ~ "IA",
    STATEFP == "31" ~ "NE"
  )) |>
  select(city = NAME, state) |>
  filter_out(city == "Kansas City", state == "MO") |>
  mutate(
    city = str_replace_all(
      city, "([\\w\\s]*)([:punct:])([\\w\\s]*)", "\\1\\2?\\3"
    ),
    city = str_replace(city, "St\\.\\?", "(St.?|Saint)"),
    city = str_replace(city, "City", "Ci?ty")
  )

rownames(kc_region_places) <- NULL

kc_region_places <- split(kc_region_places, f = kc_region_places$state)

names(kc_region_places) <- tolower(names(kc_region_places))

# Save
ls <- list(
  kcarea_zctas = kc_region_zctas,
  kcarea_places = kc_region_places,
  us_zctas = us_zctas$ZCTA5CE20
)

saveRDS(ls, "data/prep/residence_data.rds")


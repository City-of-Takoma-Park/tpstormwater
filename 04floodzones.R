# purpose - visualize georeferenecd/digitized flood areas from public works

library(leaflet)
library(leafletwrappers)
library(tidyverse)
library(sf)

# read in
flood_areas <- st_read("./data/processed/flood_projected.geojson") %>%
  filter(!is.na(id))

# create area field
flood_areas <- flood_areas %>%
  mutate(area = as.numeric(st_area(.)),
         id = seq(1, nrow(.)))

flood_df <- st_drop_geometry(flood_areas)

color_flood <- pal_numeric(var = "area", "Blues", df = st_drop_geometry(flood_areas))


# generate map
map_flood_areas <- leaflet(flood_areas) %>%
  addTiles() %>%
  addpoly_legend(title_select = "Floodzone area (meters squared)", .polfillopacity = 0.7,.polweight = 1.1, .pollinecolor = "grey", df_select  = flood_df, pal_funct_select = color_flood, variable_select = "area", group_select = "Flood areas", labels_select = label_output(flood_df, "ID: {id} <p></p>
                                                                                                                           Area: {area %>% tpfuncts::commafy()} m^2")) %>%
  add_wards_new()


st_write(flood_areas, "./data/output/flood_areas.geojson", delete_dsn = T)

htmlwidgets::saveWidget(map_flood_areas, "./data/output/map_flood_areas.html")

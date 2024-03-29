# purpose - map stormwater sites and streetscapes identified by public works
# date created: 4/19/2022

library(tidyverse)
library(sf)
library(leaflet)
library(leafletwrappers)
library(tidygeocoder)

# read in
bioretent <- openxlsx::read.xlsx("./data/contract_sites/contract sites.xlsx", sheet = "Bioretention Sites") %>%
  rename_all(tolower) %>%
  mutate(addressfull = paste0(location, ", Takoma Park, MD")) %>%
  rename(area.sq.ft. =  `area.(sq.ft.)`)

biocols <- colnames(bioretent)

bioretent_addresses <- tidygeocoder::geo(bioretent$addressfull %>% unique(), full_results = T, return_type = "geographies", method = "census")


bioretent_shp <- bioretent_addresses %>%
  # filter(!is.na(long)) %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326, na.fail = F) %>%
  left_join(bioretent, by= c("address" = "addressfull")) %>%
  st_cast("POINT")


saveRDS(bioretent_shp, file = "./data/output/bioretent_shp.rds")


# read in missing bioretention corrected by public works
bioretent_missing_fixed <- read.csv("./data/processed/bioretent_missing_fixed.csv")

bioretent_missing_adjst <- bioretent_missing_fixed %>%
  filter(GPS.coordinates..Anna. != "repeat") %>%
  select(address, grep("addressfull", biocols, invert = T, value = T), GPS.coordinates..Anna.) %>%
  separate(GPS.coordinates..Anna., into = c("lat", "long"), sep = ",") %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326, na.fail = F)


bioretent_shp_fixed <- bioretent_shp %>%
  select(address, grep("addressfull", biocols, invert = T, value = T)) %>%
  filter(!address %in% bioretent_missing_adjst$address) %>%
  rbind(bioretent_missing_adjst)

# bioretention_new <- st_read("./data/source/Bioretention_Area_new/Bioretention_Area.shp") %>%
#   st_transform(4326)

# read in and create sf from streetscapes
streetscape <- openxlsx::read.xlsx("./data/contract_sites/contract sites.xlsx", sheet = "Streetscape Sites (UPDATED)") %>%
  rename_all(tolower) %>%
  mutate(type = str_to_title(type))

lat_long <- str_split(streetscape$gps.coordinates, pattern = ", ")
lat <- map_chr(lat_long, ~ .x[1])
long <- map_chr(lat_long, ~ .x[2])

streetscape_shp <- streetscape %>%
  mutate(lat = lat,
         long = long) %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326, na.fail = F) %>%
  st_cast("POINT")

# create palette bioretention
pal_bioretent <- pal_numeric(var = "area.sq.ft.", df = st_drop_geometry(bioretent_shp_fixed), colors = "PRGn")

# bioretention_pnt <- bioretention_new %>%
#   st_drop_geometry() %>%
#   mutate(geometry = st_centroid(bioretention_new$geometry)) %>%
#   st_as_sf(sf_column_name = "geometry") %>%
#   mutate(Completion = as.character(Completion),
#          Completion = case_when(Completion == 0 ~ "Missing",
#                                 T ~ Completion))

p <- "<p></p>"

bio_labs <- leafletwrappers::label_output(st_drop_geometry(bioretent_shp_fixed),
                                          label_text =
                                            "Site name: {site.name}{p}
                                                 Type: {type}{p}
                                                 Location: {location}{p}
                                          Area (square feet): {area.sq.ft.}{p}")
# bio_col <- c(RColorBrewer::brewer.pal(8, "Dark2"), RColorBrewer::brewer.pal(8, "Set1"))
#
#
# pal_bio <- colorFactor(palette = bio_col, domain = bioretention_pnt$Completion %>% sort())

# functions for mapping bioretention structures and streetscapes
bio_marker_leg <- function(basemap) {
  basemap %>%
    addCircleMarkers(
      radius = 0.1,
      color = "#BF40BF",
      group = "Bioretention structures",
      stroke = T,
      weight = 10,
      opacity = 0.8,
      data = bioretent_shp_fixed,
      popup = ~ bio_labs,
    )
    # addLegend(pal = "#CF9FFF",
    #           values = ~ area.sq.ft.,
    #           group = "Bioretention structures",
    #           data = bioretent_shp_fixed,
    #           title = "Bioretention area (sq ft.)")
}

pal_type <- leaflet::colorFactor(palette = RColorBrewer::brewer.pal(5, "Set1")[-4], domain = streetscape_shp$type)

street_labs <- label_output(streetscape_shp, "Site name: {site.name}{p}
                            Typer: {type}{p}
                            Location: {location}{p}
                            Area: {area}")

map_streetscape <- function(basemap){
  basemap %>%
    addCircleMarkers(radius = 0.1, group = "Streetscapes", stroke = T, weight = 10, data = streetscape_shp, popup = ~ street_labs, color = ~ pal_type(type)) %>%
    addLegend(pal = pal_type, values = ~ type, group = "Streetscapes", data = streetscape_shp, title = "Streetscapes")
}

# function for generating map with different provider tiles
map_bioretent <- function(providertype = "Esri"){
  leaflet() %>%
    addProviderTiles(providers[[providertype]]) %>%
    bio_marker_leg() %>%
    map_streetscape() %>%
    addLayersControl(overlayGroups = c("Bioretention structures", "Streetscapes"), position = "topleft", options = layersControlOptions(collapsed = F)) %>%
    hideGroup("Streetscapes") %>%
    add_wards_new() %>%
    addControl(position = "bottomleft", html = "Click on the dot to see more information on the streetscape or bioretention structure") %>%
    add_wards_new()

}

esri_streetscape <- map_bioretent()
cart_streetscape <- map_bioretent("CartoDB")
default_streetscape <- leaflet() %>%
  addTiles() %>%
  bio_marker_leg() %>%
  map_streetscape() %>%
  addLayersControl(overlayGroups = c("Bioretention structures", "Streetscapes"), position = "topleft", options = layersControlOptions(collapsed = F)) %>%
  hideGroup("Streetscapes") %>%
  add_wards_new() %>%
  addControl(position = "bottomleft", html = "Click on the dot to see more information on the streetscape or bioretention structure") %>%
  add_wards_new()


htmlwidgets::saveWidget(esri_streetscape, "./data/output/esri_streetscape.html")
htmlwidgets::saveWidget(cart_streetscape, "./data/output/cart_streetscape.html")
htmlwidgets::saveWidget(default_streetscape, "./data/output/default_streetscape.html")

# write final
htmlwidgets::saveWidget(esri_streetscape, "./data/output/streetscape-map.html")



# identify missing
bioretent_missing <- bioretent_shp_fixed %>%
  filter(st_is_empty(.))

streetscape_missing <- streetscape_shp %>%
  filter(st_is_empty(.))

openxlsx::write.xlsx(st_drop_geometry(bioretent_missing), "./data/output/bioretent_missing.xlsx", asTable = T)

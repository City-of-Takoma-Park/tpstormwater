library(dplyr)
library(tidyverse)
library(sf)
library(leaflet)
library(glue)

library(tpfuncts)
library(leafletwrappers)

st_layers("./data/source/TakomaParkStormwater.gdb")

# read in stormwater
stormwater_conveyance <- st_read("./data/source/TakomaParkStormwater.gdb", layer = "CONVEYANCE") %>%
  st_transform(4326) %>%
  rename_all(tolower)

stormwater_conveyance <- st_read("./data/source/new.gdb", layer = "CONVEYANCE") %>%
  st_transform(4326) %>%
  rename_all(tolower)


recode_structures <- function(var){
  case_when(var == "ID" ~ "Ditch intersection",
            var == "IN" ~ "Inlet",
            var == "MH" ~ "Manhole structure",
            var == "JB" ~ "Junction box",
            var == "PC" ~ "Pipe connection",
            var == "YC" ~ "Wye connection",
            var == "CI" ~ "Capped inlet",
            var == "EW" ~ "End wall",
            var == "HW" ~ "Head wall",
            var == "SH" ~ "Springhead",
            var == "RR" ~ "Rip rap ditch",
            var == "ES" ~ "Endsection",
            var == "PP" ~ "Projecting pipe",
            var == "PB" ~ "Bend",
            var == "SW" ~ "SWMF structure",
            var == "PD" ~ "Pipe direction",
            var == " " ~ NA_character_)
}

stormwater_structures <- st_read("./data/source/TakomaParkStormwater.gdb", layer = "STRUCTURES") %>%
  st_transform(4326) %>%
  rename_all(tolower) %>%
  mutate(str_name = recode_structures(str_type))

stormwater_structures %>%
  pull(str_type) %>%
  unique()



stormwater_structures_new <- st_read("./data/source/new.gdb", layer = "STRUCTURES") %>%
  st_transform(4326) %>%
  rename_all(tolower) %>%
  mutate(str_name = recode_structures(str_type))

# write structires
st_write(stormwater_structures_new, "./data/output/stormwater_structures_new.shp")

stormwater_npdes <- st_read("./data/source/TakomaParkStormwater.gdb", layer = "NPDESFeatures_Net_Junctions") %>%
  st_transform(4326) %>%
  rename_all(tolower)

stormwater_npdes_new <- st_read("./data/source/new.gdb", layer = "NPDESFeatures_Net_Junctions") %>%
  st_transform(4326) %>%
  rename_all(tolower)


# stormwater_metadata <- st_read("./data/source/TakomaParkStormwater.gdb", layer = "METADATA") %>%
#   # st_transform(4326) %>%
#   rename_all(tolower)

stormwater_npdes

summary(stormwater_structures %>% mutate(across(.cols = is.character, .fns = ~ as.factor(.x))))

stormwater_conveyance$conv_type %>% unique()

stormwater_conveyance$feat_status %>% unique()

write.csv(stormwater_structures %>% st_drop_geometry(), "./data/output/stormwater_structures.csv")


# read bioretention
bioretention <- st_read("./data/source/Bioretention_Area/Bioretention_Area.shp")

bioretention_new <- st_read("./data/source/Bioretention_Area_new/Bioretention_Area.shp")

bioretention_test <- st_read("./data/bioretent_test/Bioretention_Area/Bioretention_Area.shp")

bioretention_test_2 <- st_read("./data/bioretent_test/Bioretention_Area/Bioretention_Area.shp")

bioretention_new <- st_read("./data/source/Bioretention_Area_new/Bioretention_Area.shp") %>%
  st_transform(4326)



drain_area_new <-st_layers("./data/source/drainagearea_new.gdb")

# read in nhgis data
nhgis_dfs <- read.csv("./data/source/nhgis/nhgis0015_csv/nhgis0015_ds244_20195_2019_blck_grp.csv") %>%
  mutate(
    age_un18_tot = ALT0E003 + ALT0E004 + ALT0E005+ ALT0E006 + ALT0E027 + ALT0E028 + ALT0E029 + ALT0E030,
         age_un18_pct = pct_round(age_un18_tot, ALT0E001),
         age_over65_tot = ALT0E020 + ALT0E021 + ALT0E022 + ALT0E023 + ALT0E024 + ALT0E025 + ALT0E044 + ALT0E045 + ALT0E046 + ALT0E047 + ALT0E048 + ALT0E049,
         age_over65_pct = pct_round(age_over65_tot, ALT0E001),
         inc_belowpov_tot = ALWVE002 + ALWVE003,
         inc_belowpov_pct = pct_round(inc_belowpov_tot, ALWVE001),
         disab_tot = ALYWE003 + ALYWE006,
         disab_pct = pct_round(disab_tot, ALYWE001),
         vehic_avail_tot = AL0NE004 + AL0NE005 + AL0NE006 + AL0NE007 + AL0NE008 + AL0NE011 + AL0NE012 + AL0NE013 + AL0NE014 + AL0NE015,
         vehic_avail_pct = pct_round(vehic_avail_tot, AL0NE001)) %>%
  rename(pop_tot = ALT0E001,
         pov_stat_tot = ALWVE001,
         households_tot = ALYWE001,
         hous_units_tot = AL0NE001,
         race_black_tot = ALUCE003,
         race_aian_tot = ALUCE004,
         race_asian_tot = ALUCE005,
         race_nhpi_tot = ALUCE006,
         race_other_tot = ALUCE007,
         race_two_tot = ALUCE008,
         race_hisp_tot = ALUKE012,
         race_white_tot = ALUKE003) %>%
  mutate(race_black_pct = pct_round(race_black_tot, ALUCE001),
         race_asian_pct = pct_round(race_asian_tot, ALUCE001),
         race_other_pct = pct_round(race_other_tot, ALUCE001),
         race_two_pct = pct_round(race_two_tot, ALUCE001),
         race_white_pct = pct_round(race_white_tot, ALUKE001),
         race_hisp_pct = pct_round(race_hisp_tot, ALUKE001)) %>%
  rename_all(tolower) %>%
  select(gisjoin, state, statea, county, tracta, blkgrpa, geoid, name_e, contains("_tot"), contains("_pct"))


# identify block groups in takoma park
bgs_2019 <- list("Block Group" = c("2", "3", "1", "2", "4", "3", "2", "1", "1", "2", "1", "3", "1", "2", "1"),
                 "Census Tract" = c("7017.04", "7017.01", "7017.04", "7017.01", "7018", "7018", "7018", "7018", "7017.02", "7020", "7017.03", "7017.03", "7017.04", "7017.03", "7017.01"))

bgs_2019_match <- glue::glue("Block Group {bgs_2019[['Block Group']]}, Census Tract {bgs_2019[['Census Tract']]}, Montgomery County, Maryland")

demog_stormwater_process_select_tp <- nhgis_dfs %>%
  filter(name_e %in% bgs_2019_match)


# read in shapefile
bg_shapes <- st_read("./data/source/nhgis/md_bg/MD_blck_grp_2019.shp") %>%
  rename_all(tolower) %>%
  st_transform(4326)

# join data to shape
bg_shapes_join <- bg_shapes %>%
  right_join(demog_stormwater_process_select_tp, by = "gisjoin")

pal_num <- function(var, rev = F){
  pal_numeric(var = var, colors = "YlGn", df = bg_shapes_join %>% st_drop_geometry(), reverse = rev)
}

# create a geometry-stripped version
bg_shapes_asdata <- bg_shapes_join %>% st_drop_geometry


labels <- map(seq(1, nrow(bg_shapes_asdata)), function(i) {
  glue("{bg_shapes_asdata[i, 'name_e']}<p></p>
       Population: {bg_shapes_asdata[i, 'pop_tot']}<p></p>
       Households: {bg_shapes_asdata[i, 'households_tot']}<p></p>
       Percent white: {bg_shapes_asdata[i, 'race_white_pct']}%<p></p>
       Percent Black: {bg_shapes_asdata[i, 'race_black_pct']}%<p></p>
       Percent Hispanic: {bg_shapes_asdata[i, 'race_hisp_pct']}%<p></p>
       Percent Asian: {bg_shapes_asdata[i, 'race_asian_pct']}%<p></p>
       Percent under-18: {bg_shapes_asdata[i, 'age_un18_pct']}%<p></p>
       Percent 65 and over: {bg_shapes_asdata[i, 'age_over65_pct']}%<p></p>
       Percent households with a person with a disability: {bg_shapes_asdata[i, 'disab_pct']}%<p></p>
       Percent households with a vehicle: {bg_shapes_asdata[i, 'vehic_avail_pct']}%")
})


label_shapes <- function(var, group) {

  map(nrow(bg_shapes_asdata), function (i) {

    var <- gsub("_pct", "", var)

    pct_var <- paste0(var, "_pct")
    tot_var <- paste0(var, "_tot")

    glue("{bg_shapes_asdata[i, 'name_e']}<p></p>
                           Total {group}: {bg_shapes_asdata[i, tot_var]}<p></p>
                           Percent {group}: {bg_shapes_asdata[i, pct_var]}")
  })
}

addpoly_legend_demog <- function(basemap, var, group_name, rev = F){
  # labs <- label_shapes(var = var, group = group_name)

  addpoly_legend(basemap_select = basemap,
                 pal_funct_select = pal_num(var = var, rev = rev),
                 variable_select = var,
                 df_select = bg_shapes_join %>% st_drop_geometry(),
                 group_select = group_name,
                 title_select = "Percent",
                 labels_select = labels)
}


# color_pal <- function(df, var, funct_return = F) {
#   funct <- colorNumeric("viridis", range(bg_md[[var]], na.rm = T))
#
#   if (funct_return == T){
#     return(funct)
#   }
#   return(funct(bg_md[[var]]))
# }

# bg_shapes_join_data <- st_drop_geometry(bg_shapes_join)
#
# leaflet(bg_merge) %>%
#   addPolygons(group = "Black", fill = T, color = ~ color_pal(bg_merge_data$ALUCE003))

stormwater_structures <- stormwater_structures %>%
  group_by(str_type) %>%
  mutate(str_type_total = n())

bg_shapes_join_montcounty <- bg_shapes_join %>%
  filter(grepl("Montgomery", county))

st_write(bg_shapes_join_montcounty, "./data/output/bg_shapes_2019_join_montcounty.geojson", delete_dsn = T)

overlay_groups <- c("Stormwater infrastructure",
                    "Stormwater structures",
                    "NPDES",
                    "White",
                    "Black",
                    "Hispanic",
                    "Asian",
                    "Other",
                    "Youth (under 18)",
                    "Elderly (65 and over)",
                    "Below poverty line",
                    "Households with a person with a disability",
                    "Households without a vehicle")

demog_data_map <- leaflet(bg_shapes_join) %>%
  add_wards() %>%
  add_city() %>%
  addTiles(options = tileOptions(opacity = 0.3)) %>%
  addMarkers(
    group = overlay_groups[2],
    data = stormwater_structures,
    popup = ~ glue(
      "Structure ID: {structure_id}
      <p></p>Structure type: {str_type}
      <p></p> Structure-type total: {str_type_total}
      <p></p>Structure Depth: {str_depth %>% round(2)}
      <p></p>Maintenance condition: {maint_cond}
      <p></p>Date: {date_ver}
      <p></p>NPDEs Outflow: {npdes_outf}
      <p></p>Subbasin: {subbasin}"
    ),
    popupOptions = popupOptions()
  ) %>%
  leaflet::addMarkers(
    group = overlay_groups[3],
    data = stormwater_npdes
    ) %>%
  addpoly_legend_demog(var = "race_white_pct", group_name = overlay_groups[4], rev = F) %>%
  addpoly_legend_demog(var = "race_black_pct", group_name = overlay_groups[5], rev = F) %>%
  addpoly_legend_demog(var = "race_hisp_pct", group_name = overlay_groups[6], rev = F) %>%
  addpoly_legend_demog(var = "race_asian_pct", group_name = overlay_groups[7], rev = F) %>%
  addpoly_legend_demog(var = "race_other_pct", group_name = overlay_groups[8], rev = F) %>%
  addpoly_legend_demog(var = "age_un18_pct", group_name = overlay_groups[9], rev = F) %>%
  addpoly_legend_demog(var = "age_over65_pct", group_name = overlay_groups[10]) %>%
  addpoly_legend_demog(var = "inc_belowpov_pct", group_name = overlay_groups[11]) %>%
  addpoly_legend_demog(var = "disab_pct", group_name = overlay_groups[12]) %>%
  addpoly_legend_demog(var = "vehic_avail_pct", group_name = overlay_groups[13]) %>%
  addLayersControl(overlayGroups = overlay_groups, data = bg_shapes_join, options = layersControlOptions(collapsed = F)) %>%
  hideGroup(overlay_groups[-c(1, 4)]) %>%
  addPolylines(group = overlay_groups[1], data = stormwater_conveyance, stroke = T, weight = 2.25, fill = T, fillColor = "Blue")

summary(stormwater_structures$str_type %>% as.factor)


demog_data_map_cluster <- leaflet(bg_shapes_join) %>%
  add_wards() %>%
  add_city() %>%
  addTiles(options = tileOptions(opacity = 0.3)) %>%
  leaflet::addMarkers(group = overlay_groups[2], data = stormwater_structures, popup = ~ glue("Structure ID: {structure_id}
  <p></p>Structure type: {str_type}
  <p></p>Structure Depth: {str_depth %>% round(2)}
  <p></p>Maintenance condition: {maint_cond}
  <p></p>Date: {date_ver}
  <p></p>NPDEs Outflow: {npdes_outf}
                                                                                                              <p></p>Subbasin: {subbasin}"),
                      clusterOptions = leaflet::markerClusterOptions()) %>%
  leaflet::addMarkers(group = overlay_groups[3], data = stormwater_npdes) %>%
  addpoly_legend_demog(var = "race_white_pct", group_name = overlay_groups[4], rev = F) %>%
  addpoly_legend_demog(var = "race_black_pct", group_name = overlay_groups[5], rev = F) %>%
  addpoly_legend_demog(var = "race_hisp_pct", group_name = overlay_groups[6], rev = F) %>%
  addpoly_legend_demog(var = "race_asian_pct", group_name = overlay_groups[7], rev = F) %>%
  addpoly_legend_demog(var = "race_other_pct", group_name = overlay_groups[8], rev = F) %>%
  addpoly_legend_demog(var = "age_un18_pct", group_name = overlay_groups[9], rev = F) %>%
  addpoly_legend_demog(var = "age_over65_pct", group_name = overlay_groups[10]) %>%
  addpoly_legend_demog(var = "inc_belowpov_pct", group_name = overlay_groups[11]) %>%
  addpoly_legend_demog(var = "disab_pct", group_name = overlay_groups[12]) %>%
  addpoly_legend_demog(var = "vehic_avail_pct", group_name = overlay_groups[13]) %>%
  addLayersControl(overlayGroups = overlay_groups, data = bg_shapes_join, options = layersControlOptions(collapsed = F)) %>%
  hideGroup(overlay_groups[-c(1, 4)]) %>%
  addPolylines(group = overlay_groups[1], data = stormwater_conveyance, stroke = T, weight = 2.25, fill = T, fillColor = "Blue")


htmlwidgets::saveWidget(demog_data_map, file = "./data/output/stormwater_infrastructure_map_structinc.html", selfcontained = F)

htmlwidgets::saveWidget(demog_data_map_cluster, file = "./data/output/stormwater_infrastructure_map_clusterstruct.html", selfcontained = F)

# add years to structs
stormwater_structures$date_ver %>% range(na.rm = T)

stormwater_structures_year <- stormwater_structures %>%
  mutate(year = format(date_ver, "%Y"))

stormwater_structures_year %>%
  group_by(year) %>%
  summarize(n())

# create custom color palette
stromwater_colors <- c(
  RColorBrewer::brewer.pal(9, "Set1"),
  RColorBrewer::brewer.pal(5, "Paired")[c(1, 5)]
)
pal_stormwater <- colorFactor(palette = stromwater_colors, domain = sort(stormwater_structures$str_name))

overlay_groups_struct <- c(
  "Stormwater structures",
  "Stormwater conveyance",
  "NPDES",
  "White",
  "Black",
  "Hispanic",
  "Asian",
  "Other",
  "Youth (under 18)",
  "Elderly (65 and over)",
  "Below poverty line",
  "Households with a person with a disability",
  "Households without a vehicle"
)


id_color <- function(string){
  case_when(string == "Ditch intersection" ~ stromwater_colors[1],
            string == "End wall" ~ stromwater_colors[2],
            string == "Endsection" ~ stromwater_colors[3],
            string == "Head wall" ~ stromwater_colors[4],
            string == "Inlet" ~ stromwater_colors[5],
            string == "Junction box" ~ stromwater_colors[6],
            string == "Manhole structure" ~ stromwater_colors[7],
            string == "Pipe connection" ~ stromwater_colors[8],
            string == "Pipe direction" ~ stromwater_colors[9],
              string == "Projecting pipe" ~ stromwater_colors[10]
  )
}

get_storm_color <- function(df){
  map_chr(df[["str_name"]], ~ id_color(.x))
}


# color stormwater structures
demog_data_map_points <- leaflet(bg_shapes_join) %>%
  add_wards() %>%
  add_city() %>%
  addTiles(options = tileOptions(opacity = 0.2)) %>%
  addCircleMarkers(
    radius = 0.1,
    color = ~ id_color(str_name),
    group = overlay_groups_struct[1],
    stroke = T,
    weight = 10,
    data = stormwater_structures,
    popup = ~ glue(
    "Structure ID: {structure_id}
    <p></p>Structure type: {str_name}, {str_type_total} total
    <p></p>Structure Depth: {str_depth %>% round(2)}
    <p></p>Maintenance condition: {maint_cond}
    <p></p>Date: {date_ver}
    <p></p>NPDEs Outflow: {npdes_outf}
    <p></p>Subbasin: {subbasin}"
    )
  ) %>%
  addMarkers(group = overlay_groups_struct[3], data = stormwater_npdes) %>%
  addpoly_legend_demog(var = "race_white_pct", group_name = overlay_groups_struct[4], rev = F) %>%
  addpoly_legend_demog(var = "race_black_pct", group_name = overlay_groups_struct[5], rev = F) %>%
  addpoly_legend_demog(var = "race_hisp_pct", group_name = overlay_groups_struct[6], rev = F) %>%
  addpoly_legend_demog(var = "race_asian_pct", group_name = overlay_groups_struct[7], rev = F) %>%
  addpoly_legend_demog(var = "race_other_pct", group_name = overlay_groups_struct[8], rev = F) %>%
  addpoly_legend_demog(var = "age_un18_pct", group_name = overlay_groups_struct[9], rev = F) %>%
  addpoly_legend_demog(var = "age_over65_pct", group_name = overlay_groups_struct[10]) %>%
  addpoly_legend_demog(var = "inc_belowpov_pct", group_name = overlay_groups_struct[11]) %>%
  addpoly_legend_demog(var = "disab_pct", group_name = overlay_groups_struct[12]) %>%
  addpoly_legend_demog(var = "vehic_avail_pct", group_name = overlay_groups_struct[13]) %>%
  addLegend(pal = pal_stormwater, values = ~ str_name, group = overlay_groups_struct[1], data = stormwater_structures, title = "Stormwater structures") %>%
  addLayersControl(overlayGroups = overlay_groups_struct, data = bg_shapes_join, options = layersControlOptions(collapsed = F)) %>%
  hideGroup(overlay_groups_struct[-c(1, 4)]) %>%
  addPolylines(group = overlay_groups_struct[2], data = stormwater_conveyance, stroke = T, weight = 2.25, fill = T, fillColor = "Blue")

htmlwidgets::saveWidget(demog_data_map_points, file = "./data/output/stormwater_structs_pointstype.html", selfcontained = F)

stormwater_structures$str_name %>% as.factor %>% summary

### add in mult-structures as new layers

unique_str_types <- stormwater_structures[["str_name"]] %>%
  unique() %>%
  sort

stm_text <-paste0(
  "Structure ID: {structure_id}
    <p></p>Structure type: {str_name}, {str_type_total} total
    <p></p>Structure Depth: {str_depth %>% round(2)}
    <p></p>Maintenance condition: {maint_cond}
    <p></p>Date: {date_ver}
    <p></p>NPDEs Outflow: {npdes_outf}
    <p></p>Subbasin: {subbasin}"
)

strm_marker <- function(basemap, df, grp){
  basemap %>%
    addCircleMarkers(
      radius = 0.1,
      color = ~ pal_stormwater(str_name),
      group = grp,
      stroke = T,
      weight = 10,
      data = df,
      popup = ~ glue(stm_text),
    )
}

strm_legend <- function(basemap, df, grp){
  basemap %>%
    addLegend(pal = pal_stormwater,
              values = ~ str_name,
              group = grp,
              data = df,
              title = grp)
}



## same but clusterd
strm_cluster <- function(basemap, df, grp){
  basemap %>%
    leaflet::addMarkers(group = grp,
                        data = df,
                        popup = ~ glue(stm_text),
                        clusterOptions = leaflet::markerClusterOptions())
}

strm_markerleg <- function(basemap, str_name_string = NULL, cluster = F) {

  df <- stormwater_structures
  grp <- overlay_groups_struct[1]

  if (!is.null(str_name_string)){
    df <- stormwater_structures %>%
      filter(str_name == str_name_string)

    grp <- paste0(overlay_groups_struct[1], ": ", str_name_string)
  }

  if (cluster){
    map <- basemap %>%
      strm_cluster(df = df, grp = grp)
  }

  else {
    map <- basemap %>%
      strm_marker(df = df, grp = grp) %>%
      strm_legend(df = df, grp = grp)

  }
  # browser()

  map

}


overlay_groups_multi_struct <- c(
  "Stormwater structures",
  "Stormwater conveyance",
  "NPDES",
  "Bioretention structures",
  paste0("Stormwater structures: ", unique_str_types),
  "White",
  "Black",
  "Hispanic",
  "Asian",
  "Other",
  "Youth (under 18)",
  "Elderly (65 and over)",
  "Below poverty line",
  "Households with a person with a disability",
  "Households without a vehicle"
)

pal_bioretent <- pal_numeric(var = "Sto_Volume", df = st_drop_geometry(bioretention_new), colors = "RdBu")

bioretention_pnt <- bioretention_new %>%
  st_drop_geometry() %>%
  mutate(geometry = st_centroid(bioretention_new$geometry)) %>%
  st_as_sf(sf_column_name = "geometry") %>%
  mutate(Completion = as.character(Completion),
         Completion = case_when(Completion == 0 ~ "Missing",
                                T ~ Completion))

bio_labs <- leafletwrappers::label_standardize(st_drop_geometry(bioretention_pnt),
                                               label_text =
                                                 "Type: {Type}<p></p>
                                               Completion date: {Completion}<p></p>
                                               Area: {ACTULAREA %>% commafy}")
bio_col <- c(RColorBrewer::brewer.pal(8, "Dark2"), RColorBrewer::brewer.pal(8, "Set1"))

pal_bio <- colorFactor(palette = bio_col, domain = bioretention_pnt$Completion %>% sort())

leaflet(bioretention_pnt) %>%
  addCircleMarkers(
    radius = 0.1,
    color = ~ pal_bio(Completion),
    group = "Bioretention structures",
    stroke = T,
    weight = 10,
    data = bioretention_pnt,
    popup = ~ bio_labs,
  ) %>%
  addLegend(pal = pal_bio,
            values = ~ Completion,
            group = "Bioretention structures",
            data = bioretention_pnt,
            title = "Bioretention structures")


bio_marker_leg <- function(basemap) {
  basemap %>%
    addCircleMarkers(
      radius = 0.1,
      color = ~ pal_bio(Completion),
      group = "Bioretention structures",
      stroke = T,
      weight = 10,
      data = bioretention_pnt,
      popup = ~ bio_labs,
    ) %>%
    addLegend(pal = pal_bio,
              values = ~ Completion,
              group = "Bioretention structures",
              data = bioretention_pnt,
              title = "Bioretention structures")
}

demog_data_map_points_multistruct <- leaflet(bg_shapes_join) %>%
  add_wards() %>%
  # add_city() %>%
  addTiles(options = tileOptions(opacity = 0.2)) %>%
  addpoly_legend_demog(var = "race_white_pct", group_name = overlay_groups_multi_struct[14], rev = F) %>%
  addpoly_legend_demog(var = "race_black_pct", group_name = overlay_groups_multi_struct[15], rev = F) %>%
  addpoly_legend_demog(var = "race_hisp_pct", group_name = overlay_groups_multi_struct[16], rev = F) %>%
  addpoly_legend_demog(var = "race_asian_pct", group_name = overlay_groups_multi_struct[17], rev = F) %>%
  addpoly_legend_demog(var = "race_other_pct", group_name = overlay_groups_multi_struct[18], rev = F) %>%
  addpoly_legend_demog(var = "age_un18_pct", group_name = overlay_groups_multi_struct[19], rev = F) %>%
  addpoly_legend_demog(var = "age_over65_pct", group_name = overlay_groups_multi_struct[20]) %>%
  addpoly_legend_demog(var = "inc_belowpov_pct", group_name = overlay_groups_multi_struct[21]) %>%
  addpoly_legend_demog(var = "disab_pct", group_name = overlay_groups_multi_struct[22]) %>%
  addpoly_legend_demog(var = "vehic_avail_pct", group_name = overlay_groups_multi_struct[23]) %>%
  strm_markerleg() %>%
  addMarkers(group = overlay_groups_multi_struct[3], data = stormwater_npdes) %>%
  strm_markerleg(unique_str_types[1]) %>%
  strm_markerleg(unique_str_types[2]) %>%
  strm_markerleg(unique_str_types[3]) %>%
  strm_markerleg(unique_str_types[4]) %>%
  strm_markerleg(unique_str_types[5]) %>%
  strm_markerleg(unique_str_types[6]) %>%
  strm_markerleg(unique_str_types[7]) %>%
  strm_markerleg(unique_str_types[8]) %>%
  strm_markerleg(unique_str_types[9]) %>%
  strm_markerleg(unique_str_types[10]) %>%
  addLayersControl(overlayGroups = overlay_groups_multi_struct, data = bg_shapes_join, options = layersControlOptions(collapsed = F), position = "topleft") %>%
  bio_marker_leg() %>%
  hideGroup(overlay_groups_multi_struct[-c(1, 15)]) %>%
  # addpoly_legend(df_select = st_drop_geometry(bioretention_new),
  #                pal_funct_select = pal_bioretent,
  #                variable_select = "Sto_Volume",
  #                group_select = "Bioretention structures",
  #                title_select = "Bioretention structures",
  #                labels_select = bio_labs,
  #                .data = bioretention_new,
  #                .polfillopacity = 1) %>%
  addPolylines(group = overlay_groups_multi_struct[2], data = stormwater_conveyance, stroke = T, weight = 2.25, fill = T, fillColor = "Blue")

htmlwidgets::saveWidget(demog_data_map_points_multistruct, "data/output/demog_data_map_points_multistruct.html", selfcontained = F)
htmlwidgets::saveWidget(demog_data_map_points_multistruct, "./data/output/structure-map.html", selfcontained = T)

# renv::init()

demog_data_map_points_multiclust <- leaflet(bg_shapes_join) %>%
  add_wards() %>%
  add_city() %>%
  addTiles(options = tileOptions(opacity = 0.2)) %>%
  strm_markerleg() %>%
  addMarkers(group = overlay_groups_multi_struct[3], data = stormwater_npdes) %>%
  strm_markerleg(unique_str_types[1], cluster = T) %>%
  strm_markerleg(unique_str_types[2], cluster = T) %>%
  strm_markerleg(unique_str_types[3], cluster = T) %>%
  strm_markerleg(unique_str_types[4], cluster = T) %>%
  strm_markerleg(unique_str_types[5], cluster = T) %>%
  strm_markerleg(unique_str_types[6], cluster = T) %>%
  strm_markerleg(unique_str_types[7], cluster = T) %>%
  strm_markerleg(unique_str_types[8], cluster = T) %>%
  strm_markerleg(unique_str_types[9], cluster = T) %>%
  strm_markerleg(unique_str_types[10], cluster = T) %>%
  addpoly_legend_demog(var = "race_white_pct", group_name = overlay_groups_multi_struct[14], rev = F) %>%
  addpoly_legend_demog(var = "race_black_pct", group_name = overlay_groups_multi_struct[15], rev = F) %>%
  addpoly_legend_demog(var = "race_hisp_pct", group_name = overlay_groups_multi_struct[16], rev = F) %>%
  addpoly_legend_demog(var = "race_asian_pct", group_name = overlay_groups_multi_struct[17], rev = F) %>%
  addpoly_legend_demog(var = "race_other_pct", group_name = overlay_groups_multi_struct[18], rev = F) %>%
  addpoly_legend_demog(var = "age_un18_pct", group_name = overlay_groups_multi_struct[19], rev = F) %>%
  addpoly_legend_demog(var = "age_over65_pct", group_name = overlay_groups_multi_struct[20]) %>%
  addpoly_legend_demog(var = "inc_belowpov_pct", group_name = overlay_groups_multi_struct[21]) %>%
  addpoly_legend_demog(var = "disab_pct", group_name = overlay_groups_multi_struct[22]) %>%
  addpoly_legend_demog(var = "vehic_avail_pct", group_name = overlay_groups_multi_struct[23]) %>%
  addLayersControl(overlayGroups = overlay_groups_multi_struct, data = bg_shapes_join, options = layersControlOptions(collapsed = F), position = "topleft") %>%
  hideGroup(overlay_groups_multi_struct[-c(1, 14)]) %>%
  addPolylines(group = overlay_groups_multi_struct[2], data = stormwater_conveyance, stroke = T, weight = 2.25, fill = T, fillColor = "Blue")

htmlwidgets::saveWidget(demog_data_map_points_multiclust, "data/output/demog_data_map_points_multiclust.html", selfcontained = F)

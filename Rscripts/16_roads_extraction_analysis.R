
roads_data <- sf::st_read(file.path(AbidjanDir,"/OSM_data/abidjan_buildings_with_road_distances.gpkg"))
height_data <-  sf::st_read(file.path(AbidjanDir,"/Building Height/abidjan_height.shp"))

abidjan <- sf::st_read(file.path(AbidjanDir,"/10_districts_shapefile/Abidjan.shp"))

abidjan_footprint <- sf::st_read(file.path(AbidjanDir, "/Building Footprint/cluster_classifications/abidjan_grids_clustered.shp"))

roads <- st_transform(roads, crs = st_crs(abidjan))


abidjan_union <- abidjan %>% 
  st_union() %>% 
  st_sf(geometry = .)



abidjan_roads <- st_intersection(roads, abidjan_union)


st_write(abidjan_roads, "C:/Users/laure/Urban Malaria Proj Dropbox/urban_malaria/data/abidjan/OSM_data/abidjan_osm_roads.shp")


ggplot() +
  geom_sf(data = abidjan, fill = "gray95", color = "black", linewidth = 0.3) +
  geom_sf(data = abidjan_roads %>% 
            filter(fclass == "tertiary"|
                     fclass == "secondary"|
                     fclass == "path"|
                     fclass == "pedestrian"|
                     fclass == "footway"),
          aes(color = fclass), linewidth = 0.4) +  
  scale_color_viridis_d(option = "plasma", end = 0.85) +
  labs(
    title = "OSM Roads in Abidjan",
    subtitle = "Clipped from Ivory Coast OSM road network",
    color = "Road Type"
  ) +
  theme_minimal() +
  #facet_wrap(~fclass)
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold"),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )



############################
#Extraction
############################


buildings <- st_transform(abidjan_footprint, st_crs(abidjan_roads))




road_types <- c("tertiary", "secondary", "footway", "path", "pedestrian")


dist_matrix <- map_dfc(road_types, ~{
  # Map each road type to a 
  # distance vector and bind as columns
  r_type <- .x
  r_subset <- roads[roads$fclass == r_type, ]
  col_name <- paste0("dist_", r_type)
  
  if (nrow(r_subset) > 0) {
    dist_vals <- st_nn(buildings, r_subset, k = 1, returnDist = TRUE)$dist %>% unlist()
  } else {
    dist_vals <- rep(NA_real_, nrow(buildings))
  }
  
  setNames(tibble(dist_vals), col_name)
})


buildings <- bind_cols(buildings, dist_matrix)


st_write(buildings, "abidjan_buildings_with_road_distances.shp")



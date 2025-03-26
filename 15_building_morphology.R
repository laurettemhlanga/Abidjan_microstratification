
source("load_path.R", echo = F)


slum_data <- read.csv(file.path(AbidjanDir, "Abidjan slums.csv"))
slum_data <- slum_data[complete.cases(slum_data$Longitude, slum_data$Latitude), ]
slum_sf <-  st_as_sf(slum_data, coords = c("Longitude", "Latitude"), crs = 4326)
slum_sf <- slum_sf %>% 
  mutate(Slum = "Yes")


building_grids <- st_read(file.path(AbidjanDir, "abidjan_grids", "abidjan_grids.shp")) %>% 
  mutate(X = row_number())

building_metrics <- read.csv(file.path(AbidjanDir, "Building Footprint", "abidjan_building_metrics.csv"))
plot_building_metrics <- left_join(building_grids, building_metrics, by = "X")

ggplot() +
    geom_sf(data = plot_building_metrics, aes(geometry = geometry, fill = perimeter_mean)) +
    scale_fill_continuous(low = "lightblue", high = "darkblue", na.value = "black")+
    geom_sf(data = slum_sf, aes(geometry = geometry), color = "red", size = 1, shape = 16, alpha = 0.5)+
    geom_sf(data = abidjan_shp, aes(geometry = geometry), colour = "black", size = 10, alpha = 0) + 
    labs(title = "Average Perimeter and slum locations") +
    map_theme()



morphology_metrics <- c("area_mean", "perimeter_mean", "compact_mean", "angle_mean", "shape_mean")

for (metric in morphology_metrics) {
  
  p1 <- ggplot()+
    geom_sf(data = plot_building_metrics, aes(geometry = geometry, fill = !!sym(metric))) +
    scale_fill_continuous(low = "lightblue", high = "darkblue", na.value = "black")+
    geom_sf(data = slum_sf, aes(geometry = geometry), color = "red", size = 1, shape = 16, alpha = 0.5)+
    geom_sf(data = abidjan_shp, aes(geometry = geometry), colour = "black", size = 10, alpha = 0) +
    labs(title = paste("Map of", metric, "and slum locations"),
         fill = metric) +
    map_theme()
  
  ggsave(filename = paste0(plots,"/", Sys.Date(),"_", metric, "_Abidjan.pdf"), 
         plot = p1,
         width = 6, height = 4)
  
}

#group grids by slum presense
abj_grid <- st_join(plot_building_metrics, slum_sf, join = st_intersects)

ggplot()+
  geom_sf(data = abidjan_shp)+
  geom_sf(data = abj_grid, aes(geometry = geometry, fill = Slum))+ 
  labs(title = "Slum presence in Abidjan",
       fill = "Slums?",
       x = "",
       y = "")+
  map_theme()

#distribution of metrics in slum vs non-slum grids

selected_abj<- abj_grid %>%
  st_drop_geometry() %>% 
  dplyr::select(area_mean, perimeter_mean, compact_mean, angle_mean, shape_mean, settled_mean, Slum)

slums_melted <- melt(selected_abj, id.vars = "Slum", 
                              measure.vars = c("area_mean", "perimeter_mean", "compact_mean", "angle_mean", "shape_mean"),
                              variable.name = "metric", value.name = "value")


ggplot(slums_melted, aes(x = value, fill = Slum)) +
  geom_histogram(position = "identity", alpha = 0.7) +  
  scale_fill_manual(values = c("purple", "orange")) + 
  facet_wrap(~variable, scales = "free")+
  labs(title = "Distribution of Metrics: Slum vs Non-Slums",
       x = "Value",
       y = "Frequency",
       fill = "Slum") +
  theme_manuscript()+
  theme(legend.position = "top")

##create classification for "suggested slums"

max_slum_values <- abj_grid %>% 
  filter(Slum == "Yes") %>% 
  summarize(
    max_area = max(area_mean, na.rm = T),
    max_perimeter = max(perimeter_mean, na.rm = T),
    max_compact = max(compact_mean, na.rm = T),
    max_shape = max(shape_mean, na.rm = T),
    max_angle = max(angle_mean, na.rm = T)
  )

# slum_classification <- abj_grid %>%
#   dplyr::select(area_mean, perimeter_mean, compact_mean, angle_mean, shape_mean, settled_mean, Slum) %>% 
#   mutate(
#     area_slums = ifelse(area_mean <= max_slum_values$max_area, "Yes", NA),
#     perimeter_slums = ifelse(perimeter_mean <= max_slum_values$max_perimeter, "Yes", NA),
#     compact_slums = ifelse(compact_mean <= max_slum_values$max_compact, "Yes", NA),
#     shape_slums = ifelse(shape_mean <= max_slum_values$max_shape, "Yes", NA),
#     angle_slums = ifelse(angle_mean <= max_slum_values$max_angle, "Yes", NA)) 

slum_classification <- abj_grid %>%
  mutate(
    area_slums = case_when(
      Slum == "Yes" ~ "Slum",
      area_mean <= max_slum_values$max_area ~ "Suggested Slum",
      TRUE ~ NA_character_
    ),
    perimeter_slums = case_when(
      Slum == "Yes" ~ "Slum",
      perimeter_mean <= max_slum_values$max_perimeter ~ "Suggested Slum",
      TRUE ~ NA_character_
    ),
    compact_slums = case_when(
      Slum == "Yes" ~ "Slum",
      compact_mean <= max_slum_values$max_compact ~ "Suggested Slum",
      TRUE ~ NA_character_
    ),
    shape_slums = case_when(
      Slum == "Yes" ~ "Slum",
      shape_mean <= max_slum_values$max_shape ~ "Suggested Slum",
      TRUE ~ NA_character_
    ),
    angle_slums = case_when(
      Slum == "Yes" ~ "Slum",
      angle_mean <= max_slum_values$max_angle ~ "Suggested Slum",
      TRUE ~ NA_character_
    )
  )

slum_classification2 <- slum_classification %>%
  rowwise() %>%
  mutate(
    slum_count = sum(c(area_slums, perimeter_slums, compact_slums, shape_slums, angle_slums) == "Suggested Slum", na.rm = T),
    all_slums = case_when(
      Slum == "Yes" ~ "Slum",
      slum_count >= 3 ~ "Suggested Slum",
      TRUE ~ NA_character_
    )) %>%
  ungroup() %>%
  dplyr::select(-slum_count) 



#plot classifications

ggplot()+
  geom_sf(data = slum_classification, aes(geometry = geometry, fill = shape_slums))+
  map_theme()

slum_type <- c("area_slums", "perimeter_slums", "compact_slums", "angle_slums", "shape_slums", "all_slums")

for (slumfill in slum_type) {
  
  p2 <- ggplot()+
    geom_sf(data = slum_classification, aes(geometry = geometry, fill = !!sym(slumfill))) +
    labs(title = paste("Map of Suggested slums by", slumfill),
         fill = slumfill) +
    map_theme()
  
  ggsave(filename = paste0(plots,"/", slumfill, "_", "Abidjan.pdf"), 
         plot = p2,
         width = 6, height = 4)
  
}

ggplot()+
  geom_sf(data = slum_classification2, aes(geometry = geometry, fill = all_slums))+
  map_theme()


#### use median
median_slum_values <- abj_grid %>% 
  filter(Slum == "Yes") %>% 
  summarize(
    median_area = median(area_mean, na.rm = T),
    median_perimeter = median(perimeter_mean, na.rm = T),
    median_compact = median(compact_mean, na.rm = T),
    median_shape = median(shape_mean, na.rm = T),
    median_angle = median(angle_mean, na.rm = T)
  )


slum_classification_median <- abj_grid %>%
  mutate(
    area_slums = case_when(
      Slum == "Yes" ~ "Slum",
      area_mean <= median_slum_values$median_area ~ "Suggested Slum",
      TRUE ~ "Not Slum"
    ),
    perimeter_slums = case_when(
      Slum == "Yes" ~ "Slum",
      perimeter_mean <= median_slum_values$median_perimeter ~ "Suggested Slum",
      TRUE ~ "Not Slum"
    ),
    compact_slums = case_when(
      Slum == "Yes" ~ "Slum",
      compact_mean <= median_slum_values$median_compact ~ "Suggested Slum",
      TRUE ~ "Not Slum"
    ),
    shape_slums = case_when(
      Slum == "Yes" ~ "Slum",
      shape_mean <= median_slum_values$median_shape ~ "Suggested Slum",
      TRUE ~ "Not Slum"
    ),
    angle_slums = case_when(
      Slum == "Yes" ~ "Slum",
      angle_mean <= median_slum_values$median_angle ~ "Suggested Slum",
      TRUE ~ "Not Slum"
    )
  )

slum_classification_median <- slum_classification_median %>%
  rowwise() %>%
  mutate(
    slum_count = sum(c(area_slums, perimeter_slums, compact_slums, shape_slums, angle_slums) == "Suggested Slum", na.rm = T),
    all_slums = case_when(
      Slum == "Yes" ~ "Slum",
      slum_count >= 3 ~ "Suggested Slum",
      TRUE ~ "Not Slum"
    )) %>%
  ungroup() %>%
  dplyr::select(-slum_count) 

for (slumfill in slum_type) {
  
  p3 <- ggplot()+
    geom_sf(data = slum_classification_median, aes(geometry = geometry, fill = !!sym(slumfill))) +
    geom_sf(data = abidjan_shp, aes(geometry = geometry), colour = "black", size = 10, alpha = 0.1) + 
    labs(title = paste("Map of Suggested slums by", slumfill, "(Median)"),
         fill = slumfill) +
    map_theme()
  
  ggsave(filename = paste0(plots,"/", slumfill, "_median_", "Abidjan.pdf"), 
         plot = p3,
         width = 6, height = 4)
  
}


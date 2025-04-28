####Settlement Types/Areas

source("load_path.R", echo=FALSE) 
source("~/Abidjan/load_path.R", echo=FALSE)


NASAdata <- file.path(AbidjanDir, "Autonome D_Abidjan")
BuiltupDir <-  file.path(AbidjanDir, "Built up area")
Abi_grid <- file.path(NASAdata, "OB_Abidjan_WGS84_grid_with_building_counts", 
                      "OB_Abidjan_grid_with_building_counts.shp")

Built_areas <- st_read(file.path(BuiltupDir, "Built up area.shp"))
small_set <- sf::st_read(file.path(SmallsetDir, "Small settlement area.shp"))
slum_data <- read.csv(file.path(AbidjanDir, "Abidjan slums.csv"))
Abi_griddata <- st_read(Abi_grid)

slum_data <- slum_data[complete.cases(slum_data$Longitude, slum_data$Latitude), ]
slum_sf <-  st_as_sf(slum_data, coords = c("Longitude", "Latitude"), crs = 4326)


#layer with small settlements: Small settlements are likely rural/suburban areas

ggplot()+
  geom_sf(data = df_abidjan1)+
  geom_sf(color = "black", fill = "#ece9f7") +
  geom_sf(data = small_set, color = "red", fill = "transparent", size = 1) +  
  labs(title = "Abidjan Health Districts with Small Settlements", fill = "", x = NULL, y = NULL) +
  map_theme()
 

#plot Abidjan slums

ggplot()+
  geom_sf(data = df_abidjan1)+
  geom_sf(data = slum_sf, color="purple", size =1)+
  geom_sf(color = "black", fill ="#ece9f7")+
  labs(title = "Slums in Abidjan", x = "Longitude", y = "Latitude") +
  map_theme()

#plot built up areas

ggplot()+
  geom_sf(data = df_abidjan1)+ 
  geom_sf(color = "black", fill = "#ece9f7") +
  geom_sf(data = Built_areas, color = "green", fill = "transparent", size = 1) +  
  labs(title = "Abidjan Health Districts with Built up Areas", fill = "", x = NULL, y = NULL) +
  map_theme()


#plot settlement types in Abidjan
descriptive_text <- paste("The slum communities identified and included in this map are:",
                          toString(slum_data[, "Slum.Name"]))
wrapped_text <- strwrap(descriptive_text, width = 50)  
wrapped_text <- paste(wrapped_text, collapse = "\n")

ggplot()+
  geom_sf(data = df_abidjan1, color = "black", fill = "#ece9f7" )+ 
  geom_sf(data = small_set, color = "red", fill = "transparent", size = 0.5) +
  geom_sf(data = slum_sf, color="purple", size = 1.2)+
  geom_sf(data = Built_areas, color = "green", fill = "transparent", size = 0.5) + 
  labs(title = "Housing Structure in Abidjan", fill = "", x = "Longitude", y = "Latitude", caption = wrapped_text)+
  theme(plot.caption = element_text(hjust = 0.5, margin = margin(t = 10, b = 10, unit = "pt")))+
  map_theme()

ggplot() +
  geom_sf(data = df_abidjan1, color = "black") +
  geom_sf(data = small_set, aes(geometry = geometry, fill = "small_set"), alpha = 0.2) +
  geom_sf(data = slum_sf, aes(geometry = geometry, color = "slum_sf"), size = 2) +
  geom_sf(data = Built_areas, aes(geometry = geometry, fill = "Built_areas"), alpha = 0.2) +
  scale_fill_manual(name = "",
                    values = c("small_set" = "red",  "Built_areas" = "green"),
                    labels = c("Small Settlements", "Built Areas")) +
  scale_color_manual(name = "",
                     values = c( "slum_sf" = "purple"),
                     labels = c("Slum")) +
  labs(title = "Housing Structure in Abidjan",
       caption = wrapped_text) +
  theme(plot.caption = element_text(hjust = 0.5, margin = margin(t = 10, b = 10, unit = "pt"))) +
  map_theme()


######################################################
# BUILDING COUNTS 
######################################################

ggplot()+
  geom_sf(data = df_abidjan1, aes())+
  geom_sf(data= Abi_griddata, aes(fill = total_buil))+
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(title = "Building Counts in Abidjan") +
  map_theme()

Abi_build <- st_intersection(df_abidjan1, Abi_griddata)
ggplot()+
  geom_sf(data= Abi_build, aes(fill = total_buil))+
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(title = "Building Counts in Abidjan") +
  map_theme()


### Building counts with slums
ggplot() +
  #geom_sf(data = df_abidjan1, aes()) +
  #geom_sf(data = Abi_griddata, aes(fill = total_buil)) +
  geom_sf(data= Abi_build, aes(fill = total_buil))+
  geom_sf(data = slum_sf, aes(geometry = geometry), color = "black", size = 1) +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(title = "Building Counts and Slums in Abidjan") +
  map_theme()


### slum count in each health district
slum_districts <- st_join(df_abidjan1, slum_sf, join = st_intersects)

slum_counts <- slum_districts %>%
  group_by(NOM) %>%
  summarise(Slum_Count = n()) # %>%
  #st_drop_geometry()

#Abidjan_var <- read.csv(file.path(AbidjanDir, "Abidjan Data Variables.csv"))
#joined_df <- left_join(Abidjan_var, slum_counts, by = c("HealthDistrict" = "NOM")) #, all.x = TRUE)
#write.csv(joined_df, file.path(AbidjanDir, "Abidjan Data Variables.csv"), row.names = FALSE)

slum_sf$shapes <- "Slums"

ggplot() +
  geom_sf(data = slum_counts, aes(geometry = geometry, fill = Slum_Count), color = "black", size = 0.2) + 
  geom_text_repel(data = slum_counts, aes(label = str_to_sentence(NOM), geometry = geometry), 
                  color = 'black',stat = "sf_coordinates", min.segment.length = 0, size = 3.5, force = 1) +
  geom_sf(data = slum_sf, aes(geometry = geometry, shape = shapes), color = "black", size = 1) +
  scale_fill_gradient(name = "Slum Count", low = "lightyellow", high = "brown") + 
  scale_shape_manual(name = "", values = c("Slums" = 16)) + 
  labs(title = "Slum Count in Abidjan Health Districts", x= "", y = "") +
  theme_void() +
  map_theme()
 

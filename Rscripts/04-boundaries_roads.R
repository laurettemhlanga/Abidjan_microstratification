source("load_path.R", echo=FALSE)

###visualizing boundaries


reg_bounds <- st_read(file.path(Region_bounds, "Region boundaries.shp"))
dist_bounds <- st_read(file.path(District_bounds, "District boundaries.shp"))
dept_bounds <- st_read(file.path(Department_bounds, "Department boundaries.shp"))




#####display boundaries

ggplot(data = Abi_shapefile[[3]])+ 
  geom_sf(fill = 	"#ece9f7")+
  #geom_sf(data = abidjan_map, color = "black", fill = "#ece9f7") +
  geom_sf(data = reg_bounds, color = "pink") +
  geom_sf(data = dist_bounds, color = "lightblue",) +
  geom_sf(data = dept_bounds, color = "lightgreen",) +
  labs(title = "Cote d'Ivoire with marked boundaries", fill = "", x = NULL, y = NULL) +
  map_theme()

st_equals(reg_bounds, dist_bounds)
st_equals(dist_bounds, dept_bounds)

#The region, district and department boundaries above are identical.

##################################


majrds <- st_read(file.path(major_roads, "Major road.shp"))
view(majrds)
resrds <- st_read(file.path(residential_road, "Residential road.shp"))
view(resrds)
water_body <- st_read(file.path(waterbody, "Waterbody.shp"))
view(water_body)
water_way <- st_read(file.path(waterway, "Waterway.shp"))
view(water_way)

##### Water Bodies
ggplot()+
  geom_sf(data = df_abidjan1, color = "darkgrey", fill = "white")+
  geom_sf(data = water_body, aes(fill = sub_type))+
  scale_fill_manual(name = "Water Body Type", 
                    values = c("water" = "lightblue",   
                               "wetland" = "darkgreen",
                               "reservior" = "orange"),
                    labels = c("Water", "Wetland", "Reservoir"),  
                    guide = guide_legend(title.position = "top",   
                                         title.hjust = 0.5),   
  ) +
  labs(title = "Types of Water bodies in Abidjan", fill = "") +    
  map_theme()  




# Plot the map
ggplot() +
  geom_sf(data = water_bodies, aes(fill = subtype_column)) +
  scale_fill_manual(name = "Water Body Type",    # Legend title
                    values = c("subtype1" = "blue",    # Assign colors to subtypes
                               "subtype2" = "green",
                               "subtype3" = "red",
                               # Add more subtypes as needed
                    ),
                    labels = c("Subtype 1", "Subtype 2", "Subtype 3"),   # Legend labels
                    na.value = "grey",    # Color for NA values (if any)
                    guide = guide_legend(title.position = "top",    # Legend position and title
                                         title.hjust = 0.5),     # Center the title
  ) +
  labs(title = "Water Bodies Map", fill = "") +    # Plot title and legend label
  theme_minimal()    # Optional: Use a minimal theme for the plot


#####
ggplot() + 
  geom_sf(data = abidjan_map, color = "darkgrey", fill = "white") +
  geom_sf(data = reg_bounds, color = "pink") +
  geom_sf(data = majrds, color = "black") +
  geom_sf(data = resrds, color = "orange",) +
  geom_sf(data = water_body, color = "darkblue",) +
  geom_sf(data = water_way, color = "lightblue",) +
  labs(title = "Abidjan Complete Map", fill = "", x = NULL, y = NULL) +
  map_theme()


#####Abidjan complete map: Roads, water bodies and water ways in
ggplot() + 
  geom_sf(data = abidjan_map, color = "darkgrey", fill = "white") +
  geom_sf(data = reg_bounds, color = "pink") +
  geom_sf(data = majrds, aes(color = "Major Roads")) +  
  geom_sf(data = resrds, aes(color = "Residential Roads")) +  
  geom_sf(data = water_body, aes(color = "Water Bodies")) +  
  geom_sf(data = water_way, aes(color = "Waterways")) +  
  labs(title = "Abidjan Complete Map", x = NULL, y = NULL) +
  scale_color_manual(name = "Legend", 
                     values = c("Major Roads" = "black", 
                                "Residential Roads" = "orange",
                                "Water Bodies" = "darkblue",
                                "Waterways" = "lightblue"), 
                     labels = c("Major Roads", "Residential Roads", "Water Bodies", "Waterways")) +  
  map_theme()
 
###### Cropped Map
####

ggplot() +
  geom_sf(data = st_intersection(majrds, Abi_shapefile[[3]]),aes(color = "Major Roads"))+
  labs(title = "Abidjan Major roads", x = NULL, y = NULL) +
  scale_color_manual(name = "Legend", 
                     values = c("Major Roads" = "black",), 
                     labels = c("Major Roads")) + 
  map_theme()

#####

majrds_cropped <- st_crop(majrds, abidjan_map)
resrds_cropped <- st_crop(resrds, abidjan_map)
water_body_cropped<- st_crop(water_body, abidjan_map)
water_way_cropped <- st_crop(water_way, abidjan_map)

ggplot() + 
  geom_sf(data = abidjan_map, color = "darkgrey", fill = "white") +
  geom_sf(data = majrds_cropped, aes(color = "Major Roads")) +  
  geom_sf(data = resrds_cropped, aes(color = "Residential Roads")) +  
  geom_sf(data = water_body_cropped, aes(color = "Water Bodies")) +  
  geom_sf(data = water_way_cropped, aes(color = "Waterways")) +  
  labs(title = "Abidjan Complete Map", x = NULL, y = NULL) +
  scale_color_manual(name = "Legend", 
                     values = c("Major Roads" = "black", 
                                "Residential Roads" = "orange",
                                "Water Bodies" = "darkblue",
                                "Waterways" = "lightblue"), 
                     labels = c("Major Roads", "Residential Roads", "Water Bodies", "Waterways")) +  
  map_theme()
  
  
  # Plot the cropped geometries
  plot(cropped_geometries)
  


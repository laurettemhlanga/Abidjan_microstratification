source("load_path.R", echo=FALSE)

library(readxl)
library(hrbrthemes)

ymin = 5.220694
ymax  = 5.636327

xmin = -4.420968
xmax  = -3.721849

#usd_rate <- #need to exract it from online for the conversion 


real_estate <- read_excel(file.path(AbidjanDir, "real_estate_data/All_data_realestate.xlsx")) %>% 
  drop_na(lat,long) %>% 
  filter(lat > ymin & lat < ymax) %>% 
  filter(long > xmin & long < xmax) 


new_names <- c( "city","neighborhood", 
                "number_of_bed_rooms", 
                "area_square_meters", "lat", 
                "long", "price", "link", 
                "listing_date","type" )
  
names(real_estate) <- new_names

palettes <- list(rev(RColorBrewer::brewer.pal(11, "RdYlBu")))[[1]][11:1]


#retrieve slum points
slum_data <- read.csv(file.path(AbidjanDir, "Abidjan Slums.csv"))%>% 
  drop_na(Latitude ,Longitude)

slum_sf <-  st_as_sf(slum_data, coords = c("Longitude", "Latitude"), crs = 4326)


real_estate_clean <- real_estate %>% 
  mutate(area_square_meters = as.numeric(area_square_meters), 
         price_square_meter = price / (100000 * area_square_meters),
         classes = cut(price_square_meter, 
                     c(2008,  380000, 500000, 571428, 694737,
                       800000,  907912, 1000000,
                       1150000, 1333334, 52600000)/100000,  
                     labels = c("[0.0201, 3.8]", "(3.8, 5.0]", "(5.0, 5.8]",
                                "(5.8, 7.0]", "(7.0, 8.0]", "(8.0, 9.1]",
                                "(9.1, 10.0]", "(10.0, 11.5]", "(11.5, 13.4]",
                                "(13.4, 526]"), 
                     include.lowest = T )) %>% 
  drop_na(price_square_meter)



real_estate_clean %>%
  # filter out the extreme values to get the general 
  # distribution of the prices per square meter
  filter(price_square_meter < 20) %>% 
  ggplot(aes(x=price_square_meter)) +
  geom_histogram( bin=1000, fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  xlab("price per square meter (X 100000)")+
  ylab("frequency") +
  #theme_ipsum() +
  theme(plot.title = element_text(size=15))



#get Abidjan health district from the first file  
Abidjan = Abi_shapefile[[3]] %>% 
  filter(NAME_1 == "Abidjan")
df_abidjan1 = st_intersection(Abi_shapefile[[7]], Abidjan)

real_estate_sf <-  st_as_sf(real_estate_clean, coords = c( "long", "lat"), crs = 4326)


ggplot()+
  # just for visusalisation to see the distribution of the 
  # real estate prices in Abidjan 
  geom_sf(data = df_abidjan1, fill = "white")+
  geom_text_repel(
    data = df_abidjan1,
    aes(label = str_to_sentence(NOM), geometry = geometry),color ='black',
    stat = "sf_coordinates", min.segment.length = 0, size = 3.5, force = 1)+
  geom_sf(data = real_estate_sf, aes(geometry = geometry))+
  #labs(color = "price/square meters (X 100000)")+
  map_theme()



#health_districts_price <- st_join(df_abidjan1, real_estate_sf, join = st_intersects)
health_districts_price <- st_join(abidjan_shp, real_estate_sf, join = st_intersects)


#Average price per square foot
average_data <- health_districts_price %>% 
  group_by(NOM) %>% 
  mutate(average_hd = mean(price_square_meter, na.rm = T), 
         mean_classes = cut(average_hd, 
                       c(2008,  380000, 500000, 571428, 694737,
                         800000,  907912, 1000000,
                         1150000, 1333334, 52600000)/100000, 
                       labels = c("[0.0201, 3.8]", "(3.8, 5.0]", "(5.0, 5.8]",
                                  "(5.8, 7.0]", "(7.0, 8.0]", "(8.0, 9.1]",
                                  "(9.1, 10.0]", "(10.0, 11.5]", "(11.5, 13.4]",
                                  "(13.4, 526]"), include.lowest = T ))%>% 
  drop_na(average_hd)

#plot range of housing prices
ggplot() +
  geom_sf(data = df_abidjan1, color = "black") +
  geom_sf(data = average_data, aes(geometry = geometry,
                                   color = mean_classes,
                                   fill = mean_classes), alpha = 0.8) +
  # geom_point(data = real_estate_clean, aes(x = long, y = lat, 
  #                                          fill = classes, 
  #                                          color = classes), alpha = 0.5, size = 3) +
  # geom_sf(data = slum_sf, aes(geometry = geometry), fill = "red", size = 4, shape = 25) +
  # geom_sf_text(data = slum_sf, aes(geometry = geometry, label = Slum.Name)) +
  labs(x = "", y = "", color = "", fill = "") +
  scale_fill_manual(values = palettes,
                    limits = c("[0.0201, 3.8]", "(3.8, 5.0]", "(5.0, 5.8]",
                               "(5.8, 7.0]", "(7.0, 8.0]", "(8.0, 9.1]",
                               "(9.1, 10.0]", "(10.0, 11.5]", "(11.5, 13.4]",
                               "(13.4, 526]")) +
  scale_color_manual(values = palettes,
                     limits = c("[0.0201, 3.8]", "(3.8, 5.0]", "(5.0, 5.8]",
                                "(5.8, 7.0]", "(7.0, 8.0]", "(8.0, 9.1]",
                                "(9.1, 10.0]", "(10.0, 11.5]", "(11.5, 13.4]",
                                "(13.4, 526]")) +
  scale_shape_manual(values = c(25),labels = c("Slum"))+
  map_theme()


#Summarized average price per square meter for each health district
avg_hd <- health_districts_price %>% 
  group_by(NOM) %>% 
  summarise(mean_price = mean(price_square_meter, na.rm = T))

ggplot()+
  geom_sf(data = avg_hd, aes(geometry = geometry, fill = mean_price))+
  scale_fill_gradientn(
    colors = c("#00ffff", "#00d9e9", "#00b3cf", "#008fb1", "#006d90", "#004c6d"),  # Custom hex colors (light to dark blue)
    na.value = "#b2beb5") +
  map_theme()
  
  
avg_hd <- avg_hd %>% 
  st_drop_geometry()

write.csv(avg_hd, file.path(AbidjanDir, "housing_prices.csv"))



# Plot Range of prices
palettes <- c("#b9ffff", 
              "#91d9e2", 
              "#6cb4c5", 
              "#4a90a8", 
              "#286d8a", 
              "#004c6d") 


ggplot() +
  geom_sf(data = abidjan_shp, color = "black", size = 1) +
  geom_sf(data = average_data, aes(geometry = geometry,
                                   #color = "black",
                                   fill = mean_classes)) +
  labs(x = "", y = "", color = "", fill = "") +
  scale_fill_manual(values = palettes,
                    limits = c("[0.0201, 3.8]", "(3.8, 5.0]", "(5.0, 5.8]",
                               "(5.8, 7.0]", 
                               "(10.0, 11.5]", 
                               "(13.4, 526]")) +
  geom_sf(data = slum_sf, aes(geometry = geometry, shape = "Slums"), 
          colour = "#de425b", size = 3, alpha = 0.5) +
  labs(x = "", y = "", color = "", fill = "", shape = "") +
  scale_shape_manual(values = c("Slums" = 19)) +
  map_theme()



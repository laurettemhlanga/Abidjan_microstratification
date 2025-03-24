rm(list=ls())

source("load_path.R")

# Funding limitations

globalfund_malaria <- read.csv(file.path(DriveDir, "projects/Manuscripts/ongoing/Abidjan/malaria global fund.csv")) %>% 
  slice(-c(6, 7))


ggplot(globalfund_malaria, aes(x = Year, y = Disbursed/1e6)) +
  # color = as.factor(Grant.Cycle), group = Grant.Cycle)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "blue", size = 2) +
  labs(title = "Funding Disbursed towards Malaria Programs in Côte d'Ivoire", 
       x = "Year", 
       y = "Amount Disbursed (US$ millions)",
       caption = "Adapted from The Global Fund: https://data.theglobalfund.org/location/CIV/financial-insights") +
  theme_manuscript() 

ggsave(paste0(PlotsDir,"/", "malaria_funding_", Sys.Date(), '_.png'),  width = 8, height =5)


# Study Area

plot_CIV <- Abi_shapefile[[2]] %>%
  mutate(area_label = ifelse(NAME_1 == "Abidjan", "Abidjan", "Other Districts"))

ggplot()+
  geom_sf(data = plot_CIV, aes(fill = area_label), colour = "black") +
  scale_fill_manual(values = c("Abidjan" = "#d73027", 
                               "Other Districts" = "lightyellow")) +
  # labs(title = "Autonomous District of Abidjan in Côte d'Ivoire",
  #      fill = "")+
  map_theme()

ggsave(paste0(PlotsDir,"/", "Abidjan_district_", Sys.Date(), '_.pdf'),  width = 8, height =5)


# Health Districts in Abidjan

abidjan_shp <- df_abidjan1 %>% 
  dplyr::filter(!NOM %in% c("JACQUEVILLE", "DABOU", "ALEPE", "GRAND-BASSAM", "AGBOVILLE"))

ggplot()+
  geom_sf(data = abidjan_shp, colour = "black", fill = "lightpink")+
  # geom_text_repel(data = abidjan_shp,
  #                 aes(label=str_to_sentence(NOM), geometry=geometry), color ='black',
  #                 stat = "sf_coordinates", min.segment.length = 0, size = 3, force = 1)+
  #labs(title = "Abidjan's Ten Health Districts",
   #    x = "", y = "")+
  map_theme()

ggsave(paste0(PlotsDir,"/", "Abidjan_health_districts_", Sys.Date(), '_.pdf'),  width = 8, height =5)


## Real Estate Prices and Slums (read in 00_real_estate first)

ggplot() +
  geom_sf(data = abidjan_shp, color = "black") +
  geom_sf(data = average_data, aes(geometry = geometry,
                                   color = mean_classes,
                                   fill = mean_classes), alpha = 0.8) +
  geom_sf(data = slum_sf, aes(geometry = geometry, shape = "Slums"), 
          colour = "green", size = 3, alpha = 0.5) +
  labs(x = "", y = "", color = "", fill = "", shape = "") +
  # scale_fill_manual(values = palettes,
  #                   limits = c("[0.0201, 3.8]", "(3.8, 5.0]", "(5.0, 5.8]",
  #                              "(5.8, 7.0]", "(7.0, 8.0]", "(8.0, 9.1]",
  #                              "(9.1, 10.0]", "(10.0, 11.5]", "(11.5, 13.4]",
  #                              "(13.4, 526]")) +
  # scale_color_manual(values = palettes,
  #                    limits = c("[0.0201, 3.8]", "(3.8, 5.0]", "(5.0, 5.8]",
  #                               "(5.8, 7.0]", "(7.0, 8.0]", "(8.0, 9.1]",
  #                               "(9.1, 10.0]", "(10.0, 11.5]", "(11.5, 13.4]",
  #                               "(13.4, 526]")) +
  scale_shape_manual(values = c("Slums" = 19)) +
  map_theme()

##edit
ggplot() +
  geom_sf(data = abidjan_shp, color = "black") +
  geom_sf(data = average_data, aes(geometry = geometry,
                                   #color = mean_classes,
                                   fill = average_hd),
          alpha = 0.8
  ) +
  geom_sf(data = slum_sf, aes(geometry = geometry, shape = "Slums"), 
          colour = "green", size = 3, alpha = 0.5) +
  labs(x = "", y = "", color = "", fill = "", shape = "") +
  scale_shape_manual(values = c("Slums" = 19)) +
  map_theme()


##Urban extent
urbanicity <- st_read(file.path(AbidjanDir, "Abidjan_urban_percentage.geojson"))

urbanicity$urbanPercentage <- as.numeric(urbanicity$urbanPercentage)


ggplot()+
  geom_sf(data = urbanicity, aes(geometry = geometry, fill = urbanPercentage)) +
  labs(title = "Percentage of Urban Area in Abidjan",
       fill = "Urbanicity %")+
  map_theme()
  
ggplot() +
  geom_sf(data = urbanicity, aes(geometry = geometry, fill = urbanPercentage)) +
  labs(title = "Percentage of Urban Area in Abidjan",
       fill = "Urbanicity %") +
  scale_fill_manual(
    breaks = c(25, 50, 75, 100),  # Corrected: 5 breakpoints for 4 bins
    labels = c("0-25%", "26-50%", "51-75%", "76-100%")) +
  map_theme()

  
  

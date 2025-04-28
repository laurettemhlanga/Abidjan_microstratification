source("load_path.R", echo=FALSE) 


########## Yearly rainfall 2013 ############

pattern2013 <- paste0("2013[0-9]+.tif")



rainfall_2013 <- list.files(file.path(Rainfall2013_23), 
                            pattern = pattern2013, 
                            full.names = TRUE)



#################################################################################
#####################DATA FROM CLIMESERV FOLDER##################################
#################################################################################

tag <- c(3:9, 0:3)
patterns <- paste0(c(rep(201, 7), rep(202, 4)), tag, "[0-9]+.tif")


rainfall <- lapply(seq_along(patterns), 
                   function (x) list.files(file.path(RainfallPlus),
                                           pattern = patterns[x],
                                           full.names = TRUE))


rainfall <- unlist(rainfall)

rainfall_data =  lapply(seq_along(rainfall), 
                        function(x) raster::raster(rainfall[x]))


raindata13b = rainfall_data %>%
  # CAVEAT:takes long to run
  purrr::map(~raster::extract(., df_abidjan1,
                              buffer = buffer,
                              fun = mean, df =TRUE)) %>%
  purrr::reduce(left_join, by = c("ID"))



long_rainfall_data <- df_abidjan1 %>% 
  # check is the sub-setting is correct and summarising 
  # makes sense and please find out the unit measurement 
  st_drop_geometry() %>% 
  dplyr::select(NOM) %>% 
  cbind(raindata13b) %>%
  reshape::melt(id = c("NOM", "ID")) %>% 
  mutate(year = substr(variable, 2, 5), 
         month = substr(variable, 6, 7))


summerised_rainfall <- long_rainfall_data %>% 
  group_by(NOM, ID, year, month) %>% 
  summarise(monthly_rainfall = sum(value, na.rm = T)) %>% 
  ungroup() #%>% 
  # group_by(NOM, ID, year) %>% 
  # mutate(yearly_rainfall = sum(value, na.rm = T)) %>% 
  # rename(daily_rainfall = value) %>% 
  # ungroup() %>% 
  # group_by(NOM, ID) %>% 
  # mutate(overall_rainfall = mean(daily_rainfall, na.rm = T)) 
write.csv(summerised_rainfall,file.path(RainfallPlus, "monthly_rainfall_sums_2013_2023.csv"))

summerised_rainfall_year <- long_rainfall_data %>% 
  group_by(NOM, ID, month) %>% 
  summarise(monthly_rainfall = sum(value, na.rm = T)) %>% 
  ungroup()

write.csv(summerised_rainfall,file.path(RainfallPlus, "monthly_rainfall_sums_all_years.csv"))

rainfall_plottingdata <- inner_join(df_abidjan1, summerised_rainfall)

##########################################################################################
####### Plots
##########################################################################################

# 
# ggplot(data = df_abidjan1) +
#   geom_sf(color = "black", fill = "white") +
#   geom_sf(data = rainfall_plottingdata, aes(geometry = geometry, fill = monthly_rainfall )) +
#   facet_wrap(~year)+
#   scale_fill_continuous(name = "Average Rainfall", low = "grey", high = "darkblue") +
#   labs(title = "Average Rainfall", fill = "", x = NULL, y = NULL) +
#   map_theme() 
# 
# 
# ggplot(data = df_abidjan1) +
#   geom_sf(color = "black", fill = "white") +
#   geom_sf(data = rainfall_plottingdata, aes(geometry = geometry, fill = overall_rainfall)) +
#   scale_fill_continuous(name = "Average Rainfall", low = "grey", high = "darkblue") +
#   labs(title = "Average Rainfall", fill = "", x = NULL, y = NULL) +
#   map_theme() 


################################################################################
########### TIME SERIES PLOTS##########################################
##########################################################################

#summerised_rainfall <-read.csv(file.path(RainfallPlus, "all_rainfall_data.csv")) 


overall_month <- summerised_rainfall %>%  
  group_by(month) %>% summarise(avg_month = sum(monthly_rainfall, na.rm=T)) %>%  ungroup()


ggplot(overall_month, aes(x=month, y =avg_month, group=1)) +
  geom_line()

year_13 <- summerised_rainfall %>%  filter(year == "2013") %>%  
  group_by(month) %>% summarise(all_month_rain = sum(monthly_rainfall, na.rm=T))

year_21 <- summerised_rainfall %>%  filter(year == "2021") %>%  
  group_by(month) %>% summarise(all_month_rain = sum(monthly_rainfall, na.rm=T))

p1 <- ggplot(year_13, aes(x=month, y =all_month_rain, group=1)) +
  geom_line(color ="#ff0085", linewidth= 1) +
  theme_manuscript()+
  labs(y= "Total rainfall in millmeters in Abidjan in 2013", x = "month")+
  ylim(0, 3700)

p2 <-ggplot(year_21, aes(x=month, y =all_month_rain, group=1)) +
  geom_line(color ="#ff0085", linewidth= 1) +
  theme_manuscript()+
  labs(y= "Total rainfall in millmeters in Abidjan in 2021", x = "month")+
  ylim(0, 3700)

p1 + p2

ggsave(file.path(plots, "Rainfall", "240412_ifeoma_edits", "rainfall_2013_2012.pdf"),
       width = 12, height = 5, dpi = 300)

##########################################################################
# plot time series with yearly rainfall
##########################################################################

ggplot(rainfall_data, aes(x = year, y = yearly_rainfall, color = NOM)) +
  geom_line() +
  scale_x_continuous(breaks = seq(min(rainfall_data$year), max(rainfall_data$year), by = 1)) +
  labs(title = "Rainfall trend 2013-2023",
       x = "Year",
       y = "Yearly rainfall",
       color = "Health district") +
  map_theme()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  
        axis.text.y = element_text(angle = 45, hjust = 1))


##########################################################################
# plot time series with monthly data
##########################################################################


rainfall_data$date <- as.Date(paste(rainfall_data$year, rainfall_data$month, "01", sep = "-"))

ggplot(rainfall_data, aes(x = date, y = monthly_rainfall, color = NOM)) +
  geom_line() +
  scale_x_date(date_labels = "%b %Y", date_breaks = "6 month") +
  #facet_wrap(~NOM)+  
  labs(title = "Monthly rainfall by health district",
       x = "Year",
       y = "Average monthly rainfall",
       color = "Health district") +
  map_theme()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  
        axis.text.y = element_text(angle = 45, hjust = 1))

##########################################################################
# plot time series with daily rainfall data
##########################################################################


rainfall_data <- summerised_rainfall %>%
  mutate(date = ymd(substr(variable, 2, 9)))

ggplot(rainfall_data, aes(x = date, y = daily_rainfall, color = NOM)) +
  geom_line() +
  scale_x_continuous(breaks = seq(min(rainfall_data$year), max(rainfall_data$year), by = 1)) +
  #facet_wrap(~NOM)+
  labs(title = "Daily rainfall trends in each health district",
       x = "Year",
       y = "Rainfall",
       color = "Health district") +
  map_theme()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  
        axis.text.y = element_text(angle = 45, hjust = 1))


##########################################################################
# Interactive daily rainfall plot
##########################################################################

plot_ly(rainfall_data, x = ~date, y = ~daily_rainfall, 
        color = ~NOM, text = ~paste("Date: ", date, "<br>Rainfall: ", daily_rainfall)) %>%
  add_lines() %>%
  layout(title = "Daily rainfall trends in each health district",
         xaxis = list(title = "Date"),
         yaxis = list(title = "Rainfall"),
         hovermode = "closest")


##########################################################################
# individual health districts plot with monthly data
##########################################################################


rainfall_data$date <- as.Date(paste(rainfall_data$year, rainfall_data$month, "01", sep = "-"))
plots_folder <- file.path(plots, "Rainfall", "27-02-2024")

health_districts <- unique(rainfall_data$NOM)

for (district in health_districts) {
  
  district_data <- subset(rainfall_data, NOM == district)
  
  plot <- ggplot(data = district_data, aes(x = date, y = monthly_rainfall)) +
    geom_line() +
    scale_x_date(date_labels = "%b %Y", date_breaks = "4 month") +
    labs(title = paste("Rainfall trends for", district),
         x = "Date",
         y = "Average monthly rainfall") +
    map_theme()+
    theme(axis.text.x = element_text(angle = 45, hjust = 1),  
          axis.text.y = element_text(angle = 45, hjust = 1))
  
  print(plot)
  
  # plot_filename <- paste0(plots_folder, "/", gsub("\\s", "_", district), "_rainfall_plot.png")
  # ggsave(filename = plot_filename, plot = plot, width = 10, height = 6)
}




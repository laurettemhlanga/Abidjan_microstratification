###############################################################################################
####### WATER FREQUENCY/ FLOOD RISK ####################
###############################################################################
rm(list=ls())

source("~/Abidjan/load_path.R", echo=FALSE)

RainfallPlus <- file.path(Climatedata, "Extracted_ClimeServ_CHIRPS_")

WaterFreqDir <- file.path(NASAdata, "Abidjan Water Frequency")
WaterFreq_Q1 <- file.path(WaterFreqDir, "Abidjan_WF_Q1")
WaterFreq_Q2 <- file.path(WaterFreqDir, "Abidjan_WF_Q2")
WaterFreq_Q3 <- file.path(WaterFreqDir, "Abidjan_WF_Q3")
WaterFreq_Q4 <- file.path(WaterFreqDir, "Abidjan_WF_Q4")
WaterFreq_all <- file.path(WaterFreqDir, "Abidjan_WF_ALL")

##compute mode function
compute_mode <- function(x, ...) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# naming columns according to filename
extract_info <- function(file_path) {
  file_name <- basename(file_path)
  file_name_without_extension <- tools::file_path_sans_ext(file_name)
  return(file_name_without_extension)
}

### Data for all quarters
waterfreq <- list.files(file.path(WaterFreq_all), 
                          pattern = ".tif", full.names = TRUE)

waterfreq_data <- lapply(seq_along(waterfreq), 
                            function(x) raster::raster(waterfreq[[x]]))

plot_allQmean <- waterfreq_data %>%
  purrr::map(~raster::extract(., df_abidjan1,
                              fun = mean, df = TRUE)) %>%
  purrr::reduce(left_join, by = "ID")
colnames(plot_allQmean)[-1] <- sapply(waterfreq, extract_info)

###mode
plot_allQmode <- waterfreq_data %>%
  purrr::map(~raster::extract(., df_abidjan1,
                              fun = compute_mode, df =TRUE))%>%
  purrr::reduce(left_join, by = c("ID"))
colnames(plot_allQmode)[-1] <- sapply(waterfreq, extract_info)



####calculate mean across all quarters and write to variable summary
row_means <- rowMeans(plot_allQmean[, -1], na.rm = TRUE)
plot_allQmean$overall_meanWF <-row_means

#write.csv(plot_allQmean, file.path(WaterFreqDir, "mean_water_frequency.csv"), row.names = FALSE)
#write.csv(plot_allQmode, file.path(WaterFreqDir, "mode_water_frequency.csv"), row.names = FALSE)


# Variables <- read.csv(file.path(AbidjanDir, "Abidjan Data Variables.csv"))
# Variables$mean_WF <- plot_allQmean$overall_meanWF
# write.csv(Variables,file.path(AbidjanDir, "Abidjan Data Variables.csv"), row.names = FALSE)
  

#### Transform 
all_mean <- read.csv(file.path(WaterFreqDir, "mean_water_frequency.csv"))
all_mean$Healthdistrict <- df_abidjan1$NOM
all_mean$geometry <- df_abidjan1$geometry

column_names <- colnames(all_mean)[-c(1, ncol(all_mean))]
# Function to extract quarter and year
extract_quarter_year <- function(name) {
  parts <- unlist(strsplit(name, "_"))
  list(quarter = as.integer(gsub("[^0-9]", "", parts[length(parts) - 1])),
       year = as.integer(gsub("[^0-9]", "", parts[length(parts)])))
}
extracted_info <- lapply(column_names, extract_quarter_year)
quarters <- sapply(extracted_info, function(x) x$quarter)
years <- sapply(extracted_info, function(x) x$year)
plot_all_mean <- all_mean %>%
  pivot_longer(cols = -c(ID, Healthdistrict, geometry),
               names_to = "Quarter_Value",
               values_to = "Value") %>%
  mutate(year = as.integer(years[match(Quarter_Value, column_names)]),
         quarter = as.integer(quarters[match(Quarter_Value, column_names)]))



####mode
all_mode <- read.csv(file.path(WaterFreqDir, "mode_water_frequency.csv"))
#translate
translations <- c("0" = " 0-1%", "1" = "1-5%",
                  "5" = "5-25%", "25" = "25-50%", 
                  "50" = "50-75%", "75" = ">75%")

translate_value <- function(x) {
  return(translations[as.character(x)])
}
for (i in 2:42){
  all_mode[, i] <- sapply(all_mode[, i], translate_value)
}

all_mode$Healthdistrict <- df_abidjan1$"NOM"

column_names <- colnames(all_mean)[-c(1, ncol(all_mode))]  
col_info <- strsplit(column_names, "_") 
quarters <- sapply(col_info, "[[", 1) 
years <- sapply(col_info, "[[", 2)
plot_all_mode <- all_mode %>%
  pivot_longer(cols = -c(ID, Healthdistrict),
               names_to = "Quarter_Value",
               values_to = "Value") %>%
  mutate(year = as.integer(years[match(Quarter_Value, colnames(all_mode))]),
         quarter = quarters[match(Quarter_Value, colnames(all_mode))])

plot_all_mode <- left_join(plot_all_mode, df_abidjan1, by = c("Healthdistrict" = "NOM")) # %>%
  #drop_na()


########################PLOTS##################################
###all quarters in 2014
# plot_2014 <- all_mean %>%
#   dplyr::select (ends_with("2014"), Healthdistrict , geometry)

plot_2014 <- plot_all_mean %>%
  dplyr::filter(Quarter_Value %in% c("Q1_2014", "Q2_2014", "Q3_2014", "Q4_2014"))

ggplot()+
  geom_sf(data = plot_2014, aes(geometry = geometry, fill = Value))+
  scale_fill_viridis_c(name = "Mean Water Frequency", option = "D") +
  facet_wrap(~Quarter)+
  labs(title = "Water Frequency 2014")+
  map_theme()
  
  
###ALL years

# Define years from 2013 to 2023
years <- 2013:2023

# Create an empty list to store the plots
plot_list <- list()

# Loop through each year
for (year in years) {
 
  plot_data <- plot_all_mean %>%
    filter(Quarter_Value %in% c(paste0("Q1_", year), paste0("Q2_", year), paste0("Q3_", year), paste0("Q4_", year)))
  
  plot <- ggplot() +
    geom_sf(data = plot_data, aes(geometry = geometry, fill = Value)) +
    scale_fill_viridis_c(name = "Mean Water Frequency", option = "D") +
    facet_wrap(~Quarter) +
    labs(title = paste("Water Frequency", year)) +
    map_theme()
  
  plot_list[[as.character(year)]] <- plot
}

# Plot all the years
plot_list

#2023

plot_2023 <- plot_all_mean %>%
  dplyr::filter(Quarter_Value %in% c("Q1_2023", "Q2_2023", "Q3_2023", "Q4_2023"))

ggplot()+
  geom_sf(data = plot_2023, aes(geometry = geometry, fill = Value))+
  scale_fill_viridis_c(name = "Mean Water Frequency", option = "D") +
  #facet_wrap(~Quarter)+
  labs(title = "Water Frequency 2023")+
  map_theme()

#######Timeseries with flood points
flood_data <-read.csv(file.path(AbidjanDir, "Flood list.csv"))

ggplot() +
  geom_line(data = plot_all_mean, aes(x = quarter, y = Value,  color = Healthdistrict)) +
  geom_point(data = flood_data, aes(x = quarter, y = Rainfall, color = HealthDistrict), size =2) + ##figure out flood points per year
  labs(title = "Quarterly Water Frequency with Flood points",
      caption = "Highlighted points indicate floods in specific regions, 2023 only has Q1 data",
      x = "Quarter",
      y = "Mean Water Frequency",
      color = "Health District") +
  facet_wrap(~year)+
  theme_manuscript() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#mean water frequency per quarter
ggplot()+
  geom_sf(data = plot_all_mean, aes(geometry =geometry, fill= Value))+
  facet_wrap(~Quarter)+
  labs(title = "Water Frequency in Abidjan 2013-2023", fill = "Avg. Water Frequency", x = NULL, y = NULL) +
  map_theme()
  
##mean water frequency all quarters, all years

ggplot()+
  geom_sf(data = plot_all_mean, aes(geometry =geometry, fill= Value))+
  facet_wrap(~Quarter_Value)+
  labs(title = "Water Frequency in Abidjan 2013-2023", fill = "Avg. Water Frequency", x = NULL, y = NULL) +
  map_theme()


#plot map of overall mean WF 
ggplot()+
  geom_sf(data = all_mean, aes(geometry = geometry, fill =overall_meanWF), 
          color = "black", size = 0.2)+
  geom_text_repel(data =all_mean, 
                  aes(label=str_to_sentence(Healthdistrict), geometry=geometry), color ='black',
                  stat = "sf_coordinates", min.segment.length = 0, size = 3.5, force = 1)+
  scale_fill_gradient(name = "Water Frequency", low = "lightyellow", high = "darkblue") + 
  labs(title = "Average Water Freqency (2013-2023)", x= "", y = "") +
  map_theme()

  


############################################################################################################
 

####how to summarize for each health district to have a flood_frequency metric
### sum of all quaters/number of quarters using all_mean



ggplot()+
  geom_sf(data = plot_all_mode, aes(geometry = geometry, fill = Value)) + #replace fill = with classification of interest
  facet_wrap(~Year)+
  labs(title = "Water Frequency in Abidjan", fill = "Water Frequency", x = NULL, y = NULL) +
  map_theme() 


ggplot()+
  geom_sf(data = all_mean, aes(geometry = geometry, fill = Q1_2013)) + #replace fill = with classification of interest
  #facet_wrap(~Year, ncol = 3)+#
  labs(title = "Mean Water Frequency Q1_2013", fill = "Mean Water Frequency", x = NULL, y = NULL) +
  map_theme()

ggplot(plot_all_mode, aes(x = ID, y = Value)) +
  geom_point() +
  facet_wrap(~ Year, scales = "free_y") +
  labs(x = "Health District", y = "Mean Water Frequency", title = "Water Frequency Across Years")+
  scale_x_continuous(breaks = 1:15)
theme_manuscript() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



